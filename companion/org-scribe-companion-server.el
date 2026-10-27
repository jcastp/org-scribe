;;; org-scribe-companion-server.el --- Visual companion: HTTP server -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; A small HTTP server on Emacs's built-in `make-network-process', for the
;; companion's browser page.  Not `simple-httpd': that is one global server
;; with a global root and port, which org-roam-ui also sets.
;;
;; What it serves (see the implementation plan, "The HTTP API"):
;;   GET  /              the page
;;   GET  /static/...    files under companion/web/
;;   GET  /api/project   the snapshot
;;   GET  /api/strings   the UI strings
;;   GET  /api/events    Server-Sent Events
;;   POST /api/command   one write operation
;;
;; Security: it listens on 127.0.0.1 only; every request's Host header must
;; be this server's own; every /api/ request carries the per-start token; a
;; POST must be JSON and, when it names an Origin, our own; request bodies
;; are capped.  Every non-SSE answer is `Connection: close'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url-util)
(require 'org-scribe-messages)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)

(defvar org-scribe-companion-port)

;;; State

(defconst org-scribe-companion-server--web-directory
  (expand-file-name "web" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the static files, found relative to this library.")

(defconst org-scribe-companion-server--max-body (* 1024 1024)
  "Largest request body accepted, in bytes.")

(defconst org-scribe-companion-server--max-head 16384
  "Largest request line plus headers accepted, in bytes.")

(defvar org-scribe-companion-server--ping-seconds 25
  "Seconds between the keep-alive comments on event streams.")

(defvar org-scribe-companion-server--request-timeout 10
  "Seconds a connection may take to send a whole request.")

(defvar org-scribe-companion-server--process nil "The listening process, or nil.")
(defvar org-scribe-companion-server--port nil "The port actually listened on.")
(defvar org-scribe-companion-server--token nil "The token of this server start.")
(defvar org-scribe-companion-server--root nil "The project root being served.")
(defvar org-scribe-companion-server--subscribers nil "Live event-stream processes.")
(defvar org-scribe-companion-server--ping-timer nil)

(defun org-scribe-companion-server-running-p ()
  "Return non-nil when the server is listening."
  (and org-scribe-companion-server--process
       (process-live-p org-scribe-companion-server--process)))

(defun org-scribe-companion-server-port () "The port served on." org-scribe-companion-server--port)
(defun org-scribe-companion-server-token () "The token of this start." org-scribe-companion-server--token)
(defun org-scribe-companion-server-root () "The project root served." org-scribe-companion-server--root)

(defun org-scribe-companion-server-set-root (root)
  "Serve the project at ROOT from now on."
  (setq org-scribe-companion-server--root (file-name-as-directory (expand-file-name root))))

(defun org-scribe-companion-server-url ()
  "Return the URL that opens the page, token included."
  (format "http://127.0.0.1:%d/?t=%s"
          org-scribe-companion-server--port org-scribe-companion-server--token))

;;; Token

(defun org-scribe-companion-server--make-token ()
  "Return a fresh random token (64 hex characters).
Read from /dev/urandom when there is one, else hashed from Emacs's own
random state."
  (or (ignore-errors
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally "/dev/urandom" nil 0 32)
          (when (= (buffer-size) 32)
            (mapconcat (lambda (byte) (format "%02x" byte))
                       (buffer-string) ""))))
      (secure-hash 'sha256 (format "%s%s%s%s" (random t) (current-time)
                                   (emacs-pid) (random)))))

;;; Responses

(defconst org-scribe-companion-server--reasons
  '((200 . "OK") (400 . "Bad Request") (403 . "Forbidden") (404 . "Not Found")
    (405 . "Method Not Allowed") (413 . "Payload Too Large")
    (415 . "Unsupported Media Type") (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error") (501 . "Not Implemented"))
  "Status code -> reason phrase.")

(defun org-scribe-companion-server--bytes (string)
  "Return STRING as UTF-8 bytes."
  (if (multibyte-string-p string) (encode-coding-string string 'utf-8-unix) string))

(defun org-scribe-companion-server--close (proc)
  "Close the connection PROC."
  (when (process-live-p proc) (delete-process proc)))

(defun org-scribe-companion-server--respond (proc status type body)
  "Answer on PROC with STATUS, content TYPE and BODY, then close."
  (process-put proc 'handled t)
  (let ((bytes (org-scribe-companion-server--bytes body)))
    (ignore-errors
      (process-send-string
       proc
       (concat
        (encode-coding-string
         (format (concat "HTTP/1.1 %d %s\r\nContent-Type: %s\r\n"
                         "Content-Length: %d\r\nConnection: close\r\n"
                         "Cache-Control: no-store\r\n"
                         "X-Content-Type-Options: nosniff\r\n"
                         "Content-Security-Policy: default-src 'self'; "
                         "style-src 'self' 'unsafe-inline'; img-src 'self' data:\r\n\r\n")
                 status (alist-get status org-scribe-companion-server--reasons "")
                 type (length bytes))
         'utf-8-unix)
        bytes))))
  (org-scribe-companion-server--close proc))

(defun org-scribe-companion-server--respond-json (proc status plist)
  "Answer on PROC with STATUS and the JSON form of PLIST."
  (org-scribe-companion-server--respond
   proc status "application/json; charset=utf-8" (json-serialize plist)))

(defun org-scribe-companion-server--fail (proc status code message-key)
  "Answer on PROC with STATUS and a JSON error: CODE and the message MESSAGE-KEY."
  (org-scribe-companion-server--respond-json
   proc status
   (list :ok :false :error (list :code code :message (org-scribe-msg message-key)))))

;;; Parsing

(defun org-scribe-companion-server--parse-head (head)
  "Parse HEAD, the request line and headers, into a plist, or nil if malformed.
The plist has `:method', `:target' and `:headers' (an alist with
lower-case names).  A repeated Host header makes the request malformed."
  (let* ((lines (split-string (decode-coding-string head 'utf-8-unix t) "\r\n"))
         (request (split-string (car lines) " "))
         headers)
    (when (and (= (length request) 3)
               (string-prefix-p "HTTP/1." (nth 2 request)))
      (catch 'bad
        (dolist (line (cdr lines))
          (let ((colon (string-search ":" line)))
            (unless (and colon (> colon 0)) (throw 'bad nil))
            (let ((name (downcase (substring line 0 colon)))
                  (value (string-trim (substring line (1+ colon)))))
              (when (and (equal name "host") (assoc "host" headers)) (throw 'bad nil))
              (push (cons name value) headers))))
        (list :method (nth 0 request) :target (nth 1 request)
              :headers (nreverse headers))))))

(defun org-scribe-companion-server--split-target (target)
  "Split TARGET into (PATH . QUERY-ALIST), with the path percent-decoded."
  (let* ((mark (string-search "?" target))
         (path (if mark (substring target 0 mark) target))
         (query (and mark (substring target (1+ mark)))))
    (cons (decode-coding-string (url-unhex-string path) 'utf-8-unix t)
          (and query (url-parse-query-string query)))))

;;; Checks

(defun org-scribe-companion-server--host-ok-p (headers)
  "Return non-nil when the Host in HEADERS is this server's own.
Blocks DNS rebinding."
  (let ((host (cdr (assoc "host" headers))))
    (and host
         (member (downcase host)
                 (list (format "127.0.0.1:%d" org-scribe-companion-server--port)
                       (format "localhost:%d" org-scribe-companion-server--port))))))

(defun org-scribe-companion-server--origin-ok-p (headers)
  "Return non-nil when HEADERS carry no Origin, or this server's own."
  (let ((origin (cdr (assoc "origin" headers))))
    (or (null origin)
        (member origin (list (format "http://127.0.0.1:%d" org-scribe-companion-server--port)
                             (format "http://localhost:%d" org-scribe-companion-server--port))))))

(defun org-scribe-companion-server--token-ok-p (headers query)
  "Return non-nil when HEADERS or QUERY carry the right token."
  (let ((given (or (cdr (assoc "x-org-scribe-token" headers))
                   (cadr (assoc "t" query)))))
    (and (stringp given)
         org-scribe-companion-server--token
         (string= given org-scribe-companion-server--token))))

;;; Static files

(defconst org-scribe-companion-server--mime-types
  '(("html" . "text/html; charset=utf-8") ("css" . "text/css; charset=utf-8")
    ("js" . "text/javascript; charset=utf-8") ("mjs" . "text/javascript; charset=utf-8")
    ("json" . "application/json; charset=utf-8") ("svg" . "image/svg+xml")
    ("png" . "image/png") ("jpg" . "image/jpeg") ("ico" . "image/x-icon")
    ("woff2" . "font/woff2") ("txt" . "text/plain; charset=utf-8"))
  "The only extensions served, with their MIME types.")

(defun org-scribe-companion-server--static-file (relative)
  "Return the file for RELATIVE under the web directory, or nil if not servable.
Refuses `..' and absolute paths, anything that leaves the directory once
symlinks are resolved, directories, and any extension not in
`org-scribe-companion-server--mime-types'."
  (when (and (not (string-empty-p relative))
             (not (string-match-p "\\(\\`\\|/\\)\\.\\.\\(/\\|\\'\\)\\|[\0\\\\]\\|\\`/" relative))
             (assoc (downcase (or (file-name-extension relative) ""))
                    org-scribe-companion-server--mime-types))
    (let ((file (expand-file-name relative org-scribe-companion-server--web-directory)))
      (and (file-regular-p file)
           (file-in-directory-p file org-scribe-companion-server--web-directory)
           file))))

(defun org-scribe-companion-server--serve-static (proc relative)
  "Answer on PROC with the static file RELATIVE, or 404."
  (if-let* ((file (org-scribe-companion-server--static-file relative)))
      (org-scribe-companion-server--respond
       proc 200
       (cdr (assoc (downcase (file-name-extension file))
                   org-scribe-companion-server--mime-types))
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file)
         (buffer-string)))
    (org-scribe-companion-server--fail proc 404 "not-found" 'companion-http-not-found)))

;;; Event streams

(defun org-scribe-companion-server--event-bytes (event data)
  "Return the bytes of the SSE message EVENT with DATA, a plist."
  (org-scribe-companion-server--bytes
   (format "event: %s\ndata: %s\n\n" event (json-serialize data))))

(defun org-scribe-companion-server--send-to-subscribers (bytes)
  "Send BYTES to every live subscriber, dropping the dead ones.
Each subscriber is first given a chance to report that its browser went
away: writing to a socket whose peer has closed can block Emacs."
  (dolist (proc org-scribe-companion-server--subscribers)
    (when (process-live-p proc)
      (ignore-errors (accept-process-output proc 0))))
  (setq org-scribe-companion-server--subscribers
        (cl-remove-if-not #'process-live-p org-scribe-companion-server--subscribers))
  (dolist (proc org-scribe-companion-server--subscribers)
    (condition-case nil
        (process-send-string proc bytes)
      (error (delete-process proc))))
  (setq org-scribe-companion-server--subscribers
        (cl-remove-if-not #'process-live-p org-scribe-companion-server--subscribers)))

(defun org-scribe-companion-server-broadcast (event data)
  "Send the event EVENT (a string) with DATA (a plist) to every subscriber."
  (org-scribe-companion-server--send-to-subscribers
   (org-scribe-companion-server--event-bytes event data)))

(defun org-scribe-companion-server--ping ()
  "Send a keep-alive comment to every subscriber."
  (org-scribe-companion-server--send-to-subscribers ": ping\n\n"))

(defun org-scribe-companion-server--subscribe (proc)
  "Turn the connection PROC into an event-stream subscriber."
  (process-put proc 'handled t)
  (process-send-string
   proc
   (encode-coding-string
    (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
            "Cache-Control: no-cache\r\nConnection: keep-alive\r\n"
            "X-Content-Type-Options: nosniff\r\n\r\n: connected\n\n")
    'utf-8-unix))
  (push proc org-scribe-companion-server--subscribers))

;;; Routing

(defun org-scribe-companion-server--api-project (proc)
  "Answer on PROC with the snapshot."
  (condition-case err
      (org-scribe-companion-server--respond
       proc 200 "application/json; charset=utf-8"
       (org-scribe-companion--snapshot-json org-scribe-companion-server--root))
    (error
     (org-scribe-companion-server--respond-json
      proc 500 (list :ok :false :error (list :code "internal"
                                             :message (error-message-string err)))))))

(defun org-scribe-companion-server--api-command (proc request body)
  "Answer on PROC to the POST REQUEST with BODY."
  (let ((type (cdr (assoc "content-type" (plist-get request :headers)))))
    (if (not (and type (string-prefix-p "application/json" (downcase type))))
        (org-scribe-companion-server--fail proc 415 "invalid" 'companion-http-not-json)
      (let ((parsed (ignore-errors
                      (json-parse-string (decode-coding-string body 'utf-8-unix t)
                                         :object-type 'plist :null-object nil
                                         :false-object nil))))
        (if (not (and (listp parsed) (stringp (plist-get parsed :op))))
            (org-scribe-companion-server--fail proc 400 "invalid" 'companion-http-bad-request)
          (org-scribe-companion-server--respond-json
           proc 200
           (condition-case err
               (org-scribe-companion-commands-run
                (plist-get parsed :op) (plist-get parsed :args)
                org-scribe-companion-server--root)
             (error (org-scribe-companion-commands-error
                     "invalid" (error-message-string err))))))))))

(defun org-scribe-companion-server--route (proc request body)
  "Route REQUEST (with BODY) arriving on PROC."
  (let* ((headers (plist-get request :headers))
         (method (plist-get request :method))
         (split (org-scribe-companion-server--split-target (plist-get request :target)))
         (path (car split))
         (query (cdr split)))
    (cond
     ((not (org-scribe-companion-server--host-ok-p headers))
      (org-scribe-companion-server--fail proc 403 "forbidden" 'companion-http-forbidden))
     ((not (member method '("GET" "POST")))
      (org-scribe-companion-server--fail proc 405 "invalid" 'companion-http-method))
     ((string-prefix-p "/api/" path)
      (cond
       ((not (org-scribe-companion-server--token-ok-p headers query))
        (org-scribe-companion-server--fail proc 403 "forbidden" 'companion-http-forbidden))
       ((and (equal method "POST") (not (org-scribe-companion-server--origin-ok-p headers)))
        (org-scribe-companion-server--fail proc 403 "forbidden" 'companion-http-forbidden))
       ((and (equal method "GET") (equal path "/api/project"))
        (org-scribe-companion-server--api-project proc))
       ((and (equal method "GET") (equal path "/api/strings"))
        (org-scribe-companion-server--respond-json
         proc 200 (org-scribe-companion--strings org-scribe-companion-server--root)))
       ((and (equal method "GET") (equal path "/api/events"))
        (org-scribe-companion-server--subscribe proc))
       ((and (equal method "POST") (equal path "/api/command"))
        (org-scribe-companion-server--api-command proc request body))
       (t (org-scribe-companion-server--fail proc 404 "not-found" 'companion-http-not-found))))
     ((not (equal method "GET"))
      (org-scribe-companion-server--fail proc 405 "invalid" 'companion-http-method))
     ((equal path "/")
      (org-scribe-companion-server--serve-static proc "index.html"))
     ((string-prefix-p "/static/" path)
      (org-scribe-companion-server--serve-static proc (substring path (length "/static/"))))
     (t (org-scribe-companion-server--fail proc 404 "not-found" 'companion-http-not-found)))))

;;; Connections

(defun org-scribe-companion-server--try-handle (proc)
  "Handle the request buffered on PROC once it is complete."
  (let* ((buffer (process-get proc 'buffer))
         (end (string-search "\r\n\r\n" buffer)))
    (cond
     ((null end)
      (when (> (length buffer) org-scribe-companion-server--max-head)
        (org-scribe-companion-server--fail proc 431 "invalid" 'companion-http-bad-request)))
     (t
      (let* ((request (org-scribe-companion-server--parse-head (substring buffer 0 end)))
             (headers (plist-get request :headers))
             (length-text (cdr (assoc "content-length" headers)))
             (length (if length-text
                         (and (string-match-p "\\`[0-9]+\\'" length-text)
                              (string-to-number length-text))
                       0))
             (body-start (+ end 4)))
        (cond
         ((or (null request) (null length) (assoc "transfer-encoding" headers))
          (org-scribe-companion-server--fail proc 400 "invalid" 'companion-http-bad-request))
         ((> length org-scribe-companion-server--max-body)
          (org-scribe-companion-server--fail proc 413 "invalid" 'companion-http-too-large))
         ((< (- (length buffer) body-start) length))
         (t
          (condition-case err
              (org-scribe-companion-server--route
               proc request (substring buffer body-start (+ body-start length)))
            (error
             (message "org-scribe-companion: %s" (error-message-string err))
             (org-scribe-companion-server--close proc))))))))))

(defun org-scribe-companion-server--filter (proc chunk)
  "Accumulate CHUNK from the connection PROC and handle a complete request."
  (unless (process-get proc 'handled)
    (process-put proc 'buffer (concat (process-get proc 'buffer) chunk))
    (org-scribe-companion-server--try-handle proc)))

(defun org-scribe-companion-server--sentinel (proc _event)
  "Forget the connection PROC when it has closed."
  (unless (process-live-p proc)
    (setq org-scribe-companion-server--subscribers
          (delq proc org-scribe-companion-server--subscribers))))

(defun org-scribe-companion-server--accept (_server client _message)
  "Prepare the new connection CLIENT: a deadline for sending its request."
  (process-put client 'buffer "")
  (run-at-time org-scribe-companion-server--request-timeout nil
               (lambda ()
                 (when (and (process-live-p client) (not (process-get client 'handled)))
                   (delete-process client)))))

;;; Start and stop

(defun org-scribe-companion-server--listen (service)
  "Start listening on SERVICE (a port number, or t for any); return the process."
  (make-network-process
   :name "org-scribe-companion" :server t :host "127.0.0.1" :service service
   :family 'ipv4 :coding 'binary :noquery t
   :filter #'org-scribe-companion-server--filter
   :sentinel #'org-scribe-companion-server--sentinel
   :log #'org-scribe-companion-server--accept))

(defun org-scribe-companion-server-start (root &optional port)
  "Start the server for the project at ROOT, preferring PORT.
PORT defaults to `org-scribe-companion-port'.  When busy, the next ten
ports are tried, then any free one; 0 or t means any free port at once.
Returns the port.  Signals nothing if already running: it just returns
the port."
  (if (org-scribe-companion-server-running-p)
      org-scribe-companion-server--port
    (let* ((port (or port org-scribe-companion-port))
           (candidates (if (and (integerp port) (> port 0))
                           (number-sequence port (+ port 10))
                         nil))
           process)
      (while (and candidates (not process))
        (setq process (condition-case nil
                          (org-scribe-companion-server--listen (pop candidates))
                        (file-error nil))))
      (unless process
        (setq process (org-scribe-companion-server--listen t))
        (when (and (integerp port) (> port 0))
          (message "%s" (org-scribe-msg 'companion-port-fallback port
                                        (process-contact process :service)))))
      (setq org-scribe-companion-server--process process
            org-scribe-companion-server--port (process-contact process :service)
            org-scribe-companion-server--token (org-scribe-companion-server--make-token)
            org-scribe-companion-server--subscribers nil
            org-scribe-companion-server--ping-timer
            (run-at-time org-scribe-companion-server--ping-seconds
                         org-scribe-companion-server--ping-seconds
                         #'org-scribe-companion-server--ping))
      (org-scribe-companion-server-set-root root)
      org-scribe-companion-server--port)))

(defun org-scribe-companion-server-stop ()
  "Tell the subscribers goodbye, then close them and the server."
  (when (org-scribe-companion-server-running-p)
    (org-scribe-companion-server-broadcast "bye" nil))
  (when (timerp org-scribe-companion-server--ping-timer)
    (cancel-timer org-scribe-companion-server--ping-timer))
  (dolist (proc org-scribe-companion-server--subscribers)
    (when (process-live-p proc) (delete-process proc)))
  (when (process-live-p org-scribe-companion-server--process)
    (delete-process org-scribe-companion-server--process))
  (setq org-scribe-companion-server--process nil
        org-scribe-companion-server--port nil
        org-scribe-companion-server--token nil
        org-scribe-companion-server--root nil
        org-scribe-companion-server--subscribers nil
        org-scribe-companion-server--ping-timer nil))

(provide 'org-scribe-companion-server)

;;; org-scribe-companion-server.el ends here
