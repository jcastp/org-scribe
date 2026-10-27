;;; test-companion-server.el --- Tests for the companion's HTTP server -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-server.el' and the change
;; events in `companion/org-scribe-companion.el' (Phase 2).  The server
;; listens on any free port; requests are made with a raw client, so Host,
;; Origin and the sizes can be set exactly.  Every test stops the server.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'url)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "capture" "templates" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-companion)

(defconst test-companion-server--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

;;; Fixtures

(defmacro test-companion-server--with-server (root-var &rest body)
  "Serve a copy of the demo novel on a free port, run BODY, stop the server.
ROOT-VAR is bound to the project directory."
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-server-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-port 0))
     (unwind-protect
         (progn
           (copy-directory (expand-file-name "examples/demo-novel" test-companion-server--repo)
                           ,root-var t t t)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           (org-scribe-companion--start ,root-var)
           ,@body)
       (org-scribe-companion--stop)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (org-scribe-companion--clear-cache)
       (delete-directory ,root-var t))))

(defun test-companion-server--port ()
  (org-scribe-companion-server-port))

(defun test-companion-server--token ()
  (org-scribe-companion-server-token))

(defun test-companion-server--wait (client predicate &optional seconds)
  "Let processes run until PREDICATE is non-nil or SECONDS pass.
CLIENT is only used while it is alive; otherwise any process may run."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output (and (process-live-p client) client) 0.05))))

(defun test-companion-server--raw (request)
  "Send the raw string REQUEST and return the whole answer as a unibyte string."
  (let* ((answer "")
         (client (make-network-process
                  :name "test-companion-client" :host "127.0.0.1"
                  :service (test-companion-server--port) :coding 'binary
                  :noquery t
                  :filter (lambda (_p chunk) (setq answer (concat answer chunk))))))
    (unwind-protect
        (progn
          (process-send-string client (encode-coding-string request 'utf-8))
          (test-companion-server--wait client (lambda () (not (process-live-p client))))
          answer)
      (when (process-live-p client) (delete-process client)))))

(cl-defun test-companion-server--get (path &key (token t) host origin (method "GET")
                                           body content-type)
  "Request PATH and return (STATUS HEADERS BODY).
TOKEN t sends the right token as a header, a string sends that, nil none.
HOST and ORIGIN override the defaults; BODY and CONTENT-TYPE make a request body."
  (let* ((port (test-companion-server--port))
         (raw (test-companion-server--raw
               (concat
                (format "%s %s HTTP/1.1\r\n" method path)
                (format "Host: %s\r\n" (or host (format "127.0.0.1:%d" port)))
                (cond ((eq token t)
                       (format "X-Org-Scribe-Token: %s\r\n" (test-companion-server--token)))
                      ((stringp token) (format "X-Org-Scribe-Token: %s\r\n" token)))
                (when origin (format "Origin: %s\r\n" origin))
                (when content-type (format "Content-Type: %s\r\n" content-type))
                (when body (format "Content-Length: %d\r\n" (string-bytes body)))
                "\r\n" (or body ""))))
         (split (string-search "\r\n\r\n" raw)))
    (list (and (string-match "\\`HTTP/1\\.1 \\([0-9]+\\)" raw)
               (string-to-number (match-string 1 raw)))
          (and split (substring raw 0 split))
          (and split (decode-coding-string (substring raw (+ split 4)) 'utf-8)))))

(defun test-companion-server--status (result) (car result))
(defun test-companion-server--body (result) (nth 2 result))

;;; Routes

(ert-deftest test-companion-server-index-page ()
  (test-companion-server--with-server root
    (let ((result (test-companion-server--get "/" :token nil)))
      (should (= 200 (car result)))
      (should (string-match-p "Content-Type: text/html" (nth 1 result)))
      (should (string-match-p "<script type=\"module\"" (nth 2 result))))))

(ert-deftest test-companion-server-static-files ()
  (test-companion-server--with-server root
    (let ((js (test-companion-server--get "/static/app.js" :token nil))
          (css (test-companion-server--get "/static/style.css" :token nil)))
      (should (= 200 (car js)))
      (should (string-match-p "text/javascript" (nth 1 js)))
      (should (= 200 (car css)))
      (should (string-match-p "text/css" (nth 1 css))))))

(ert-deftest test-companion-server-project-with-token ()
  (test-companion-server--with-server root
    (let* ((result (test-companion-server--get "/api/project"))
           (parsed (json-parse-string (test-companion-server--body result) :object-type 'alist)))
      (should (= 200 (car result)))
      (should (string-match-p "application/json" (nth 1 result)))
      (should (equal "The Morning Routine" (alist-get 'title (alist-get 'project parsed))))
      (should (= 3 (length (alist-get 'acts parsed)))))))

(ert-deftest test-companion-server-token-in-the-query-also-works ()
  (test-companion-server--with-server root
    (let ((result (test-companion-server--get
                   (format "/api/project?t=%s" (test-companion-server--token))
                   :token nil)))
      (should (= 200 (car result))))))

(ert-deftest test-companion-server-strings-are-the-project-languages ()
  (test-companion-server--with-server root
    (let ((parsed (json-parse-string
                   (test-companion-server--body (test-companion-server--get "/api/strings"))
                   :object-type 'alist)))
      (should (equal "Project" (alist-get 'companion-ui-project parsed)))
      (should (alist-get 'companion-ui-loading parsed))
      (should-not (alist-get 'default-scene-name parsed)))
    ;; a Spanish project gets Spanish strings
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Writing project: P\n# Type: novel\n# Language: es\n"))
    (org-scribe-project-type-cache-clear)
    (let ((parsed (json-parse-string
                   (test-companion-server--body (test-companion-server--get "/api/strings"))
                   :object-type 'alist)))
      (should (equal "Proyecto" (alist-get 'companion-ui-project parsed))))))

(ert-deftest test-companion-server-non-ascii-title-survives ()
  (test-companion-server--with-server root
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Writing project: Ñandú\n# Type: novel\n# Title: Ñandú y áéí «sí»\n# Language: es\n"))
    (org-scribe-project-type-cache-clear)
    (let ((parsed (json-parse-string
                   (test-companion-server--body (test-companion-server--get "/api/project"))
                   :object-type 'alist)))
      (should (equal "Ñandú y áéí «sí»" (alist-get 'title (alist-get 'project parsed)))))))

(ert-deftest test-companion-server-unknown-path-is-404 ()
  (test-companion-server--with-server root
    (should (= 404 (car (test-companion-server--get "/nope" :token nil))))
    (should (= 404 (car (test-companion-server--get "/api/nope"))))
    (should (= 404 (car (test-companion-server--get "/static/missing.js" :token nil))))))

(ert-deftest test-companion-server-other-methods-are-405 ()
  (test-companion-server--with-server root
    (should (= 405 (car (test-companion-server--get "/" :method "DELETE"))))
    (should (= 405 (car (test-companion-server--get "/api/project" :method "PUT"))))))

(ert-deftest test-companion-server-works-with-url-retrieve ()
  "A real HTTP client, not just the raw one, gets the project."
  (test-companion-server--with-server root
    (let* ((url-request-extra-headers
            `(("X-Org-Scribe-Token" . ,(test-companion-server--token))))
           (buffer (url-retrieve-synchronously
                    (format "http://127.0.0.1:%d/api/project" (test-companion-server--port))
                    t t 10)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (looking-at "HTTP/1.1 200"))
            (should (search-forward "The Morning Routine" nil t)))
        (kill-buffer buffer)))))

;;; Security

(ert-deftest test-companion-server-api-without-token-is-403 ()
  (test-companion-server--with-server root
    (should (= 403 (car (test-companion-server--get "/api/project" :token nil))))
    (should (= 403 (car (test-companion-server--get "/api/strings" :token "wrong"))))
    (should (= 403 (car (test-companion-server--get "/api/events" :token nil))))
    (should (= 403 (car (test-companion-server--get
                         "/api/command" :token nil :method "POST"
                         :body "{}" :content-type "application/json"))))))

(ert-deftest test-companion-server-foreign-host-is-403 ()
  (test-companion-server--with-server root
    (dolist (host (list "evil.example.com" "127.0.0.1" "127.0.0.1:1"
                        (format "evil.example.com:%d" (test-companion-server--port))))
      (should (= 403 (car (test-companion-server--get "/" :host host :token nil))))
      (should (= 403 (car (test-companion-server--get "/api/project" :host host)))))
    ;; localhost is ours as well
    (should (= 200 (car (test-companion-server--get
                         "/" :host (format "localhost:%d" (test-companion-server--port))))))))

(ert-deftest test-companion-server-missing-host-is-403 ()
  (test-companion-server--with-server root
    (let ((raw (test-companion-server--raw "GET / HTTP/1.1\r\n\r\n")))
      (should (string-prefix-p "HTTP/1.1 403" raw)))))

(ert-deftest test-companion-server-path-traversal-is-refused ()
  (test-companion-server--with-server root
    (dolist (path '("/static/../org-scribe-companion.el"
                    "/static/%2e%2e/org-scribe-companion.el"
                    "/static/..%2forg-scribe-companion.el"
                    "/static/%2e%2e%2f%2e%2e%2fCLAUDE.md"
                    "/static//etc/passwd"
                    "/static/..\\org-scribe-companion.el"
                    "/static/web/../../org-scribe-companion.el"))
      (let ((result (test-companion-server--get path :token nil)))
        (should (member (car result) '(403 404)))
        (should-not (string-match-p "defun\\|provide" (or (nth 2 result) "")))))))

(ert-deftest test-companion-server-only-whitelisted-extensions ()
  (test-companion-server--with-server root
    (let ((file (expand-file-name "secret.el" org-scribe-companion-server--web-directory)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "(secret)"))
            (should (= 404 (car (test-companion-server--get "/static/secret.el" :token nil)))))
        (delete-file file)))))

(ert-deftest test-companion-server-symlink-out-of-the-web-directory-is-refused ()
  (test-companion-server--with-server root
    (let ((link (expand-file-name "leak.txt" org-scribe-companion-server--web-directory))
          (target (expand-file-name "target.txt" root)))
      (with-temp-file target (insert "outside"))
      (unwind-protect
          (progn
            (make-symbolic-link target link)
            (should (= 404 (car (test-companion-server--get "/static/leak.txt" :token nil)))))
        (when (file-symlink-p link) (delete-file link))))))

(ert-deftest test-companion-server-post-must-be-json ()
  (test-companion-server--with-server root
    (should (= 415 (car (test-companion-server--get
                         "/api/command" :method "POST" :body "{}"
                         :content-type "text/plain"))))
    (should (= 415 (car (test-companion-server--get
                         "/api/command" :method "POST" :body "{}"))))))

(ert-deftest test-companion-server-post-foreign-origin-is-403 ()
  (test-companion-server--with-server root
    (let ((body "{\"op\":\"save\"}"))
      (should (= 403 (car (test-companion-server--get
                           "/api/command" :method "POST" :body body
                           :content-type "application/json"
                           :origin "http://evil.example.com"))))
      (should (= 403 (car (test-companion-server--get
                           "/api/command" :method "POST" :body body
                           :content-type "application/json"
                           :origin "null"))))
      (should (= 200 (car (test-companion-server--get
                           "/api/command" :method "POST" :body body
                           :content-type "application/json"
                           :origin (format "http://127.0.0.1:%d"
                                           (test-companion-server--port)))))))))

(ert-deftest test-companion-server-oversized-body-is-413 ()
  (test-companion-server--with-server root
    (let ((raw (test-companion-server--raw
                (format (concat "POST /api/command HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n"
                                "X-Org-Scribe-Token: %s\r\nContent-Type: application/json\r\n"
                                "Content-Length: %d\r\n\r\n{}")
                        (test-companion-server--port) (test-companion-server--token)
                        (1+ org-scribe-companion-server--max-body)))))
      (should (string-prefix-p "HTTP/1.1 413" raw)))))

(ert-deftest test-companion-server-malformed-requests-are-400 ()
  (test-companion-server--with-server root
    (let ((host (format "Host: 127.0.0.1:%d\r\n" (test-companion-server--port))))
      (should (string-prefix-p "HTTP/1.1 400"
                               (test-companion-server--raw
                                (concat "GARBAGE\r\n" host "\r\n"))))
      (should (string-prefix-p "HTTP/1.1 400"
                               (test-companion-server--raw
                                (concat "GET / HTTP/1.1\r\n" host host "\r\n"))))
      (should (string-prefix-p "HTTP/1.1 400"
                               (test-companion-server--raw
                                (concat "POST /api/command HTTP/1.1\r\n" host
                                        "Content-Length: abc\r\n\r\n"))))
      (should (string-prefix-p "HTTP/1.1 400"
                               (test-companion-server--raw
                                (concat "POST /api/command HTTP/1.1\r\n" host
                                        "Transfer-Encoding: chunked\r\n\r\n")))))))

(ert-deftest test-companion-server-listens-on-loopback-only ()
  (test-companion-server--with-server root
    (should (equal [127 0 0 1 ?\0] (vconcat (seq-take (process-contact
                                                      org-scribe-companion-server--process
                                                      :local)
                                                     4)
                                           [?\0])))))

;;; Commands

(ert-deftest test-companion-server-unknown-command-is-unsupported ()
  (test-companion-server--with-server root
    (let* ((result (test-companion-server--get
                    "/api/command" :method "POST" :body "{\"op\":\"fly\",\"args\":{}}"
                    :content-type "application/json; charset=utf-8"))
           (parsed (json-parse-string (test-companion-server--body result) :object-type 'alist)))
      (should (= 200 (car result)))
      (should (eq :false (alist-get 'ok parsed)))
      (should (equal "unsupported" (alist-get 'code (alist-get 'error parsed))))
      (should (string-match-p "fly" (alist-get 'message (alist-get 'error parsed)))))
    (should (= 400 (car (test-companion-server--get
                         "/api/command" :method "POST" :body "not json"
                         :content-type "application/json"))))))

;;; Event streams

(defun test-companion-server--subscribe ()
  "Open an event stream; return (PROCESS . ACCUMULATOR-CELL)."
  (let* ((cell (list ""))
         (client (make-network-process
                  :name "test-companion-sse" :host "127.0.0.1"
                  :service (test-companion-server--port) :coding 'binary :noquery t
                  :filter (lambda (_p chunk) (setcar cell (concat (car cell) chunk))))))
    (process-send-string
     client (format "GET /api/events?t=%s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n\r\n"
                    (test-companion-server--token) (test-companion-server--port)))
    (test-companion-server--wait client (lambda () (string-match-p ": connected" (car cell))))
    (cons client cell)))

(ert-deftest test-companion-server-event-stream-headers ()
  (test-companion-server--with-server root
    (let ((stream (test-companion-server--subscribe)))
      (unwind-protect
          (progn
            (should (string-prefix-p "HTTP/1.1 200" (cadr stream)))
            (should (string-match-p "Content-Type: text/event-stream" (cadr stream)))
            (should (= 1 (length org-scribe-companion-server--subscribers))))
        (delete-process (car stream))))))

(ert-deftest test-companion-server-changed-after-a-buffer-edit ()
  (test-companion-server--with-server root
    (let ((org-scribe-companion--debounce-seconds 0.05)
          (stream (test-companion-server--subscribe)))
      (unwind-protect
          (progn
            (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
              (goto-char (point-max))
              (insert "\nA new line.\n"))
            (test-companion-server--wait (car stream)
                                         (lambda () (string-match-p "event: changed" (cadr stream))))
            (should (string-match-p "event: changed\ndata: {\"revision\":\"m[^\"]*\\*[^\"]*\"}"
                                    (cadr stream)))
            ;; the revision in the event is the snapshot's
            (let ((revision (alist-get 'revision (json-parse-string
                                                  (test-companion-server--body
                                                   (test-companion-server--get "/api/project"))
                                                  :object-type 'alist))))
              (should (string-match-p (regexp-quote revision) (cadr stream)))))
        (delete-process (car stream))))))

(ert-deftest test-companion-server-many-edits-send-one-event ()
  (test-companion-server--with-server root
    (let ((org-scribe-companion--debounce-seconds 0.2)
          (stream (test-companion-server--subscribe)))
      (unwind-protect
          (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
            (goto-char (point-max))
            (dotimes (_ 20) (insert "x"))
            (test-companion-server--wait (car stream)
                                         (lambda () (string-match-p "event: changed" (cadr stream))))
            (test-companion-server--wait (car stream) #'ignore 0.5)
            (should (= 1 (with-temp-buffer
                           (insert (cadr stream))
                           (how-many "event: changed" (point-min) (point-max))))))
        (delete-process (car stream))))))

(ert-deftest test-companion-server-changed-after-a-save ()
  (test-companion-server--with-server root
    (let ((org-scribe-companion--debounce-seconds 0.05))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (goto-char (point-max))
        (insert "\nx\n")
        (setq org-scribe-companion--last-revision nil))
      (let ((stream (test-companion-server--subscribe)))
        (unwind-protect
            (progn
              (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
                (save-buffer))
              (test-companion-server--wait (car stream)
                                           (lambda () (string-match-p "event: changed" (cadr stream))))
              (should (string-match-p "event: changed" (cadr stream))))
          (delete-process (car stream)))))))

(ert-deftest test-companion-server-ping-keeps-the-stream-alive ()
  (test-companion-server--with-server root
    (let ((stream (test-companion-server--subscribe)))
      (unwind-protect
          (progn
            (org-scribe-companion-server--ping)
            (test-companion-server--wait (car stream)
                                         (lambda () (string-match-p ": ping" (cadr stream))))
            (should (string-match-p ": ping" (cadr stream))))
        (delete-process (car stream))))))

(ert-deftest test-companion-server-dead-subscribers-are-dropped ()
  (test-companion-server--with-server root
    (let ((stream (test-companion-server--subscribe)))
      (delete-process (car stream))
      (test-companion-server--wait
       (car stream) (lambda () (null org-scribe-companion-server--subscribers)))
      (should (null org-scribe-companion-server--subscribers))
      ;; and a broadcast to nobody is harmless
      (org-scribe-companion-server-broadcast "changed" (list :revision "x")))))

(ert-deftest test-companion-server-broadcast-right-after-a-browser-leaves-does-not-hang ()
  "The server has not yet seen the close; sending must still not block."
  (test-companion-server--with-server root
    (let ((stream (test-companion-server--subscribe)))
      (delete-process (car stream))
      (org-scribe-companion-server-broadcast "changed" (list :revision "x"))
      (org-scribe-companion-server-broadcast "changed" (list :revision "y"))
      (org-scribe-companion--stop)
      (should-not (org-scribe-companion-server-running-p)))))

(ert-deftest test-companion-server-stop-says-bye-and-closes ()
  (test-companion-server--with-server root
    (let ((stream (test-companion-server--subscribe)))
      (org-scribe-companion--stop)
      (test-companion-server--wait (car stream) (lambda () (not (process-live-p (car stream)))))
      (should (string-match-p "event: bye" (cadr stream)))
      (should-not (process-live-p (car stream)))
      (should-not (org-scribe-companion-server-running-p))
      (should (null org-scribe-companion--hooked-buffers)))))

;;; Lifecycle

(ert-deftest test-companion-server-start-is-idempotent-and-token-is-fresh ()
  (test-companion-server--with-server root
    (let ((port (test-companion-server--port))
          (token (test-companion-server--token)))
      (should (= 64 (length token)))
      (should (string-match-p "\\`[0-9a-f]+\\'" token))
      (should (equal (org-scribe-companion--start root)
                     (format "http://127.0.0.1:%d/?t=%s" port token)))
      (should (= port (test-companion-server--port)))
      (org-scribe-companion--stop)
      (org-scribe-companion--start root)
      (should-not (equal token (test-companion-server--token))))))

(ert-deftest test-companion-server-busy-port-falls-back ()
  (let ((blocker (make-network-process :name "blocker" :server t :host "127.0.0.1"
                                       :service t :family 'ipv4 :noquery t)))
    (unwind-protect
        (let ((busy (process-contact blocker :service)))
          (test-companion-server--with-server root
            (org-scribe-companion--stop)
            (let ((org-scribe-companion-port busy))
              (org-scribe-companion--start root)
              (should (org-scribe-companion-server-running-p))
              (should-not (= busy (test-companion-server--port))))))
      (delete-process blocker))))

(ert-deftest test-companion-server-open-calls-the-browser-function-with-the-url ()
  (test-companion-server--with-server root
    (org-scribe-companion--stop)
    (let* ((browsed-url nil)
           (org-scribe-companion-browser-function (lambda (url) (setq browsed-url url))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-scribe-companion--open root))
      (should (equal browsed-url (org-scribe-companion-server-url)))
      (should (string-match-p "\\`http://127\\.0\\.0\\.1:[0-9]+/\\?t=[0-9a-f]+\\'" browsed-url)))))

(ert-deftest test-companion-server-stop-command-reports ()
  (test-companion-server--with-server root
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (org-scribe-companion-stop)
        (should (equal said (org-scribe-msg 'companion-stopped)))
        (org-scribe-companion-stop)
        (should (equal said (org-scribe-msg 'companion-not-running)))))))

(ert-deftest test-companion-server-switching-project-serves-the-new-one ()
  (test-companion-server--with-server root
    (let ((other (file-name-as-directory (make-temp-file "test-companion-other-" t))))
      (unwind-protect
          (progn
            (copy-directory (expand-file-name "examples/demo-novel" test-companion-server--repo)
                            other t t t)
            (with-temp-file (expand-file-name ".org-scribe-project" other)
              (insert "# Writing project: Other\n# Type: novel\n# Title: Other Novel\n# Language: en\n"))
            (let ((stream (test-companion-server--subscribe))
                  (url-before (org-scribe-companion-server-url)))
              (unwind-protect
                  (progn
                    (org-scribe-project-type-cache-clear)
                    (should (equal url-before (org-scribe-companion--start other)))
                    (should (equal other (org-scribe-companion-server-root)))
                    (let ((parsed (json-parse-string
                                   (test-companion-server--body
                                    (test-companion-server--get "/api/project"))
                                   :object-type 'alist)))
                      (should (equal "Other Novel"
                                     (alist-get 'title (alist-get 'project parsed))))))
                (delete-process (car stream)))))
        (dolist (buffer (buffer-list))
          (when-let* ((file (buffer-file-name buffer)))
            (when (string-prefix-p other file)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (delete-directory other t)))))

(ert-deftest test-companion-server-messages-in-both-languages ()
  (dolist (key '(companion-opened companion-stopped companion-not-running
                 companion-port-fallback companion-error-unsupported-op
                 companion-http-forbidden companion-http-not-found
                 companion-http-method companion-http-bad-request
                 companion-http-too-large companion-http-not-json
                 companion-ui-project companion-ui-loading companion-ui-error))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

(provide 'test-companion-server)
;;; test-companion-server.el ends here
