;;; org-scribe-dictionary.el --- Dictionary and language tools for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dictionary and language tools for creative writing:
;; - RAE (Real Academia Española) API integration for Spanish definitions
;; - Synonym lookup via WordReference
;; - Translation support (requires gt package)

;;; Code:

(require 'json)
(require 'url)
(require 'org)
(require 'eww)
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)
(require 'org-scribe-mythes)
(require 'org-scribe-wikcionario)

(declare-function powerthesaurus-lookup-dwim "powerthesaurus")

;;; Shared presentation

(defun org-scribe--side-window (buffer)
  "Display BUFFER in the language-tools side window and select it.
Shared by the definition and synonym lookups so both feel like one tool."
  (let ((window (display-buffer-in-side-window
                 buffer
                 `((side . right)
                   (window-width . ,org-scribe-sinonimo-window-width)
                   (window-parameters . ((no-delete-other-windows . t)))))))
    (when window (select-window window))
    window))

;;; RAE Dictionary API

(defun org-scribe-rae-format-conjugations (conjugations)
  "Format verb conjugations from the RAE API response."
  (when conjugations
    (insert "#+begin_example\n")
    (maphash
     (lambda (mood forms)
       (insert (format "\n%s:\n" (upcase (if (stringp mood) mood (symbol-name mood)))))
       (when (hash-table-p forms)
         (maphash
          (lambda (tense persons)
            (insert (format "  %s:\n" (if (stringp tense) tense (symbol-name tense))))
            (when (hash-table-p persons)
              (maphash
               (lambda (person form)
                 (insert (format "    %s: %s\n"
                                 (if (stringp person) person (symbol-name person))
                                 form)))
               persons)))
          forms)))
     conjugations)
    (insert "#+end_example\n\n")))

(defun org-scribe-rae-format-result (json-data palabra)
  "Format the JSON-DATA response from RAE API into an org-mode buffer.
PALABRA is the word that was looked up."
  (let* ((data (gethash "data" json-data))
         (word (gethash "word" data))
         (meanings (gethash "meanings" data)))
    (insert (format "#+TITLE: %s\n\n" word))

    ;; Process each meaning
    (dolist (meaning meanings)
      (let ((origin (gethash "origin" meaning))
            (senses (gethash "senses" meaning))
            (conjugations (gethash "conjugations" meaning)))

        ;; Etymology - origin is an object with 'text' or 'raw' field
        (when origin
          (insert "* Etimología\n")
          (let ((origin-text (or (gethash "text" origin)
                                 (gethash "raw" origin))))
            (when origin-text
              (insert (format "%s\n\n" origin-text)))))

        ;; Definitions
        (insert "* Definiciones\n\n")
        (let ((sense-num 1))
          (dolist (sense senses)
            (let ((category (gethash "category" sense))
                  (verb-category (gethash "verb_category" sense))
                  (description (gethash "description" sense))
                  (synonyms (gethash "synonyms" sense))
                  (antonyms (gethash "antonyms" sense)))

              ;; Build category string
              (let ((cat-str (cond
                              ((and category verb-category)
                               (format "/%s %s/" category verb-category))
                              (category (format "/%s/" category))
                              (t ""))))
                (insert (format "** %d. %s\n" sense-num cat-str)))

              ;; Description (this is the actual definition)
              (when description
                (insert (format "%s\n\n" description)))

              ;; Synonyms
              (when (and synonyms (> (length synonyms) 0))
                (insert (format "*** Sinónimos: %s\n\n"
                                (mapconcat 'identity synonyms ", "))))

              ;; Antonyms
              (when (and antonyms (> (length antonyms) 0))
                (insert (format "*** Antónimos: %s\n\n"
                                (mapconcat 'identity antonyms ", "))))

              (setq sense-num (1+ sense-num)))))

        ;; Conjugations (for verbs)
        (when conjugations
          (insert "* Conjugaciones\n\n")
          (org-scribe-rae-format-conjugations conjugations))))))

;;;###autoload
(defun org-scribe-rae-api-lookup (palabra)
  "Look up PALABRA in the RAE dictionary using the API.
Displays the word definition, etymology, and meanings in a buffer.
Includes improved error handling for network issues."
  (interactive "sPalabra a buscar en la RAE: ")
  (when (string-empty-p (string-trim palabra))
    (user-error (org-scribe-msg 'error-word-empty)))
  (let* ((url (format "https://rae-api.com/api/words/%s"
                      (url-hexify-string palabra)))
         (buffer-name (format "*RAE: %s*" palabra))
         (url-request-method "GET"))
    (url-retrieve
     url
     (lambda (status palabra buffer-name)
       (let ((response-buffer (current-buffer)))
         (unwind-protect
             (org-scribe-with-error-handling "org-scribe-rae-api-lookup"
               (if (plist-get status :error)
                   (message (org-scribe-msg 'error-word-lookup (plist-get status :error)))
                 ;; Move past HTTP headers
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 ;; Extract and decode the response body as UTF-8
                 (let* ((body-start (point))
                        (raw-body (buffer-substring-no-properties body-start (point-max)))
                        (decoded-body (decode-coding-string raw-body 'utf-8)))
                   (condition-case err
                       (let* ((json-object-type 'hash-table)
                              (json-array-type 'list)
                              (json-key-type 'string)
                              (json-data (json-read-from-string decoded-body))
                              (ok (gethash "ok" json-data))
                              (output-buffer (get-buffer-create buffer-name)))
                       (with-current-buffer output-buffer
                         (erase-buffer)
                         (org-mode)
                         (if ok
                             (org-scribe-rae-format-result json-data palabra)
                           ;; Word not found - show suggestions
                           (let ((suggestions (gethash "suggestions" json-data)))
                             (insert (format "* %s\n\n" (org-scribe-msg 'msg-word-not-found palabra)))
                             (insert (format "** %s\n" (org-scribe-msg 'msg-word-suggestions)))
                             (dolist (suggestion suggestions)
                               (insert (format "- %s\n" suggestion)))))
                         (goto-char (point-min)))
                       (display-buffer output-buffer))
                     (json-error
                      (message (org-scribe-msg 'error-word-parse err)))))))
           (when (buffer-live-p response-buffer)
             (kill-buffer response-buffer)))))
     (list palabra buffer-name)
     nil   ; no SILENT
     t)))  ; INHIBIT-COOKIES

;;;###autoload
(defun org-scribe-rae-api-random ()
  "Get a random word from the RAE dictionary."
  (interactive)
  (let* ((url "https://rae-api.com/api/random")
         (url-request-method "GET"))
    (url-retrieve
     url
     (lambda (status)
       (let ((response-buffer (current-buffer)))
         (unwind-protect
             (org-scribe-with-error-handling "org-scribe-rae-api-random"
               (if (plist-get status :error)
                   (message (org-scribe-msg 'error-random-word (plist-get status :error)))
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 ;; Extract and decode the response body as UTF-8
                 (let* ((body-start (point))
                        (raw-body (buffer-substring-no-properties body-start (point-max)))
                        (decoded-body (decode-coding-string raw-body 'utf-8)))
                   (condition-case err
                       (let* ((json-object-type 'hash-table)
                              (json-array-type 'list)
                              (json-key-type 'string)
                              (json-data (json-read-from-string decoded-body))
                              (palabra (gethash "word" (gethash "data" json-data))))
                         (when palabra
                           (org-scribe-rae-api-lookup palabra)))
                     (json-error
                      (message (org-scribe-msg 'error-random-word-parse err)))))))
           (when (buffer-live-p response-buffer)
             (kill-buffer response-buffer))))))))

;;; Definition Lookup (backend dispatch)

;; Two backends, selected by `org-scribe-dictionary-backend': a local
;; Wikcionario served by kiwix-serve, and the online rae-api.com service.
;; org-scribe never starts or supervises the local server — it probes, and
;; says so when nothing answers.

(defun org-scribe--wikcionario-show (word)
  "Show WORD's local Wikcionario article in a side window.
Return non-nil when an article was shown.  Discloses a substituted headword:
Wikcionario stores inflected forms as stubs pointing at the dictionary form,
so a lookup for one lands on the lemma's article and the writer must be told."
  (when-let* ((entry (org-scribe-wikcionario-resolve word))
              (headword (car entry))
              (url (cdr entry)))
    (let* ((buffer (generate-new-buffer "*temp-dictionary*"))
           (side-window (org-scribe--side-window buffer)))
      ;; `display-buffer-in-side-window' can decline; fall back to the
      ;; selected window rather than signalling on a nil window.
      (if side-window
          (with-selected-window side-window
            (eww url)
            (kill-buffer buffer))
        (kill-buffer buffer)
        (eww url)))
    (unless (string-equal-ignore-case headword word)
      (message (org-scribe-msg 'msg-dictionary-lemma-used word headword)))
    t))

(defun org-scribe--wikcionario-report-miss (word)
  "Report that WORD has no local entry, offering suggestions when there are any."
  (if-let* ((suggestions (org-scribe-wikcionario-suggest word)))
      (let ((buffer (get-buffer-create (format "*Dictionary: %s*" word))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-mode)
            (insert (format "* %s\n\n" (org-scribe-msg 'msg-word-not-found word)))
            (insert (format "** %s\n" (org-scribe-msg 'msg-word-suggestions)))
            (dolist (suggestion suggestions)
              (insert (format "- %s\n" suggestion))))
          (goto-char (point-min))
          (read-only-mode 1))
        (org-scribe--side-window buffer))
    (message (org-scribe-msg 'msg-dictionary-not-found word))))

;;;###autoload
(defun org-scribe-dictionary-lookup (palabra &optional reprobe)
  "Look up PALABRA's definition, offline or online.

Uses a local Wikcionario or the online RAE API according to
`org-scribe-dictionary-backend'.  Under the default `auto' the local server
is tried first and the RAE used when it is not running.

With a prefix argument, REPROBE discards the cached availability of the local
server — use it after starting kiwix-serve mid-session, since availability is
probed once and then trusted so that lookups cost no latency."
  (interactive "sPalabra a buscar: \nP")
  (when (string-empty-p (string-trim palabra))
    (user-error (org-scribe-msg 'error-word-empty)))
  (let* ((backend org-scribe-dictionary-backend)
         (local (unless (eq backend 'rae-api)
                  (org-scribe-wikcionario-available-p reprobe))))
    (cond
     ((and local (org-scribe--wikcionario-show palabra)))
     ;; Local server is up but has no entry: report it rather than silently
     ;; going online, which would make a coverage gap look like a network hop.
     (local
      (org-scribe--wikcionario-report-miss palabra))
     ;; Offline-only: name the service, since that is the actionable part.
     ((eq backend 'wikcionario)
      (message (org-scribe-msg 'error-wikcionario-unreachable
                               org-scribe-wikcionario-url)))
     (t
      (when (eq backend 'auto)
        (message (org-scribe-msg 'msg-dictionary-fallback-online palabra)))
      (org-scribe-rae-api-lookup palabra)))))

;;; Synonym Lookup

;; Two backends, selected by `org-scribe-thesaurus-backend': the offline
;; MyThes thesaurus (`org-scribe-mythes.el') and the original WordReference
;; page rendered with eww.  Both present their result in the same side
;; window, so the command feels identical either way.

(defun org-scribe--sinonimo-render-mythes (palabra groups &optional headword)
  "Render GROUPS, the MyThes meaning groups for PALABRA, in a side window.

HEADWORD is the entry that actually answered.  When it differs from PALABRA
— because the lookup fell back to a lemma — the substitution is stated in
the buffer and echoed, since a writer who asked about \"corriendo\" and is
shown \"correr\" would otherwise think the tool misread the question.

The result is an Org buffer rather than a rendered web page, so the writer
can yank a candidate straight into the manuscript.  Senses are numbered
because most Spanish MyThes entries carry no grammatical label, and
unlabelled senses would otherwise be indistinguishable from one another."
  (let* ((substituted (and headword
                           (not (string-equal-ignore-case headword palabra))))
         (buffer (get-buffer-create
                  (format "*%s*" (org-scribe-msg 'msg-thesaurus-title palabra))))
         (sense 0))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: %s\n\n"
                        (org-scribe-msg 'msg-thesaurus-title
                                        (or headword palabra))))
        (when substituted
          ;; An Org comment: visible to the writer, never exported or counted.
          (insert (format "# %s\n\n"
                          (org-scribe-msg 'msg-thesaurus-lemma-used
                                          palabra headword))))
        (dolist (group groups)
          (setq sense (1+ sense))
          (insert (format "* %s%s\n"
                          (org-scribe-msg 'msg-thesaurus-sense sense)
                          (if (car group) (format " /%s/" (car group)) "")))
          (insert (format "%s\n\n" (mapconcat #'identity (cdr group) ", ")))))
      (goto-char (point-min))
      (read-only-mode 1)
      (use-local-map (copy-keymap (or (current-local-map) (make-sparse-keymap))))
      (local-set-key (kbd "q") #'quit-window))
    (when substituted
      (message (org-scribe-msg 'msg-thesaurus-lemma-used palabra headword)))
    (org-scribe--side-window buffer)))

(defun org-scribe--sinonimo-wordreference (palabra)
  "Open the WordReference synonym page for PALABRA in a side window."
  (let ((url (concat "https://www.wordreference.com/sinonimos/" palabra))
        ;; eww replaces this placeholder with its own buffer once it loads.
        (temp-buffer (generate-new-buffer "*temp-sinonimos*")))
    (let ((side-window (org-scribe--side-window temp-buffer)))
      (with-selected-window side-window
        (eww url)
        (kill-buffer temp-buffer)
        (read-only-mode 1)
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key (kbd "q") #'quit-window)))))

;;;###autoload
(defun org-scribe-sinonimo (palabra)
  "Busca sinónimos de PALABRA en una ventana lateral.

Uses the offline MyThes thesaurus or the online WordReference page
according to `org-scribe-thesaurus-backend'.  Under the default `auto',
MyThes is tried first and WordReference is used when the thesaurus is not
installed or has no entry for PALABRA — a word absent from the local data
should still get an answer rather than a shrug."
  (interactive "s¿Qué palabra quieres buscar? ")
  (when (string-empty-p (string-trim palabra))
    (user-error (org-scribe-msg 'error-word-empty)))
  (let* ((backend org-scribe-thesaurus-backend)
         (entry (unless (eq backend 'wordreference)
                  (org-scribe-mythes-lookup-entry palabra))))
    (cond
     (entry
      (org-scribe--sinonimo-render-mythes palabra (cdr entry) (car entry)))
     ;; Offline-only: report the reason rather than silently going online.
     ((eq backend 'mythes)
      (message (org-scribe-msg
                (if (org-scribe-mythes-available-p)
                    'msg-thesaurus-no-synonyms
                  'error-thesaurus-not-found)
                (if (org-scribe-mythes-available-p)
                    palabra
                  org-scribe-mythes-directory))))
     (t
      (when (and (eq backend 'auto) (org-scribe-mythes-available-p))
        (message (org-scribe-msg 'msg-thesaurus-fallback-online palabra)))
      (org-scribe--sinonimo-wordreference palabra)))))

;;;###autoload
(defun org-scribe-thesaurus-lookup ()
  "Look up the word at point in the English thesaurus.

Delegates to `powerthesaurus-lookup-dwim', an optional dependency.  The
guard is the point of this wrapper: the hydra used to call that command
directly, so on an install without powerthesaurus the package's own menu
threw a void-function error instead of degrading."
  (interactive)
  (if (fboundp 'powerthesaurus-lookup-dwim)
      (call-interactively #'powerthesaurus-lookup-dwim)
    (message (org-scribe-msg 'msg-command-unavailable "powerthesaurus"))))

(provide 'org-scribe-dictionary)

;;; org-scribe-dictionary.el ends here
