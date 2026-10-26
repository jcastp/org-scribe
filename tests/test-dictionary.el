;;; test-dictionary.el --- Tests for dictionary and language tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-scribe-dictionary.el.
;; Covers module loading, function availability, and input validation.
;;
;; Note: nothing here touches the network.  Functions that call the RAE
;; API run against a mocked `url-retrieve'; the formatting functions
;; (rae-format-result, rae-format-conjugations) are tested with mock data.

;;; Code:

(require 'ert)
(require 'json)
(require 'cl-lib)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../language" default-directory)))

(require 'org-scribe-dictionary)

;;; Function Availability Tests

(ert-deftest test-dictionary-functions-defined ()
  "Test that all dictionary functions are defined."
  (should (fboundp 'org-scribe-rae-api-lookup))
  (should (fboundp 'org-scribe-rae-api-random))
  (should (fboundp 'org-scribe-sinonimo))
  (should (fboundp 'org-scribe-rae-format-result))
  (should (fboundp 'org-scribe-rae-format-conjugations)))

;;; Input Validation Tests

(ert-deftest test-dictionary-rae-lookup-empty-word ()
  "Test that empty word raises a user-error."
  (should-error (org-scribe-rae-api-lookup "")
                :type 'user-error))

(ert-deftest test-dictionary-rae-lookup-whitespace-word ()
  "Test that whitespace-only word raises a user-error."
  (should-error (org-scribe-rae-api-lookup "   ")
                :type 'user-error))

(ert-deftest test-dictionary-sinonimo-empty-word ()
  "Test that empty word raises a user-error in synonym lookup."
  (should-error (org-scribe-sinonimo "")
                :type 'user-error))

(ert-deftest test-dictionary-sinonimo-whitespace-word ()
  "Test that whitespace-only word raises user-error in synonym lookup."
  (should-error (org-scribe-sinonimo "  ")
                :type 'user-error))

;;; org-scribe-rae-format-result Tests (with mock data)

(defun test-dictionary--make-mock-json ()
  "Create a mock JSON data hash table for testing rae-format-result."
  (let* ((sense (make-hash-table :test 'equal))
         (meaning (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (json (make-hash-table :test 'equal)))
    ;; Sense
    (puthash "category" "adj." sense)
    (puthash "description" "De color azul." sense)
    (puthash "synonyms" '() sense)
    (puthash "antonyms" '() sense)
    ;; Meaning
    (puthash "origin" nil meaning)
    (puthash "senses" (list sense) meaning)
    (puthash "conjugations" nil meaning)
    ;; Data
    (puthash "word" "azul" data)
    (puthash "meanings" (list meaning) data)
    ;; Root
    (puthash "data" data json)
    (puthash "ok" t json)
    json))

(ert-deftest test-dictionary-format-result-inserts-title ()
  "Test that rae-format-result inserts word as title."
  (with-temp-buffer
    (org-mode)
    (org-scribe-rae-format-result (test-dictionary--make-mock-json) "azul")
    (goto-char (point-min))
    (should (search-forward "azul" nil t))))

(ert-deftest test-dictionary-format-result-inserts-definitions ()
  "Test that rae-format-result inserts definitions section."
  (with-temp-buffer
    (org-mode)
    (org-scribe-rae-format-result (test-dictionary--make-mock-json) "azul")
    (goto-char (point-min))
    (should (search-forward "Definiciones" nil t))))

(ert-deftest test-dictionary-format-result-inserts-description ()
  "Test that rae-format-result inserts the sense description."
  (with-temp-buffer
    (org-mode)
    (org-scribe-rae-format-result (test-dictionary--make-mock-json) "azul")
    (goto-char (point-min))
    (should (search-forward "De color azul." nil t))))

(ert-deftest test-dictionary-format-result-with-synonyms ()
  "Test that rae-format-result shows synonyms when present."
  (let* ((sense (make-hash-table :test 'equal))
         (meaning (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (json (make-hash-table :test 'equal)))
    (puthash "category" "adj." sense)
    (puthash "description" "De color azul." sense)
    (puthash "synonyms" '("celeste" "cobalto") sense)
    (puthash "antonyms" '() sense)
    (puthash "origin" nil meaning)
    (puthash "senses" (list sense) meaning)
    (puthash "conjugations" nil meaning)
    (puthash "word" "azul" data)
    (puthash "meanings" (list meaning) data)
    (puthash "data" data json)
    (with-temp-buffer
      (org-mode)
      (org-scribe-rae-format-result json "azul")
      (goto-char (point-min))
      (should (search-forward "celeste" nil t)))))

(ert-deftest test-dictionary-format-result-with-origin ()
  "Test that rae-format-result shows etymology when present."
  (let* ((origin (make-hash-table :test 'equal))
         (sense (make-hash-table :test 'equal))
         (meaning (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (json (make-hash-table :test 'equal)))
    (puthash "text" "Del árabe azraq." origin)
    (puthash "category" "adj." sense)
    (puthash "description" "De color azul." sense)
    (puthash "synonyms" '() sense)
    (puthash "antonyms" '() sense)
    (puthash "origin" origin meaning)
    (puthash "senses" (list sense) meaning)
    (puthash "conjugations" nil meaning)
    (puthash "word" "azul" data)
    (puthash "meanings" (list meaning) data)
    (puthash "data" data json)
    (with-temp-buffer
      (org-mode)
      (org-scribe-rae-format-result json "azul")
      (goto-char (point-min))
      (should (search-forward "Etimología" nil t))
      (should (search-forward "árabe" nil t)))))

(defun test-dictionary--json-from (sense-plist &optional origin-plist)
  "Return mock RAE JSON for `faro' with one sense from SENSE-PLIST.
ORIGIN-PLIST, when given, fills the meaning's origin."
  (let ((sense (make-hash-table :test 'equal))
        (meaning (make-hash-table :test 'equal))
        (data (make-hash-table :test 'equal))
        (json (make-hash-table :test 'equal)))
    (cl-loop for (k v) on sense-plist by #'cddr do (puthash k v sense))
    (when origin-plist
      (let ((origin (make-hash-table :test 'equal)))
        (cl-loop for (k v) on origin-plist by #'cddr do (puthash k v origin))
        (puthash "origin" origin meaning)))
    (puthash "senses" (list sense) meaning)
    (puthash "word" "faro" data)
    (puthash "meanings" (list meaning) data)
    (puthash "data" data json)
    json))

(defun test-dictionary--render (json)
  "Return the text `org-scribe-rae-format-result' writes for JSON."
  (with-temp-buffer
    (org-mode)
    (org-scribe-rae-format-result json "faro")
    (buffer-string)))

;; Real response fields for `faro', sense 2, captured from rae-api.com.
(defconst test-dictionary--faro-sense
  '("raw" "2. m. Farol con potente reverbero.Sin.:fanal, farol, foco, lámpara, linterna, luz1."
    "category" "noun" "gender" "masculine"
    "description" "Farol con potente reverbero"
    "synonyms" ("fanal" "farol" "luz1")))

(ert-deftest test-dictionary-format-result-etymology-is-the-raw-sentence ()
  "The etymology is the `raw' sentence, not the bare `text' list.
Regression: it showed \"pharus,pháros,pháros\"."
  (let ((out (test-dictionary--render
              (test-dictionary--json-from
               test-dictionary--faro-sense
               '("raw" "Del lat. pharus, y este del gr. φάρος pháros."
                 "text" "pharus,pháros,pháros")))))
    (should (string-match-p "Del lat\\. pharus, y este del gr\\." out))
    (should-not (string-match-p "pharus,pháros" out))))

(ert-deftest test-dictionary-format-result-etymology-falls-back-to-text ()
  (let ((out (test-dictionary--render
              (test-dictionary--json-from test-dictionary--faro-sense
                                          '("raw" "" "text" "pharus")))))
    (should (string-match-p "^pharus$" out))))

(ert-deftest test-dictionary-format-result-label-is-the-rae-abbreviation ()
  "A sense is labelled with the RAE's own Spanish abbreviation.
Regression: it showed the API's English category (/noun/)."
  (let ((out (test-dictionary--render (test-dictionary--json-from test-dictionary--faro-sense))))
    (should (string-match-p "^\\*\\* 1\\. /m\\./$" out))
    (should-not (string-match-p "noun" out))))

(ert-deftest test-dictionary-sense-label-reads-several-abbreviations ()
  (should (equal (org-scribe--rae-sense-label
                  (test-dictionary--json-sense
                   '("raw" "17. adj. coloq. De posición social y económica elevada. Casa, gente bien."
                     "description" "De posición social y económica elevada")))
                 "adj. coloq."))
  ;; Description not found in `raw': the leading abbreviations
  (should (equal (org-scribe--rae-sense-label
                  (test-dictionary--json-sense
                   '("raw" "6. m. pl. Der. Cosas materiales." "description" "Otra cosa")))
                 "m. pl. Der."))
  ;; No `raw': no label (the category is used instead)
  (should-not (org-scribe--rae-sense-label
               (test-dictionary--json-sense '("description" "De color azul.")))))

(defun test-dictionary--json-sense (plist)
  "Return a sense hash table from PLIST."
  (let ((sense (make-hash-table :test 'equal)))
    (cl-loop for (k v) on plist by #'cddr do (puthash k v sense))
    sense))

(ert-deftest test-dictionary-format-result-strips-homonym-index ()
  "Synonyms lose the RAE's homonym index.  Regression: `luz1'."
  (let ((out (test-dictionary--render (test-dictionary--json-from test-dictionary--faro-sense))))
    (should (string-match-p "Sinónimos: fanal, farol, luz$" out))
    (should-not (string-match-p "luz1" out))))

;;; org-scribe-rae-format-conjugations Tests (with mock data)

(ert-deftest test-dictionary-format-conjugations-inserts-example-block ()
  "Test that rae-format-conjugations wraps content in #+begin_example."
  (let ((conjugations (make-hash-table :test 'equal)))
    ;; Simple mock conjugation: one mood with one tense
    (let ((tenses (make-hash-table :test 'equal))
          (persons (make-hash-table :test 'equal)))
      (puthash "1sg" "corro" persons)
      (puthash "presente" persons tenses)
      (puthash "indicativo" tenses conjugations))
    (with-temp-buffer
      (org-scribe-rae-format-conjugations conjugations)
      (let ((content (buffer-string)))
        (should (string-match-p "begin_example" content))
        (should (string-match-p "end_example" content))
        (should (string-match-p "INDICATIVO" content))))))

(ert-deftest test-dictionary-format-conjugations-nil ()
  "Test that nil conjugations are handled gracefully."
  (with-temp-buffer
    (org-scribe-rae-format-conjugations nil)
    ;; Should not insert anything for nil
    (should (= 0 (buffer-size)))))

;;; Response-Buffer Cleanup Tests (L9)

(defmacro test-dictionary--with-mocked-http (json-body &rest body)
  "Run BODY with `url-retrieve' mocked to synchronously call back.
JSON-BODY is the literal JSON text used as the HTTP response body.
The fake HTTP buffer created for each call is pushed onto the
`test-dictionary--http-buffers' list so callers can assert on
liveness after BODY runs."
  (declare (indent 1))
  `(let ((test-dictionary--http-buffers nil))
     (cl-letf (((symbol-function 'url-retrieve)
                (lambda (_url callback &optional cbargs &rest _)
                  (let ((buf (generate-new-buffer " *fake-http*")))
                    (push buf test-dictionary--http-buffers)
                    (with-current-buffer buf
                      (insert "HTTP/1.1 200 OK\n\n")
                      (insert ,json-body)
                      (apply callback (list nil) cbargs))))))
       ,@body)
     test-dictionary--http-buffers))

(ert-deftest test-dictionary-rae-lookup-kills-response-buffer ()
  "org-scribe-rae-api-lookup kills its HTTP response buffer (L9)."
  (let ((buffers (test-dictionary--with-mocked-http
                     "{\"ok\":true,\"data\":{\"word\":\"azul\",\"meanings\":[]}}"
                   (org-scribe-rae-api-lookup "azul"))))
    (should buffers)
    (should (cl-every (lambda (b) (not (buffer-live-p b))) buffers))))

(ert-deftest test-dictionary-rae-lookup-kills-response-buffer-on-word-not-found ()
  "The response buffer is killed even when the word is not found (L9)."
  (let ((buffers (test-dictionary--with-mocked-http
                     "{\"ok\":false,\"suggestions\":[]}"
                   (org-scribe-rae-api-lookup "azulx"))))
    (should buffers)
    (should (cl-every (lambda (b) (not (buffer-live-p b))) buffers))))

(ert-deftest test-dictionary-rae-lookup-kills-response-buffer-on-parse-error ()
  "The response buffer is killed even when the JSON body is malformed (L9)."
  (let ((buffers (test-dictionary--with-mocked-http
                     "not valid json"
                   (org-scribe-rae-api-lookup "azul"))))
    (should buffers)
    (should (cl-every (lambda (b) (not (buffer-live-p b))) buffers))))

(ert-deftest test-dictionary-rae-random-kills-response-buffer ()
  "org-scribe-rae-api-random kills its HTTP response buffer (L9).
It also chains into org-scribe-rae-api-lookup, whose own response
buffer must be cleaned up too."
  (let ((buffers (test-dictionary--with-mocked-http
                     "{\"data\":{\"word\":\"azul\"}}"
                   (org-scribe-rae-api-random))))
    (should (= 2 (length buffers)))
    (should (cl-every (lambda (b) (not (buffer-live-p b))) buffers))))

;;; Online synonyms (the RAE API)

(defconst test-dictionary--corriendo-json
  "{\"ok\":true,\"data\":{\"word\":\"correr\",\"meanings\":[{\"senses\":[
{\"raw\":\"2. intr. Ir deprisa. Esas motos corren mucho.Sin.:acelerar, volar.\",
 \"description\":\"Ir deprisa\",\"category\":\"verb\",\"synonyms\":[\"acelerar\",\"volar\"]},
{\"raw\":\"11. intr. Dicho de una paga: Ir devengándose.\",
 \"description\":\"Dicho de una paga: Ir devengándose\",\"synonyms\":null},
{\"raw\":\"14. intr. Pasar por la oficina.Sin.:recurrir1.\",
 \"description\":\"Pasar por la oficina\",\"synonyms\":[\"recurrir1\"]}]}]}}"
  "An RAE response shaped like the real one for `corriendo'.")

(ert-deftest test-dictionary-rae-synonym-groups ()
  "Each sense with synonyms is one group, labelled the RAE's way."
  (let* ((json-object-type 'hash-table) (json-array-type 'list) (json-key-type 'string)
         (json (json-read-from-string test-dictionary--corriendo-json)))
    (should (equal (org-scribe--rae-synonym-groups json)
                   '(("intr." "acelerar" "volar") ("intr." "recurrir"))))))

(ert-deftest test-dictionary-sinonimo-rae-renders-like-mythes ()
  "The RAE's synonyms go through the MyThes renderer, headword disclosed.
Regression: the online fallback was the WordReference page, which
answers HTTP 418, so eww showed a blank buffer."
  (let ((rendered nil))
    (cl-letf (((symbol-function 'org-scribe--sinonimo-render-mythes)
               (lambda (&rest args) (setq rendered args))))
      (let ((buffers (test-dictionary--with-mocked-http test-dictionary--corriendo-json
                       (org-scribe--sinonimo-rae "corriendo"))))
        (should (cl-every (lambda (b) (not (buffer-live-p b))) buffers))))
    (should (equal rendered
                   '("corriendo" (("intr." "acelerar" "volar") ("intr." "recurrir"))
                     "correr")))))

(ert-deftest test-dictionary-sinonimo-rae-reports-a-miss ()
  "No entry, or no synonyms: a message, never a blank buffer."
  (dolist (body '("{\"ok\":false,\"suggestions\":[]}"
                  "{\"ok\":true,\"data\":{\"word\":\"xyzzyq\",\"meanings\":[]}}"))
    (let ((rendered nil) (said nil))
      (cl-letf (((symbol-function 'org-scribe--sinonimo-render-mythes)
                 (lambda (&rest _) (setq rendered t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (test-dictionary--with-mocked-http body
          (let ((org-scribe-message-language 'en))
            (org-scribe--sinonimo-rae "xyzzyq"))))
      (should-not rendered)
      (should (string-match-p "No synonyms found for: xyzzyq" said)))))

(ert-deftest test-dictionary-rae-fetch-reads-a-404-miss-as-a-miss ()
  "A missing word is HTTP 404 with a JSON body: ON-JSON sees \"ok\" as nil.
Regression: the 404 was reported as a lookup error, and JSON false was
read as `:json-false', which is true, so a miss was rendered as an entry."
  (let ((seen 'unset))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional cbargs &rest _)
                 (with-temp-buffer
                   (insert "HTTP/1.1 404 Not Found\n\n"
                           "{\"error\":\"NOT_FOUND\",\"ok\":false,\"suggestions\":null}")
                   (apply callback '(:error (error http 404)) cbargs)))))
      (org-scribe--rae-fetch "xyzzyq" (lambda (json) (setq seen (gethash "ok" json 'absent)))))
    (should (null seen))))

(ert-deftest test-dictionary-rae-fetch-reports-other-errors ()
  "Any error but a 404 is reported, and ON-JSON is not called."
  (let ((called nil) (said nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional cbargs &rest _)
                 (with-temp-buffer
                   (apply callback '(:error (error http 500)) cbargs))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (let ((org-scribe-message-language 'en))
        (org-scribe--rae-fetch "faro" (lambda (_) (setq called t)))))
    (should-not called)
    (should (string-match-p "500" said))))

;;; Optional dependency: powerthesaurus

(ert-deftest test-dictionary-thesaurus-lookup-without-powerthesaurus-messages ()
  "The thesaurus command degrades to a message when powerthesaurus is absent.
The hydra binds this command; before the guard existed it bound
`powerthesaurus-lookup-dwim' directly, so the menu threw a
void-function error on any install without that optional package."
  (let ((msg nil))
    (cl-letf (((symbol-function 'powerthesaurus-lookup-dwim) nil)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (org-scribe-thesaurus-lookup))
    (should (string-match-p "powerthesaurus" msg))))

(ert-deftest test-dictionary-thesaurus-lookup-delegates-when-available ()
  "The thesaurus command calls powerthesaurus when it is installed."
  (let (called)
    (cl-letf (((symbol-function 'powerthesaurus-lookup-dwim)
               (lambda () (interactive) (setq called t))))
      (org-scribe-thesaurus-lookup))
    (should called)))

;;; The word to look up defaults to the one the writer is on

(defvar test-dictionary--prompt nil)

(defun test-dictionary--interactive-args (fn)
  "Return FN's interactive arguments, with RET answered at the prompt.
`read-string' is stubbed to return its DEFAULT, as RET does; the prompt
it was given is recorded in `test-dictionary--prompt'."
  (cl-letf (((symbol-function 'read-string)
             (lambda (prompt &optional _initial _history default &rest _)
               (setq test-dictionary--prompt prompt)
               (or default ""))))
    (advice-eval-interactive-spec (cadr (interactive-form fn)))))


(ert-deftest test-dictionary-lookups-default-to-word-at-point ()
  "RET in the dictionary, RAE and synonym prompts looks up the word at point.
They used to prompt with an empty minibuffer, so the writer retyped a word
already on screen, accents included."
  (dolist (fn '(org-scribe-dictionary-lookup org-scribe-rae-api-lookup
                org-scribe-sinonimo))
    (with-temp-buffer
      (insert "La canción del faro")
      (goto-char 6)
      (should (equal "canción" (car (test-dictionary--interactive-args fn))))
      (should (string-match-p "canción" test-dictionary--prompt)))))

(ert-deftest test-dictionary-lookups-default-to-region ()
  "An active region is the default, so a phrase can be looked up."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "dijo a bocajarro que no")
    (set-mark 6)
    (goto-char 17)
    (activate-mark)
    (should (equal "a bocajarro"
                   (car (test-dictionary--interactive-args
                         #'org-scribe-sinonimo))))))

(ert-deftest test-dictionary-lookup-prompts-are-localized ()
  "The prompts come from the language pack, not literals in the spec."
  (with-temp-buffer
    (let ((org-scribe-message-language 'en))
      (should (equal "" (car (test-dictionary--interactive-args
                              #'org-scribe-dictionary-lookup))))
      (should (string-prefix-p (org-scribe-msg 'prompt-dictionary-word)
                               test-dictionary--prompt))
      (should-not (string-match-p "default" test-dictionary--prompt)))))

;;; Run tests

(defun org-scribe-dictionary-run-tests ()
  "Run all dictionary tests."
  (interactive)
  (ert "^test-dictionary-"))

(provide 'test-dictionary)

;;; test-dictionary.el ends here
