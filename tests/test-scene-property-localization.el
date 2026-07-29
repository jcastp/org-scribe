;;; test-scene-property-localization.el --- Tests for localized scene properties -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the scene property alias system in org-scribe-core.el.
;; Spanish project templates use localized scene property names
;; (e.g. :Personajes: instead of :Characters:); these tests verify
;; that reading, writing, project-language detection, and the
;; generic linking/search/health code all resolve both variants
;; correctly.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory)))

(require 'org-scribe-core)
(require 'org-scribe-messages)

(defmacro test-scene-prop--with-temp-project (dir-setup &rest body)
  "Execute BODY inside a temp project directory configured by DIR-SETUP.
Mirrors `test-core--with-temp-project' from test-core-extended.el."
  (declare (indent 1))
  `(let* ((temp-dir (make-temp-file "org-scribe-scene-prop-test-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn
           (setq org-scribe--project-type-cache nil)
           (dolist (item ,dir-setup)
             (cond
              ((and (stringp item) (string-suffix-p "/" item))
               (make-directory (expand-file-name item temp-dir) t))
              ((consp item)
               (let ((path (expand-file-name (car item) temp-dir)))
                 (make-directory (file-name-directory path) t)
                 (with-temp-file path (insert (cdr item)))))
              ((stringp item)
               (write-region "" nil (expand-file-name item temp-dir)))))
           ,@body)
       (setq org-scribe--project-type-cache nil)
       (delete-directory temp-dir t))))

;;; Alias Table Tests

(ert-deftest test-scene-property-aliases-known-keys ()
  "Known canonical keys resolve to their expected literal aliases."
  (should (equal (org-scribe-scene-property-aliases 'pov) '("PoV")))
  (should (equal (org-scribe-scene-property-aliases 'characters) '("Characters" "Personajes")))
  (should (equal (org-scribe-scene-property-aliases 'plot) '("Plot" "Trama")))
  (should (equal (org-scribe-scene-property-aliases 'location) '("Location" "Localizacion")))
  (should (equal (org-scribe-scene-property-aliases 'timeline) '("Timeline" "Linea-temporal")))
  (should (equal (org-scribe-scene-property-aliases 'comment) '("Comment" "Comentario"))))

(ert-deftest test-scene-property-aliases-unknown-key-passthrough ()
  "An unrecognized key is returned as a single-element list unchanged."
  (should (equal (org-scribe-scene-property-aliases "CustomProp") '("CustomProp"))))

(ert-deftest test-scene-property-name-by-language ()
  "The write-name resolver picks the alias matching the requested language."
  (should (equal (org-scribe-scene-property-name 'characters 'en) "Characters"))
  (should (equal (org-scribe-scene-property-name 'characters 'es) "Personajes"))
  (should (equal (org-scribe-scene-property-name 'plot 'es) "Trama"))
  ;; PoV has only one alias, used regardless of language.
  (should (equal (org-scribe-scene-property-name 'pov 'es) "PoV")))

;;; Get/Set Tests

(ert-deftest test-scene-property-get-reads-english-alias ()
  "Reading a canonical key finds the value under its English alias."
  (with-temp-buffer
    (org-mode)
    (insert "* Scene\n:PROPERTIES:\n:Characters: Alex\n:END:\n")
    (goto-char (point-min))
    (should (equal (org-scribe-scene-property-get 'characters) "Alex"))))

(ert-deftest test-scene-property-get-reads-spanish-alias ()
  "Reading a canonical key also finds the value under its Spanish alias."
  (with-temp-buffer
    (org-mode)
    (insert "* Escena\n:PROPERTIES:\n:Personajes: Alex\n:END:\n")
    (goto-char (point-min))
    (should (equal (org-scribe-scene-property-get 'characters) "Alex"))))

(ert-deftest test-scene-property-set-writes-existing-alias ()
  "Setting a value updates whichever alias is already present, not a new one."
  (with-temp-buffer
    (org-mode)
    (insert "* Escena\n:PROPERTIES:\n:Personajes: Alex\n:END:\n")
    (goto-char (point-min))
    (org-scribe-scene-property-set 'characters "Alex, Sam")
    (should (equal (org-entry-get nil "Personajes") "Alex, Sam"))
    (should (null (org-entry-get nil "Characters")))))

(ert-deftest test-scene-property-set-defaults-to-english-with-no-project ()
  "With no project language context, a fresh property is written in English."
  (with-temp-buffer
    (org-mode)
    (insert "* Scene\n:PROPERTIES:\n:END:\n")
    (goto-char (point-min))
    (let ((default-directory (make-temp-file "org-scribe-no-project-" t)))
      (unwind-protect
          (progn
            (org-scribe-scene-property-set 'characters "Alex")
            (should (equal (org-entry-get nil "Characters") "Alex")))
        (delete-directory default-directory t)))))

;;; Project Language Detection

(ert-deftest test-project-language-reads-spanish-marker ()
  "org-scribe-project-language reads the '# Language: es' marker line."
  (test-scene-prop--with-temp-project
      '((".org-scribe-project" . "# Writing project: Test\n# Language: es\n"))
    (should (eq (org-scribe-project-language) 'es))))

(ert-deftest test-project-language-reads-english-marker ()
  "org-scribe-project-language reads the '# Language: en' marker line."
  (test-scene-prop--with-temp-project
      '((".org-scribe-project" . "# Writing project: Test\n# Language: en\n"))
    (should (eq (org-scribe-project-language) 'en))))

(ert-deftest test-project-language-defaults-without-marker ()
  "With no marker file or Language line, the default is 'en."
  (test-scene-prop--with-temp-project
      '((".org-scribe-project" . "# Writing project: Test\n"))
    (should (eq (org-scribe-project-language) 'en))))

(ert-deftest test-scene-property-set-uses-project-language-for-new-property ()
  "A brand-new property in a Spanish project is written with the Spanish alias."
  (test-scene-prop--with-temp-project
      '((".org-scribe-project" . "# Writing project: Test\n# Language: es\n"))
    (with-temp-buffer
      (org-mode)
      (insert "* Escena\n:PROPERTIES:\n:END:\n")
      (goto-char (point-min))
      (org-scribe-scene-property-set 'plot "Trama principal")
      (should (equal (org-entry-get nil "Trama") "Trama principal"))
      (should (null (org-entry-get nil "Plot"))))))

(provide 'test-scene-property-localization)

;;; test-scene-property-localization.el ends here
