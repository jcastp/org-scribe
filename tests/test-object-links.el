;;; test-object-links.el --- Tests for object linking system -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for linking/org-scribe-object-links.el: the heading predicate (flat
;; layout accepted; old nested layout and untyped headings refused), the
;; registry entry, link creation, and the health report's use of objects.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (dolist (dir '("core" "search" "linking" "capture" "templates" "reporting"))
    (add-to-list 'load-path (expand-file-name (concat "../" dir) default-directory))))

(require 'org-scribe-object-links)
(require 'org-scribe-health)

(defvar test-object-links--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro test-object-links--with-notebook (content &rest body)
  "Run BODY in an Org buffer holding CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun test-object-links--names ()
  "Return the names of every object heading in the buffer."
  (let (names)
    (org-map-entries
     (lambda ()
       (when (org-scribe--object-heading-p)
         (push (org-get-heading t t t t) names))))
    (nreverse names)))

(ert-deftest test-object-links-registered ()
  (should (assq 'object org-scribe-entity-registry))
  (should (fboundp 'org-scribe--get-all-objects))
  (should (fboundp 'org-scribe-set-scene-objects))
  (should (fboundp 'org-scribe-setup-object-links)))

(ert-deftest test-object-links-flat-objects-are-recognised ()
  (test-object-links--with-notebook
      "* World\n** Objects\n*** The Key\n:PROPERTIES:\n:Type: Magical\n:END:\n**** Origin\n*** The Mug\n:PROPERTIES:\n:Type: Everyday\n:END:\n"
    (should (equal '("The Key" "The Mug") (test-object-links--names)))))

(ert-deftest test-object-links-spanish-section-is-recognised ()
  (test-object-links--with-notebook
      "* Mundo\n** Objetos\n*** La llave\n:PROPERTIES:\n:Type: Mágico\n:END:\n"
    (should (equal '("La llave") (test-object-links--names)))))

(ert-deftest test-object-links-heading-without-type-is-not-an-object ()
  (test-object-links--with-notebook
      "* World\n** Objects\n*** Untyped\n"
    (should-not (test-object-links--names))))

(ert-deftest test-object-links-old-nested-layout-is-not-read ()
  "A subsection of Objects, and the objects below it, are not entities."
  (test-object-links--with-notebook
      "* World\n** Objects\n*** Symbolic Objects\n**** The Ring\n:PROPERTIES:\n:Type: Symbolic\n:END:\n"
    (should-not (test-object-links--names))))

(ert-deftest test-object-links-shipped-notebooks-hold-distinct-flat-objects ()
  (dolist (file '("novel-en/design.org.template" "novel-es/diseno.org.template"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name (concat "org-scribe-templates/" file)
                                              test-object-links--root))
      (org-mode)
      (let ((names (test-object-links--names)))
        (should (= 3 (length names)))
        (should (= 3 (length (delete-dups (copy-sequence names)))))))))

(ert-deftest test-object-links-create-link ()
  (let ((alist '(("Key" . ("obj-001" . "Key")))))
    (should (string= "[[id:obj-001][Key]]" (org-scribe--create-object-link "Key" alist)))
    (should (string= "Lamp" (org-scribe--create-object-link "Lamp" alist)))))

(ert-deftest test-object-links-scene-property-is-in-every-drawer ()
  (should (memq 'objects org-scribe--scene-property-keys))
  (dolist (spec '(("novel-en/novel.org.template" . ":Objects:")
                  ("novel-es/novela.org.template" . ":Objetos:")
                  ("short-story-en/story.org.template" . ":Objects:")
                  ("short-story-es/cuento.org.template" . ":Objetos:")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name (concat "org-scribe-templates/" (car spec))
                                              test-object-links--root))
      (should (search-forward (format "\n%s\n" (cdr spec)) nil t)))))

;;; Health

(ert-deftest test-object-links-health-counts-object-references ()
  "A scene's :Objects: links must keep those objects from being orphans."
  (let ((file (make-temp-file "novel" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:Objects: [[id:obj-001][Key]]\n:END:\n"))
          (let ((ids (org-scribe--health-collect-referenced-ids file)))
            (should (gethash "obj-001" ids))))
      (delete-file file))))

(ert-deftest test-object-links-health-detects-the-old-nested-layout ()
  (let ((file (make-temp-file "design" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* World\n** Objects\n*** Symbolic Objects\n**** The Ring\n:PROPERTIES:\n:Type: Symbolic\n:END:\n*** The Key\n:PROPERTIES:\n:Type: Magical\n:END:\n"))
          (should (equal '("The Ring") (org-scribe--health-nested-objects file))))
      (when-let* ((buf (find-buffer-visiting file))) (kill-buffer buf))
      (delete-file file))))

(ert-deftest test-object-links-health-no-hint-for-the-flat-layout ()
  (let ((file (make-temp-file "design" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* World\n** Objects\n*** The Key\n:PROPERTIES:\n:Type: Magical\n:END:\n**** Origin\n"))
          (should-not (org-scribe--health-nested-objects file)))
      (when-let* ((buf (find-buffer-visiting file))) (kill-buffer buf))
      (delete-file file))))

(provide 'test-object-links)

;;; test-object-links.el ends here
