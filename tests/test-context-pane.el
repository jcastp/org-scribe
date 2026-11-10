;;; test-context-pane.el --- Tests for the scene context pane -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The pane is optional: its switches (global and per project), the module
;; staying unloaded while off, what it collects and draws for a scene (full,
;; empty, location only, broken link, hidden characters, large cast, Spanish),
;; the header line, the refresh throttle and the layout.  The "module stays
;; unloaded" check runs in a fresh `emacs -Q' subprocess, because the test
;; runner has already loaded the module.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'org-scribe)
(require 'org-scribe-context-pane)

(defconst test-context-pane--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout, for the subprocess's load path.")

;;; Fixtures

(defun test-context-pane--manuscript (lang)
  "Return the manuscript text for LANGUAGE (`en' or `es')."
  (let ((p (lambda (key) (org-scribe-lang-property key lang))))
    (concat
     "#+TODO: TOWRITE | FINISHED PURGE\n"
     "* Act One\n** Chapter One\n"
     "*** TOWRITE The Heist :ignore:\n"
     ":PROPERTIES:\n:ID: heist-id\n"
     (format ":%s: [[id:ana-id][Ana]]\n" (funcall p 'pov))
     (format ":%s: [[id:ana-id][Ana]], [[id:bob-id][Bob]]\n" (funcall p 'characters))
     (format ":%s: [[id:vault-id][The Vault]]\n" (funcall p 'location))
     (format ":%s: [[id:thread-id][Main Line]]\n" (funcall p 'plot))
     (format ":%s: [[id:pp-id][4. Inciting Incident]]\n" (funcall p 'plot-point))
     (format ":%s: Ana cracks the vault\n" (funcall p 'description))
     (format ":%s:\n" (funcall p 'summary))
     (format ":%s: Wants the diamond\n" (funcall p 'scene-motivation))
     (format ":%s: The guard\n" (funcall p 'conflict-source))
     (format ":%s:\n" (funcall p 'gap))
     (format ":%s: The vault is empty\n" (funcall p 'outcome))
     ":END:\n"
     "Ana opened the safe and found nothing at all.\n\n"
     "*** TOWRITE Empty Scene :ignore:\n"
     ":PROPERTIES:\n:ID: empty-id\n"
     (format ":%s:\n" (funcall p 'pov))
     (format ":%s:\n" (funcall p 'characters))
     ":END:\n"
     "*** TOWRITE Place Only :ignore:\n"
     ":PROPERTIES:\n:ID: place-id\n"
     (format ":%s: [[id:vault-id][The Vault]]\n" (funcall p 'location))
     ":END:\n"
     "*** TOWRITE Broken :ignore:\n"
     ":PROPERTIES:\n:ID: broken-id\n"
     (format ":%s: [[id:gone-id][Ghost]]\n" (funcall p 'pov))
     (format ":%s: [[id:gone-id][Ghost]]\n" (funcall p 'location))
     ":END:\n")))

(defun test-context-pane--design (lang)
  "Return the design notebook text for LANGUAGE."
  (let ((h (lambda (key) (org-scribe-lang-heading key lang)))
        (m (lambda (key) (plist-get (org-scribe-lang-pack lang) :messages))))
    (cl-flet ((label (key) (alist-get key (funcall m nil))))
      (concat
       "* " (funcall h 'cast) "\n** " (funcall h 'characters) "\n"
       "*** Ana\n:PROPERTIES:\n:ID: ana-id\n:Role: Protagonist\n:Goal: Steal the diamond\n:Weight: 1\n:END:\n"
       "- " (label 'capture-char-lie) " Nobody can be trusted,\n  so work alone.\n"
       "**** " (label 'capture-char-desire) "\n"
       "- " (label 'capture-char-goal) " Take the diamond.\n"
       "- " (label 'capture-char-motivation) " Pay the debt.\n"
       "**** " (label 'capture-char-need) "\n"
       "- " (label 'capture-char-goal) " Trust one person.\n"
       "*** Bob\n:PROPERTIES:\n:ID: bob-id\n:Role: Ally\n:Goal: Open the door\n:END:\n"
       "*** Carl\n:PROPERTIES:\n:ID: carl-id\n:Role: Opponent\n:Goal: Catch Ana\n:END:\n"
       "*** Dora\n:PROPERTIES:\n:ID: dora-id\n:Role: Supporting\n:Weight: -1\n:END:\n"
       "* " (funcall h 'world) "\n** " (funcall h 'setting) "\n"
       "*** The Vault\n:PROPERTIES:\n:ID: vault-id\n:Type: Setting\n:END:\n"
       "- " (label 'capture-loc-echo) " Cold and closed.\n"
       "- " (label 'capture-loc-hazard) " One way out.\n"
       "*** The Alley\n:PROPERTIES:\n:ID: alley-id\n:Type: Setting\n:END:\n"
       "- " (label 'capture-loc-echo) " Wet and narrow.\n"
       "* " (funcall h 'plot-part) "\n** " (funcall h 'plot-threads) "\n"
       "*** Main Line\n:PROPERTIES:\n:ID: thread-id\n:THREAD-TYPE: Main\n:END:\n"
       "#+begin_comment\nInstruction, not prose.\n#+end_comment\n\n"
       "Ana against the vault.\nOne night.\n\n"
       "**** Resolution\n\nLater.\n"
       "** " (funcall h 'plot-points) "\n"
       "*** 4. Inciting Incident\n:PROPERTIES:\n:ID: pp-id\n:END:\n\n"
       "The call that cannot be refused.\n"
       "*** 9. Midpoint\n:PROPERTIES:\n:ID: pp2-id\n:END:\n\n"
       "Everything changes here.\n"))))

(defmacro test-context-pane--with-project (lang &rest body)
  "Run BODY in a temp novel project in LANG, point on \"The Heist\".
The global switch is on; `root', `manuscript' and `design' are bound."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "test-ctx-" t)))
          (default-directory root)
          (org-id-locations-file (expand-file-name "ids" root))
          (org-id-locations nil)
          (org-id-track-globally t)
          (org-scribe-context-pane-enable t)
          (org-scribe-message-language ,lang)
          (manuscript (expand-file-name (org-scribe-lang-file 'manuscript-novel ,lang) root))
          (design (expand-file-name (org-scribe-lang-file 'design ,lang) root)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert (format "# Type: novel\n# Language: %s\n" ,lang)))
           (with-temp-file manuscript (insert (test-context-pane--manuscript ,lang)))
           (with-temp-file design (insert (test-context-pane--design ,lang)))
           (org-scribe-project-type-cache-clear)
           (with-current-buffer (find-file-noselect manuscript)
             (goto-char (point-min))
             (re-search-forward "^\\*\\*\\* TOWRITE The Heist")
             ,@body))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p root (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (org-scribe-project-type-cache-clear)
       (delete-directory root t))))

(defun test-context-pane--goto (title)
  "Move point to the scene heading TITLE."
  (goto-char (point-min))
  (re-search-forward (format "^\\*\\*\\* TOWRITE %s" (regexp-quote title))))

(defun test-context-pane--text (data)
  "Return what the pane body draws for DATA, as plain text."
  (with-temp-buffer
    (org-scribe-context-pane--insert-scene data)
    (buffer-substring-no-properties (point-min) (point-max))))

;;; Switches

(ert-deftest test-context-pane-gate-nil-when-undecided ()
  (test-context-pane--with-project 'en
    (should (null (org-scribe-context-pane-gate root)))))

(ert-deftest test-context-pane-enable-and-disable-for-project ()
  (test-context-pane--with-project 'en
    (org-scribe-context-pane-enable-for-project)
    (should (eq 'yes (org-scribe-context-pane-gate root)))
    (org-scribe-context-pane-disable-for-project)
    (should (eq 'no (org-scribe-context-pane-gate root)))
    (should (null (org-scribe-companion-gate root)))))

(ert-deftest test-context-pane-switch-off-is-a-user-error ()
  (test-context-pane--with-project 'en
    (let ((org-scribe-context-pane-enable nil) (asked nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (setq asked t) t)))
        (should-error (org-scribe-context-pane-open) :type 'user-error))
      (should-not asked)
      (should (null (org-scribe-context-pane-gate root))))))

(ert-deftest test-context-pane-undecided-asks-once-and-records ()
  (test-context-pane--with-project 'en
    (let ((asked 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t))
                ((symbol-function 'org-scribe-workspace) #'ignore))
        (org-scribe-context-pane-open)
        (org-scribe-context-pane-open)
        (should (= 1 asked))
        (should (eq 'yes (org-scribe-context-pane-gate root)))))))

(ert-deftest test-context-pane-declined-records-no ()
  (test-context-pane--with-project 'en
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (org-scribe-context-pane-open)
      (should (eq 'no (org-scribe-context-pane-gate root))))))

(ert-deftest test-context-pane-module-not-loaded-while-switch-off ()
  "In a fresh session the switch off leaves the module unloaded."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (base (make-temp-file "test-ctx-load-" t))
         (form (format "(let ((repo %S) (base %S))
  (setq org-id-locations-file (expand-file-name \"ids\" base)
        make-backup-files nil create-lockfiles nil)
  (load (expand-file-name \"test-load.el\" repo) nil t)
  (with-temp-file (expand-file-name \".org-scribe-project\" base)
    (insert \"# Writing project: Probe\\n# Type: novel\\n\"))
  (let ((default-directory (file-name-as-directory base)))
    (princ (format \"RESULT:%%S\"
                   (list :error (condition-case e (org-scribe-context-pane-open)
                                  (user-error (cadr e)))
                         :loaded (featurep 'org-scribe-context-pane)
                         :layout (assq 'context org-scribe-workspace-layouts)
                         :close (progn (org-scribe-context-pane-close) t)
                         :loaded-after-close (featurep 'org-scribe-context-pane))))))"
                       test-context-pane--repo base)))
    (unwind-protect
        (let* ((output (with-temp-buffer
                         (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                         (buffer-string)))
               (result (and (string-match "RESULT:\\(.*\\)\\'" output)
                            (car (read-from-string (match-string 1 output))))))
          (should result)
          (should (stringp (plist-get result :error)))
          (should-not (plist-get result :loaded))
          (should-not (plist-get result :layout))
          (should-not (plist-get result :loaded-after-close)))
      (delete-directory base t))))

(ert-deftest test-context-pane-messages-in-both-languages ()
  (dolist (key '(context-pane-error-disabled context-pane-ask-enable
                 context-pane-ask-reenable context-pane-staying-disabled
                 context-pane-enabled-for-project context-pane-disabled-for-project
                 context-pane-error-not-manuscript
                 context-pane-pov context-pane-no-pov context-pane-goal
                 context-pane-motivation context-pane-need context-pane-lie
                 context-pane-description context-pane-summary context-pane-cast
                 context-pane-no-characters context-pane-more context-pane-hidden
                 context-pane-location context-pane-echo context-pane-hazard
                 context-pane-threads context-pane-plot-point context-pane-scene
                 context-pane-none context-pane-missing context-pane-words
                 context-pane-no-scene))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es)))
  (dolist (prop org-scribe-context-pane-scene-properties)
    (let ((key (intern (format "context-pane-prop-%s" prop))))
      (should (alist-get key org-scribe-messages-en))
      (should (alist-get key org-scribe-messages-es)))))

;;; Collecting and drawing

(ert-deftest test-context-pane-full-scene ()
  (test-context-pane--with-project 'en
    (let* ((data (org-scribe-context-pane--scene-data))
           (text (test-context-pane--text data))
           (pov (plist-get data :pov)))
      (should (equal "The Heist" (plist-get data :title)))
      (should (equal '("Act One" "Chapter One" "The Heist") (plist-get data :path)))
      (should (equal "Steal the diamond" (plist-get pov :goal)))
      (should (equal "Pay the debt." (plist-get pov :motivation)))
      (should (equal "Trust one person." (plist-get pov :need)))
      (should (equal "Nobody can be trusted, so work alone." (plist-get pov :lie)))
      (should (string-match-p "^Echo: Cold and closed\\.$" text))
      (should (string-match-p "^Hazard: One way out\\.$" text))
      ;; thread premise skips the comment block and stops at the blank line
      (should (string-match-p "^  Ana against the vault\\. One night\\.$" text))
      (should (string-match-p "^  The call that cannot be refused\\.$" text))
      (should (string-match-p "^Description: Ana cracks the vault$" text))
      (should-not (string-match-p "^Summary:" text)))))

(ert-deftest test-context-pane-pane-order ()
  (test-context-pane--with-project 'en
    (let* ((text (test-context-pane--text (org-scribe-context-pane--scene-data)))
           (positions (mapcar (lambda (s) (string-match s text))
                              '("^PoV" "^Description" "^Cast" "^Location"
                                "^Plot threads" "^Plot point" "^Scene$"))))
      (should (cl-every #'identity positions))
      (should (equal positions (sort (copy-sequence positions) #'<))))))

(ert-deftest test-context-pane-scene-block-order-and-blanks ()
  (test-context-pane--with-project 'en
    (let* ((text (test-context-pane--text (org-scribe-context-pane--scene-data)))
           (block (substring text (string-match "^Scene$" text))))
      (should (string-match-p "Motivation: Wants the diamond" block))
      (should (< (string-match "Motivation:" block) (string-match "Conflict:" block)
                 (string-match "Outcome:" block)))
      ;; the blank Gap is skipped, not shown as an empty label
      (should-not (string-match-p "Gap" block))
      (should-not (string-match-p "At stake" block)))))

(ert-deftest test-context-pane-cast-order-dots-and-hidden ()
  (test-context-pane--with-project 'en
    (let* ((text (test-context-pane--text (org-scribe-context-pane--scene-data)))
           (cast (substring text (string-match "^Cast" text)
                            (string-match "^Location" text))))
      ;; in the scene first (filled dot), then the rest (hollow dot)
      (should (string-match-p "● Ana · Protagonist · Steal the diamond" cast))
      (should (string-match-p "● Bob · Ally · Open the door" cast))
      (should (string-match-p "○ Carl · Opponent · Catch Ana" cast))
      (should (< (string-match "● Bob" cast) (string-match "○ Carl" cast)))
      ;; Dora has a negative Weight: left out, with one dim line
      (should-not (string-match-p "Dora" cast))
      (should (string-match-p "(1 hidden)" cast)))))

(ert-deftest test-context-pane-empty-scene-says-so ()
  (test-context-pane--with-project 'en
    (test-context-pane--goto "Empty Scene")
    (let* ((data (org-scribe-context-pane--scene-data))
           (text (test-context-pane--text data)))
      (should (null (plist-get data :pov)))
      (should (string-match-p "no PoV set yet" text))
      ;; the whole cast is still listed, all hollow
      (should (string-match-p "○ Ana" text))
      (should-not (string-match-p "●" text))
      (should (string-match-p "^Location\nnone set$" text))
      (should-not (string-match-p "^Scene$" text)))))

(ert-deftest test-context-pane-location-only-scene ()
  (test-context-pane--with-project 'en
    (test-context-pane--goto "Place Only")
    (let ((text (test-context-pane--text (org-scribe-context-pane--scene-data))))
      (should (string-match-p "^The Vault$" text))
      (should (string-match-p "^Echo: Cold and closed" text))
      (should (string-match-p "no PoV set yet" text))
      (should (string-match-p "^Plot threads\nnone set$" text)))))

(ert-deftest test-context-pane-broken-link-is-reported ()
  (test-context-pane--with-project 'en
    (test-context-pane--goto "Broken")
    (let* ((data (org-scribe-context-pane--scene-data))
           (text (test-context-pane--text data)))
      (should (plist-get (plist-get data :pov) :missing))
      (should (plist-get (car (plist-get data :locations)) :missing))
      (should (string-match-p "Ghost (deleted)" text)))))

(ert-deftest test-context-pane-large-cast-collapses ()
  (test-context-pane--with-project 'en
    (let ((org-scribe-context-pane-cast-collapse-threshold 1)
          (org-scribe-context-pane--cast-expanded nil))
      (let ((collapsed (test-context-pane--text
                        (org-scribe-context-pane--scene-data))))
        ;; only Carl is off-scene and visible: 1 is not over the threshold
        (should (string-match-p "○ Carl" collapsed)))
      (let ((org-scribe-context-pane-cast-collapse-threshold 0))
        (let ((collapsed (test-context-pane--text
                          (org-scribe-context-pane--scene-data))))
          (should (string-match-p "\\+ 1 more: Carl" collapsed))
          (should-not (string-match-p "○ Carl" collapsed))
          ;; the characters in the scene are never collapsed
          (should (string-match-p "● Ana" collapsed)))
        (let ((org-scribe-context-pane--cast-expanded t))
          (should (string-match-p "○ Carl"
                                  (test-context-pane--text
                                   (org-scribe-context-pane--scene-data)))))))))

(ert-deftest test-context-pane-spanish-project ()
  (test-context-pane--with-project 'es
    (let* ((data (org-scribe-context-pane--scene-data))
           (text (test-context-pane--text data))
           (pov (plist-get data :pov)))
      (should (equal "Pay the debt." (plist-get pov :motivation)))
      (should (string-match-p "^Mentira: Nobody can be trusted" text))
      (should (string-match-p "^Reparto$" text))
      (should (string-match-p "^Eco: Cold and closed" text))
      (should (string-match-p "^Escollo: One way out" text))
      (should (string-match-p "^Escenario$" text))
      (should (string-match-p "^Hilos de la trama$" text))
      ;; the scene block reads the Spanish property names
      (should (string-match-p "^Motivación: Wants the diamond" text))
      (should (string-match-p "^Desenlace: The vault is empty" text))
      (should (string-match-p "(1 ocultos)" text))
      (should (string-match-p "Palabras: [0-9]+"
                              (org-scribe-context-pane--header data))))))

;;; Header line

(ert-deftest test-context-pane-header-path-and-words ()
  (test-context-pane--with-project 'en
    (let* ((data (org-scribe-context-pane--scene-data))
           (header (org-scribe-context-pane--header data)))
      (should (string-match-p "Act One › Chapter One › The Heist" header))
      (should (string-match-p "Words: [1-9][0-9]*" header))
      (should (= (plist-get data :words)
                 (save-excursion
                   (org-back-to-heading t)
                   (org-scribe--count-words-region
                    (point) (save-excursion (org-end-of-subtree t t) (point)))))))))

(ert-deftest test-context-pane-words-match-the-in-place-count ()
  "Counting in a scratch buffer gives the number the word-count code gives."
  (test-context-pane--with-project 'en
    (dolist (title '("The Heist" "Empty Scene" "Place Only"))
      (test-context-pane--goto title)
      (should (= (org-scribe-context-pane--scene-words)
                 (save-excursion
                   (org-back-to-heading t)
                   (org-scribe--count-words-region
                    (point) (save-excursion (org-end-of-subtree t t) (point)))))))))

(ert-deftest test-context-pane-header-escapes-percent ()
  (should (string-match-p "100%%"
                          (org-scribe-context-pane--header
                           '(:path ("100% sure") :words 3)))))

(ert-deftest test-context-pane-short-story-has-no-chapter ()
  (let* ((root (file-name-as-directory (make-temp-file "test-ctx-ss-" t)))
         (default-directory root)
         (org-id-locations-file (expand-file-name "ids" root))
         (org-id-locations nil)
         (manuscript (expand-file-name "story.org" root)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" root)
            (insert "# Type: short-story\n# Language: en\n"))
          (with-temp-file manuscript
            (insert "#+TODO: TOWRITE | FINISHED PURGE\n* Story Content\n"
                    "** TOWRITE Only Scene :ignore:\n:PROPERTIES:\n:ID: only-id\n:END:\nSome words here.\n"))
          (with-temp-file (expand-file-name "design.org" root) (insert "* Cast\n"))
          (org-scribe-project-type-cache-clear)
          (with-current-buffer (find-file-noselect manuscript)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* TOWRITE Only Scene")
            (let ((data (org-scribe-context-pane--scene-data)))
              (should (equal "Only Scene" (plist-get data :title)))
              (should (equal '("Story Content" "Only Scene") (plist-get data :path)))
              (should (null (plist-get data :pov)))))
          (set-buffer-modified-p nil))
      (when-let* ((b (get-file-buffer manuscript))) (kill-buffer b))
      (org-scribe-project-type-cache-clear)
      (delete-directory root t))))

(ert-deftest test-context-pane-outside-a-scene-is-nil ()
  (test-context-pane--with-project 'en
    (goto-char (point-min))
    (should (null (org-scribe-context-pane--scene-data)))
    (re-search-forward "^\\*\\* Chapter One")
    (should (null (org-scribe-context-pane--scene-data)))))

;;; Refresh, throttle and layout

(ert-deftest test-context-pane-refresh-recomputes-only-on-change ()
  (test-context-pane--with-project 'en
    (let ((calls 0)
          (org-scribe-context-pane--source (current-buffer))
          (org-scribe-context-pane--last-key nil)
          (org-scribe-context-pane--drawn-id nil))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let ((pane (org-scribe-context-pane--buffer)))
          (unwind-protect
              (cl-letf* ((orig (symbol-function 'org-scribe-context-pane--scene-data))
                         ((symbol-function 'org-scribe-context-pane--scene-data)
                          (lambda () (cl-incf calls) (funcall orig))))
                (split-window-right)
                (set-window-buffer (next-window) pane)
                (org-scribe-context-pane--refresh)
                (org-scribe-context-pane--refresh)
                (org-scribe-context-pane--refresh)
                (should (= 1 calls))
                ;; an edit changes the tick: recompute
                (insert " ")
                (org-scribe-context-pane--refresh)
                (should (= 2 calls))
                ;; another scene: recompute
                (test-context-pane--goto "Empty Scene")
                (org-scribe-context-pane--refresh)
                (should (= 3 calls))
                (with-current-buffer pane
                  (should (string-match-p "no PoV set yet" (buffer-string)))))
            (kill-buffer pane)))))))

(ert-deftest test-context-pane-buffer-is-read-only ()
  (let ((buffer (org-scribe-context-pane--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (should buffer-read-only)
          (should-error (insert "x") :type 'buffer-read-only))
      (kill-buffer buffer))))

(ert-deftest test-context-pane-layout-opens-and-closes ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (org-scribe-context-pane-mode 1)
      (unwind-protect
          (let ((pane (get-buffer org-scribe-context-pane--buffer-name)))
            (should pane)
            (should (= 2 (length (window-list))))
            (should (get-buffer-window pane))
            (should (timerp org-scribe-context-pane--timer))
            (with-current-buffer pane
              (should (string-match-p "Steal the diamond" (buffer-string)))
              (should header-line-format)))
        (org-scribe-context-pane-mode -1))
      (should-not (get-buffer org-scribe-context-pane--buffer-name))
      (should (null org-scribe-context-pane--timer))
      (should (= 1 (length (window-list)))))))

(ert-deftest test-context-pane-is-an-exclusive-layout ()
  (should (eq 'org-scribe-context-pane-mode
              (cdr (assq 'context org-scribe-workspace-layouts))))
  (should (memq 'org-scribe-context-pane-mode org-scribe-exclusive-modes)))

(ert-deftest test-context-pane-refuses-a-file-that-is-not-the-manuscript ()
  (test-context-pane--with-project 'en
    (with-current-buffer (find-file-noselect design)
      (should-error (org-scribe-context-pane-mode 1) :type 'user-error)
      (should-not org-scribe-context-pane-mode))))

;;; Regression tests for the review of commit 157ec9c

(ert-deftest test-context-pane-spanish-project-with-english-messages ()
  "The notebook's labels follow the project's language, not the interface's."
  (test-context-pane--with-project 'es
    (let ((org-scribe-message-language 'en))
      (let ((pov (plist-get (org-scribe-context-pane--scene-data) :pov)))
        (should (equal "Pay the debt." (plist-get pov :motivation)))
        (should (equal "Trust one person." (plist-get pov :need)))
        (should (string-match-p "Nobody can be trusted" (plist-get pov :lie)))
        (let ((loc (car (plist-get (org-scribe-context-pane--scene-data) :locations))))
          (should (equal "Cold and closed." (plist-get loc :echo))))))))

(ert-deftest test-context-pane-shows-every-location-and-plot-point ()
  (test-context-pane--with-project 'en
    (org-set-property "Location" "[[id:vault-id][The Vault]], [[id:alley-id][The Alley]]")
    (org-set-property "Plot-point" "[[id:pp-id][4. Inciting Incident]], [[id:pp2-id][9. Midpoint]]")
    (let* ((data (org-scribe-context-pane--scene-data))
           (text (test-context-pane--text data)))
      (should (= 2 (length (plist-get data :locations))))
      (should (= 2 (length (plist-get data :plot-points))))
      (should (string-match-p "^The Alley$" text))
      (should (string-match-p "^Echo: Wet and narrow\\.$" text))
      (should (string-match-p "^9\\. Midpoint$" text))
      (should (string-match-p "^  Everything changes here\\.$" text)))))

(ert-deftest test-context-pane-plain-names-and-dead-links-in-the-cast ()
  (test-context-pane--with-project 'en
    (org-set-property "Characters"
                      "Bob, [[id:gone-id][Ghost]], Zed, [[id:carl-id][Carl]]")
    (let* ((data (org-scribe-context-pane--scene-data))
           (cast (plist-get data :cast))
           (text (test-context-pane--text data)))
      ;; a plain name that matches a character puts that character in the scene
      (should (plist-get (seq-find (lambda (c) (equal "Bob" (plist-get c :name))) cast)
                         :in-scene))
      (should (plist-get (seq-find (lambda (c) (equal "Carl" (plist-get c :name))) cast)
                         :in-scene))
      ;; a dead link and an unknown name are said, not dropped
      (should (string-match-p "● Ghost (deleted)" text))
      (should (string-match-p "● Zed (not in the cast)" text))
      (should-not (string-match-p "○ Bob" text)))))

(ert-deftest test-context-pane-unknown-scene-property-is-ignored ()
  (test-context-pane--with-project 'en
    (let ((org-scribe-context-pane-scene-properties '(gap bogus-key outcome "str")))
      (let ((props (plist-get (org-scribe-context-pane--scene-data) :properties)))
        (should (equal '(outcome) (mapcar #'car props)))))))

(ert-deftest test-context-pane-collection-failure-is-reported-not-hidden ()
  (test-context-pane--with-project 'en
    (let ((org-scribe-context-pane--source (current-buffer))
          (org-scribe-context-pane--last-key nil)
          (org-scribe-context-pane--drawn-id nil))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let ((pane (org-scribe-context-pane--buffer)))
          (unwind-protect
              (cl-letf (((symbol-function 'org-scribe-context-pane--scene-data)
                         (lambda () (error "boom"))))
                (split-window-right)
                (set-window-buffer (next-window) pane)
                (let ((inhibit-message t)) (org-scribe-context-pane--refresh))
                (with-current-buffer pane
                  (should (string-match-p "Cannot read this scene: boom" (buffer-string)))
                  (should-not (string-match-p "not in a scene" (buffer-string)))))
            (kill-buffer pane)))))))

(ert-deftest test-context-pane-keys-from-the-pane-act-on-the-manuscript ()
  (test-context-pane--with-project 'en
    (org-scribe-context-pane-enable-for-project)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((source (current-buffer)))
        ;; the open key, pressed in the pane, closes the layout
        (org-scribe-context-pane-mode 1)
        (select-window (get-buffer-window org-scribe-context-pane--buffer-name))
        (org-scribe-context-pane-open)
        (should-not (buffer-local-value 'org-scribe-context-pane-mode source))
        (should (= 1 (length (window-list))))
        ;; so does the workspace's "normal" (F8 0)
        (with-current-buffer source (org-scribe-context-pane-mode 1))
        (select-window (get-buffer-window org-scribe-context-pane--buffer-name))
        (org-scribe-workspace 'normal)
        (should-not (buffer-local-value 'org-scribe-context-pane-mode source))
        (should (= 1 (length (window-list))))))))

(ert-deftest test-context-pane-q-closes-the-layout ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((source (current-buffer)))
        (org-scribe-context-pane-mode 1)
        (with-current-buffer org-scribe-context-pane--buffer-name
          (should (eq 'org-scribe-context-pane-close (key-binding "q")))
          (org-scribe-context-pane-close))
        (should-not (buffer-local-value 'org-scribe-context-pane-mode source))
        (should (null org-scribe-context-pane--timer))
        (should (= 1 (length (window-list))))))))

(ert-deftest test-context-pane-survives-a-revert ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (org-scribe-context-pane-mode 1)
      (unwind-protect
          (progn
            (set-buffer-modified-p nil)
            (revert-buffer t t)
            (should org-scribe-context-pane-mode)
            (should (= 2 (length (window-list))))
            (should (timerp org-scribe-context-pane--timer)))
        (org-scribe-context-pane-mode -1)))))

(ert-deftest test-context-pane-second-setup-does-not-leak-a-timer ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((old (run-with-idle-timer 1000 t #'ignore)))
        (setq org-scribe-context-pane--timer old)
        (org-scribe-context-pane-mode 1)
        (unwind-protect
            (progn (should-not (memq old timer-idle-list))
                   (should-not (eq old org-scribe-context-pane--timer)))
          (org-scribe-context-pane-mode -1))))))

(ert-deftest test-context-pane-failed-setup-restores-the-windows ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (split-window-below)
      (let ((before (length (window-list))))
        (cl-letf (((symbol-function 'split-window-right)
                   (lambda (&rest _) (error "Window too small"))))
          (should-error (org-scribe-context-pane-mode 1)))
        (should-not org-scribe-context-pane-mode)
        (should (= before (length (window-list))))
        (should (null org-scribe-context-pane--timer))
        (should-not (get-buffer org-scribe-context-pane--buffer-name))))))

(ert-deftest test-context-pane-wide-setting-is-clamped ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((org-scribe-context-pane-width-percent 0.99))
        (org-scribe-context-pane-mode 1)
        (unwind-protect
            (should (<= (window-total-width
                         (get-buffer-window org-scribe-context-pane--buffer-name))
                        (ceiling (* 0.62 (frame-width)))))
          (org-scribe-context-pane-mode -1))))))

(ert-deftest test-context-pane-reading-the-cast-never-writes-the-notebook ()
  (test-context-pane--with-project 'en
    ;; a character with no ID yet
    (with-temp-file design
      (insert (concat (test-context-pane--design 'en)
                      "")))
    (let ((text (with-temp-buffer
                  (insert-file-contents design)
                  (goto-char (point-min))
                  (re-search-forward "^\\*\\*\\* Carl\n:PROPERTIES:\n:ID: carl-id\n")
                  (replace-match "*** Carl\n:PROPERTIES:\n")
                  (buffer-string))))
      (with-temp-file design (insert text)))
    (let ((disk-before (with-temp-buffer (insert-file-contents design) (buffer-string))))
      (let ((cast (plist-get (org-scribe-context-pane--scene-data) :cast)))
        ;; Carl is listed although he has no ID, and nothing was minted
        (should (seq-find (lambda (c) (equal "Carl" (plist-get c :name))) cast))
        (should (null (plist-get (seq-find (lambda (c) (equal "Carl" (plist-get c :name)))
                                           cast)
                                 :id))))
      (should (equal disk-before
                     (with-temp-buffer (insert-file-contents design) (buffer-string))))
      (when-let* ((buffer (find-buffer-visiting design)))
        (should-not (buffer-modified-p buffer))))))

(ert-deftest test-context-pane-an-edit-does-not-reread-the-notebook ()
  (test-context-pane--with-project 'en
    (let ((reads 0))
      (cl-letf* ((orig (symbol-function 'org-scribe-context-pane--read-cast))
                 ((symbol-function 'org-scribe-context-pane--read-cast)
                  (lambda () (cl-incf reads) (funcall orig))))
        (org-scribe-context-pane--scene-data)
        (org-scribe-context-pane--scene-data)
        (should (= 1 reads))
        ;; typing in the manuscript leaves the notebook alone: still cached
        (goto-char (point-max)) (insert " more words")
        (test-context-pane--goto "The Heist")
        (org-scribe-context-pane--scene-data)
        (should (= 1 reads))
        ;; changing the notebook invalidates it
        (with-current-buffer (find-buffer-visiting design)
          (goto-char (point-max)) (insert "\n"))
        (org-scribe-context-pane--scene-data)
        (should (= 2 reads))))))

(ert-deftest test-context-pane-dead-links-rescan-once ()
  (test-context-pane--with-project 'en
    (test-context-pane--goto "Broken")
    (let ((scans 0))
      (cl-letf (((symbol-function 'org-id-update-id-locations)
                 (lambda (&optional files &rest _)
                   ;; only the pane's own targeted rescan: org-id's is its own
                   (when files (cl-incf scans)))))
        ;; two dead IDs in the scene (PoV and location)
        (org-scribe-context-pane--scene-data)
        (should (<= scans 1))
        (org-scribe-context-pane--scene-data)
        (should (<= scans 1))))))

(ert-deftest test-context-pane-entity-opens-beside-not-over-the-manuscript ()
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (org-scribe-context-pane-mode 1)
      (unwind-protect
          (let* ((source (current-buffer))
                 (pane (get-buffer org-scribe-context-pane--buffer-name))
                 (window (org-scribe-context-pane--visit-window)))
            ;; two windows, one per buffer: the manuscript's is split below
            (should (window-live-p window))
            (should-not (eq pane (window-buffer window)))
            (should (= 3 (length (window-list))))
            (should (get-buffer-window source))
            (should (get-buffer-window pane))
            ;; with a third window already there, it is reused
            (let ((windows (length (window-list))))
              (set-window-buffer window (get-buffer-create "*other*"))
              (should (eq window (org-scribe-context-pane--visit-window)))
              (should (= windows (length (window-list))))))
        (org-scribe-context-pane-mode -1)
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

(ert-deftest test-context-pane-refusal-comes-before-the-question ()
  (test-context-pane--with-project 'en
    (let ((asked 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t)))
        (with-current-buffer (find-file-noselect design)
          (should-error (org-scribe-context-pane-open) :type 'user-error)))
      (should (= 0 asked))
      (should (null (org-scribe-context-pane-gate root))))))

(ert-deftest test-context-pane-clean-strips-emphasis-and-keeps-long-text ()
  (should (equal "It happens to Alex, really and code."
                 (org-scribe-context-pane--clean
                  "It happens *to* Alex, /really/ and =code=.")))
  (should (equal "a/b and 2*3*4" (org-scribe-context-pane--clean "a/b and 2*3*4")))
  (let ((long (mapconcat #'identity (make-list 40 "word") " ")))
    (should (equal long (org-scribe-context-pane--clean long)))))

(ert-deftest test-context-pane-chapter-heading-shows-position ()
  (test-context-pane--with-project 'en
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Chapter One")
    (let ((data (org-scribe-context-pane--position-data)))
      (should (plist-get data :no-scene))
      (should (equal '("Act One" "Chapter One") (append (plist-get data :path) nil))))))

(ert-deftest test-context-pane-collapsed-line-has-no-dangling-ellipsis ()
  (let ((org-scribe-context-pane--cast-expanded nil)
        (org-scribe-context-pane-cast-collapse-threshold 1))
    (with-temp-buffer
      (org-scribe-context-pane--insert-cast
       (mapcar (lambda (n) (list :name n)) '("Ann" "Bob" "Cy")))
      (should-not (string-match-p "\\.\\.\\.\\|…" (buffer-string))))
    (with-temp-buffer
      (org-scribe-context-pane--insert-cast
       (mapcar (lambda (n) (list :name n)) '("Ann" "Bob" "Cy" "Di")))
      (should (string-match-p "…" (buffer-string))))))

(ert-deftest test-context-pane-setup-check-reports-switch-and-gate ()
  (let ((org-scribe-context-pane-enable nil))
    (with-temp-buffer
      (org-scribe--setup-check-context-pane)
      (should (string-match-p "context-pane-enable.*global switch" (buffer-string)))
      (should-not (featurep 'org-scribe-context-pane-never-loaded)))))

;;; Gaps from the test review of commit 157ec9c

(defun test-context-pane--pane-text ()
  "Return the pane buffer's text, without properties."
  (with-current-buffer org-scribe-context-pane--buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-context-pane-live-idle-timer-follows-point ()
  "The real timer, not a direct call, redraws the pane after point moves."
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((org-scribe-context-pane-idle-delay 0.05)
            (source (current-buffer)))
        (org-scribe-context-pane-mode 1)
        (unwind-protect
            (progn
              (should (string-match-p
                       "The Heist"
                       (buffer-local-value
                        'header-line-format
                        (get-buffer org-scribe-context-pane--buffer-name))))
              (test-context-pane--goto "Place Only")
              (set-window-point (get-buffer-window source) (point))
              ;; The timer itself: an idle, repeating timer on the tick.
              (should (timerp org-scribe-context-pane--timer))
              (should (eq 'org-scribe-context-pane--tick
                          (timer--function org-scribe-context-pane--timer)))
              (should (timer--repeat-delay org-scribe-context-pane--timer))
              (if noninteractive
                  ;; Batch Emacs never goes idle, so fire it the way the
                  ;; event loop does.
                  (timer-event-handler org-scribe-context-pane--timer)
                (let ((deadline (+ (float-time) 3)))
                  (while (and (< (float-time) deadline)
                              (not (string-match-p
                                    "Place Only"
                                    (buffer-local-value
                                     'header-line-format
                                     (get-buffer org-scribe-context-pane--buffer-name)))))
                    (sit-for 0.1))))
              (should (string-match-p
                       "Place Only"
                       (buffer-local-value
                        'header-line-format
                        (get-buffer org-scribe-context-pane--buffer-name)))))
          (org-scribe-context-pane-mode -1))
        ;; and once closed, the timer is gone
        (should (null org-scribe-context-pane--timer))))))

(ert-deftest test-context-pane-toggle-cast-button-expands-and-collapses ()
  "Pressing the \"more\" button shows every name, and again collapses."
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (let ((org-scribe-context-pane-cast-collapse-threshold 0))
        (org-scribe-context-pane-mode 1)
        (unwind-protect
            (with-current-buffer org-scribe-context-pane--buffer-name
              (goto-char (point-min))
              (let ((button (text-property-search-forward
                             'action #'org-scribe-context-pane--toggle-cast #'eq)))
                (should button)
                (goto-char (prop-match-beginning button))
                (should-not org-scribe-context-pane--cast-expanded)
                (push-button)
                (should org-scribe-context-pane--cast-expanded)
                (should-not (string-match-p "RET to expand"
                                            (test-context-pane--pane-text)))))
          (org-scribe-context-pane-mode -1))))))

(ert-deftest test-context-pane-narrowed-source-is-read-widened-and-restored ()
  "A narrowed manuscript still gets its scene, and stays narrowed."
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (org-scribe-context-pane-mode 1)
      (unwind-protect
          (progn
            (test-context-pane--goto "Place Only")
            (org-back-to-heading t)
            (org-narrow-to-subtree)
            (let ((beg (point-min)) (end (point-max)))
              (set-window-point (get-buffer-window (current-buffer)) (point))
              (org-scribe-context-pane--refresh t)
              (should (string-match-p
                       "Act One › Chapter One › Place Only"
                       (buffer-local-value
                        'header-line-format
                        (get-buffer org-scribe-context-pane--buffer-name))))
              (should (= beg (point-min)))
              (should (= end (point-max)))))
        (widen)
        (org-scribe-context-pane-mode -1)))))

(ert-deftest test-context-pane-real-frame-layout ()
  "On a graphical frame the pane takes its configured share of the width."
  (skip-unless (display-graphic-p))
  (test-context-pane--with-project 'en
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (org-scribe-context-pane-mode 1)
      (unwind-protect
          (let* ((pane (get-buffer-window org-scribe-context-pane--buffer-name))
                 (share (/ (float (window-total-width pane))
                           (frame-width))))
            (should (< (abs (- share org-scribe-context-pane-width-percent)) 0.05)))
        (org-scribe-context-pane-mode -1)))))

(provide 'test-context-pane)
;;; test-context-pane.el ends here
