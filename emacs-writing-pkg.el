;;; emacs-writing-pkg.el --- Package metadata for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Code:

(define-package "emacs-writing" "0.1.0"
  "Comprehensive toolkit for creative writing in Org-mode"
  '((emacs "29.1")
    (org "9.6")
    (org-ql "0.8")
    (writeroom-mode "3.7")
    (wc-mode "1.3"))
  :keywords '("writing" "org-mode" "novel" "fiction")
  :url "https://codeberg.org/jcastp/emacs-writing"
  :authors '(("Javier Castilla" . "jcastp@pm.me"))
  :maintainer '("Javier Castilla" . "jcastp@pm.me"))

;;; emacs-writing-pkg.el ends here
