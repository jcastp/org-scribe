;;; test-story-time.el --- Tests for the story-time parser -*- lexical-binding: t; -*-

;;; Commentary:

;; The parser is pure, so every test passes a CONTEXT directly; no project.

;;; Code:

(require 'ert)
(require 'org-scribe-story-time)

(defconst test-st--day (* 24 60))

(defun test-st--key (string &optional context)
  "Return STRING's key under CONTEXT, or nil."
  (plist-get (org-scribe-story-time-key string context) :key))

(ert-deftest test-story-time-relative-days ()
  (should (= (test-st--key "Day 1") 360))
  (should (= (test-st--key "Day 3") (+ (* 2 test-st--day) 360)))
  (should (= (test-st--key "Day 3, 14:00") (+ (* 2 test-st--day) 840)))
  (should (= (test-st--key "day 3,14:00") (+ (* 2 test-st--day) 840))))

(ert-deftest test-story-time-no-day-zero ()
  "Day -1 is the day before Day 1, and Day 0 is unreadable."
  (should (= (test-st--key "Day -1") (+ (- test-st--day) 360)))
  (should (< (test-st--key "Day -1") (test-st--key "Day 1")))
  (should (null (org-scribe-story-time-key "Day 0" nil)))
  (should (eq 'unreadable (org-scribe-story-time-problem "Day 0" nil))))

(ert-deftest test-story-time-night-ordering ()
  "Day 3 < Night 3 < Night 3 after midnight < Day 4."
  (let ((d3 (test-st--key "Day 3"))
        (n3 (test-st--key "Night 3"))
        (late (test-st--key "Night 3, 02:00"))
        (d4 (test-st--key "Day 4, 07:00")))
    (should (< d3 n3))
    (should (< n3 late))
    (should (< late d4))
    (should (= (test-st--key "Day 3, 22:00") (test-st--key "Night 3, 22:00")))))

(ert-deftest test-story-time-clock-beats-default ()
  (should (< (test-st--key "Day 3, 05:00") (test-st--key "Day 3")))
  (should (> (test-st--key "Day 3, 23:00") (test-st--key "Night 3"))))

(ert-deftest test-story-time-years ()
  (should (= (test-st--key "Year -9") (* -9 365 test-st--day)))
  (should (= (test-st--key "Year -9, Day 40")
             (+ (* -9 365 test-st--day) (* 39 test-st--day) 360)))
  (should (< (test-st--key "Year -9, Day 40") (test-st--key "Day -3")))
  (should (eq 'year (plist-get (org-scribe-story-time-key "Year -9" nil) :precision))))

(ert-deftest test-story-time-precision ()
  (should (eq 'day (plist-get (org-scribe-story-time-key "Day 2" nil) :precision)))
  (should (eq 'clock (plist-get (org-scribe-story-time-key "Day 2, 09:30" nil) :precision))))

(ert-deftest test-story-time-spanish-words-and-accents ()
  (should (= (test-st--key "Día 3, 14:00") (test-st--key "Day 3, 14:00")))
  (should (= (test-st--key "dia 3") (test-st--key "Day 3")))
  (should (= (test-st--key "Noche 3, 02:00") (test-st--key "Night 3, 02:00")))
  (should (= (test-st--key "Año -9") (test-st--key "Year -9"))))

(ert-deftest test-story-time-unreadable ()
  (dolist (text '("Dya 3" "Two years before Day 1" "Every Thursday"
                  "09:00 promised, 10:40 died" "Day 3, 25:00" "Day"))
    (should (null (org-scribe-story-time-key text nil)))
    (should (eq 'unreadable (org-scribe-story-time-problem text nil))))
  (should (eq 'blank (org-scribe-story-time-problem "  " nil)))
  (should (eq 'blank (org-scribe-story-time-problem nil nil))))

(ert-deftest test-story-time-timestamps-need-an-anchor ()
  (let ((ts "<2026-03-14 Sat 18:00>"))
    (should (null (org-scribe-story-time-key ts nil)))
    (should (eq 'no-anchor (org-scribe-story-time-problem ts nil)))))

(ert-deftest test-story-time-timestamps ()
  (let ((ctx (list :mode 'dates
                   :day-one (org-scribe-story-time--parse-date "2026-03-14"))))
    (should (= (test-st--key "<2026-03-14 Sat 18:00>" ctx) (* 18 60)))
    (should (= (test-st--key "[2026-03-15]" ctx) (+ test-st--day 360)))
    (should (= (test-st--key "<2026-03-13 Fri>" ctx) (- 360 test-st--day)))
    ;; Day N keeps working beside dates.
    (should (= (test-st--key "Day 2" ctx) (+ test-st--day 360)))
    (should (= (test-st--key "<2026-03-15 Sun>" ctx) (test-st--key "Day 2" ctx)))))

(defconst test-st--calendar
  '(:months (("Frostmoon" . 30) ("Thawmoon" . 31) ("Sunmoon" . 30))
    :era "AE" :day-one 100))

(ert-deftest test-story-time-invented-calendar ()
  (let* ((one (+ (* 312 91) 30 2))   ; 3 Thawmoon 312
         (cal (list :months (plist-get test-st--calendar :months)
                    :era "AE" :day-one one))
         (ctx (list :mode 'calendar :calendar cal)))
    (should (= (test-st--key "3 Thawmoon 312" ctx) 360))
    (should (= (test-st--key "4 Thawmoon 312" ctx) (+ test-st--day 360)))
    (should (= (test-st--key "14 Frostmoon 312 AE, 18:00" ctx)
               (+ (* (- (+ (* 312 91) 13) one) test-st--day) 1080)))
    (should (< (test-st--key "14 frostmoon 312" ctx) (test-st--key "Day 1" ctx)))
    ;; Out of range or unknown month.
    (should (null (test-st--key "31 Frostmoon 312" ctx)))
    (should (null (test-st--key "3 Nomoon 312" ctx)))
    (should (null (test-st--key "3 Thawmoon 312" nil)))))

(ert-deftest test-story-time-mode-defaults-to-relative ()
  (should (eq 'relative (org-scribe-story-time-mode nil))))

(provide 'test-story-time)
;;; test-story-time.el ends here
