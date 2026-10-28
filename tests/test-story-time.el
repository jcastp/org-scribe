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

;;; Naming a day

(ert-deftest test-story-time-bare-iso-date-counts-as-a-timestamp ()
  (let ((ctx (list :mode 'dates :day-one (calendar-absolute-from-gregorian '(3 10 2026)))))
    (should (equal (plist-get (org-scribe-story-time-key "2026-03-14" ctx) :key)
                   (plist-get (org-scribe-story-time-key "<2026-03-14>" ctx) :key)))
    (should (org-scribe-story-time-key "2026-03-14 18:00" ctx))
    (should (eq 'no-anchor (org-scribe-story-time-problem "2026-03-14" (list :mode 'dates))))))

(ert-deftest test-story-time-day-label-names-far-off-years ()
  (let* ((ctx (list :mode 'relative))
         (parsed (org-scribe-story-time-key "Year -9, Day 40" ctx)))
    (let ((org-scribe-message-language 'en))
      (cl-letf (((symbol-function 'org-scribe-project-language) (lambda (&rest _) 'en)))
        (should (equal "Year -9, Day 40" (org-scribe-story-time-day-label parsed ctx)))
        (should (equal "Year -9" (org-scribe-story-time-day-label
                                  (org-scribe-story-time-key "Year -9" ctx) ctx)))
        (should-not (org-scribe-story-time-day-label
                     (org-scribe-story-time-key "Day 3" ctx) ctx))))))

(ert-deftest test-story-time-day-label-gives-the-date-in-dates-mode ()
  (let ((ctx (list :mode 'dates :day-one (calendar-absolute-from-gregorian '(3 10 2026)))))
    (should (equal "2026-03-14"
                   (org-scribe-story-time-day-label
                    (org-scribe-story-time-key "<2026-03-14 18:00>" ctx) ctx)))
    (should (equal "2026-03-12"
                   (org-scribe-story-time-day-label
                    (org-scribe-story-time-key "Day 3" ctx) ctx)))))

(ert-deftest test-story-time-day-label-gives-the-calendar-date ()
  (let* ((months '(("Thaw" . 30) ("Sow" . 30) ("Frost" . 30)))
         (ctx (list :mode 'calendar
                    :calendar (list :months months :era "EC" :day-one 28082))))
    (should (equal "6 Thaw 312"
                   ;; Day 1 is count 28082 = 312*90+2, i.e. "3 Thaw 312"; +3 days
                   (org-scribe-story-time-day-label (list :key (* 3 1440)) ctx)))
    (should (equal "1 Sow 312"
                   (org-scribe-story-time-day-label (list :key (* 28 1440)) ctx)))))

(ert-deftest test-story-time-remark-after-a-comma-is-ignored-for-sorting ()
  (let ((ctx '(:mode relative)))
    (should (equal (org-scribe-story-time-key "Day 3, 14:00" ctx)
                   (org-scribe-story-time-key "Day 3, 14:00, in the cellar" ctx)))
    (should (equal (org-scribe-story-time-key "Day 3" ctx)
                   (org-scribe-story-time-key "Día 3, por la mañana" ctx)))
    (should (equal (org-scribe-story-time-key "Year -9, Day 40" ctx)
                   (org-scribe-story-time-key "Year -9, Day 40, flashback" ctx)))))

(ert-deftest test-story-time-remark-never-hides-a-bad-time ()
  (let ((ctx '(:mode relative)))
    (dolist (text '("Day 3, 25:00" "Day 3, Night 4" "Day 3, 14h" "Dya 3, in the cellar"))
      (should-not (org-scribe-story-time-key text ctx))
      (should (eq 'unreadable (org-scribe-story-time-problem text ctx))))))

(ert-deftest test-story-time-remark-after-a-calendar-date ()
  (let ((ctx (list :mode 'calendar
                   :calendar '(:months (("Thaw" . 30) ("Sow" . 30)) :era "AE" :day-one 0))))
    (should (equal (org-scribe-story-time-key "5 Thaw 312" ctx)
                   (org-scribe-story-time-key "5 Thaw 312, the feast" ctx)))))

(ert-deftest test-story-time-problem-names-a-calendar-that-cannot-place-dates ()
  (let ((months '(("Thaw" . 30) ("Sow" . 30))))
    ;; no usable Calendar at all (heading missing or table empty)
    (should (eq 'calendar-incomplete
                (org-scribe-story-time-problem "5 Thaw 312" (list :mode 'calendar))))
    ;; months but a DAY-ONE that did not parse
    (should (eq 'calendar-incomplete
                (org-scribe-story-time-problem
                 "5 Thaw 312" (list :mode 'calendar
                                    :calendar (list :months months :era "AE" :day-one nil)))))
    ;; a complete calendar: a real typo stays a typo
    (should (eq 'unreadable
                (org-scribe-story-time-problem
                 "5 Thwa 312" (list :mode 'calendar
                                    :calendar (list :months months :era "AE" :day-one 0)))))
    ;; text that is not shaped like a calendar date is still just unreadable
    (should (eq 'unreadable
                (org-scribe-story-time-problem "por la tarde" (list :mode 'calendar))))
    ;; relative notation keeps working in calendar mode
    (should-not (org-scribe-story-time-problem "Day 3" (list :mode 'calendar)))))

(ert-deftest test-story-time-timestamp-must-be-the-whole-value ()
  (let ((ctx (list :mode 'dates :day-one (calendar-absolute-from-gregorian '(3 14 2026)))))
    (should (org-scribe-story-time-key "<2026-03-10 Tue 09:00>" ctx))
    (should (org-scribe-story-time-key "  2026-03-10 " ctx))
    ;; a stamp inside prose, and a range, are not times to sort on
    (should-not (org-scribe-story-time-key "Recuerda <2026-03-10> de niña" ctx))
    (should-not (org-scribe-story-time-key "<2026-03-14>--<2026-03-20>" ctx))
    (should (eq 'unreadable (org-scribe-story-time-problem "<2026-03-14>--<2026-03-20>" ctx)))
    (should (eq 'unreadable (org-scribe-story-time-problem "Recuerda <2026-03-10>" (list :mode 'dates))))))

(ert-deftest test-story-time-refuses-days-the-month-does-not-have ()
  (let ((ctx (list :mode 'dates :day-one (calendar-absolute-from-gregorian '(3 14 2026)))))
    (should-not (org-scribe-story-time-key "<2026-02-31>" ctx))
    (should-not (org-scribe-story-time-key "<2026-02-29>" ctx))
    (should (org-scribe-story-time-key "<2028-02-29>" ctx))
    (should (equal 739689 (org-scribe-story-time--parse-date "2026-03-14")))
    (should-not (org-scribe-story-time--parse-date "2026-02-31"))
    (should-not (org-scribe-story-time--parse-date "2026-03-14xyz"))))

(provide 'test-story-time)
;;; test-story-time.el ends here
