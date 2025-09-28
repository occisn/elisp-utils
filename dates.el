;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))


(defun my/lisp-timestamp-to-YYYY-MM-DD (date1)
  "Convert lisp timestamp DATE1 to YYYY-MM-DD format.
(v1, available in occisn/elisp-utils GitHub repository)"
  (format-time-string "%Y-%m-%d" date1))

(ert-deftest init-test-lisp-timestamp-to-YYYY-MM-DD ()
  :tags '(elisp-utils)
  (should (string= "1970-01-01" (my/lisp-timestamp-to-YYYY-MM-DD 0)))
  (should (string= "1970-01-31" (my/lisp-timestamp-to-YYYY-MM-DD (* 3600 24 30)))))

(defun my/YYYY-MM-DD-to-lisp-timestamp (str1)
  "2023-09-04 --> time
(v1, available in occisn/elisp-utils GitHub repository)"
  (date-to-time (concat str1 " 00:00:00")))

(defun my/today-YYYY-MM-DD ()
  "--> 2023-08-16 equivalent for today
(v1, available in occisn/elisp-utils GitHub repository)"
  (format-time-string "%Y-%m-%d"))

(defun my/today-YYYY ()
  "--> 2023 or equivalent for today
(v1, available in occisn/elisp-utils GitHub repository)"
  (format-time-string "%Y"))

(defun my/today-MM ()
  "--> 01, 02, ... 12
(v1, available in occisn/elisp-utils GitHub repository)"
  (format-time-string "%m"))

(defun my/today-DD ()
  "--> 01, 02, ... 31
(v1, available in occisn/elisp-utils GitHub repository)"
  (format-time-string "%d"))

(defun my/day-in-week-in-French (day-in-week1)
  "1 --> lundi
(v1, available in occisn/elisp-utils GitHub repository)"
  (cond
   ((string= day-in-week1 "1") "lundi")
   ((string= day-in-week1 "2") "mardi")
   ((string= day-in-week1 "3") "mercredi")
   ((string= day-in-week1 "4") "jeudi")
   ((string= day-in-week1 "5") "vendredi")
   ((string= day-in-week1 "6") "samedi")
   ((string= day-in-week1 "7") "dimanche")
   (t (error "Day in week not recognized: %s" day-in-week1))))

(ert-deftest init-test-day-in-week-in-French ()
  :tags '(elisp-utils)
  (should (string= "lundi" (my/day-in-week-in-French "1")))
  (should (string= "dimanche" (my/day-in-week-in-French "7"))))

(defun my/date-to-day-in-week-in-French (date1)
  "2023-09-04 --> lundi"
  (my/day-in-week-in-French (format-time-string "%u" (my/YYYY-MM-DD-to-lisp-timestamp date1))))

(ert-deftest test-date-to-day-in-week-in-French ()
  :tags '(elisp-utils)
  (should (string= "lundi" (my/date-to-day-in-week-in-French "2023-09-04")))
  (should (string= "jeudi" (my/date-to-day-in-week-in-French "2022-11-10"))))

(defun my/today-day-in-week-in-French ()
  "lundi, mardi... today
(v1, available in occisn/elisp-utils GitHub repository)"
  (my/day-in-week-in-French (format-time-string "%u")))

(defun my/day-number-in-French (day1)
  "01 --> 1er, 02 --> 2, ..., 31 --> 31
(v1, available in occisn/elisp-utils GitHub repository)"
  (cond
   ((string= day1 "01") "1er")
   ((string= day1 "02") "2")
   ((string= day1 "03") "3")
   ((string= day1 "04") "4")
   ((string= day1 "05") "5")
   ((string= day1 "06") "6")
   ((string= day1 "07") "7")
   ((string= day1 "08") "8")
   ((string= day1 "09") "9")
   (t day1)))

(ert-deftest test-day-number-in-French ()
  :tags '(elisp-utils)
  (should (string= "1er" (my/day-number-in-French "01")))
  (should (string= "9" (my/day-number-in-French "09")))
  (should (string= "10" (my/day-number-in-French "10")))
  (should (string= "31" (my/day-number-in-French "31"))))

(defun my/month-in-French (month1)
  "01 --> janvier, ..., 12 --> décembre
(v1, available in occisn/elisp-utils GitHub repository)"
  (cond
   ((string= month1 "01") "janvier")
   ((string= month1 "02") "février")
   ((string= month1 "03") "mars")
   ((string= month1 "04") "avril")
   ((string= month1 "05") "mai")
   ((string= month1 "06") "juin")
   ((string= month1 "07") "juillet")
   ((string= month1 "08") "août")
   ((string= month1 "09") "septembre")
   ((string= month1 "10") "octobre")
   ((string= month1 "11") "novembre")
   ((string= month1 "12") "décembre")
   (t (error "Month not recognized: %s" month1))))

(ert-deftest test-month-in-French ()
  :tags '(elisp-utils)
  (should (string= "janvier" (my/month-in-French "01")))
  (should (string= "décembre" (my/month-in-French "12"))))

(defun my/english-month-to-number (month)
  "Jan --> 1, Dec --> 12
(v1, available in occisn/elisp-utils GitHub repository)"
  (cond ((equal "Jan" month) 1)
        ((equal "Feb" month) 2)
        ((equal "Mar" month) 3)
        ((equal "Apr" month) 4)
        ((equal "May" month) 5)
        ((equal "Jun" month) 6)
        ((equal "Jul" month) 7)
        ((equal "Aug" month) 8)
        ((equal "Sep" month) 9)
        ((equal "Oct" month) 10)
        ((equal "Nov" month) 11)
        ((equal "Dec" month) 12)
        (t (error "Month not recognized: %s" month))))

(ert-deftest test-english-month-to-number ()
  :tags '(elisp-utils)
  (should (= 1 (my/english-month-to-number "Jan")))
  (should (= 12 (my/english-month-to-number "Dec"))))

(defun my/today-in-French ()
  "'25 août 2023' or similar
(v1, available in occisn/elisp-utils GitHub repository)"
  (concat (my/day-number-in-French (my/today-DD))
          " "
          (my/month-in-French (my/today-MM))
          " "
          (my/today-YYYY)))

;;; end
