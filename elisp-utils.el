;;; -*- lexical-binding: t; -*-

;;; === to test:
;;;    C-c C-k
;;;    M-: (ert '(tag elisp-utils))

;;; all functions in the same file to facilitate tests

;;; ===
;;; =============
;;; === DATES ===
;;; =============

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
  "lundi, mardi... dimanche
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

(cl-defun my/today-in-French (&optional with-day-in-week-p)
   "Return '25 août 2023' or similar.
If WITH-DAY-IN-WEEK-P, return 'mardi 25 août 2023' or similar.
(v2, available in occisn/elisp-utils GitHub repository)"
   (let* ((today-DD (format-time-string "%d"))   ; 01, 02
          (day-number-in-French
           (cond
            ((string= today-DD  "01") "1er")
            ((string= today-DD  "02") "2")
            ((string= today-DD  "03") "3")
            ((string= today-DD  "04") "4")
            ((string= today-DD  "05") "5")
            ((string= today-DD  "06") "6")
            ((string= today-DD  "07") "7")
            ((string= today-DD  "08") "8")
            ((string= today-DD  "09") "9")
            (t today-DD)))
          (today-MM (format-time-string "%m"))   ; 01, 02
          (month-in-French
           (cond
            ((string= today-MM  "01") "janvier")
            ((string= today-MM  "02") "février")
            ((string= today-MM  "03") "mars")
            ((string= today-MM  "04") "avril")
            ((string= today-MM  "05") "mai")
            ((string= today-MM  "06") "juin")
            ((string= today-MM  "07") "juillet")
            ((string= today-MM  "08") "août")
            ((string= today-MM  "09") "septembre")
            ((string= today-MM  "10") "octobre")
            ((string= today-MM  "11") "novembre")
            ((string= today-MM  "12") "décembre")
            (t (error "Month not recognized: %s" today-MM)))
           )
          (today-YYYY (format-time-string "%Y")) ; 2023
          (today-in-French
           (concat day-number-in-French
                   " "
                   month-in-French
                   " "
                   today-YYYY))) ; end of let*
     
     (if with-day-in-week-p
         (let* ((day-in-week (format-time-string "%u")) ; 1, 2... 7
                (day-in-week-in-French
                 (cond
                  ((string= day-in-week "1") "lundi")
                  ((string= day-in-week "2") "mardi")
                  ((string= day-in-week "3") "mercredi")
                  ((string= day-in-week "4") "jeudi")
                  ((string= day-in-week "5") "vendredi")
                  ((string= day-in-week "6") "samedi")
                  ((string= day-in-week "7") "dimanche")
                  (t (error "Day in week not recognized: %s" day-in-week)))))
           (concat day-in-week-in-French " " today-in-French))
       today-in-French)))

;; v1:
;;
;; (defun my/today-in-French ()
;;   "'25 août 2023' or similar
;; (v1, available in occisn/elisp-utils GitHub repository)"
;;   (concat (my/day-number-in-French (my/today-DD))
;;           " "
;;           (my/month-in-French (my/today-MM))
;;           " "
;;           (my/today-YYYY)))

;;; ===
;;; =============
;;; === FILES ===
;;; =============

(defun my/insert-directories-in-file-list (files)
  "Take a list of files, and return the same list with directories intertwined.

For instance :
d1/a.org d1/b.org d2/c.org d3/d.org
-->
d1/ d1/a.org d1/b.org d2/ d2/c.org d3/ d3/d.org
(v1)"
  (let ((current-dir "")
	(files-intertwined-with-directories nil))
    (cl-loop for filename in files
	     for dir1 = (file-name-directory filename)
	     do (progn
		  (when (not (string= current-dir dir1))
		    (push dir1 files-intertwined-with-directories)
		    (setq current-dir dir1))
		  (push filename files-intertwined-with-directories)))
    (reverse files-intertwined-with-directories)))

(ert-deftest test-insert-directories-in-file-list ()
  :tags '(elisp-utils)
  (should (equal
	   '("d1/" "d1/a.org" "d1/b.org" "d2/" "d2/c.org" "d3/" "d3/d.org")
	   (my/insert-directories-in-file-list '("d1/a.org" "d1/b.org" "d2/c.org" "d3/d.org")))))

(defun my/get-file-last-modification-date (file-full-name)
   "Return the date of last modification (as Lisp timestamp) of FILE-FULL-NAME file.
(v1, available in occisn/elisp-utils GitHub repository)"
   (nth 5 (file-attributes file-full-name)))

(defun my/file-size-Mo (filename)
  "Return file size of FILENAME in Mo.
(v1, available in occisn/elisp-utils GitHub repository)"
  (round
   (/
    (file-attribute-size
     (file-attributes filename))
    1000000)))

(defun my/nb-of-elements-in-directory (folder)
   "Return number of elements in FOLDER, including sub-folders (no recursive investigation of subdirectories).
(v1, available in occisn/elisp-utils GitHub repository)"
   (- (length (directory-files folder)) 2))

(defun my/size-of-folder-in-Mo (folder)
   "Return the size of FOLDER.
Requires PowerShell on Windows.
May return 0 in case of problem encoutered by PowerShell.
(v1, available in occisn/elisp-utils GitHub repository)"
   (let* ((cmd1 (format "(Get-ChildItem '%s' -Recurse | Measure-Object -Property Length -Sum -ErrorAction Stop).Sum" folder))
	  (cmd2 (format "powershell.exe -Command \"%s\"" cmd1))
	  (res (shell-command-to-string cmd2)))
     (/ (string-to-number (string-trim res)) 1000000)))

(defun my/list-of-directories-and-subdirectories-from (root &optional sorted-p)
   "Return the list of directories and subdirectories under ROOT (not included).
If SORTED-P is true, the list is alphabetically sorted.
Requires 'f' package.
(v1, available in occisn/elisp-utils GitHub repository)"
   (let* ((list1 nil))
     (f-directories root (lambda (folder) (push folder list1)) t)
     (if sorted-p
         (sort list1 #'string<)
       list1)))

;;; ===
;;; ==============
;;; === MACROS ===
;;; ==============

(defmacro aprogn (&rest body)
  "Anaphoric progn.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(let*
       ,@(cl-loop for remaining-clauses on body
		  until (<= (length remaining-clauses) 1)
		  collect `(it ,(car remaining-clauses)) into bindings
		  finally (return (list bindings (car remaining-clauses))))))

(ert-deftest test-aprogn ()
  :tags '(elisp-utils)
  (should (equal
	   '(let* ((it (+ 1 1)) (it (* it 3))) (+ it 4))
	   (macroexpand-all '(aprogn (+ 1 1) (* it 3) (+ it 4)))))
  (should (null (aprogn)))
  (should (= 4 (aprogn (+ 2 2))))
  (should (= 10 (aprogn
		 (+ 1 1)
		 (* it 3)
		 (+ it 4)))))

(defmacro amapcar (form list)
  "Anaphoric mapcar.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(mapcar (lambda (it) ,form) ,list))

(ert-deftest test-amapcar ()
  :tags '(elisp-utils)
  (should (equal '(4 6) (amapcar (* 2 it) '(2 3)))))

(defmacro let+ (bindings-list &rest body)
  "Let+ macro.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((first-binding (car bindings-list)))
    (cond ((null bindings-list)
           `(progn ,@body))
          ((eq :instruction (car first-binding))
           `(progn ,@(cdr first-binding) (let+ ,(cdr bindings-list) ,@body)))
          ((eq :labels (car first-binding))
           `(cl-labels (,(cdr first-binding)) (let+ ,(cdr bindings-list) ,@body)))
          ((consp (car first-binding))
           `(cl-multiple-value-bind ,(car first-binding) ,@(cdr first-binding) (let+ ,(cdr bindings-list) ,@body)))
          (t `(let (,first-binding) (let+ ,(cdr bindings-list) ,@body))))))
;; See metabang-bind https://common-lisp.net/project/metabang-bind/user-guide.html

(ert-deftest test-let+ ()
  :tags '(elisp-utils)
  (should (equal '(3 4 5 6 7) (let+ ((a 3)
                                     (:instruction (+ 4 5) (unless (>= a 2) (error "abc")))
                                     (b 4)
                                     (:labels add-b (x) "blabla" (+ x b))
                                     ((c d) (list 5 6))
                                     (e (add-b a)))
                                    (list a b c d e))) ))
(defmacro awhen (test &rest body)
  "Anaphoric when.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(let ((it ,test))
     (when it ,@body)))

(ert-deftest test-awhen ()
  :tags '(elisp-utils)
  (should (= 7 (awhen (* 2 2) (+ 3 it))))
  (should (null (awhen (= 2 3) (+ 3 it)))))

(defmacro aif (test clause1 clause2)
  "Anaphoric if.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(let ((it ,test))
     (if it ,clause1 ,clause2)))

(ert-deftest test-aif ()
  :tags '(elisp-utils)
  (should (= 7 (aif (* 2 2) (+ 3 it) "no")))
  (should (string= "no" (aif (= 2 3) "yes" "no"))))


;;; ===
;;; ===============
;;; === NUMBERS ===
;;; ===============

;;; same purpose as the other one below, apparently
(defun my/number-to-string-with-comma-as-thousand-separator (num)
  "Return a string corresponding to number NUM formatted with thousand separators (commas).
For instance: 1234 --> '1,234'
(v1 as of 2025-09-07, available in occisn/elisp-utils GitHub repository)"
  (let ((str (number-to-string num)))
    (while (string-match "\\(.*[0-9]\\)\\([0-9]\\{3\\}\\)" str)
      (setq str (replace-match "\\1,\\2" nil nil str)))
    str))

(ert-deftest test-number-to-string-with-comma-as-thousand-separator ()
  :tags '(elisp-utils)
  (should (string= "123,456" (my/number-to-string-with-comma-as-thousand-separator 123456))))

;;; same purpose as the one above, apparently
(defun my/add-number-grouping (number &optional separator)
  "Return a string corresponding to NUMBER, which each 3-digit group separated by SEPARATOR, by default a comma.

For instance: 123456 as a number--> 123,456 as a string
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((num (number-to-string number))
	(op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat 
		 (match-string 1 num) op
		 (match-string 2 num))))
    num))

(ert-deftest test-number-grouping ()
  :tags '(elisp-utils)
  (should (string= "1,234,567" (my/add-number-grouping 1234567))))

(defun my/reverse-number (n &optional acc0)
  "Reverse the N, which is supposed to be an integer >= 0.
For instance: 123 --> 321.
ACC0 is an accumulator used during recursion.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((acc (or acc0 0)))
    (if (= n 0)
	acc
      (let ((f (floor n 10))
	    (r (mod n 10)))
	(my/reverse-number f (+ (* 10 acc) r))))))

(ert-deftest test-reverse-number ()
  :tags '(elisp-utils)
  (should (= 0 (my/reverse-number 0)))
  (should (= 1 (my/reverse-number 1)))
  (should (= 321 (my/reverse-number 123))))

(defun my/isqrt--traditional (n)
  "Return the integer square root of N (largest integer <= sqrt(N)).
N is supposed to be >= 0.
This code is written in traditional Emacs Lisp, without cl-lib (where the equivalent exists: cl-isqrt).
(v1, available in occisn/elisp-utils GitHub repository)"
  (cond
   ((= n 0) 0)
   ((< n 4) 1)
   (t
    ;; Newton's method
    (let* ((x n)
           (y (/ (+ x (/ n x)) 2)))
      (while (< y x)
        (setq x y)
        (setq y (/ (+ x (/ n x)) 2)))
      x))))

(ert-deftest test-isqrt--traditional ()
  :tags '(elisp-utils)
  (should (= 0 (my/isqrt--traditional 0)))
  (should (= 1 (my/isqrt--traditional 1)))
  (should (= 1 (my/isqrt--traditional 2)))
  (should (= 3 (my/isqrt--traditional 10)))
  (should (= 4 (my/isqrt--traditional 16)))
  (should (= 5 (my/isqrt--traditional 27)))
  (should (= 9 (my/isqrt--traditional 99)))
  (should (= 10 (my/isqrt--traditional 100)))
  (should (= 11111 (my/isqrt--traditional 123456789))))

(defun my/primep (n)
  "Return t if and only if N is prime. N is supposed to be an integer >= 1.
(v1, available in occisn/elisp-utils GitHub repository)"
  (cond ((= 1 n) nil)
	((member n '(2 3 5 7)) t)
	((cl-evenp n) nil)
	((zerop (mod n 3)) nil)
	(t (cl-loop for factor from 5 by 6
		    with root-n = (cl-isqrt n)
		    while (<= factor root-n)
		    never (or (zerop (mod n factor))
			      (zerop (mod n (+ factor 2))))))))
;; Inspired by https://github.com/tkych/cl-mod-prime

(ert-deftest test-primep ()
  :tags '(elisp-utils)
  (should (not (my/primep 1)))
  (should (my/primep 2))
  (should (my/primep 3))
  (should (not (my/primep 4))))

(defun my/primep--traditional (n)
  "Return t if and only if N is prime. N is supposed to be an integer >= 1.
This code is written in traditional Emacs Lisp, without cl-lib.
Requires my/isqrt--traditional.
(v1, available in occisn/elisp-utils GitHub repository)"
  
  (cond ((= 1 n) nil)
	((member n '(2 3 5 7)) t)
	((zerop (mod n 2)) nil)
	((zerop (mod n 3)) nil)
	(t (let ((factor 5)
                 (root-n (my/isqrt--traditional n))
                 (result t))
             (while (<= factor root-n)
               (when (or (zerop (mod n factor))
			 (zerop (mod n (+ factor 2))))
                 (setq result nil))
               (setq factor (+ factor 6))) ; end of while
             result))))
;; Inspired by https://github.com/tkych/cl-mod-prime.

(ert-deftest test-primep--traditional ()
  :tags '(elisp-utils)
  (should (not (my/primep--traditional 1)))
  (should (my/primep--traditional 2))
  (should (my/primep--traditional 3))
  (should (not (my/primep--traditional 4))))

(defun my/gcd--traditional (a b)
  "Return gcd of A and B.
Traditional equivalent of cl-gcd.
(v1, available in occisn/elisp-utils GitHub repository)"
  (if (zerop b)
      (abs a)
    (my/gcd--traditional b (mod a b))))

(ert-deftest test-gcd--traditional ()
  :tags '(elisp-utils)
  (should (= 3 (my/gcd--traditional 6 15))))

(defun my/lcm--traditional (a b)
  "Return lcm of A and B.
Traditional equivalent of cl-lcm.
Requires my/gcd--traditional.
(v1, available in occisn/elisp-utils GitHub repository)"
  (/ (abs (* a b)) (my/gcd--traditional a b)))

(ert-deftest test-lcm--traditional ()
  :tags '(elisp-utils)
  (should (= 12 (my/lcm--traditional 3 4))))

(defun my/largest-prime-factor--traditional (n)
  "Return the largest prime factor of N. N is supposed to be an integer > 1.
Requires my/isqrt--traditional
(v2, available in occisn/elisp-utils GitHub repository)"
  (let ((largest 0))
    ;; Remove factors of 2
    (while (= 0 (mod n 2))
         (setf largest 2)
         (setf n (/ n 2)))

    ;; Remove factors of 3
    (while (= 0 (mod n 3))
         (setf largest 3)
         (setf n (/ n 3)))

    ;; Test divisors of the form 6k-1 and 6k+1
    (let ((i 5)
          (isqrt-n (my/isqrt--traditional n)))
      (while (<= i isqrt-n)
           (cond
             ((= 0 (mod n i))
              (setf largest i)
              (setf n (/ n i)))
             ((= 0 (mod n (+ i 2)))
              (setf largest (+ i 2))
              (setf n (/ n (+ i 2))))
             (t
              (setf i (+ i 6))))))

    ;; If n is still > 1, it is prime
    (if (> n 1)
        (setf largest n))

    largest))

(ert-deftest test-largest-prime-factor--traditional ()
  :tags '(elisp-utils)
  (should (= 2 (my/largest-prime-factor--traditional 2)))
  (should (= 17 (my/largest-prime-factor--traditional 76576500)))
  (should (= 29 (my/largest-prime-factor--traditional 13195))))

(defun my/eratosthenes-sieve (lim)
  "Return a boolean vector representing the result of Eratosthenes sieve on |[ 0 ; LIM |[.
In this vector, t = prime ; nil = non prime.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((bv (make-bool-vector lim t)))
    (aset bv 0 nil)    ; 0 is not prime
    (aset bv 1 nil)    ; 1 is not prime
    ;; 2 is prime, so change nothing in the vector
    ;; 4 and subsequent even numbers are not prime:
    (cl-loop for i from 4 below lim by 2
	     do (aset bv i nil))
    ;; Sieve:
    (cl-loop for i from 3
	     while (<= (* i i) lim)
	     when (aref bv i)
	     do (cl-loop for j from (* i i) below lim by (* 2 i)
			 do (aset bv j nil)))
    bv))
;; Inspired by https://fr.wikipedia.org/wiki/Crible_d%27%C3%89ratosth%C3%A8ne

(ert-deftest test-eratosthenes-sieve ()
  :tags '(elisp-utils)
  (should (= 76127
             (let* ((lim 1000)
	            (bv (my/eratosthenes-sieve lim)))
               (cl-loop for i from 0 below lim
	                when (aref bv i)
	                sum i)))) ; sum of primes below 1000 = 76127
  )
(defun my/eratosthenes-sieve--traditional (lim)
  "Return a boolean vector representing the result of Eratosthenes sieve on |[ 0 ; LIM |[.
In this vector, t = prime ; nil = non prime.
Written in traditional Emacs Lisp, without cl-lib.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((bv (make-bool-vector lim t)))
    (aset bv 0 nil)    ; 0 is not prime
    (aset bv 1 nil)    ; 1 is not prime
    ;; 2 is prime, so change nothing in the vector
    ;; 4 and subsequent even numbers are not prime:
    (let ((i 4))
      (while (< i lim)
        (aset bv i nil)
        (setq i (+ i 2))))
    ;; Sieve:
    (let ((i 3))
      (while (<= (* i i) lim)
        (when (aref bv i)
          (let ((j (* i i)))
            (while (< j lim)
              (aset bv j nil)
              (setq j (+ j (* 2 i))))))
        (setq i (1+ i)))
      bv)))
;; Inspired by https://fr.wikipedia.org/wiki/Crible_d%27%C3%89ratosth%C3%A8ne

(ert-deftest test-eratosthenes-sieve--traditional ()
  :tags '(elisp-utils)
  (should (= 76127
             (let* ((lim 1000)
	            (bv (my/eratosthenes-sieve--traditional lim)))
               (cl-loop for i from 0 below lim
	                when (aref bv i)
	                sum i)))) ; sum of primes below 1000 = 76127
  )

;;; ===
;;; ======================
;;; === NUMBERS-FRENCH ===
;;; ======================

;; References:
;; -----------
;; https://www.dictionnaire-academie.fr/article/QDL057
;; https://www.academie-francaise.fr/questions-de-langue#58_strong-em-nombres-criture-lecture-accord-em-strong
;; https://www.agathe-redactrice.net/orthographe-simple/nombres-en-lettres/
;; tests below are all verified with https://leconjugueur.lefigaro.fr/frnombre.php

(defun my/en-toutes-lettres (n)
  "This function converts a number N into its written equivalent in words in French, according to the rules prior to 1990 reform.
For instance : 101 --> 'cent un'.
N shall be <= 999 999 999 999
(v1 as of 2017-02-16, available in occisn/elisp-utils GitHub repository)"

  (let ((unites ["zero" "un" "deux" "trois" "quatre" "cinq" "six" "sept" "huit" "neuf" "dix" "onze" "douze" "treize" "quatorze" "quinze" "seize" "dix-sept" "dix-huit" "dix-neuf"])

        (dizaines ["void" "dix" "vingt" "trente" "quarante" "cinquante" "soixante" "soixante-dix" "quatre-vingt" "quatre-vingt-dix"]))

    (letrec ((sub
              (lambda (n &optional a-la-fin)
	        
	        (cond
	         
	         ((>= n 1000000000)     ; un milliard
	          (let* ((mmm (floor (/ n 1000000000)))
		         (mmm-en-toutes-lettres (funcall sub mmm t))
		         ;; t ci-dessus car milliards est considéré comme un nom
		         ;; donc mmm est "à la fin"
		         (reste (mod n 1000000000))
		         (reste-en-toutes-lettres (funcall sub reste a-la-fin)))
		    (concat mmm-en-toutes-lettres
			    " milliard"
			    (when (> mmm 1) "s")
			    (when (>= reste 1) " ")
			    (when (>= reste 1) reste-en-toutes-lettres))))
	         
	         ((>= n 1000000)        ; un million
	          (let* ((mm (floor (/ n 1000000)))
		         (mm-en-toutes-lettres (funcall sub mm t))
		         ;; t ci-dessus car millions est considéré comme un nom
		         ;; donc mm est "à la fin"
		         (reste (mod n 1000000))
		         (reste-en-lettres (funcall sub reste a-la-fin)))
		    (concat mm-en-toutes-lettres
			    " million"
			    (when (> mm 1) "s")
			    (when (>= reste 1) " ")
			    (when (>= reste 1) reste-en-lettres))))
	         
	         ((>= n 1000)           ; 
	          (let* ((m (floor (/ n 1000)))
		         (m-en-lettres (funcall sub m nil))
		         (reste (mod n 1000))
		         (reste-en-lettres (funcall sub reste a-la-fin)))
		    (concat (when (>= m 2) m-en-lettres)
			    (when (>= m 2) " ")
			    "mille"
			    (when (>= reste 1) " ")
			    (when (>= reste 1) reste-en-lettres))))	      
	         
	         ((<= n 19) (aref unites n))
	         ((<= n 69) (let* ((d (floor (/ n 10)))
			           (d-en-lettres (aref dizaines d))
			           (u (mod n 10)))
			      (cond
			       ((= u 0) d-en-lettres)
			       ((= u 1) (concat d-en-lettres " et un"))
			       (t (concat d-en-lettres "-" (aref unites u))))))
	         ((= n 70) "soixante-dix")
	         ((= n 71) "soixante et onze")
	         ((<= n 79) (concat "soixante-" (aref unites (- n 60))))
	         ((= n 80) (if a-la-fin "quatre-vingts" "quatre-vingt"))
	         ((<= n 99) (concat "quatre-vingt-" (aref unites (- n 80))))
	         ((= n 100) "cent")
	         ((<= n 999) (let* ((c (floor (/ n 100)))
				    (c-en-lettres (funcall sub c nil))
				    (du (mod n 100))
				    (du-en-lettres (funcall sub du a-la-fin)))
			       (concat (when (<= n 199) "cent")
				       (when (> n 199) c-en-lettres)
				       (when (> n 199) " cent")
				       (when (and a-la-fin (= du 0)) "s")
				       (when (> du 0) " ")
				       (when (> du 0) du-en-lettres)))))))) ; end of letrec definitions

      (funcall sub n t))))

(ert-deftest test-en-toutes-lettres ()
  :tags '(elisp-utils)
  (should (string= (my/en-toutes-lettres 3) "trois"))
  (should (string= (my/en-toutes-lettres 20) "vingt"))
  (should (string= (my/en-toutes-lettres 21) "vingt et un"))
  (should (string= (my/en-toutes-lettres 31) "trente et un"))
  (should (string= (my/en-toutes-lettres 46) "quarante-six"))
  (should (string= (my/en-toutes-lettres 72) "soixante-douze"))
  (should (string= (my/en-toutes-lettres 79) "soixante-dix-neuf"))
  (should (string= (my/en-toutes-lettres 80) "quatre-vingts"))
  (should (string= (my/en-toutes-lettres 81) "quatre-vingt-un"))
  (should (string= (my/en-toutes-lettres 89) "quatre-vingt-neuf"))
  (should (string= (my/en-toutes-lettres 90) "quatre-vingt-dix"))
  (should (string= (my/en-toutes-lettres 91) "quatre-vingt-onze"))
  (should (string= (my/en-toutes-lettres 99) "quatre-vingt-dix-neuf"))
  (should (string= (my/en-toutes-lettres 100) "cent"))
  (should (string= (my/en-toutes-lettres 153) "cent cinquante-trois"))
  (should (string= (my/en-toutes-lettres 180) "cent quatre-vingts"))
  (should (string= (my/en-toutes-lettres 200) "deux cents"))
  (should (string= (my/en-toutes-lettres 299) "deux cent quatre-vingt-dix-neuf"))
  (should (string= (my/en-toutes-lettres 300) "trois cents"))
  (should (string= (my/en-toutes-lettres 326) "trois cent vingt-six"))
  (should (string= (my/en-toutes-lettres 623) "six cent vingt-trois"))
  (should (string= (my/en-toutes-lettres 651) "six cent cinquante et un"))
  (should (string= (my/en-toutes-lettres 821) "huit cent vingt et un"))
  (should (string= (my/en-toutes-lettres 999) "neuf cent quatre-vingt-dix-neuf"))
  (should (string= (my/en-toutes-lettres 1000) "mille"))
  (should (string= (my/en-toutes-lettres 1001) "mille un"))
  (should (string= (my/en-toutes-lettres 1100) "mille cent"))
  (should (string= (my/en-toutes-lettres 1999) "mille neuf cent quatre-vingt-dix-neuf"))
  (should (string= (my/en-toutes-lettres 2000) "deux mille"))
  (should (string= (my/en-toutes-lettres 2001) "deux mille un"))
  (should (string= (my/en-toutes-lettres 9555) "neuf mille cinq cent cinquante-cinq"))
  (should (string= (my/en-toutes-lettres 10000) "dix mille"))
  (should (string= (my/en-toutes-lettres 10032) "dix mille trente-deux"))
  (should (string= (my/en-toutes-lettres 10200) "dix mille deux cents"))
  (should (string= (my/en-toutes-lettres 80000) "quatre-vingt mille"))
  (should (string= (my/en-toutes-lettres 100000) "cent mille"))
  (should (string= (my/en-toutes-lettres 500000) "cinq cent mille"))
  (should (string= (my/en-toutes-lettres 180000) "cent quatre-vingt mille"))
  (should (string= (my/en-toutes-lettres 532000) "cinq cent trente-deux mille"))
  (should (string= (my/en-toutes-lettres 999999) "neuf cent quatre-vingt-dix-neuf mille neuf cent quatre-vingt-dix-neuf"))
  (should (string= (my/en-toutes-lettres 1000000) "un million"))
  (should (string= (my/en-toutes-lettres 1000022) "un million vingt-deux"))
  (should (string= (my/en-toutes-lettres 1000100) "un million cent"))
  (should (string= (my/en-toutes-lettres 100000000) "cent millions"))
  (should (string= (my/en-toutes-lettres 200000200) "deux cents millions deux cents"))
  (should (string= (my/en-toutes-lettres 999999999) "neuf cent quatre-vingt-dix-neuf millions neuf cent quatre-vingt-dix-neuf mille neuf cent quatre-vingt-dix-neuf"))
  (should (string= (my/en-toutes-lettres 1000000000) "un milliard"))
  (should (string= (my/en-toutes-lettres 1001001100) "un milliard un million mille cent"))
  (should (string= (my/en-toutes-lettres 2000000000) "deux milliards"))
  (should (string= (my/en-toutes-lettres 80080080080) "quatre-vingts milliards quatre-vingts millions quatre-vingt mille quatre-vingts"))
  (should (string= (my/en-toutes-lettres 82082082082) "quatre-vingt-deux milliards quatre-vingt-deux millions quatre-vingt-deux mille quatre-vingt-deux"))
  (should (string= (my/en-toutes-lettres 91091091091) "quatre-vingt-onze milliards quatre-vingt-onze millions quatre-vingt-onze mille quatre-vingt-onze"))
  (should (string= (my/en-toutes-lettres 100000100000) "cent milliards cent mille"))
  (should (string= (my/en-toutes-lettres 100100100100) "cent milliards cent millions cent mille cent"))
  (should (string= (my/en-toutes-lettres 100005300567) "cent milliards cinq millions trois cent mille cinq cent soixante-sept"))
  (should (string= (my/en-toutes-lettres 123456789123) "cent vingt-trois milliards quatre cent cinquante-six millions sept cent quatre-vingt-neuf mille cent vingt-trois"))
  (should (string= (my/en-toutes-lettres 200200200200) "deux cents milliards deux cents millions deux cent mille deux cents"))
  (should (string= (my/en-toutes-lettres 386428075105) "trois cent quatre-vingt-six milliards quatre cent vingt-huit millions soixante-quinze mille cent cinq"))
  (should (string= (my/en-toutes-lettres 999999999999) "neuf cent quatre-vingt-dix-neuf milliards neuf cent quatre-vingt-dix-neuf millions neuf cent quatre-vingt-dix-neuf mille neuf cent quatre-vingt-dix-neuf")))

;;; ===
;;; ===============
;;; === STRINGS ===
;;; ===============

(defun my/string-remove-surrounding-quotes (s)
  "Remove quotes at the beginning and at the end of a string.
(v1, available in occisn/elisp-utils GitHub repository)"
  (aprogn
   s
   (string-remove-prefix "\"" it)
   (string-remove-suffix "\"" it)))

(ert-deftest test-string-remove-surrounding-quotes ()
  :tags '(elisp-utils)
  (should (string= "abcdef" (my/string-remove-surrounding-quotes "\"abcdef\""))))

(defun my/string-suffix-p (suffix str &optional ignore-case)
  "Return tt if STR finished by SUFFIX.
Ignore case.
(v1, available in occisn/elisp-utils GitHub repository)
Source: https://stackoverflow.com/questions/22403751/check-if-a-string-ends-with-a-suffix-in-emacs-lisp" 
  (let ((begin2 (- (length str) (length suffix)))
        (end2 (length str)))
    (when (< begin2 0) (setq begin2 0))
    (eq t (compare-strings suffix nil nil
                           str begin2 end2
                           ignore-case))))

(ert-deftest test-string-suffix ()
  :tags '(elisp-utils)
  (should (my/string-suffix-p "def" "abcdef"))
  (should (my/string-suffix-p "DEF" "abcdef" t))
  (should (my/string-suffix-p "def" "abcDEF" t))
  (should (not (my/string-suffix-p "def" "abcdefg"))))

(defun my/split-string-at-first-delimiter (s)
  "Split string S at the first occurrence of either a space or a line return.
For instance: 'aa bb cc' --> ('aa' 'bb cc')
(v1, available in occisn/elisp-utils GitHub repository)"
  (if (string-match "\\( \\|\n\\)" s)
      (list (substring s 0 (match-beginning 0))
            (substring s (match-end 0)))
    (list s)))

(ert-deftest test-split-string-at-first-delimiter ()
  :tags '(elisp-utils)
  (should (equal '("aa" "bb cc") (my/split-string-at-first-delimiter "aa bb cc")))
  (should (equal '("aa" "bb cc") (my/split-string-at-first-delimiter "aa\nbb cc"))))

;;; ===
;;; ==================
;;; === TRAMPOLINE ===
;;; ==================

(defun my/trampoline (thunk)
  "Trampoline for infinite recursion, even if no tail-call optimization.

Usage:
(my/trampoline (fn args))
where FN is the recursive function, which returns :
(i) either the result of the recursion (typically when the termination condition is met)
(ii) either (lambda () (fn ...)) for next recursion step.

The argument THUNK is the initial (fn ...)" 
  (while (functionp thunk)
    (setq thunk (funcall thunk)))
  thunk)

;;; Example: calculation of pi by Leibniz formula
;;; ---------------------------------------------

;;; Version 1: tail call (not optimized in Emacs Lisp)

(cl-defun %sub-A (i acc)
  (if (= 0 i)
      (+ acc 1)
    (let* ((sign (if (= 0 (mod i 2)) 1 -1))
           (denominator (+ 1.0 (* 2.0 i)))
           (term (/ sign denominator)))
      (%sub-A (- i 1) (+ acc term)))))

(defun leibniz-A ()
  (* 4.0 (%sub-A 1000000 0.0)))

;; (leibniz-A)
;; leads to an error:
;;     Lisp nesting exceeds ‘max-lisp-eval-depth’

;;; Version 2, with trampoline

(defun %sub-B (i acc)
  (if (= 0 i)
      (+ acc 1)
    (let* ((sign (if (= 0 (mod i 2)) 1 -1))
           (denominator (+ 1.0 (* 2.0 i)))
           (term (/ sign denominator)))
      (lambda () (%sub-B (- i 1) (+ acc term))))))
;; lexical binding shall be activated

(defun leibniz-B ()
  (* 4.0 (my/trampoline (%sub-B 1000000 0.0))))

;; (leibniz-B)
;; --> 3.141593653588793 in several seconds

;;; Test with factorial
;;; -------------------

(cl-defun %fact (i &optional (acc 0))
  (if (= 0 i)
      acc
    (lambda () (%fact (- i 1) (* acc i)))))

(ert-deftest test-trampoline ()
  :tags '(elisp-utils)
  (should (= 720 (my/trampoline (%fact 6 1)))))


;;; ===
;;; === end ===
