;;; -*- lexical-binding: t; -*-

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

;; end
