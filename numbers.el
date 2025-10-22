;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))

;;; same purpose as the other one below, apparently
(defun my/number-to-string-with-comma-as-thousand-separator (num)
  "Return a string corresponding to number NUM formatted with thousand separators (commas).
For instance: 1234 --> '1,234'
v1 as of 2025-09-07"
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

;; usage in traditional Emacs Lisp without cl-lib
(when nil  
  (letrec (
           ;; 'reverse-number'
           ;; Reverse the N, which is supposed to be an integer >= 0.
           ;; For instance: 123 --> 321.
           ;; ACC0 is an accumulator used during recursion.
           ;; (v1, available in occisn/elisp-utils GitHub repository)
           (sub-reverse-number (lambda (n acc0)
                                 
                                 (let ((acc (or acc0 0)))
                                   (if (= n 0)
	                               acc
                                     (let ((f (floor n 10))
	                                   (r (mod n 10)))
	                               (funcall sub-reverse-number f (+ (* 10 acc) r)))))))
           (reverse-number (lambda (n)
                             (funcall sub-reverse-number n 0))))
    
    ;; do something
    ))

(defun my/isqrt (n)
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

(ert-deftest test-isqrt ()
  :tags '(elisp-utils)
  (should (= 0 (my/isqrt 0)))
  (should (= 1 (my/isqrt 1)))
  (should (= 1 (my/isqrt 2)))
  (should (= 3 (my/isqrt 10)))
  (should (= 4 (my/isqrt 16)))
  (should (= 5 (my/isqrt 27)))
  (should (= 9 (my/isqrt 99)))
  (should (= 10 (my/isqrt 100)))
  (should (= 11111 (my/isqrt 123456789))))

(defun my/primep (n)
  "Return t if and only if N is prime. N is supposed to be an integer >= 1.
Inspired by https://github.com/tkych/cl-mod-prime
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

(ert-deftest test-primep ()
  :tags '(elisp-utils)
  (should (not (my/primep 1)))
  (should (my/primep 2))
  (should (my/primep 3))
  (should (not (my/primep 4))))

(defun my/primep--traditional (n)
  "Return t if and only if N is prime. N is supposed to be an integer >= 1.
Inspired by https://github.com/tkych/cl-mod-prime.
This code is written in traditional Emacs Lisp, without cl-lib.
(v1, available in occisn/elisp-utils GitHub repository)"
  
  (let ((isqrt (lambda (n)
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
                     x))))))
    
    (cond ((= 1 n) nil)
	  ((member n '(2 3 5 7)) t)
	  ((zerop (mod n 2)) nil)
	  ((zerop (mod n 3)) nil)
	  (t (let ((factor 5)
                   (root-n (funcall isqrt n))
                   (result t))
               (while (<= factor root-n)
                 (when (or (zerop (mod n factor))
			   (zerop (mod n (+ factor 2))))
                   (setq result nil))
                 (setq factor (+ factor 6))) ; end of while
               result)))))

(ert-deftest test-primep--traditional ()
  :tags '(elisp-utils)
  (should (not (my/primep--traditional 1)))
  (should (my/primep--traditional 2))
  (should (my/primep--traditional 3))
  (should (not (my/primep--traditional 4))))

;;;; === end
