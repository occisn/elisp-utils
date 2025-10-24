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

(defun my/largest-prime-factor (n)
  "Return the largest prime factor of N. N is supposed to be an integer > 1.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((i 2))
    (while (> n 1)
      (if (= 0 (mod n i)) ; n multiple of i
	  (setq n (/ n i))
	(setq i (+ i 1))))
    i))
;; Inspired by https://stackoverflow.com/questions/23287/algorithm-to-find-largest-prime-factor-of-a-number
;; Could perhaps be improved by testing only odd divisors, divisors under 6n+-1 format, etc.

(ert-deftest test-largest-prime-factor ()
  :tags '(elisp-utils)
  (should (= 2 (my/largest-prime-factor 2)))
  (should (= 29 (my/largest-prime-factor 13195))))

(defun my/eratosthenes-sieve (lim)
  "Return a boolean vector representing the result of Eratosthenes sieve on |[ 0 ; LIM |[.
In this vector, t = prime ; nil = non prime.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((bv (make-bool-vector lim t)))
    (aset bv 0 nil) ; 0 is not prime
    (aset bv 1 nil) ; 1 is not prime
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
  

;;;; === end
  )
