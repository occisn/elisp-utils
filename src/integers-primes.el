;;; -*- lexical-binding: t; -*-

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

;; end
