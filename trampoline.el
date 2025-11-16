;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))

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

;;; end
