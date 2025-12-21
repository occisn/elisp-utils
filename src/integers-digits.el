;;; -*- lexical-binding: t; -*-

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

;; end
