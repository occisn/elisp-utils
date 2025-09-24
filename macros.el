;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))

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


;;; === end
