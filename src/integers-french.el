;;; -*- lexical-binding: t; -*-

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

;; end
