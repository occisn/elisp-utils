# elisp-utils

Personal utilities for Emacs Lisp.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

Many functions are proposed with a 'traditional' implementation, not relying on `cl-xxx` functions.

A test suite is proposed through file `z_tests.el` (instructions inside).

File **dates-and-times.el**  
   - function `my/lisp-timestamp-to-YYYY-MM-DD`  
   - function `my/YYYY-MM-DD-to-lisp-timestamp`  
   - function `my/today-YYYY-MM-DD`  
   - function `my/today-YYYY`  
   - function `my/today-MM`  
   - function `my/today-DD`  
   - function `my/day-in-week-in-French`  
   - function `my/date-to-day-in-week-in-French`  
   - function `my/today-day-in-week-in-French`  
   - function `my/day-number-in-French`  
   - function `my/month-in-French`  
   - function `my/english-month-to-number`  
   - function `my/today-in-French`
   
File **integers.el**  
   - function `my/number-to-string-with-comma-as-thousand-separator`  
   - function `my/add-number-grouping`  
   - function `my/isqrt--traditional`  
   - function `my/gcd--traditional`  
   - function `my/lcm--traditional`  

File **integers-digits.el**  
   - function `my/reverse-number`  

File **integers-french.el**  
   - function `my/en-toutes-lettres`

File **integers-primes.el**  
   - functions `my/primep` and `my/primep-_traditional`  
   - function `my/largest-prime-factor--traditional`  
   - functions `my/eratosthenes-sieve` and `my/eratosthenes-sieve--traditional`  
   - functions `my/next-prime` and `my/next-prime--traditional`  
   - functions `my/nth-prime` and `my/nth-prime--traditional`

File **files.el**  
   - function `my/insert-directories-in-file-list`  
   - function `my/get-file-last-modification-date`  
   - function `my/file-size-Mo`  
   - function `my/nb-of-elements-in-directory`  
   - function `my/size-of-folder-in-Mo`  
   - function `my/list-of-directories-and-subdirectories-from`
   
File **macros.el**  
   - macro `aprogn`  
   - macro `amapcar`  
   - macro `let+`  
   - macro `awhen`  
   - macro `aif`
   
File **strings.el**  
   - function `my/string-remove-surrounding-quotes`  
   - function `my/string-suffix-p`  
   - function `my/split-string-at-first-delimiter`
   
File **trampoline.el**  
   - function `my/trampoline` with an example
   
Any comment? Open an [issue](https://github.com/occisn/elisp-utils/issues), or start a discussion [here](https://github.com/occisn/elisp-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

(end of README)
