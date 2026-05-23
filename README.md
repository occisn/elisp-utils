# elisp-utils

Personal utilities for Emacs Lisp.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

Many functions are proposed with a `--traditional` implementation, not relying on `cl-xxx` functions.

## Table of contents

- [Conventions](#conventions)
- [Loading](#loading)
- [Quick start](#quick-start)
- [Running the tests](#running-the-tests)
- [Function index](#function-index)

## Conventions

- **`my/` prefix** &mdash; personal namespace marker on every utility function. It is just a convention; feel free to rename or drop the prefix when copy-pasting into your own code. A few macros in `macros.el` (`aprogn`, `amapcar`, `awhen`, `aif`, `let+`) deliberately do *not* carry the prefix, because they read better unprefixed.
- **`--traditional` suffix** &mdash; an alternate implementation that avoids `cl-lib` and uses only core Emacs Lisp. Pick this variant if you want to keep your code `cl-lib`-free; pick the unsuffixed one for the more idiomatic / often shorter version.
- **`%` prefix** &mdash; internal helper, not meant to be called directly (currently only in `trampoline.el`, for the recursive sub-routines that feed `my/trampoline`).

## Loading

Load a single file:

```elisp
(load-file "src/integers.el")
```

Load everything at once:

```elisp
(dolist (f '("integers" "integers-primes" "integers-digits" "integers-french"
             "macros" "strings" "files" "dates-and-times" "trampoline"))
  (load-file (concat "src/" f ".el")))
```

## Quick start

Three lines, from cold Emacs to a working call:

```elisp
(load-file "src/integers.el")
(my/isqrt--traditional 99)      ; => 9
(my/gcd--traditional 6 15)      ; => 3
```

## Running the tests

Tests are co-located with the functions they cover, defined inline with `ert-deftest` and tagged `elisp-utils`. Run them from the shell:

```bash
# All elisp-utils tests across every file
emacs --batch \
  -l src/integers.el -l src/integers-primes.el -l src/integers-digits.el \
  -l src/integers-french.el -l src/macros.el -l src/strings.el \
  -l src/files.el -l src/dates-and-times.el -l src/trampoline.el \
  -f ert-run-tests-batch-and-exit

# A single test
emacs --batch -l src/integers.el \
  --eval "(ert-run-tests-batch-and-exit 'test-isqrt--traditional)"
```

(The legacy `src/z_tests.el` aggregator predates the file split and is no longer the canonical entry point.)

## Function index

`(test)` means an `ert-deftest` is provided in the same file. `(requires …)` lists in-repo dependencies a copy-paste reuser must also grab. No `show-xxx` companion functions exist in the repository.

File **dates-and-times.el**
   - function `my/lisp-timestamp-to-YYYY-MM-DD` `(test)`<br>
     `(my/lisp-timestamp-to-YYYY-MM-DD 0)` → `"1970-01-01"`
   - function `my/YYYY-MM-DD-to-lisp-timestamp`<br>
     `(my/YYYY-MM-DD-to-lisp-timestamp "2023-09-04")` → Lisp timestamp for that date at 00:00:00
   - function `my/today-YYYY-MM-DD`<br>
     `(my/today-YYYY-MM-DD)` → `"2026-05-23"` (today)
   - function `my/today-YYYY`<br>
     `(my/today-YYYY)` → `"2026"`
   - function `my/today-MM`<br>
     `(my/today-MM)` → `"05"`
   - function `my/today-DD`<br>
     `(my/today-DD)` → `"23"`
   - function `my/day-in-week-in-French` `(test)`<br>
     `(my/day-in-week-in-French "1")` → `"lundi"`
   - function `my/date-to-day-in-week-in-French` `(test)` (requires `my/YYYY-MM-DD-to-lisp-timestamp`, `my/day-in-week-in-French`)<br>
     `(my/date-to-day-in-week-in-French "2023-09-04")` → `"lundi"`
   - function `my/today-day-in-week-in-French` (requires `my/day-in-week-in-French`)<br>
     `(my/today-day-in-week-in-French)` → `"samedi"` (today)
   - function `my/day-number-in-French` `(test)`<br>
     `(my/day-number-in-French "01")` → `"1er"`
   - function `my/month-in-French` `(test)`<br>
     `(my/month-in-French "08")` → `"août"`
   - function `my/english-month-to-number` `(test)`<br>
     `(my/english-month-to-number "Jan")` → `1`
   - function `my/today-in-French` (uses `cl-defun`)<br>
     `(my/today-in-French t)` → `"mardi 25 août 2023"` or similar

File **integers.el**
   - function `my/number-to-string-with-comma-as-thousand-separator` `(test)`<br>
     `(my/number-to-string-with-comma-as-thousand-separator 123456)` → `"123,456"`
   - function `my/add-number-grouping` `(test)`<br>
     `(my/add-number-grouping 1234567 " ")` → `"1 234 567"`
   - function `my/isqrt--traditional` `(test)`<br>
     `(my/isqrt--traditional 99)` → `9`
   - function `my/gcd--traditional` `(test)`<br>
     `(my/gcd--traditional 6 15)` → `3`
   - function `my/lcm--traditional` `(test)` (requires `my/gcd--traditional`)<br>
     `(my/lcm--traditional 3 4)` → `12`

File **integers-digits.el**
   - function `my/reverse-number` `(test)`<br>
     `(my/reverse-number 123)` → `321`

File **integers-french.el**
   - function `my/en-toutes-lettres` `(test)`<br>
     `(my/en-toutes-lettres 81)` → `"quatre-vingt-un"`

File **integers-primes.el**
   - function `my/primep` `(test)` (uses `cl-loop`, `cl-evenp`, `cl-isqrt`)<br>
     `(my/primep 7)` → `t`
     - traditional variant: `my/primep--traditional` `(test)` (requires `my/isqrt--traditional`)<br>
       `(my/primep--traditional 9)` → `nil`
   - function `my/largest-prime-factor--traditional` `(test)` (requires `my/isqrt--traditional`)<br>
     `(my/largest-prime-factor--traditional 13195)` → `29`
   - function `my/eratosthenes-sieve` `(test)` (uses `cl-loop`)<br>
     `(aref (my/eratosthenes-sieve 100) 7)` → `t` (7 is prime)
     - traditional variant: `my/eratosthenes-sieve--traditional` `(test)`<br>
       `(aref (my/eratosthenes-sieve--traditional 100) 9)` → `nil` (9 is not prime)
   - function `my/next-prime` `(test)` (requires `my/primep`; uses `cl-loop`, `cl-evenp`)<br>
     `(my/next-prime 13)` → `17`
     - traditional variant: `my/next-prime--traditional` `(test)` (requires `my/primep--traditional`)<br>
       `(my/next-prime--traditional 100)` → `101`
   - function `my/nth-prime` `(test)` (requires `my/primep`, `my/next-prime`; uses `cl-loop`)<br>
     `(my/nth-prime 6)` → `13`
     - traditional variant: `my/nth-prime--traditional` `(test)` (requires `my/primep--traditional`, `my/next-prime--traditional`)<br>
       `(my/nth-prime--traditional 168)` → `997`

File **files.el**
   - function `my/insert-directories-in-file-list` `(test)` (uses `cl-loop`)<br>
     `(my/insert-directories-in-file-list '("d1/a.org" "d1/b.org" "d2/c.org"))` → `("d1/" "d1/a.org" "d1/b.org" "d2/" "d2/c.org")`
   - function `my/get-file-last-modification-date`<br>
     `(my/get-file-last-modification-date "~/notes.org")` → Lisp timestamp of last modification
   - function `my/file-size-Mo`<br>
     `(my/file-size-Mo "~/big-archive.zip")` → size in megabytes as integer, e.g. `42`
   - function `my/nb-of-elements-in-directory`<br>
     `(my/nb-of-elements-in-directory "~/Documents/")` → e.g. `17`
   - function `my/size-of-folder-in-Mo` (requires PowerShell — Windows only)<br>
     `(my/size-of-folder-in-Mo "C:/Users/me/projects/")` → total size in megabytes
   - function `my/list-of-directories-and-subdirectories-from` (requires external `f` package)<br>
     `(my/list-of-directories-and-subdirectories-from "~/projects/" t)` → alphabetically sorted list of every subdirectory under `~/projects/`

File **macros.el**
   - macro `aprogn` `(test)` (uses `cl-loop`)<br>
     `(aprogn (+ 1 1) (* it 3) (+ it 4))` → `10`
   - macro `amapcar` `(test)`<br>
     `(amapcar (* 2 it) '(2 3))` → `(4 6)`
   - macro `let+` `(test)` (uses `cl-labels`, `cl-multiple-value-bind`)<br>
     `(let+ ((a 3) ((c d) (list 5 6))) (list a c d))` → `(3 5 6)` (also supports `:instruction` and `:labels` clauses)
   - macro `awhen` `(test)`<br>
     `(awhen (* 2 2) (+ 3 it))` → `7`
   - macro `aif` `(test)`<br>
     `(aif (* 2 2) (+ 3 it) "no")` → `7`

File **strings.el**
   - function `my/string-remove-surrounding-quotes` `(test)` (requires `aprogn` from `macros.el`)<br>
     `(my/string-remove-surrounding-quotes "\"abcdef\"")` → `"abcdef"`
   - function `my/string-suffix-p` `(test)`<br>
     `(my/string-suffix-p "DEF" "abcdef" t)` → `t`
   - function `my/split-string-at-first-delimiter` `(test)`<br>
     `(my/split-string-at-first-delimiter "aa bb cc")` → `("aa" "bb cc")`

File **trampoline.el**
   - function `my/trampoline` `(test)`<br>
     with `(cl-defun %fact (i &optional (acc 0)) (if (= 0 i) acc (lambda () (%fact (- i 1) (* acc i)))))`, `(my/trampoline (%fact 6 1))` → `720`. The file also includes a worked-out Leibniz-formula example (`leibniz-A` without trampoline overflows the stack; `leibniz-B` with the trampoline returns `3.141593653588793`).

Any comment? Open an [issue](https://github.com/occisn/elisp-utils/issues), or start a discussion [here](https://github.com/occisn/elisp-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

(end of README)
