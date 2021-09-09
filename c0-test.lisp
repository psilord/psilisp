(in-package #:c0)

;; -------------------------------------------------------------------
;; unit testing.
;; -------------------------------------------------------------------


(defun run-tests ()
  (flet ((expect (expectation program &rest results)
           (let* ((prog-results (test :program program :output :string))
                  (prog-repl-output (capture-repl-output prog-results))
                  (matched (every #'string= results prog-repl-output)))
             (format t "PGM: ~S~%  E:~A V:~S R:~S -> ~A~%"
                     program expectation results prog-repl-output matched)
             (ecase expectation
               (:ok matched)
               (:nok (not matched))))))

    (if
     (and
      ;; --------------
      ;; Immediates
      ;; --------------

      (expect :ok (fxlower) "-2305843009213693952")
      (expect :ok -1 "-1")
      (expect :ok 0 "0")
      (expect :ok 1 "1")
      (expect :ok (fxupper) "2305843009213693951")

      (expect :ok #f "#f")
      (expect :ok #t "#t")

      (expect :ok #\A "A")
      (expect :ok #\a "a")

      (expect :ok '() "()")

      ;; --------------
      ;; Unary Primitives
      ;; --------------

      ;; fxadd1

      (expect :ok `(fxadd1 ,(fxlower)) "-2305843009213693951")
      (expect :ok '(fxadd1 -2) "-1")
      (expect :ok '(fxadd1 -1) "0")
      (expect :ok '(fxadd1 0) "1")
      (expect :ok '(fxadd1 1) "2")
      ;; Next one rolls over to smallest fixnum.
      (expect :ok `(fxadd1 ,(fxupper)) "-2305843009213693952")

      ;; fxsub1

      ;; Next one rolls over to most positive fixnum
      (expect :ok `(fxsub1 ,(fxlower)) "2305843009213693951")
      (expect :ok '(fxsub1 -1) "-2")
      (expect :ok '(fxsub1 0) "-1")
      (expect :ok '(fxsub1 1) "0")
      (expect :ok '(fxsub1 2) "1")
      (expect :ok `(fxsub1 ,(fxupper)) "2305843009213693950")

      ;; fixnum->char

      (expect :ok '(fixnum->char 65) "A")

      ;; char->fixnum

      (expect :ok '(char->fixnum #\A) "65")

      ;; fxzero?

      (expect :ok '(fxzero? ()) "#f")
      (expect :ok `(fxzero? ,(fxlower)) "#f")
      (expect :ok '(fxzero? 0) "#t")
      (expect :ok `(fxzero? ,(fxupper)) "#f")
      (expect :ok '(fxzero? #\A) "#f")
      (expect :ok `(fxzero? ,#t) "#f")
      (expect :ok `(fxzero? ,#f) "#f")

      ;; fixnum?

      (expect :ok '(fixnum? ()) "#f")
      (expect :ok `(fixnum? ,(fxlower)) "#t")
      (expect :ok '(fixnum? 0) "#t")
      (expect :ok `(fixnum? ,(fxupper)) "#t")
      (expect :ok '(fixnum? #\A) "#f")
      (expect :ok `(fixnum? ,#t) "#f")
      (expect :ok `(fixnum? ,#f) "#f")

      ;; null?

      (expect :ok '(null? ()) "#t")
      (expect :ok `(null? ,#t) "#f")
      (expect :ok `(null? ,#f) "#f")
      (expect :ok '(null? 0) "#f")
      (expect :ok '(null? #\A) "#f")

      ;; char?

      (expect :ok '(char? ()) "#f")
      (expect :ok `(char? ,#t) "#f")
      (expect :ok `(char? ,#f) "#f")
      (expect :ok '(char? 0) "#f")
      (expect :ok '(char? #\A) "#t")

      ;; boolean?

      (expect :ok '(boolean? ()) "#f")
      (expect :ok `(boolean? ,#t) "#t")
      (expect :ok `(boolean? ,#f) "#t")
      (expect :ok '(boolean? 0) "#f")
      (expect :ok '(boolean? #\A) "#f")

      ;; not

      (expect :ok '(not ()) "#f")
      (expect :ok `(not ,#t) "#f")
      (expect :ok `(not ,#f) "#t")
      (expect :ok '(not 0) "#f")
      (expect :ok '(not #\A) "#f")

      ;; fxlognot

      (expect :ok '(fxlognot 5) "-6")
      (expect :ok '(fxlognot 0) "-1")
      (expect :ok '(fxlognot -12) "11")

      ;; fx+

      (expect :ok '(fx+ (fxadd1 0) (fxadd1 1)) "3")
      (expect :ok '(fx+ 10 (fxadd1 0)) "11")
      (expect :ok '(fx+ (fxadd1 0) 10) "11")
      (expect :ok '(fx+ 10 20) "30")

      ;; fx-

      (expect :ok '(fx- (fxadd1 0) (fxadd1 1)) "-1")
      (expect :ok '(fx- 10 (fxadd1 0)) "9")
      (expect :ok '(fx- (fxadd1 0) 10) "-9")
      (expect :ok '(fx- 10 20) "-10")

      ;; fxlogand

      (expect :ok '(fxlogand (fxadd1 #xef) (fxadd1 #x2f)) "48")
      (expect :ok '(fxlogand (fxadd1 #xef) #x30) "48")
      (expect :ok '(fxlogand #xf0 (fxadd1 #x2f)) "48")
      (expect :ok '(fxlogand #xf0 #x30) "48")


      ;; fxlogor

      (expect :ok '(fxlogor (fxadd1 #xef) (fxadd1 #x0e)) "255")
      (expect :ok '(fxlogor (fxadd1 #xef) #x0f) "255")
      (expect :ok '(fxlogor #xf0 (fxadd1 #x0e)) "255")
      (expect :ok '(fxlogor #xf0 #x0f) "255")

      ;; --------------
      ;; Conditional Expressions
      ;; --------------

      (expect :ok '(if () 10 20) "10")
      (expect :ok '(if 42 10 20) "10")
      (expect :ok '(if #\A 10 20) "10")
      (expect :ok `(if ,#t 10 20) "10")
      (expect :ok `(if ,#f 10 20) "20")

      ;; --------------
      ;; Binary primitives
      ;; --------------

      ;; fx=

      (expect :ok '(fx= (fxadd1 10) (fxsub1 12)) "#t")
      (expect :ok '(fx= (fxadd1 10) 11) "#t")
      (expect :ok '(fx= 11 (fxadd1 10)) "#t")
      (expect :ok '(fx= 11 11) "#t")

      (expect :ok '(fx= (fxadd1 10) (fxsub1 10)) "#f")
      (expect :ok '(fx= (fxadd1 10) 12) "#f")
      (expect :ok '(fx= 12 (fxadd1 10)) "#f")
      (expect :ok '(fx= 11 12) "#f")

      ;; fx<

      (expect :ok '(fx< (fxadd1 10) (fxadd1 11)) "#t")
      (expect :ok '(fx< (fxadd1 10) 12) "#t")
      (expect :ok '(fx< 11 (fxadd1 11)) "#t")
      (expect :ok '(fx< 11 12) "#t")

      (expect :ok '(fx< (fxadd1 10) (fxadd1 10)) "#f")
      (expect :ok '(fx< (fxadd1 10) 11) "#f")
      (expect :ok '(fx< 11 (fxadd1 10)) "#f")
      (expect :ok '(fx< 11 11) "#f")

      ;; fx<=

      (expect :ok '(fx<= (fxadd1 10) (fxsub1 12)) "#t")
      (expect :ok '(fx<= (fxadd1 10) 11) "#t")
      (expect :ok '(fx<= 11 (fxadd1 10)) "#t")
      (expect :ok '(fx<= 11 11) "#t")

      (expect :ok '(fx<= (fxadd1 10) (fxsub1 13)) "#t")
      (expect :ok '(fx<= (fxadd1 10) 12) "#t")
      (expect :ok '(fx<= 11 (fxadd1 11)) "#t")
      (expect :ok '(fx<= 11 12) "#t")

      (expect :ok '(fx<= (fxadd1 10) (fxadd1 9)) "#f")
      (expect :ok '(fx<= (fxadd1 10) 10) "#f")
      (expect :ok '(fx<= 11 (fxadd1 9)) "#f")
      (expect :ok '(fx<= 11 10) "#f")

      ;; fx>

      (expect :ok '(fx> (fxadd1 11) (fxadd1 10)) "#t")
      (expect :ok '(fx> (fxadd1 11) 11) "#t")
      (expect :ok '(fx> 12 (fxadd1 10)) "#t")
      (expect :ok '(fx> 12 11) "#t")

      (expect :ok '(fx> (fxadd1 10) (fxadd1 11)) "#f")
      (expect :ok '(fx> (fxadd1 10) 12) "#f")
      (expect :ok '(fx> 11 (fxadd1 11)) "#f")
      (expect :ok '(fx> 11 12) "#f")

      ;; fx>=

      (expect :ok '(fx>= (fxadd1 10) (fxsub1 12)) "#t")
      (expect :ok '(fx>= (fxadd1 10) 11) "#t")
      (expect :ok '(fx>= 11 (fxadd1 10)) "#t")
      (expect :ok '(fx>= 11 11) "#t")

      (expect :ok '(fx>= (fxadd1 10) (fxsub1 11)) "#t")
      (expect :ok '(fx>= (fxadd1 10) 10) "#t")
      (expect :ok '(fx>= 13 (fxadd1 11)) "#t")
      (expect :ok '(fx>= 12 11) "#t")

      (expect :ok '(fx>= (fxadd1 9) (fxadd1 10)) "#f")
      (expect :ok '(fx>= (fxadd1 8) 10) "#f")
      (expect :ok '(fx>= 9 (fxadd1 9)) "#f")
      (expect :ok '(fx>= 10 11) "#f")

      ;; char=

      (expect :ok '(char= (fixnum->char 65) (fixnum->char 65)) "#t")
      (expect :ok '(char= (fixnum->char 65) #\A) "#t")
      (expect :ok '(char= #\A (fixnum->char 65)) "#t")
      (expect :ok '(char= #\A #\A) "#t")

      (expect :ok '(char= (fixnum->char 65) (fixnum->char 97)) "#f")
      (expect :ok '(char= (fixnum->char 65) #\B) "#f")
      (expect :ok '(char= #\B (fixnum->char 65)) "#f")
      (expect :ok '(char= #\A #\a) "#f")

      ;; char<

      (expect :ok '(char< (fixnum->char 65) (fixnum->char 66)) "#t")
      (expect :ok '(char< (fixnum->char 65) #\B) "#t")
      (expect :ok '(char< #\A (fixnum->char 66)) "#t")
      (expect :ok '(char< #\A #\B) "#t")

      (expect :ok '(char< (fixnum->char 66) (fixnum->char 65)) "#f")
      (expect :ok '(char< (fixnum->char 66) #\A) "#f")
      (expect :ok '(char< #\B (fixnum->char 65)) "#f")
      (expect :ok '(char< #\B #\A) "#f")

      ;; char<=

      (expect :ok '(char<= (fixnum->char 65) (fixnum->char 65)) "#t")
      (expect :ok '(char<= (fixnum->char 65) #\A) "#t")
      (expect :ok '(char<= #\A (fixnum->char 65)) "#t")
      (expect :ok '(char<= #\A #\A) "#t")

      (expect :ok '(char<= (fixnum->char 65) (fixnum->char 66)) "#t")
      (expect :ok '(char<= (fixnum->char 65) #\B) "#t")
      (expect :ok '(char<= #\A (fixnum->char 66)) "#t")
      (expect :ok '(char<= #\A #\B) "#t")

      (expect :ok '(char<= (fixnum->char 66) (fixnum->char 65)) "#f")
      (expect :ok '(char<= (fixnum->char 66) #\A) "#f")
      (expect :ok '(char<= #\B (fixnum->char 65)) "#f")
      (expect :ok '(char<= #\B #\A) "#f")

      ;; char>

      (expect :ok '(char> (fixnum->char 66) (fixnum->char 65)) "#t")
      (expect :ok '(char> (fixnum->char 66) #\A) "#t")
      (expect :ok '(char> #\B (fixnum->char 65)) "#t")
      (expect :ok '(char> #\B #\A) "#t")

      (expect :ok '(char> (fixnum->char 65) (fixnum->char 66)) "#f")
      (expect :ok '(char> (fixnum->char 65) #\B) "#f")
      (expect :ok '(char> #\A (fixnum->char 66)) "#f")
      (expect :ok '(char> #\A #\B) "#f")

      ;; char>=

      (expect :ok '(char>= (fixnum->char 65) (fixnum->char 65)) "#t")
      (expect :ok '(char>= (fixnum->char 65) #\A) "#t")
      (expect :ok '(char>= #\A (fixnum->char 65)) "#t")
      (expect :ok '(char>= #\A #\A) "#t")

      (expect :ok '(char>= (fixnum->char 66) (fixnum->char 65)) "#t")
      (expect :ok '(char>= (fixnum->char 66) #\A) "#t")
      (expect :ok '(char>= #\B (fixnum->char 65)) "#t")
      (expect :ok '(char>= #\B #\A) "#t")

      (expect :ok '(char>= (fixnum->char 65) (fixnum->char 66)) "#f")
      (expect :ok '(char>= (fixnum->char 65) #\B) "#f")
      (expect :ok '(char>= #\A (fixnum->char 66)) "#f")
      (expect :ok '(char>= #\A #\B) "#f")

      ;; let
      (expect :ok '(let () 42) "42")
      (expect :ok '(let ((a 10)) a) "10")
      (expect :ok '(let ((a 10) (b 20)) (fx+ a b)) "30")
      (expect :ok '(let ((x 1)) (let ((x (fx+ x 1)) (y (fx+ x 1))) y)) "2")
      (expect :ok '(fx+
                    (let ((a 10) (b 20)) (fx+ a b))
                    (let ((a 30) (b 40)) (fx+ a b))))

      ;; let*
      (expect :ok '(let* () 42) "42")
      (expect :ok '(let* ((a 10)) a) "10")
      (expect :ok '(let* ((a 10) (b 20)) (fx+ a b)) "30")
      (expect :ok '(let* ((a 10) (b (fx+ a 10)) (c (fx+ b 10))) c) "30")
      (expect :ok '(let* ((x 1)) (let* ((x (fx+ x 1)) (y (fx+ x 1))) y)) "3")

      )

     (format t "Tests All Passed!~%")
     (format t "One or more tests failed!~%"))))
