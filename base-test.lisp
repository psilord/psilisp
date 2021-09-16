(in-package #:base)

(defun test-emit ()
  (format t "Test: emit()~%")
  (let ((entry-name "scheme_entry"))
    (emit ())
    (emit () "mov $0x~X, %rax" 42)
    (emit (:cmnt ("A big pile of poo: ~A" 100)))
    (emit (:cmnt ("flux capacitor version: ~A" 100))
          "mov $0x~X, %rax" 42)
    (emit (:label entry-name))
    (emit (:label "foobar")
          "clc")
    (emit (:label "foobar" :cmnt ("looky here!")))
    (emit (:label "junk" :cmnt ("a pile of poo: ~A" 100))
          "and $0x~X, %rax" 42)))

;; only used in this file for testing the environment API.
(defstruct test-syment
  attribute) ;; a test slot whose value is :in or :out.

(defun test-symtab ()
  (format t "Test: symtab API~%")
  (let ((st (st:make-symtab)))
    (st:insert-symbol st 'foo (make-test-syment :attribute :in))
    (st:insert-symbol st 'bar (make-test-syment :attribute :out))
    (st:dump-symtab st)))

(defun test-blsymtab ()
  (format t "Test: blsymtab API~%")
  (let ((bst (bst:make-blsymtab)))
    (format t "Opening first frame of variables.~%")
    (bst:open-scope bst)
    (bst:add-definition bst 'foo (make-test-syment :attribute :in))
    (bst:add-definition bst 'feh (make-test-syment :attribute :in))
    (bst:dump-blsymtab bst)

    (format t "Opening second frame of variables.~%")
    (bst:open-scope bst)
    (bst:add-definition bst 'foo (make-test-syment :attribute :out))
    (bst:add-definition bst 'bar (make-test-syment :attribute :out))
    (bst:dump-blsymtab bst)

    (format t "Current :inner definitions are: ~S~%"
            (bst:current-definitions bst))
    (format t "Current :any definitions are: ~S~%"
            (bst:current-definitions bst :scope :any))

    (let ((var0 (bst:find-definition bst 'foo))
          (var1 (bst:find-definition bst 'feh))
          (var2 (bst:find-definition bst 'feh :scope :any))
          (var3 (bst:find-definition bst 'bar))
          (var4 (bst:find-definition bst 'qux)))
      (format t "Expect foo/:out -> got: ~S/~S~%" 'foo var0)
      (format t "Expect feh not found -> got: ~S/~S~%" 'feh var1)
      (format t "Expect feh/:in -> got: ~S/~S~%" 'feh var2)
      (format t "Expect bar/:out -> got: ~S/~S~%" 'bar var3)
      (format t "Expect qux not found -> got: ~S/~S~%" 'qux var4))

    (format t "Closing second frame of variables.~%")
    (bst:close-scope bst)
    (bst:dump-blsymtab bst)

    (let ((var5 (bst:find-definition bst 'foo))
          (var6 (bst:find-definition bst 'bar)))
      (format t "Expect foo/:in -> got: ~S/~S~%" 'foo var5)
      (format t "Expect bar/nil -> got: ~S/~S~%" 'bar var6))

    (format t "Opening another second frame of variables.~%")
    (bst:open-scope bst)
    (bst:add-definition bst 'thing (make-test-syment :attribute :in))
    (bst:add-definition bst 'stuff (make-test-syment :attribute :out))
    (bst:dump-blsymtab bst)
    (format t "Current :inner definitions are: ~S~%"
            (bst:current-definitions bst))
    (format t "Current :any definitions are: ~S~%"
            (bst:current-definitions bst :scope :any))))
