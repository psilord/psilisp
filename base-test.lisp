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
  (let ((st (make-symtab)))
    (insert-symbol st 'foo (make-test-syment :attribute :in))
    (insert-symbol st 'bar (make-test-syment :attribute :out))
    (dump-symtab st)))

(defun test-blsymtab ()
  (format t "Test: blsymtab API~%")
  (let ((bst (make-blsymtab)))
    (format t "Opening first frame of variables.~%")
    (open-scope bst)
    (add-definition bst 'foo (make-test-syment :attribute :in))
    (add-definition bst 'feh (make-test-syment :attribute :in))
    (dump-blsymtab bst)

    (format t "Opening second frame of variables.~%")
    (open-scope bst)
    (add-definition bst 'foo (make-test-syment :attribute :out))
    (add-definition bst 'bar (make-test-syment :attribute :out))
    (dump-blsymtab bst)

    (format t "Current :inner definitions are: ~S~%"
            (current-definitions bst))
    (format t "Current :any definitions are: ~S~%"
            (current-definitions bst :scope :any))

    (let ((var0 (find-definition bst 'foo))
          (var1 (find-definition bst 'feh))
          (var2 (find-definition bst 'feh :scope :any))
          (var3 (find-definition bst 'bar))
          (var4 (find-definition bst 'qux)))
      (format t "Expect foo/:out -> got: ~S/~S~%" 'foo var0)
      (format t "Expect feh not found -> got: ~S/~S~%" 'feh var1)
      (format t "Expect feh/:in -> got: ~S/~S~%" 'feh var2)
      (format t "Expect bar/:out -> got: ~S/~S~%" 'bar var3)
      (format t "Expect qux not found -> got: ~S/~S~%" 'qux var4))

    (format t "Closing second frame of variables.~%")
    (close-scope bst)
    (dump-blsymtab bst)

    (let ((var5 (find-definition bst 'foo))
          (var6 (find-definition bst 'bar)))
      (format t "Expect foo/:in -> got: ~S/~S~%" 'foo var5)
      (format t "Expect bar/nil -> got: ~S/~S~%" 'bar var6))

    (format t "Opening another second frame of variables.~%")
    (open-scope bst)
    (add-definition bst 'thing (make-test-syment :attribute :in))
    (add-definition bst 'stuff (make-test-syment :attribute :out))
    (dump-blsymtab bst)
    (format t "Current :inner definitions are: ~S~%"
            (current-definitions bst))
    (format t "Current :any definitions are: ~S~%"
            (current-definitions bst :scope :any))))
