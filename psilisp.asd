(asdf:defsystem #:psilisp
  :description "Incremental Test Lisp Language Experiments"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre :alexandria)
  :components ((:file "package")
               (:file "base") ;; stuff used by all compilers.
	       (:file "base-test")
               ;; Ghoulum's incremental lisp compiler
               (:file "c0")
               (:file "c0-test")
               ;; My first dialect to explore these topics:
               ;; alpha-conversion
               ;; free variables
               ;; mutable variable pass
               ;; closure conversion
               (:file "c1")
               (:file "c1-test-support")))
