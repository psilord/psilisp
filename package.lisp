;;;; package.lisp


;; Base stuff refactored from all compilers
(defpackage #:base
  (:use #:cl)
  (:export
   ;; shorthand for CL constant definitions
   #:defconstants
   ;; Making code labels.
   #:unique-label
   #:unique-labels
   ;; Emitting assembly instructions as strings
   #:emit
   )
  (:local-nicknames
   (#:a #:alexandria)))

;; Symbol table
(defpackage #:st
  (:use #:cl)
  (:export
   #:symtab
   #:make-symtab
   #:scope-id
   #:insert-symbol
   #:lookup-symbol
   #:all-names
   #:all-symbols
   #:dump-symtab
   )

  (:local-nicknames
   (#:a #:alexandria)))

;; Block structure symbol table
(defpackage #:bst
  (:use #:cl)
  (:export
   #:blsymtab
   #:make-blsymtab
   #:open-scope
   #:close-scope
   #:add-definition
   #:find-definition
   #:current-definitions
   #:current-scope
   #:dump-blsymtab
   )
  (:local-nicknames
   (#:a #:alexandria)))

;; Environment table built with block structured tables
(defpackage #:env
  (:use #:cl)
  (:export
   #:make-env
   #:open-scope
   #:close-scope
   #:add-definition
   #:find-definition
   #:current-definitions
   #:current-scope
   #:dump-env
   )
  (:local-nicknames
   (#:a #:alexandria)))


;; Ghoulum's scheme implementation.
(defpackage #:c0
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)))

;; My attempts at an implementation merging all kinds of stuff together.
(defpackage #:c1
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)))
