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
   ;; environment handling
   ;; symbol table API
   #:symtab
   #:symtab-syments
   #:symtab-scope-id
   #:make-symtab
   #:symtab-p
   #:insert-symbol
   #:lookup-symbol
   #:dump-symtab
   ;; block symbol table API
   #:blsymtab
   #:blsymtab-frames
   #:make-blsymtab
   #:blsymtab-p
   #:open-scope
   #:close-scope
   #:add-definition
   #:find-definition
   #:current-definitions
   #:dump-blsymtab
   ;; Aggregate env table API
   #:make-env
   #:env-open-scope
   #:env-close-scope
   #:env-add-definition
   #:env-find-definition
   #:env-current-definitions
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
