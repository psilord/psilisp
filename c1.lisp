(in-package #:c1)

;; -----------------------------------------------------------------------
;; Small Core Input Language (half Scheme, half CL)
;; Evaluation Model: Left to right.
;; Lexical scoping only. No namespace/packages yet.
;; No macros yet.
;; When at TOPLEVEL, the bodies of LET, LETREC, BEGIN, act as toplevel.
;; BEGIN at the start of a BODY can act like a toplevel too wrt DEFINE.
;; This causes LET/LETREC to not be translatable to a LAMBDA and therefore
;; a special form.
;; -----------------------------------------------------------------------
;; <toplevel> ::= <body>
;; <body> ::= <defs> <exprs>
;; <defs> ::= <def> <defs>
;;        |
;; <exprs> ::= <expr> <exprs>
;;         |
;; <expr> ::= <var>
;;        | <literal>
;;        | <prim>
;;        | <lambda>
;;        | <define> ;; UNIMPLEMENTED until I figure it out or do smth else.
;;          All sequential DEFINEs are effectively grouped into a LETREC.
;;
;;          If in toplevel context:
;;            If <var> doesn't exist:
;;              bind new global var with expr value
;;            If <var> does exist:
;;              mutate global definition as set! with expr value
;;          If NOT in toplevel context:
;;            If <var> doesn't exist:
;;              bind new var at CURRENT scope with expr value
;;            If <var> does exist:
;;              mutate var at current scope as set! with expr value
;;        | <let> ;; parallel variable or function binding
;;        | <letrec> ;; ONLY recursive function definitions, like CL LABELS
;;        | <set!>
;;        | <if>
;;        | <begin>
;;        | <application>
;;        ;; | <quote> ;; unimplemented!
;; <var> ::= <symbol>
;; <literal> ::= <fixnum> | <character> | <boolean> | <null>
;; <fixnum> ::= min-fixnum to max-fixnum
;; <character> ::= #\[utf-8 character]
;; <boolean> ::= true | false
;; <null> ::= null
;; <prim> ::= <prim-unary> | <prim-binary>
;; <prim-unary> ::= (<prim-unary-op> <expr>)
;; <prim-binary> ::= (<prim-binary-op> <expr> <expr>)
;; <prim-unary-op> ::= fxadd1 | fxsub1 | fixnum->char | char->fixnum |
;;                     fxzero? | fixnum? | null? | char? | boolean? | not |
;;                     fxlognot
;; <prim-binary-op> ::= fx+ | fx- | fx* | fx/ | fxlongand | fxlogor | fx= |
;;                      fx< | fx<= | fx> | fx>= | char= | char< | char<= |
;;                      char> | char>=
;; <lambda> ::= (lambda (<vardecls>) <body>)
;; <vardecls> ::= <vardecl> <vardecls>
;;            |
;; <vardecl> ::= <var> ;; can support more types of args as I add them.
;; <define> ::= (define <vardecl> <expr>)
;; <let> ::= (let (<let-bindings>) <body>)
;; <let-bindings> ::= <let-binding> <let-bindings>
;;                |
;; <let-binding> ::= (<vardecl> <expr>)
;;               |
;; <letrec> ::= (let (<letrec-bindings>) <body>)
;; <letrec-bindings> ::= <letrec-binding> <letrec-bindings>
;;                   |
;; <letrec-binding> ::= (<vardecl> <expr>)
;; <set!> ::= (set! <var> <expr>)
;; <if> ::= (if <expr> <expr> <expr>)
;; <begin> ::= (begin <body>) ;; special behavior with DEFINE
;; <application> ::= (<expr> <exprs>)
;; <quote> ::= (quote <expr>)

;; -----------------------------------------------------------------------
;; Some convenient machinery to deal with defclass types.
;; -----------------------------------------------------------------------
(defmacro simple-constructor (name &rest init-args)
  "Create a MAKE-NAME constructor that takes exactly INIT-ARGS in the order
specified and calls MAKE-INSTANCE on the NAME type. It assumes that the
initargs for the class are named the same as those supplied in INIT-ARGS."
  `(defun ,(a:format-symbol :c1 "MAKE-~A" name) ,init-args
     (make-instance ',name
                    ,@(when init-args
                        (mapcan (lambda (arg)
                                  `(,(a:format-symbol :keyword arg) ,arg))
                                init-args)))))

(defmacro flexible-constructor (name)
  `(defun ,(a:format-symbol :c1 "MAKE-~A" name) (&rest init-args)
     (apply 'make-instance ',name init-args)))

;; -----------------------------------------------------------------------
;; Environment Modeling
;; -----------------------------------------------------------------------

;; A symbol table entry for a variable for this compiler
;; TODO: Will get radical extension/real definition soon.
(defclass syment ()
  ;; is this symbol global (not subject to capture?)
  ((%global-p :accessor global-p :initarg :global-p :initform nil)
   ;; Is this symbol available to be mutated via set!
   (%mutable-p :accessor mutable-p :initarg :mutable-p :initform t)
   ;; Was this symbol actually mutated (at some time) via set! ?
   (%mutated-p :accessor mutated-p :initarg :mutated-p :initform nil)
   ;; Is this entry a syntax symbol (in its current lexical context):
   (%syntax-p :accessor syntax-p :initarg :syntax-p :initform nil)
   ;; is this symbol a primitive function in this context?
   (%prim-p :accessor prim-p :initarg :prim-p :initform nil)
   ;; Is this symbol known to be bound to anything?  If not, it is an unbound
   ;; variable reference.
   ;; TODO: Add this into the flow of the compiler.
   (%bound-p :accessor bound-p :initarg :bound-p :initform nil)

   ;; Stuff below is squishy, when I define/use it, then print it out.

   ;; :stack or :heap
   (%location :accessor location :initarg :location :initform :heap)
   ))
;; We'll see if I like this for now...
(defmethod print-object ((obj syment) str)
  (call-next-method)
  #++(print-unreadable-object (obj str :type t :identity nil)
       (format str "G ~A, M ~A, MD ~A, S ~A, P ~A"
               (global-p obj)
               (mutable-p obj)
               (mutated-p obj)
               (syntax-p obj)
               (prim-p obj))))

(flexible-constructor syment)

(defun make-syment/global ()
  (make-syment :global-p t :mutable-p t
               :mutated-p nil :syntax-p nil
               :prim-p nil))

(defun make-syment/local ()
  (make-syment :global-p nil :mutable-p t
               :mutated-p nil :syntax-p nil
               :prim-p nil))

(defun make-syment/syntax ()
  (make-syment :global-p t :mutable-p nil
               :mutated-p nil :syntax-p t
               :prim-p nil))

(defun make-syment/prim ()
  (make-syment :global-p t :mutable-p nil
               :mutated-p nil :syntax-p nil
               :prim-p t))

;; -----------------------------------------------------------------------
;; Tagged list handling
;; -----------------------------------------------------------------------

(defun tagged-list-p (expr)
  (and (listp expr) (symbolp (car expr))))

(defun tagged-with-p (tag expr)
  (when (listp expr)
    (eq tag (car expr))))

;; -----------------------------------------------------------------------
;; Checking tagged lists to see if they are in the environment
;; -----------------------------------------------------------------------

(defun expected-form-p (env expr datum pred &key (scope :any))
  "If the EXPR is a tagged list, and the car is DATUM, then if the symbol entry
exists, check to see if it is a PRED. Check the SCOPE in the block symbol table
and default it to :any"
  (when (tagged-with-p datum expr)
    (a:when-let ((syment (base:find-definition env datum :scope scope)))
      (funcall pred syment))))

(defun syntax-form-p (env expr datum)
  (expected-form-p env expr datum 'syntax-p))

(defun prim-form-p (env expr datum)
  (expected-form-p env expr datum 'prim-p))

;; -----------------------------------------------------------------------
;; Original Source form AST: Syntax tree of original (including sugar) input.
;; This is a traditional (in ALGOL-styl languages) for how to represent and
;; AST of the original source forms. I do this so it provides me a place to put
;; annotation information and other stuff that I need to associate with things
;; without cramming everything into progressively more complex sexp forms.
;; The :type clause is basically meaningless, but useful for me as a human
;; to keep track of the types in the AST node slots.
;; -----------------------------------------------------------------------

(defgeneric unparse (style indent node)) ;; unparse back into psilisp or other

(defclass ast () ())

(defclass toplevel (ast)
  ((%body :accessor body :initarg :body :type body)))
(simple-constructor toplevel body)

(defclass body (ast)
  ((%defs :accessor defs :initarg :defs :type defs)
   (%exprs :accessor exprs :initarg :exprs :type exprs)))
(simple-constructor body defs exprs)

(defclass defs (ast)
  ((%def :accessor def :initarg :def :type def)
   (%more :accessor more :initarg :more :type defs)))
(simple-constructor defs def more)

(defclass exprs (ast)
  ((%expr :accessor expr :initarg :expr :type expr)
   (%more :accessor more :initarg :more :type exprs)))
(simple-constructor exprs expr more)

(defclass expr (ast) ())

(defclass vardecls (ast)
  ((%vardecl :accessor vardecl :initarg :vardecl :type vardecl)
   (more :accessor more :initarg :more :type vardecls)))
(simple-constructor vardecls vardecl more)

;; If I add more types of vardecls, like keyword, rest, etc, subclass this.
(defclass vardecl (ast)
  ((%var :accessor var :initarg :var :type var)))
(simple-constructor vardecl var)

(defclass var (expr)
  ((%sym :accessor sym :initarg :sym :type symbol)))
(simple-constructor var sym)

(defclass literal (expr) ())

(defclass literal-fixnum (literal)
  ((%value :accessor value :initarg :value :type integer)))
(simple-constructor literal-fixnum value)

(defclass literal-char (literal)
  ((%value :accessor value :initarg :value :type character)))
(simple-constructor literal-char value)

(defclass literal-bool (literal)
  ((%value :accessor value :initarg :value :type T)))
(simple-constructor literal-bool value)

(defclass literal-null (literal) ())
(simple-constructor literal-null)

(defclass prim (expr)
  ((%op :accessor op :initarg :op :type var)))
(simple-constructor prim op)

(defclass prim-unary (prim)
  ((%arg0 :accessor arg0 :initarg :arg0 :type expr)))
(simple-constructor prim-unary op arg0)

(defclass prim-binary (prim)
  ((%arg0 :accessor arg0 :initarg :arg0 :type expr)
   (%arg1 :accessor arg1 :initarg :arg1 :type expr)))
(simple-constructor prim-binary op arg0 arg1)

(defclass syntax (expr) ())

(defclass define-syntax (syntax)
  ((%vardecl :accessor vardecl :initarg :vardecl :type vardecl)
   (%value :accessor value :initarg :value :type expr)))
(simple-constructor define-syntax vardecl value)

(defclass lambda-syntax (syntax)
  ((%vardecls :accessor vardecls :initarg :vardecls :type vardecls)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor lambda-syntax vardecls body)

(defclass let-syntax (syntax)
  ((%bindings :accessor bindings :initarg :bindings :type let-bindings)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor let-syntax bindings body)

(defclass let-bindings (syntax)
  ((%binding :accessor binding :initarg :binding :type let-binding)
   (%more :accessor more :initarg :more :type let-bindings)))
(simple-constructor let-bindings binding more)

(defclass let-binding (syntax)
  ((%vardecl :accessor vardecl :initarg :vardecl :type vardecl)
   (%value :accessor value :initarg :value :type expr)))
(simple-constructor let-binding vardecl value)

(defclass letrec-syntax (syntax)
  ((%bindings :accessor bindings :initarg :bindings :type letrec-bindings)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor letrec-syntax bindings body)

(defclass letrec-bindings (syntax)
  ((%binding :accessor binding :initarg :binding :type letrec-binding)
   (%more :accessor more :initarg :more :type letrec-bindings)))
(simple-constructor letrec-bindings binding more)

(defclass letrec-binding (syntax)
  ((%vardecl :accessor vardecl :initarg :vardecl :type vardecl)
   (%value :accessor value :initarg :value :type expr)))
(simple-constructor letrec-binding vardecl value)

(defclass set!-syntax (syntax)
  ((%var :accessor var :initarg :var :type var)
   (%value :accessor value :initarg :value :type expr)))
(simple-constructor set!-syntax var value)

(defclass if-syntax (syntax)
  ((%choice :accessor choice :initarg :choice :type expr)
   (%consequent :accessor consequent :initarg :consequent :type expr)
   (%alternate :accessor alternate :initarg :alternate :type expr)))
(simple-constructor if-syntax choice consequent alternate)

(defclass begin-syntax (syntax)
  ((%body :accessor body :initarg :body :type body)))
(simple-constructor begin-syntax body)

(defclass application (expr)
  ((%op :accessor op :initarg :op :type expr)
   (%args :accessor args :initarg :args :type exprs)))
(simple-constructor application op args)

;; -----------------------------------------------------------------------
;; Unparsing
;; -----------------------------------------------------------------------

(defun logit (fmt &rest args)
  (apply #'format t fmt args))

(defun logiti (indent fmt &rest args)
  (apply #'format t (concatenate 'string "~A" fmt)
         (make-string indent :initial-element #\Space)
         args))

(defmethod unparse (style indent self)
  (error "Cannot unparse a style of: ~S" style))

;; ---------
;; Unparse style: :psilisp
;; ---------

(defmethod unparse ((style (eql :psilisp)) indent (self (eql nil)))
  ;; If there isn't anything for me to unparse there, don't unparse!
  nil)

(defmethod unparse ((style (eql :psilisp)) indent (self symbol))
  ;; A way for me to indicate TODO areas in the AST.
  (logit "~S" self))

(defmethod unparse ((style (eql :psilisp)) indent (self toplevel))
  (logit ";;; PSILISP~%")
  (logit ";;; BEGIN TOPLEVEL~%")
  (unparse style indent (body self))
  (logit "~%;;; END TOPLEVEL~%"))

(defmethod unparse ((style (eql :psilisp)) indent (self body))
  (unparse style indent (defs self))
  (unparse style indent (exprs self)))

(defmethod unparse ((style (eql :psilisp)) indent (self defs))
  (unparse style indent (def self))
  (unparse style indent (more self)))

(defmethod unparse ((style (eql :psilisp)) indent (self exprs))
  (unparse style indent (expr self))
  (when (more self)
    (logit " "))
  (unparse style indent (more self)))

(defmethod unparse ((style (eql :psilisp)) indent (self vardecls))
  (unparse style indent (vardecl self))
  (when (more self)
    (logit " "))
  (unparse style indent (more self)))

(defmethod unparse ((style (eql :psilisp)) indent (self vardecl))
  (unparse style indent (var self)))

(defmethod unparse ((style (eql :psilisp)) indent (self var))
  (logit "~A" (sym self)))

(defmethod unparse ((style (eql :psilisp)) indent (self literal-fixnum))
  (logit "~S" (value self)))

(defmethod unparse ((style (eql :psilisp)) indent (self literal-char))
  (logit "~S" (value self)))

(defmethod unparse ((style (eql :psilisp)) indent (self literal-bool))
  (logit "~S" (value self)))

(defmethod unparse ((style (eql :psilisp)) indent (self literal-null))
  (logit "NULL"))

(defmethod unparse ((style (eql :psilisp)) indent (self prim))
  (logit "(")
  (unparse style indent (op self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self prim-unary))
  (logit "(")
  (unparse style indent (op self))
  (logit " ")
  (unparse style indent (arg0 self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self prim-binary))
  (logit "(")
  (unparse style indent (op self))
  (logit " ")
  (unparse style indent (arg0 self))
  (logit " ")
  (unparse style indent (arg1 self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self define-syntax))
  (logit "(DEFINE ")
  (unparse style indent (vardecl self))
  (logit " ")
  (unparse style indent (value self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self lambda-syntax))
  (logit "(LAMBDA (")
  (unparse style indent (vardecls self))
  (logit ") ")
  (unparse style indent (body self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self let-binding))
  (logit "(")
  (unparse style indent (vardecl self))
  (logit " ")
  (unparse style indent (value self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self let-bindings))
  (unparse style indent (binding self))
  (when (more self)
    (logit " ")
    (unparse style indent (more self))))

(defmethod unparse ((style (eql :psilisp)) indent (self let-syntax))
  (logit "(LET (")
  (unparse style indent (bindings self))
  (logit ") ")
  (unparse style indent (body self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self letrec-binding))
  (logit "(")
  (unparse style indent (vardecl self))
  (logit " ")
  (unparse style indent (value self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self letrec-bindings))
  (unparse style indent (binding self))
  (when (more self)
    (logit " ")
    (unparse style indent (more self))))

(defmethod unparse ((style (eql :psilisp)) indent (self letrec-syntax))
  (logit "(LETREC (")
  (unparse style indent (bindings self))
  (logit ") ")
  (unparse style indent (body self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self set!-syntax))
  (logit "(SET! ")
  (unparse style indent (var self))
  (logit " ")
  (unparse style indent (value self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self if-syntax))
  (logit "(IF ")
  (unparse style indent (choice self))
  (logit " ")
  (unparse style indent (consequent self))
  (when (alternate self)
    (logit " ")
    (unparse style indent (alternate self)))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self begin-syntax))
  (logit "(BEGIN ")
  (unparse style indent (body self))
  (logit ")"))

(defmethod unparse ((style (eql :psilisp)) indent (self application))
  (logit "(")
  (unparse style indent (op self))
  (when (args self)
    (logit " ")
    (unparse style indent (args self)))
  (logit ")"))

;; ---------
;; Unparse style: :ast
;; ---------

(defmethod unparse ((style (eql :ast)) indent (self (eql nil)))
  nil)

(defmethod unparse ((style (eql :ast)) indent (self symbol))
  ;; A way for me to indicate TODO areas in the AST.
  (logiti indent "~S~%" self))

(defmethod unparse ((style (eql :ast)) indent (self toplevel))
  (logit ";;; AST DUMP~%")
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (body self)))

(defmethod unparse ((style (eql :ast)) indent (self body))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (defs self))
  (unparse style (1+ indent) (exprs self)))

(defmethod unparse ((style (eql :ast)) indent (self defs))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (def self))
  (unparse style indent (more self)))

(defmethod unparse ((style (eql :ast)) indent (self exprs))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (expr self))
  (unparse style indent (more self)))

(defmethod unparse ((style (eql :ast)) indent (self vardecls))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (vardecl self))
  (unparse style indent (more self)))

(defmethod unparse ((style (eql :ast)) indent (self vardecl))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (var self)))

(defmethod unparse ((style (eql :ast)) indent (self var))
  (logiti indent "; ~A | ~A~%" (sym self) self))

(defmethod unparse ((style (eql :ast)) indent (self literal-fixnum))
  (logiti indent "; ~S | ~A~%" (value self) self))

(defmethod unparse ((style (eql :ast)) indent (self literal-char))
  (logiti indent "; ~S | ~A~%" (value self) self))

(defmethod unparse ((style (eql :ast)) indent (self literal-bool))
  (logiti indent "; ~S | ~A~%" (value self) self))

(defmethod unparse ((style (eql :ast)) indent (self literal-null))
  (logiti indent "; ~A~%" self))

(defmethod unparse ((style (eql :ast)) indent (self prim))
  (logiti indent "; prim 0-arg | ~A~%" self)
  (unparse style (1+ indent) (op self)))

(defmethod unparse ((style (eql :ast)) indent (self prim-unary))
  (logiti indent "; prim 1-arg | ~A~%" self)
  (unparse style (1+ indent) (op self))
  (unparse style (1+ indent) (arg0 self)))

(defmethod unparse ((style (eql :ast)) indent (self prim-binary))
  (logiti indent "; prim 2-arg | ~A~%" self)
  (unparse style (1+ indent) (op self))
  (unparse style (1+ indent) (arg0 self))
  (unparse style (1+ indent) (arg1 self)))

(defmethod unparse ((style (eql :ast)) indent (self define-syntax))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (vardecl self))
  (unparse style (1+ indent) (value self)))

(defmethod unparse ((style (eql :ast)) indent (self lambda-syntax))
  (logiti indent "; ~A~%" self)
  (unparse style (+ indent 2) (vardecls self))
  (unparse style (1+ indent) (body self)))

(defmethod unparse ((style (eql :ast)) indent (self let-binding))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (vardecl self))
  (unparse style (1+ indent) (value self)))

(defmethod unparse ((style (eql :ast)) indent (self let-bindings))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (binding self))
  (when (more self)
    (unparse style indent (more self))))

(defmethod unparse ((style (eql :ast)) indent (self let-syntax))
  (logiti indent "; ~A~%" self)
  (unparse style (+ indent 2) (bindings self))
  (unparse style (1+ indent) (body self)))

(defmethod unparse ((style (eql :ast)) indent (self letrec-binding))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (vardecl self))
  (unparse style (1+ indent) (value self)))

(defmethod unparse ((style (eql :ast)) indent (self letrec-bindings))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (binding self))
  (when (more self)
    (unparse style indent (more self))))

(defmethod unparse ((style (eql :ast)) indent (self letrec-syntax))
  (logiti indent "; ~A~%" self)
  (unparse style (+ indent 2) (bindings self))
  (unparse style (1+ indent) (body self)))

(defmethod unparse ((style (eql :ast)) indent (self set!-syntax))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (var self))
  (unparse style (1+ indent) (value self)))

(defmethod unparse ((style (eql :ast)) indent (self if-syntax))
  (logiti indent "; ~A~%" self)
  (logiti indent "; CHOICE~%")
  (unparse style (1+ indent) (choice self))
  (logiti indent "; CONSEQUENT~%")
  (unparse style (1+ indent) (consequent self))
  (when (alternate self)
    (logiti indent "; ALTERNATE~%")
    (unparse style (1+ indent) (alternate self))))

(defmethod unparse ((style (eql :ast)) indent (self begin-syntax))
  (logiti indent "; ~A~%" self)
  (unparse style (1+ indent) (body self)))

(defmethod unparse ((style (eql :ast)) indent (self application))
  (logiti indent "; ~A~%" self)
  (logiti indent "; FUNC~%")
  (unparse style (1+ indent) (op self))
  (when (args self)
    (logiti indent "; ARGS~%")
    (unparse style (1+ indent) (args self))))


;; -----------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------

(defun collect-prefix (prefixp lst &key (key #'identity))
  "Return two values. The first value is the maximal prefix of the LST when
each item from left to right is tested with PREFIXP. The second value is the
cdr of the list which includes the first item that failed the PREFIXP test. The
second list MAY contain items that PREFIX would succeed on--but wouldn't be
part of the prefix Accepts keyword: :KEY which supplies a key function into the
item and defaults to IDENTITY."
  (labels ((collect (lst accum)
             (let* ((entry (car lst))
                    (item (funcall key entry)))
               (if (funcall prefixp item)
                   (collect (cdr lst) (cons entry accum))
                   (values (reverse accum) lst)))))
    (collect lst nil)))


;; -----------------------------------------------------------------------
;; Literals
;; -----------------------------------------------------------------------

;; TODO: Some hardware decisions are made too early.
(base:defconstants
  +word-size+ 8 ;; bytes

  +fx-mask+ #x03 ;; the bits that constitute the tag
  +fx-tag+ #x00 ;; lower 2 zero bits are the tag itself.
  +fx-shift+ 2

  +bool-mask+ #xbf ;; the bits that constitute the tag
  +bool-f+ #x2f
  +bool-tag+ +bool-f+ ;; identifies both true and false objects
  +bool-t+ #x6f ;; bit pattern includes +bool-f+ !
  +bool-bit+ 6 ;; bit position of the discriminator bit in bool-t/f

  +char-mask+ #xff
  +char-tag+ #x0f
  +char-shift+ 8 ;; bits

  +null-mask+ #x3f
  +null+ +null-mask+

  )

;; NOTE: Fixnums have to know a little bit about the underlying hardware.
;; TODO: Abstract that hw knowledge away later.
(defun fixnum-bits () (- (* +word-size+ 8) +fx-shift+))
(defun fxlower () (- (expt 2 (- (fixnum-bits) 1))))
(defun fxupper () (1- (expt 2 (- (fixnum-bits) 1))))

(defun bool-true-obj () 'true)
(defun bool-false-obj () 'false)
(defun bool-true-p (x) (eq x (bool-true-obj)))
(defun bool-false-p (x) (eq x (bool-false-obj)))

(defun null-obj () 'null)

;; -----------------------------------------------------------------------
;; Accessors for core forms
;; -----------------------------------------------------------------------

;; ----
;; The basic original source form accessors and detectors
;; ----

(defun fixnum-p (x)  (and (integerp x) (<= (fxlower) x (fxupper))))
(defun char-p (x) (characterp x)) ;; lift the CL definition for now.
(defun bool-p (x) (or (bool-true-p x) (bool-false-p x)))
(defun null-p (x) (eq x (null-obj)))
(defun immediate-p (expr)
  (or (fixnum-p expr)
      (bool-p expr)
      (char-p expr)
      (null-p expr)))

(defun define-p (env expr)
  (syntax-form-p env expr 'define))
(defun define-var (expr)
  (second expr))
(defun define-expr (expr)
  (third expr))

(defun var-p (expr)  ;; TODO: Carefully consider semantics.
  (symbolp expr))

(defun lambda-p (env expr)
  (syntax-form-p env expr 'lambda))
(defun lambda-formals (expr)
  (second expr))
(defun lambda-body (expr)
  (cddr expr))

(defun let-p (env expr)
  (syntax-form-p env expr 'let))
(defun let-bindings (expr)
  (second expr))
(defun let-binding-var (binding)
  (first binding))
(defun let-binding-value (binding)
  (second binding))
(defun let-body (expr)
  (cddr expr))

(defun letrec-p (env expr)
  (syntax-form-p env expr 'letrec))
(defun letrec-bindings (expr)
  (second expr))
(defun letrec-binding-var (binding)
  (first binding))
(defun letrec-binding-value (binding)
  (second binding))
(defun letrec-body (expr)
  (cddr expr))

(defun set!-p (env expr)
  (syntax-form-p env expr 'set!))
(defun set!-var (expr)
  (second expr))
(defun set!-expr (expr)
  (third expr))

(defun if-p (env expr)
  (syntax-form-p env expr 'if))
(defun if-condition (expr)
  (second expr))
(defun if-conseq (expr)
  (third expr))
(defun if-altern (expr)
  (fourth expr))

(defun begin-p (env expr)
  (syntax-form-p env expr 'begin))
(defun begin-body (expr)
  (cdr expr))

(defun primitive-p (env expr)
  (when (listp expr)
    (prim-form-p env expr (car expr))))
(defun primitive-op (expr)
  (first expr))
(defun primitive-args (expr)
  (cdr expr))
(defun primitive-arg0 (expr)
  (second expr))
(defun primitive-arg1 (expr)
  (third expr))


;; -----------------------------------------------------------------------
;; Pass: Convert original source s-exp form to AST form.  Due to LISP's natural
;; behavior with symbol use, we do name resolution on symbols in order to
;; understand how to parse the forms we encounter.
;; -----------------------------------------------------------------------

;; literals
(defun pass/src->ast%literal-fixnum (env expr)
  (declare (ignore env))
  (make-literal-fixnum expr))

(defun pass/src->ast%literal-char (env expr)
  (declare (ignore env))
  (make-literal-char expr))

(defun pass/src->ast%literal-bool (env expr)
  (declare (ignore env))
  (make-literal-bool expr))

(defun pass/src->ast%literal-null (env expr)
  (declare (ignore env expr))
  (make-literal-null))

(defun pass/src->ast%var (env expr)
  (declare (ignore env))
  ;; So far, we'll let the syntax processor determine if
  ;; it needs to make a syment instead of doing it here.
  ;; TODO: Might revisit later...
  (make-var expr))

(defun pass/src->ast%vardecl (env expr)
  (make-vardecl (pass/src->ast%var env expr)))

(defun pass/src->ast%vardecls (env expr)
  (when expr
    (make-vardecls (pass/src->ast%vardecl env (car expr))
		   (pass/src->ast%vardecls env (cdr expr)))))

(defun pass/src->ast%primitive (env expr)
  (let ((op (primitive-op expr))
        (len (length (primitive-args expr))))
    (cond
      ((= len 0)
       (make-prim (pass/src->ast%var env op)))
      ((= len 1)
       (make-prim-unary (pass/src->ast%var env op)
                        (pass/src->ast%expr env (primitive-arg0 expr))))
      ((= len 2)
       (make-prim-binary (pass/src->ast%var env op)
                         (pass/src->ast%expr env (primitive-arg0 expr))
                         (pass/src->ast%expr env (primitive-arg1 expr))))
      (t
       (error "pass/src->ast%primitive: Too many args: ~S" expr)))))

;; TODO: Not used yet.
(defun pass/src->ast%define-syntax (env expr)
  (let ((var (define-var expr))
        (value-expr (define-expr expr)))

    (make-define-syntax (pass/src->ast%vardecl env var)
                        (pass/src->ast%expr env value-expr))))

(defun pass/src->ast%lambda-syntax (env expr)
  (let ((formals (lambda-formals expr))
        (body (lambda-body expr)))

    (base:open-scope env)
    (loop :for formal :in formals
          :do (base:add-definition env formal (make-syment/local)))

    (let ((lambda-node (make-lambda-syntax
                        (pass/src->ast%vardecls env formals)
                        (pass/src->ast%body env body))))
      (base:close-scope env)
      lambda-node)))

(defun pass/src->ast%let-binding (env expr)
  (let ((var-form (let-binding-var expr))
        (value-form (let-binding-value expr)))
    (make-let-binding (pass/src->ast%vardecl env var-form)
                      (pass/src->ast%expr env value-form))))

(defun pass/src->ast%let-bindings (env expr)
  (when expr
    (make-let-bindings
     (pass/src->ast%let-binding env (car expr))
     (pass/src->ast%let-bindings env (cdr expr)))))

(defun pass/src->ast%let-syntax (env expr)
  (let* ((bindings-form (let-bindings expr))
         (body-form (let-body expr))
         ;; convert the bindings to their AST form, using the current env.
         (bindings (pass/src->ast%let-bindings env bindings-form)))

    ;; Now augment the environment for the body.
    (base:open-scope env)
    (loop :for var :in (mapcar #'first bindings-form)
          :do (base:add-definition env var (make-syment/local)))

    (let ((let-node (make-let-syntax bindings
                                     (pass/src->ast%body env body-form))))
      (base:close-scope env)
      let-node)))

(defun pass/src->ast%letrec-binding (env expr)
  (let ((var-form (letrec-binding-var expr))
        (value-form (letrec-binding-value expr)))
    (make-letrec-binding (pass/src->ast%vardecl env var-form)
                         (pass/src->ast%expr env value-form))))

(defun pass/src->ast%letrec-bindings (env expr)
  (when expr
    (make-letrec-bindings
     (pass/src->ast%letrec-binding env (car expr))
     (pass/src->ast%letrec-bindings env (cdr expr)))))

(defun pass/src->ast%letrec-syntax (env expr)
  (let* ((bindings-form (letrec-bindings expr))
         (body-form (letrec-body expr)))

    ;; Now augment the environment for the value expressions so they see
    ;; all the variables in question.
    (base:open-scope env)
    (loop :for var :in (mapcar #'first bindings-form)
          :do (base:add-definition env var (make-syment/local)))

    (let ((letrec-node
            (make-letrec-syntax
             ;; Now all binding values are processed in the scope of the
             ;; variables being bound.
             (pass/src->ast%letrec-bindings env bindings-form)
             (pass/src->ast%body env body-form))))
      (base:close-scope env)
      letrec-node)))

(defun pass/src->ast%set!-syntax (env expr)
  (let ((var (set!-var expr))
        (value-expr (set!-expr expr)))

    ;; TODO: Prolly do some chicanery with env here for global vars if the
    ;; variable is not otherwise defined that we're trying to set!.
    (make-set!-syntax (pass/src->ast%var env var)
                      (pass/src->ast%expr env value-expr))))

(defun pass/src->ast%if-syntax (env expr)
  (make-if-syntax (pass/src->ast%expr env (if-condition expr))
                  (pass/src->ast%expr env (if-conseq expr))
                  (if (if-altern expr)
                      (pass/src->ast%expr env (if-altern expr))
                      (make-literal-null))))

(defun pass/src->ast%begin-syntax (env expr)
  (make-begin-syntax (pass/src->ast%body env (begin-body expr))))

(defun pass/src->ast%application (env expr)
  (make-application (pass/src->ast%expr env (car expr))
                    (pass/src->ast%exprs env (cdr expr))))

(defun pass/src->ast%expr (env expr)
  (cond
    ;; All the atomic like things so far.

    ;; NULL, TRUE, and FALSE can never be rebound or used as a variable.
    ;;
    ;; TODO: Enforce/typecheck this rule in LET, LAMBDA, etc

    ((null-p expr)
     (pass/src->ast%literal-null env expr))
    ((bool-p expr)
     (pass/src->ast%literal-bool env expr))

    ((fixnum-p expr)
     (pass/src->ast%literal-fixnum env expr))
    ((char-p expr)
     (pass/src->ast%literal-char env expr))

    ;; Variable reference...
    ((var-p expr)
     ;; TODO: DO something with the env here, like check to see if the
     ;; variable is defined, etc, etc, etc.
     (pass/src->ast%var env expr))



    ;; anything in a list form: syntax, primitive, application
    ((listp expr)
     (cond
       ;; syntax parsing
       ((lambda-p env expr)
        (pass/src->ast%lambda-syntax env expr))
       ((define-p env expr)
        (pass/src->ast%define-syntax env expr))
       ((let-p env expr)
        (pass/src->ast%let-syntax env expr))
       ((letrec-p env expr)
        (pass/src->ast%letrec-syntax env expr))
       ((set!-p env expr)
        (pass/src->ast%set!-syntax env expr))
       ((if-p env expr)
        (pass/src->ast%if-syntax env expr))
       ((begin-p env expr)
        (pass/src->ast%begin-syntax env expr))

       ;; primitive parsing
       ((primitive-p env expr)
        (pass/src->ast%primitive env expr))

       ;; an application is only thing left
       (t
        (pass/src->ast%application env expr))))
    (t
     (error "pass/src->ast%expr: Can't parse: ~S" expr))))

(defun pass/src->ast%exprs (env exprs)
  (when exprs
    (make-exprs (pass/src->ast%expr env (car exprs))
                (pass/src->ast%exprs env (cdr exprs)))))


;; TODO: Broken.
(defun pass/src->ast%defs (env defs)
  (when defs
    (make-defs (pass/src->ast%define-syntax env (car defs))
               (pass/src->ast%defs env (cdr defs)))))

(defun pass/src->ast%body (env exprs)
  (make-body
   ;; TODO: support DEFINE regions later. It is hard because the defines can
   ;; redefine their own symbolic form and checking for that is hairy
   ;; and I don't want to do it right now.
   nil
   (pass/src->ast%exprs env exprs)))

(defun pass/src->ast%toplevel (env exprs)
  (make-toplevel (pass/src->ast%body env exprs)))

;; Main entry. Convert one toplevel of an incoming source to AST.
(defun pass/src->ast (env exprs)
  (pass/src->ast%toplevel env exprs))

;; -----------------------------------------------------------------------
;; Test compilation of a single toplevel set of forms.
;; -----------------------------------------------------------------------

(defun init-global-environment ()
  (let ((env (base:make-blsymtab))
        (syntax-symbols
          '(define if begin lambda let letrec set! true false null))
        (prim-symbols
          '(fxadd1 fxsub1
            fixnum->char char->fixnum
            fxzero? fixnum? null? char? boolean?
            not
            fxlognot
            fx+ fx- fx* fx/ fxlogand fxlogor
            fx= fx< fx<= fx> fx>=
            char= char< char<= char> char>=)))

    ;; Construct initial environment
    (base:open-scope env)
    (loop :for sym :in syntax-symbols
          :do (base:add-definition env sym (make-syment/syntax)))
    (loop :for sym :in prim-symbols
          :do (base:add-definition env sym (make-syment/prim)))

    ;; global scope left open!
    env))

(defun c1 (unparse-style top-forms)
  (let ((env (init-global-environment))) ;; a global scope is left open....

    (let* ((ast (pass/src->ast env top-forms)))

      (unparse unparse-style 0 ast)

      (base:close-scope env)

      ast)))
