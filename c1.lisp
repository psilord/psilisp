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
;; <defs> ::= <define> <defs>
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

;; The second form of the language after closure conversion is the above, but
;; <lambda> has been replaced with <closure> and <clambda> for the closed
;; functions that contain no free variables.
;;
;; <closed-vars> ::= <var> <closed-vars>
;;            |
;; <closure> ::= (closure (<closed-vars>) <clambda>)
;;
;; The <vardecls> here contains a new argument to represent the closure and it
;; is spread through the body as appropriate.
;; <clambda> ::= (clambda (<vardecls>) <body>)
;;           | <var>

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
   (%bound-p :accessor bound-p :initarg :bound-p :initform nil)

   ;; Squshy stuff below...

   ;; Is this syment for a closed variable?
   (%closed-p :accessor closed-p :initarg :closed-p :initform nil)
   ;; a reference to the syment which was closed over.
   (%closed-syment :accessor closed-syment :initarg :closed-syment
                   :initform nil)

   ;; Does this syment represent a closure record/environment that contains its
   ;; own symbol table of closure variable syments. The key of the closure
   ;; symtab  is the variable id, the value is a syment record which ultimately
   ;; contains a reference to the symtab entry of what it closed over.
   (%closure-p :accessor closure-p :initarg :closure-p :initform nil)
   ;; symtab of free var ids to closed syment entries.
   (%closure-symtab :accessor closure-symtab :initarg :closure-symtab
                    :initform nil)

   ))
(flexible-constructor syment)

;; We'll see if I like this for now...
(defmethod print-object ((obj syment) str)
  #++(call-next-method)
  (print-unreadable-object (obj str :type nil :identity t)
    (format str
            ;; Legend:
            ;; g/G - not global / global
            ;; m/M - not mutable / mutable
            ;; u/U - not mutated / mutated
            ;; p/P - not primitive / primitive
            ;; s/S - not syntax / syntax
            ;; b/B - not bound / bound
            "SYM ~:[g~;G~]~:[m~;M~]~:[u~;U~]~:[s~;S~]~:[p~;P~]~:[b~;B~]"
            (global-p obj)
            (mutable-p obj)
            (mutated-p obj)
            (syntax-p obj)
            (prim-p obj)
            (bound-p obj))))

(defun make-syment/global (&key (bound-p nil))
  (make-syment :global-p t :mutable-p t
               :mutated-p nil :syntax-p nil
               :prim-p nil :bound-p bound-p))

(defun make-syment/local (&key (bound-p nil))
  (make-syment :global-p nil :mutable-p t
               :mutated-p nil :syntax-p nil
               :prim-p nil :bound-p bound-p))

(defun make-syment/syntax ()
  (make-syment :global-p t :mutable-p nil
               :mutated-p nil :syntax-p t
               :prim-p nil :bound-p t))

(defun make-syment/prim ()
  (make-syment :global-p t :mutable-p nil
               :mutated-p nil :syntax-p nil
               :prim-p t :bound-p t))

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
    (a:when-let ((syment
                  (env:find-definition env :var datum :scope scope)))
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

(defgeneric seq-iter (seq-obj func))
(defgeneric unparse (style indent node)) ;; unparse back into psilisp or other

;; mixin for sequences like vardecls, etc.
(defclass seq ()
  ((%more :accessor more :initarg :more)))

;; mixin to represent free variables
(defclass free-vars ()
  ;; a list of free vars (currently a simple list of conses of symbol names
  ;; and syment entries).
  ;; TODO: Spruce this up to be more formal in the AST.
  ((%free-vars :accessor free-vars :initarg :free-vars :initform nil)))

;; mixin for anything that is also a binding form and causes a symbol table
;; scope to be opened or otherwise needs a binding-form.
(defclass binding-form ()
  ((%symtab :accessor symtab :initarg :symtab :type st:symtab)))

;; The AST representation.

(defclass ast () ())

(defclass toplevel (ast binding-form)
  ((%body :accessor body :initarg :body :type body)))
(simple-constructor toplevel body symtab)

(defclass body (ast)
  ((%defs :accessor defs :initarg :defs :type defs)
   (%exprs :accessor exprs :initarg :exprs :type exprs)))
(simple-constructor body defs exprs)

(defclass defs (ast seq)
  ((%def :accessor def :initarg :def :type def)
   (%more :accessor more :initarg :more :type defs)))
(simple-constructor defs def more)

;; TODO: Figure this out.
(defclass def (ast) ())

(defclass exprs (ast seq)
  ((%expr :accessor expr :initarg :expr :type expr)
   (%more :accessor more :initarg :more :type exprs)))
(simple-constructor exprs expr more)

(defclass expr (ast) ())

(defclass vardecls (ast seq)
  ((%vardecl :accessor vardecl :initarg :vardecl :type vardecl)
   (more :accessor more :initarg :more :type vardecls)))
(simple-constructor vardecls vardecl more)

;; If I add more types of vardecls, like keyword, rest, etc, subclass this.
(defclass vardecl (ast)
  ((%var :accessor var :initarg :var :type var)))
(simple-constructor vardecl var)

(defclass var (expr)
  ((%id :accessor id :initarg :id :type symbol)
   (%syment :accessor syment :initarg :syment :type syment)))
(simple-constructor var id syment)

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

(defclass lambda-syntax (syntax free-vars binding-form)
  ((%vardecls :accessor vardecls :initarg :vardecls :type vardecls)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor lambda-syntax vardecls body symtab)

(defclass let-syntax (syntax free-vars binding-form)
  ((%bindings :accessor bindings :initarg :bindings :type let-bindings)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor let-syntax bindings body symtab)

(defclass let-bindings (syntax seq)
  ((%binding :accessor binding :initarg :binding :type let-binding)
   (%more :accessor more :initarg :more :type let-bindings)))
(simple-constructor let-bindings binding more)

(defclass let-binding (syntax)
  ((%vardecl :accessor vardecl :initarg :vardecl :type vardecl)
   (%value :accessor value :initarg :value :type expr)))
(simple-constructor let-binding vardecl value)

(defclass letrec-syntax (syntax free-vars binding-form)
  ((%bindings :accessor bindings :initarg :bindings :type letrec-bindings)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor letrec-syntax bindings body symtab)

(defclass letrec-bindings (syntax seq)
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

;; TODO: Add a desugared slot that indicates if it came from a LET or a
;; LETREC so we can undo it for better unparsing/debugging
(defclass application (expr)
  ((%op :accessor op :initarg :op :type expr)
   (%args :accessor args :initarg :args :type exprs)))
(simple-constructor application op args)

;;---- closure conversion augmentation of the above IR

;; TODO: This stuff is not at all done yet.

;; Variables known to be free that must be closed over.  Specifically, this
;; means their value (think tagged pointer) has been copied only.
(defclass closed-vars (ast seq)
  ((%var :accessor var :initarg :var :type var)
   (%more :accessor more :initarg :more :type closed-vars)))
(simple-constructor closed-vars var more)

;; A closed function: no free variables, possibly using a new closure record
;; formal too. All references to any free variables have been rewritten into
;; references out of the closure record.
(defclass clambda (expr binding-form)
  ((%vardecls :accessor vardecls :initarg :vardecls :type vardecls)
   (%body :accessor body :initarg :body :type body)))
(simple-constructor clambda vardecls body symtab)

(defclass closure (expr binding-form)
  ((%closed-vars :accessor closed-vars :initarg :closed-vars :type closed-vars)
   ;; An explicit description of the generated closure variable we're using to
   ;; represent the closure environment. It will be the only variable in the
   ;; symtab associated with this closure object (which of course contains a
   ;; symtab inside of it for each of the slots in the closure).
   (%cvar :accessor cvar :initarg :cvar :type var)
   (%clambda :accessor clambda :initarg :clambda :type (or clambda var))))
(simple-constructor closure closed-vars cvar clambda symtab)

;; -----------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------

(defun logit (fmt &rest args)
  (apply #'format t fmt args))

(defun logiti (indent fmt &rest args)
  (apply #'format t (concatenate 'string "~A" fmt)
         (make-string indent :initial-element #\Space)
         args))

(defun loglvl (lvl fmt &rest args)
  (let ((prefix (ecase lvl
                  (:error "ERROR: ")
                  (:warn "WARNING: ")
                  (:debug "DEBUG: ")
                  (:info "INFO: "))))
    (apply #'logit (concatenate 'string prefix fmt) args)))

(defmethod seq-iter (seq-obj func))

(defmethod seq-iter ((seq-obj seq) func)
  (funcall func seq-obj)
  (when (more seq-obj)
    (seq-iter (more seq-obj) func)))

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
;; Unparsing
;; -----------------------------------------------------------------------



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
  (logit "~A" (id self)))

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
  (logiti indent "; ~A | ~A @ ~A~%" (id self) self (syment self)))

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
  (logiti indent "; FV: ~A~%" (free-vars self))
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
  (logiti indent "; FV: ~A~%" (free-vars self))
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
  (logiti indent "; FV: ~A~%" (free-vars self))
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
  (logiti indent "; OP-EXPR~%")
  (unparse style (1+ indent) (op self))
  (when (args self)
    (logiti indent "; ARGS~%")
    (unparse style (1+ indent) (args self))))




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

;; TODO: Carefully consider semantics. This means that symbols are bleeding
;; from the host language of CL into psilisp.
(defun var-p (expr)
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
(defun let-binding-vars (bindings)
  (mapcar #'let-binding-var bindings))
(defun let-binding-value (binding)
  (second binding))
(defun let-binding-values (bindings)
  (mapcar #'let-binding-value bindings))
(defun let-body (expr)
  (cddr expr))

(defun letrec-p (env expr)
  (syntax-form-p env expr 'letrec))
(defun letrec-bindings (expr)
  (second expr))
(defun letrec-binding-var (binding)
  (first binding))
(defun letrec-binding-vars (bindings)
  (mapcar #'letrec-binding-var bindings))
(defun letrec-binding-value (binding)
  (second binding))
(defun letrec-binding-values (bindings)
  (mapcar #'letrec-binding-value bindings))
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
(defun pass/alphatization%literal-fixnum (env expr)
  (declare (ignore env))
  (make-literal-fixnum expr))

(defun pass/alphatization%literal-char (env expr)
  (declare (ignore env))
  (make-literal-char expr))

(defun pass/alphatization%literal-bool (env expr)
  (declare (ignore env))
  (make-literal-bool expr))

(defun pass/alphatization%literal-null (env expr)
  (declare (ignore env expr))
  (make-literal-null))

(defun pass/alphatization%var (env expr)
  (declare (ignore env))
  ;; Note: We let whomever created this var set up and fill in the syment
  ;; associated with it since it knows the context of use, like a decl or a
  ;; rederence.
  (make-var expr nil))

(defun pass/alphatization%vardecl (env expr)
  (make-vardecl (pass/alphatization%var env expr)))

(defun pass/alphatization%vardecls (env expr)
  (when expr
    (make-vardecls (pass/alphatization%vardecl env (car expr))
                   (pass/alphatization%vardecls env (cdr expr)))))

(defun pass/alphatization%primitive (env expr)
  (let ((op-var (pass/alphatization%var env (primitive-op expr)))
        (len (length (primitive-args expr))))

    ;; Known reference to a primitive symbol.
    (setf (syment op-var)
          (env:find-definition env :var (id op-var) :scope :any))

    (cond
      ((= len 0)
       (make-prim op-var))
      ((= len 1)
       (make-prim-unary op-var
                        (pass/alphatization%expr env (primitive-arg0 expr))))
      ((= len 2)
       (make-prim-binary op-var
                         (pass/alphatization%expr env (primitive-arg0 expr))
                         (pass/alphatization%expr env (primitive-arg1 expr))))
      (t
       (error "pass/alphatization%primitive: Too many args: ~S" expr)))))


;; TODO: Not used yet.
(defun pass/alphatization%define-syntax (env expr)
  (let ((var (define-var expr))
        (value-expr (define-expr expr)))

    (make-define-syntax (pass/alphatization%vardecl env var)
                        (pass/alphatization%expr env value-expr))))

(defun pass/alphatization%lambda-syntax (env expr)
  (let ((formals (lambda-formals expr))
        (body (lambda-body expr))
        ;; record the symbol table scope we're opening in case we need to
        ;; explicitly muck with it later.
        (lambda-symtab (env:open-scope env :var)))

    (let ((vardecls (pass/alphatization%vardecls env formals))) ;; process formals
      ;; put them into the scope.
      (seq-iter vardecls
                (lambda (vdcls)
                  (let* ((var (var (vardecl vdcls)))
                         (id (id var))
                         (exists (env:find-definition env :var id)))
                    (when exists
                      ;; TODO: Handle these errors better or in a different
                      ;; pass
                      (error "lambda formal ~A defined more than once." id))

                    (let ((syment (make-syment/local :bound-p t)))
                      (env:add-definition env :var id syment)
                      (setf (syment var) syment)))))

      ;; process body in the new scope.
      (let ((lambda-node
              (make-lambda-syntax vardecls
                                  (pass/alphatization%body env body)
                                  lambda-symtab)))
        (env:close-scope env :var)
        lambda-node))))

(defun pass/alphatization%let-binding (env expr)
  (let ((var-form (let-binding-var expr))
        (value-form (let-binding-value expr)))
    (make-let-binding (pass/alphatization%vardecl env var-form)
                      (pass/alphatization%expr env value-form))))

(defun pass/alphatization%let-bindings (env expr)
  (when expr
    (make-let-bindings
     (pass/alphatization%let-binding env (car expr))
     (pass/alphatization%let-bindings env (cdr expr)))))

(defun pass/alphatization%let-syntax (env expr)
  (let* ((bindings-form (let-bindings expr))
         (body-form (let-body expr))
         ;; Firt convert the bindings to their AST form,
         ;; ensuring the value expressions are using the current env.
         (bindings (pass/alphatization%let-bindings env bindings-form))
         ;; record the symbol table scope we're opening in case we need to
         ;; explicitly muck with it later.
         (symtab (env:open-scope env :var)))

    ;; Now augment the environment for the body with the new variables.
    (seq-iter bindings
              (lambda (bdngs)
                (let* ((var (var (vardecl (binding bdngs))))
                       (id (id var))
                       (exists (env:find-definition env :var id)))
                  (when exists
                    ;; TODO: Handle these errors better or in a different
                    ;; pass
                    (error "LET: ~A defined more than once." id))

                  (let ((syment (make-syment/local :bound-p t)))
                    (env:add-definition env :var id syment)
                    (setf (syment var) syment)))))

    ;; Translate the body with the new vars in scope.
    (let ((let-node (make-let-syntax bindings
                                     (pass/alphatization%body env body-form)
                                     symtab)))
      (env:close-scope env :var)
      let-node)))

(defun pass/alphatization%letrec-binding (env expr)
  (let ((var-form (letrec-binding-var expr))
        (value-form (letrec-binding-value expr)))
    (make-letrec-binding (pass/alphatization%vardecl env var-form)
                         (pass/alphatization%expr env value-form))))

(defun pass/alphatization%letrec-bindings (env expr)
  (when expr
    (make-letrec-bindings
     (pass/alphatization%letrec-binding env (car expr))
     (pass/alphatization%letrec-bindings env (cdr expr)))))

(defun pass/alphatization%letrec-syntax (env expr)
  (let* ((bindings-form (letrec-bindings expr))
         (binding-vars (letrec-binding-vars bindings-form))
         (body-form (letrec-body expr))
         (symtab (env:open-scope env :var)))

    ;; First, we find and insert all declaring variables into the symbol table.
    (loop :for var :in binding-vars
          :for syment = (make-syment/local :bound-p t) ;; bound-p seems wrong.
          :do (setf (mutated-p syment) t) ;; have to mutate binding!
              ;; TODO: observe multiple definition error.
              (env:add-definition env :var var syment))

    ;; Then we process the bindings within this new scope.
    (let ((bindings (pass/alphatization%letrec-bindings env bindings-form)))
      ;; Now, we must fixup the bindings vardecl to point to the right thing.
      (seq-iter bindings
                (lambda (bndgs)
                  (let* ((var (var (vardecl (binding bndgs))))
                         (id (id var))
                         (syment (env:find-definition env :var id)))
                    (setf (syment var) syment))))

      (let ((letrec-node
              (make-letrec-syntax
               ;; Now all binding values are processed in the scope of the
               ;; variables being bound.
               bindings
               (pass/alphatization%body env body-form)
               symtab)))
        (env:close-scope env :var)
        letrec-node))))

(defun pass/alphatization%set!-syntax (env expr)
  (let ((var (pass/alphatization%var env (set!-var expr)))
        (value (pass/alphatization%expr env (set!-expr expr))))

    ;; poke in a global variable if nothing in local scope.
    ;; TODO: We need to tell the user the best thing if we're going to
    ;; create a global variable or not automatically. Maybe it was a typo?
    ;; Maybe it was intentional? Figure out the best thing to state to the
    ;; user, if anything.
    (let* ((syment (env:find-definition env :var (id var) :scope :any))
           (syment (if syment syment (make-syment/global))))

      (unless (mutable-p syment)
        (error "ERROR: Variable ~A is not mutable via SET!" (id var)))

      ;; NOTE: This automatically performs a "mutable pass" via the symbol
      ;; table references.
      (setf (mutated-p syment) t
            (bound-p syment) t
            (syment var) syment))

    (make-set!-syntax var value)))

(defun pass/alphatization%if-syntax (env expr)
  (make-if-syntax (pass/alphatization%expr env (if-condition expr))
                  (pass/alphatization%expr env (if-conseq expr))
                  ;; TODO: Figure out good default for no alternate, it has a
                  ;; type inference ramification too, etc, etc.
                  (if (if-altern expr)
                      (pass/alphatization%expr env (if-altern expr))
                      (make-literal-null))))

(defun pass/alphatization%begin-syntax (env expr)
  (make-begin-syntax (pass/alphatization%body env (begin-body expr))))

(defun pass/alphatization%application (env expr)
  (make-application (pass/alphatization%expr env (car expr))
                    (pass/alphatization%exprs env (cdr expr))))

(defun pass/alphatization%expr (env expr)
  (cond
    ;; All the atomic like things so far.

    ;; NULL, TRUE, and FALSE can never be rebound or used as a variable.
    ;;
    ;; TODO: Enforce/typecheck this rule in LET, LAMBDA, etc

    ((null-p expr)
     (pass/alphatization%literal-null env expr))
    ((bool-p expr)
     (pass/alphatization%literal-bool env expr))

    ((fixnum-p expr)
     (pass/alphatization%literal-fixnum env expr))
    ((char-p expr)
     (pass/alphatization%literal-char env expr))

    ;; Variable reference...
    ((var-p expr)
     (let ((v (pass/alphatization%var env expr)))
       (let ((syment (env:find-definition env :var (id v) :scope :any)))
         (unless syment
           (loglvl :error
                   "ERROR: Variable reference ~A is undefined.~%" (id v))
           ;; NOTE: We add a new unbound definition in response to the
           ;; unknown variable to which we refer.
           (let ((unbound-syment (make-syment/local)))
             (env:add-definition env :var (id v) unbound-syment)
             (setf syment unbound-syment)))

         ;; Now finally bind the variable to the symbol table entry.
         (setf (syment v) syment)
         v)))

    ;; anything in a list form: syntax, primitive, application
    ((listp expr)
     (cond
       ;; syntax parsing
       ((lambda-p env expr)
        (pass/alphatization%lambda-syntax env expr))
       ((define-p env expr)
        (pass/alphatization%define-syntax env expr))
       ((let-p env expr)
        (pass/alphatization%let-syntax env expr))
       ((letrec-p env expr)
        (pass/alphatization%letrec-syntax env expr))
       ((set!-p env expr)
        (pass/alphatization%set!-syntax env expr))
       ((if-p env expr)
        (pass/alphatization%if-syntax env expr))
       ((begin-p env expr)
        (pass/alphatization%begin-syntax env expr))

       ;; primitive parsing
       ((primitive-p env expr)
        (pass/alphatization%primitive env expr))

       ;; an application is only thing left
       (t
        (pass/alphatization%application env expr))))
    (t
     (error "pass/alphatization%expr: Can't parse: ~S" expr))))

(defun pass/alphatization%exprs (env exprs)
  (when exprs
    (make-exprs (pass/alphatization%expr env (car exprs))
                (pass/alphatization%exprs env (cdr exprs)))))


;; TODO: Broken.
(defun pass/alphatization%defs (env defs)
  (when defs
    (make-defs (pass/alphatization%define-syntax env (car defs))
               (pass/alphatization%defs env (cdr defs)))))

(defun pass/alphatization%body (env exprs)
  (make-body
   ;; TODO: support DEFINE regions later. It is hard because the defines can
   ;; redefine their own symbolic form and checking for that is hairy
   ;; and I don't want to do it right now.
   nil
   (pass/alphatization%exprs env exprs)))

(defun pass/alphatization%toplevel (env exprs)
  ;; Herein we define what symbols and symbol table entries are already
  ;; available in the toplevel.
  ;;
  ;; TODO: When we start combining toplevels in a sequential order, this might
  ;; need revisiting to define whatever behavior we desire.
  (let* ((syntax-symbols
           '(define if begin lambda let letrec set! true false null))
         (prim-symbols
           '(fxadd1 fxsub1
             fixnum->char char->fixnum
             fxzero? fixnum? null? char? boolean?
             not
             fxlognot
             fx+ fx- fx* fx/ fxlogand fxlogor
             fx= fx< fx<= fx> fx>=
             char= char< char<= char> char>=))
         (symtab (env:open-scope env :var))) ;; NOTE: leave this scope open!

    ;; Construct initial environment
    (loop :for sym :in syntax-symbols
          :do (env:add-definition env :var sym (make-syment/syntax)))
    (loop :for sym :in prim-symbols
          :do (env:add-definition env :var sym (make-syment/prim)))

    (let ((toplevel (make-toplevel (pass/alphatization%body env exprs)
                                   symtab)))
      ;; and finally we close the toplevel scope after processing all toplevel
      ;; forms.
      (env:close-scope env :var)

      toplevel)))

;; Main entry. Convert one toplevel of an incoming source to AST.
(defun pass/alphatization (env exprs)
  (pass/alphatization%toplevel env exprs))

;; -----------------------------------------------------------------------
;; Perform desugaring of code. I don't have a macro system in place yet
;; so this stands in place of that for handling early binding forms like:
;; LET, LETREC -> LAMBDA equivalents
;;
;; This makes closure conversion much easier to think about....
;;
;; Must I have a desugaring pass like this to make closure
;; conversion easier to reason about? Normally macros take care of this
;; but I don't have a macro pass yet. It also impacts debugging and
;; unparsing to get the original source as found back. I could desugar in
;; the alphatization phase easily, but doing an ast->ast transform would
;; make record-keeping/debugging info _possible to keep_ when the
;; desugaring happened.
;;
;; The following is ok since I don't _currently_ have CL-like specials.
;;
;; (let ((a A) .. (z Z)) body) ->
;;  ((lambda (a .. z) body) A .. Z)
;;
;; (letrec ((a A) .. (z Z)) body) ->
;;  ;; naive translation...see paper for a better one
;;  ((lambda (a .. z) (set! a A) ... (set! z Z) body) nil .. nil)
;; -----------------------------------------------------------------------

(defgeneric pass/desugar (node))

(defmethod pass/desugar ((self (eql nil)))
  self)

(defmethod pass/desugar ((self toplevel))
  (setf (body self) (pass/desugar (body self)))
  self)

(defmethod pass/desugar ((self body))
  (setf (defs self) (pass/desugar (defs self)))
  (setf (exprs self) (pass/desugar (exprs self)))
  self)

(defmethod pass/desugar ((self defs))
  (setf (def self) (pass/desugar (def self)))
  (when (more self)
    (setf (more self) (pass/desugar (more self))))
  self)

(defmethod pass/desugar ((self def))
  self)

(defmethod pass/desugar ((self exprs))
  (setf (expr self) (pass/desugar (expr self)))
  (when (more self)
    (setf (more self) (pass/desugar (more self))))
  self)

(defmethod pass/desugar ((self vardecls))
  (setf (vardecl self) (pass/desugar (vardecl self)))
  (when (more self)
    (setf (more self) (pass/desugar (more self))))
  self)

(defmethod pass/desugar ((self vardecl))
  (setf (var self) (pass/desugar (var self)))
  self)

(defmethod pass/desugar ((self var))
  self)

;; all literals are just themselves.
(defmethod pass/desugar ((self literal))
  self)

(defmethod pass/desugar ((self prim))
  (setf (op self) (pass/desugar (op self)))
  self)

(defmethod pass/desugar ((self prim-unary))
  (setf (op self) (pass/desugar (op self))
        (arg0 self) (pass/desugar (arg0 self)))
  self)

(defmethod pass/desugar ((self prim-binary))
  (setf (op self) (pass/desugar (op self))
        (arg0 self) (pass/desugar (arg0 self))
        (arg1 self) (pass/desugar (arg1 self)))
  self)

(defmethod pass/desugar ((self define-syntax))
  (setf (vardecl self) (pass/desugar (vardecl self))
        (value self) (pass/desugar (value self)))
  self)

(defmethod pass/desugar ((self lambda-syntax))
  (setf (vardecls self) (pass/desugar (vardecls self)))
  (setf (body self) (pass/desugar (body self)))
  self)

(defmethod pass/desugar ((self let-syntax))
  ;; Convert the LET form to a LAMBDA form.
  (let ((lambda-vardecl-list nil)
        (lambda-argexpr-list nil)
        (lambda-vardecls nil)
        (lambda-arg-exprs nil))
    (seq-iter (bindings self)
              (lambda (bindings-node)
                (let ((binding (binding bindings-node)))
                  (push (vardecl binding) lambda-vardecl-list)
                  (push (value binding) lambda-argexpr-list))))

    ;; Now convert the two lists (and reverse them) to AST appropriate seq
    ;; equivalents.
    (loop :for current-decl :in lambda-vardecl-list
          :for current-arg :in lambda-argexpr-list
          :do (setf
               lambda-vardecls (make-vardecls current-decl lambda-vardecls)
               lambda-arg-exprs (make-exprs current-arg lambda-arg-exprs)))

    ;; Finally, reconstruct the AST representation of the LET form as a
    ;; an application of the lambda form to its value arguments.
    (let ((lambda-node (make-lambda-syntax lambda-vardecls
                                           (pass/desugar (body self))
                                           (symtab self))))
      (setf (free-vars lambda-node) (free-vars self))

      (make-application lambda-node (pass/desugar lambda-arg-exprs)))))

(defmethod pass/desugar ((self letrec-syntax))
  ;; TODO: this is where we convert the LETREC to a LAMBDA + SET! construct.
  ;; fields: bindings, body
  self)

(defmethod pass/desugar ((self set!-syntax))
  (setf (var self) (pass/desugar (var self))
        (value self) (pass/desugar (value self)))
  self)

(defmethod pass/desugar ((self if-syntax))
  (setf (choice self) (pass/desugar (choice self))
        (consequent self) (pass/desugar (consequent self))
        (alternate self) (pass/desugar (alternate self)))
  self)

(defmethod pass/desugar ((self begin-syntax))
  (setf (body self) (pass/desugar (body self)))
  self)

(defmethod pass/desugar ((self application))
  (setf (op self) (pass/desugar (op self))
        (args self) (pass/desugar (args self)))
  self)

;; -----------------------------------------------------------------------
;; Pass: Perform a free variable analysis on the AST and record free variables
;; into the nodes of: LAMBDA-SYNTAX, LET-SYNTAX, LETREC-SYNTAX
;; -----------------------------------------------------------------------

;; TODO: Put these utilities somewhere.
(defun fv-union (l1 l2)
  (union l1 l2 :test #'equal))

(defun fv-set-difference (l1 l2)
  (set-difference l1 l2 :test #'equal))

(defun fv-intersection (l1 l2)
  (intersection l1 l2 :test #'equal))

(defun collect-free-variables (&rest forms)
  (when forms
    (reduce #'fv-union (mapcar (lambda (f)
                                 (nth-value 1 (pass/free-variables f)))
                               forms))))

;; Returns ast node as first value, and free variables at that node as second
;; value.
(defgeneric pass/free-variables (node))

(defmethod pass/free-variables ((self (eql nil)))
  (values self nil))

(defmethod pass/free-variables ((self toplevel))
  (values self (nth-value 1 (pass/free-variables (body self)))))

(defmethod pass/free-variables ((self body))
  (values self (collect-free-variables (defs self) (exprs self))))

(defmethod pass/free-variables ((self exprs))
  (let ((free-vars nil))
    (seq-iter self
              (lambda (s)
                (setf free-vars
                      (union free-vars
                             (nth-value 1 (pass/free-variables (expr s)))))))
    (values self free-vars)))

(defmethod pass/free-variables ((self literal))
  (values self nil))

(defmethod pass/free-variables ((self var))
  (let* ((sym (syment self))
         (fv (unless (or (prim-p sym) (global-p sym))
               ;; TODO: Since I'm not sure entirely what to return here yet
               ;; (which will be figured out during closure conversion), return
               ;; a cons of the varname and the syment for it. That should be
               ;; plenty useful for now.
               (list (cons (id self) sym)))))
    (values self fv)))

(defmethod pass/free-variables ((self prim))
  (values self (nth-value 1 (pass/free-variables (op self)))))

(defmethod pass/free-variables ((self prim-unary))
  (values self (collect-free-variables (op self) (arg0 self))))

(defmethod pass/free-variables ((self prim-binary))
  (values self (collect-free-variables (op self) (arg0 self) (arg1 self))))

(defmethod pass/free-variables ((self define-syntax))
  (error "pass/free-variables for define-syntax is not yet implemented."))

(defmethod pass/free-variables ((self set!-syntax))
  (values self (collect-free-variables (var self) (value self))))

(defmethod pass/free-variables ((self if-syntax))
  (values self (collect-free-variables (choice self)
                                       (consequent self)
                                       (alternate self))))

(defmethod pass/free-variables ((self begin-syntax))
  (values self (nth-value 1 (pass/free-variables (body self)))))

(defmethod pass/free-variables ((self application))
  (values self (collect-free-variables (op self) (args self))))

(defmethod pass/free-variables ((self vardecl))
  (values self (nth-value 1 (pass/free-variables (var self)))))

(defmethod pass/free-variables ((self lambda-syntax))
  (let ((formal-vars nil))
    (seq-iter (vardecls self)
              (lambda (vardecls)
                (setf formal-vars
                      (fv-union formal-vars
                                (nth-value 1 (pass/free-variables
                                              (vardecl vardecls)))))))

    (let ((result (fv-set-difference
                   (nth-value 1 (pass/free-variables (body self)))
                   formal-vars)))
      ;; At this form, we store the viewpoint of free variables.
      (setf (free-vars self) (copy-seq result))
      (values self result))))

(defmethod pass/free-variables ((self let-syntax))
  (let ((let-vars nil)
        (free-in-bindings-vars nil))
    (seq-iter (bindings self)
              (lambda (bindings)
                (let* ((binding (binding bindings))
                       (vardecl (vardecl binding))
                       (value (value binding)))
                  ;; store the var decl we're gonna need
                  (setf let-vars
                        (fv-union let-vars
                                  (nth-value 1 (pass/free-variables vardecl))))
                  ;; collect the free-vars for this binding value
                  (setf free-in-bindings-vars
                        (fv-union free-in-bindings-vars
                                  (nth-value 1 (pass/free-variables value)))))))
    (let ((result
            (fv-union
             ;; Here, we remove the variables that our bindings all supplied.
             (fv-set-difference (nth-value 1 (pass/free-variables (body self)))
                                let-vars)
             free-in-bindings-vars)))
      (setf (free-vars self) (copy-seq result))
      (values self result))))

(defmethod pass/free-variables ((self letrec-syntax))
  ;; Gather all vardecls first, since we're going to assume all of
  ;; them are bound before processing the binding forms and the body.
  (let ((letrec-vars nil)
        (free-in-bindings-vars nil))
    (seq-iter (bindings self)
              (lambda (bindings)
                (let* ((binding (binding bindings))
                       (vardecl (vardecl binding))
                       (value (value binding)))
                  ;; gather the vardecls symbols
                  (setf letrec-vars
                        (fv-union letrec-vars
                                  (nth-value 1 (pass/free-variables vardecl))))
                  ;; gather all free vars in binding values.
                  (setf free-in-bindings-vars
                        (fv-union free-in-bindings-vars
                                  (nth-value 1 (pass/free-variables
                                                value)))))))
    (let ((result
            (fv-set-difference
             ;; union the bindings free vars and the body free vars...
             (fv-union free-in-bindings-vars
                       (nth-value 1 (pass/free-variables (body self))))
             ;; then remove the letrec-vars to leave the free vars left.
             letrec-vars)))
      (setf (free-vars self) (copy-seq result))
      (values self result))))

;; -----------------------------------------------------------------------
;; Perform ONLY :flat closure conversion for now. This is a functional-ish pass
;; that can rewrite or replace some ast nodes at the appropriate places.
;; -----------------------------------------------------------------------

(defgeneric pass/closure-conversion (style node))

(defmethod pass/closure-conversion (style (self (eql nil)))
  nil)

(defmethod pass/closure-conversion ((style (eql :flat)) (self toplevel))
  (setf (body self) (pass/closure-conversion style (body self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self body))
  (setf (defs self) (pass/closure-conversion style (defs self)))
  (setf (exprs self) (pass/closure-conversion style (exprs self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self defs))
  (setf (def self) (pass/closure-conversion style (def self)))
  (when (more self)
    (setf (more self) (pass/closure-conversion style (more self))))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self def))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self exprs))
  (setf (expr self) (pass/closure-conversion style (expr self)))
  (when (more self)
    (setf (more self) (pass/closure-conversion style (more self))))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self vardecls))
  (setf (vardecl self) (pass/closure-conversion style (vardecl self)))
  (when (more self)
    (setf (more self) (pass/closure-conversion style (more self))))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self vardecl))
  ;; TODO: a vardecl can never change during closure conversion?
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self var))
  self)

;; all literals are just themselves.
(defmethod pass/closure-conversion ((style (eql :flat)) (self literal))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self prim))
  (setf (op self) (pass/closure-conversion style (op self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self prim-unary))
  (setf (op self) (pass/closure-conversion style (op self))
        (arg0 self) (pass/closure-conversion style (arg0 self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self prim-binary))
  (setf (op self) (pass/closure-conversion style (op self))
        (arg0 self) (pass/closure-conversion style (arg0 self))
        (arg1 self) (pass/closure-conversion style (arg1 self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self define-syntax))
  (setf (vardecl self) (pass/closure-conversion style (vardecl self))
        (value self) (pass/closure-conversion style (value self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self lambda-syntax))
  ;; TODO: this is where we convert this to a closure object instead
  ;; fields: vardecls, body
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self let-syntax))
  ;; TODO: this is where we convert this to a closure object instead
  ;; fields: bindings, body
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self letrec-syntax))
  ;; TODO: this is where we convert this to a closure object instead
  ;; fields: bindings, body
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self set!-syntax))
  (setf (var self) (pass/closure-conversion style (var self))
        (value self) (pass/closure-conversion style (value self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self if-syntax))
  (setf (choice self) (pass/closure-conversion style (choice self))
        (consequent self) (pass/closure-conversion style (consequent self))
        (alternate self) (pass/closure-conversion style (alternate self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self begin-syntax))
  (setf (body self) (pass/closure-conversion style (body self)))
  self)

(defmethod pass/closure-conversion ((style (eql :flat)) (self application))
  (setf (op self) (pass/closure-conversion style (op self))
        (args self) (pass/closure-conversion style (args self)))
  self)

;; -----------------------------------------------------------------------
;; Test compilation of a single toplevel set of forms.
;; -----------------------------------------------------------------------

(defun c1 (unparse-style top-forms)
  (let ((env (env:make-env :valid-categories `(:var)))
        (ast nil)
        (toplevel-freevars nil))

    ;; Compiler passes.
    (setf
     ast (pass/alphatization env top-forms)
     ;; TODO: Prolly should push freevar analysis later to a lower-level
     ;; representation (after desugaring), but doing it here is useful for
     ;; debugging output & recording given the original source.
     (values ast toplevel-freevars) (pass/free-variables ast)
     ast (pass/desugar ast)
     ast (pass/closure-conversion :flat ast)
     )

    (unparse unparse-style 0 ast)

    (logit "Free variables at toplevel: ~A~%" toplevel-freevars)

    ast))
