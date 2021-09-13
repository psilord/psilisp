(in-package #:base)

(defparameter *label-serial-number* 0)

(defmacro defconstants (&body body)
  `(progn
     ,@(loop :for (name val) :on body :by #'cddr
             :collect `(defconstant ,name ,val))))

(defun unique-label ()
  (let ((sn *label-serial-number*))
    (incf *label-serial-number*)
    (format nil "L_~D" sn)))

(defun unique-labels (&rest names)
  (loop :for name :in names
        :collect (concatenate 'string (unique-label) "_"
                              (string-downcase (symbol-name name)))))

;; Prolly too much work went into this...
(defmacro emit ((&key (label nil) (cmnt nil))
                &rest insn) ;; Specifically &rest for indention.
  "LABEL is supplied is evaluated.
CNT is a list of a FMT string and the required args for it.
INSN, acting like a &body, is a FMT string and required args, not in list form"
  (let* ((empty-fmt "")
         (label-fmt "~A:")
         (cmnt-label-fmt "~40A")
         (insn-fmt "    ~36A")
         (cmnt-fmt "# ~A")
         (bare-cmnt-fmt "    # ~A"))
    (multiple-value-bind (line-fmt reified-args-list)
        (cond
          ((and (not label) (not cmnt) (not insn))
           (values (concatenate 'string
                                empty-fmt "~%")
                   `()))

          ((and (not label) (not cmnt) insn)
           (values (concatenate 'string
                                insn-fmt "~%")
                   `((format nil ,@insn))))

          ((and (not label) cmnt (not insn))
           (values (concatenate 'string
                                bare-cmnt-fmt "~%")
                   `((format nil ,@cmnt))))

          ((and (not label) cmnt insn)
           (values (concatenate 'string
                                insn-fmt
                                cmnt-fmt "~%")
                   `((format nil ,@insn)
                     (format nil ,@cmnt))))

          ((and label (not cmnt) (not insn))
           (values (concatenate 'string
                                label-fmt "~%")
                   `(,label)))

          ((and label (not cmnt) insn)
           (values (concatenate 'string
                                label-fmt "~%"
                                insn-fmt "~%")
                   `(,label
                     (format nil ,@insn))))

          ((and label cmnt (not insn))
           (values (concatenate 'string
                                cmnt-label-fmt
                                cmnt-fmt "~%")
                   `((format nil ,label-fmt ,label) ;; tricky!
                     (format nil ,@cmnt))))

          ((and label cmnt insn)
           (values (concatenate 'string
                                label-fmt "~%"
                                insn-fmt
                                cmnt-fmt "~%")
                   `(,label
                     (format nil ,@insn)
                     (format nil ,@cmnt)))))

      `(format *standard-output*  ,line-fmt ,@reified-args-list))))
