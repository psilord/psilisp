(in-package #:env)

;; --------
;; Environment table.
;;
;; Any environmental category like :var, :type, but which is tested with
;; EQUAL etc, can have associated with it a block structured symbol
;; table. Definitions are added or looked up in specific categories.
;; Used to manage all symbol table needs for whatever semantic namespace
;; I need.
;; --------
(defclass env ()
  ((%defined-categories :accessor defined-categories
			:initarg :defined-categories)
   (%categories :accessor categories
		:initarg :categories
		:initform (make-hash-table :test #'equal))))

(defun make-env (&key valid-categories)
  (make-instance 'env
		 :defined-categories
		 (let ((def-cats (make-hash-table :test #'equal)))
		   (dolist (vc valid-categories)
		     (setf (gethash vc def-cats) t))
		   def-cats)))

;; a check to ensure I don't have any typos in my environmental categories.
(defun valid-category-p (env category)
  (gethash category (defined-categories env)))

(defun ensure-blsymtab-in-category (env category)
  "Create an empty block structured symbol table in the environment associated
with the category if none exists and return it. If it already exists, just
return it. NOTE: This function does no scope opening/closing."
  (unless (valid-category-p env category)
    (error "ENV: Category ~S is not valid for this environment." category))

  (let ((blsymtab (gethash category (categories env))))
    (unless blsymtab
      (setf blsymtab (bst:make-blsymtab)
            (gethash category (categories env)) blsymtab))
    blsymtab))

(defun open-scope (env category)
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (bst:open-scope blsymtab)))

(defun close-scope (env category)
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (bst:close-scope blsymtab)))

(defun add-definition (env category name sym)
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (bst:add-definition blsymtab name sym)))

(defun find-definition (env category name &key (scope :inner))
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (bst:find-definition blsymtab name :scope scope)))

(defun current-definitions (env category &key (scope :inner))
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (bst:current-definitions blsymtab :scope scope)))

(defun current-scope (env category) ;; always the :inner scope.
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (bst:current-scope blsymtab)))

(defun dump-env (env)
  (format t "Dumping Environment with ~A semantic categories:~%"
          (hash-table-count (categories env)))
  (maphash (lambda (category blsymtab)
             (format t ">>> Category: ~S <<<~%" category)
             (bst:dump-blsymtab blsymtab))
           (categories env))
  (format t "End dumping Environment.~%"))
