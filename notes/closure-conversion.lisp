;; Figuring out how closure conversion can be explicitly modeled in CL.
;; The point of this note is that when I do the closure pass in the compiler
;; I know how the mutation pass, cell pass, and flat/linked closures worked.


;; the lambda* simply represents a closed lambda for book keeping purposes
(defmacro lambda* ((&rest formals) &body body)
  `(lambda ,formals
     (declare (ignorable ,@formals))
     ,@body))

(defstruct (closure (:constructor %make-closure))
  ;; reference to a closed lambda* function
  closed-func
  ;; reference to an array (of places) of closed variable values.
  closed-vars)

(defun make-closure (closed-vars closed-func) ;; makes for decent syntax...
  (%make-closure :closed-func closed-func
                 :closed-vars (coerce closed-vars 'vector)))

(defun ccv-ref (closure index)
  (aref (closure-closed-vars closure) index))
(defun (setf ccv-ref) (new-obj closure index)
  (setf (aref (closure-closed-vars closure) index) new-obj))
(defun funcall-closure (closure &rest args)
  (apply (closure-closed-func closure) closure args))
(defun no-free-vars ()
  nil)
(defun close-over-vars (&rest vars)
  vars)

;; mutable cell simulation
(defun make-cell (x)
  (vector x))
(defun cell-value (cell)
  (aref cell 0))
(defun (setf cell-value) (new-val cell)
  (setf (aref cell 0) new-val))

;; Simple open function example
(setf (fdefinition 'foo)
      (lambda (x)
        (lambda (y)
          (lambda (z)
            (+ x y z)))))

;; example flat closure representation.
;; I don't need cells because nothing is mutable.
(defun foo-0 ()
  (let ((foo-0
          (make-closure
           (no-free-vars)
           (lambda* (env-0 x)
             (make-closure
              (close-over-vars x)
              (lambda* (env-1 y)
                (make-closure
                 (close-over-vars (ccv-ref env-1 0) ;; copy x
                                  y)
                 (lambda* (env-2 z)
                   (+ (ccv-ref env-2 0)
                      (ccv-ref env-2 1)
                      z)))))))))
    foo-0))

(defun foo-0-test ()
  (let* ((f0 (foo-0))
         (g (funcall-closure f0 10))
         (h (funcall-closure g 20))
         (i (funcall-closure h 30)))
    (if (= i 60)
	(format t "Good!~%")
	(format t "Bad!~%"))))

;; ----------------------------------------------------------------

;; the initial form of the code.
(setf (fdefinition 'bar)
      ;; make an account table entry for a user.
      (lambda (user num-accounts) ;; free: nil
        ;; make a sub-account for user
        (lambda (account-name balance) ;; free: user num-accounts
          (setf num-accounts (+ num-accounts 1))
          (list
           ;; withdrawl function
           (lambda (amount) ;; free: user account-name balance
             (list user account-name (setf balance (- balance amount))))
           ;; deposit function
           (lambda (amount) ;; free: user account-name balance
             (list user account-name (setf balance (+ balance amount))))
           ;; check balance function
           (lambda () ;; free: user account-name balance
             (list user account-name balance))
           ;; check how many accounts function
           (lambda () ;; free: user num-accounts
             (list user num-accounts))))))

;; "linked closures"
;;
;; This works, but I had to be very careful about "mutable cells" and have
;; explicitly encoded that here (and in the bar-0-closure-test function).
;; NOTE: Figure out exactly the mutable variable conversion and how it should
;; work in general, and WHY it is actually necessary (and how it ultimately
;; relates to the cactus stacks in a traditional symbol table).
(defun bar-0-linked ()
  (make-closure
   (no-free-vars) ;; env-0
   (lambda* (env-0 user num-accounts-cell)
     (make-closure
      (close-over-vars env-0 user num-accounts-cell) ;; env-1
      (lambda* (env-1 account-name balance-cell)
        (setf (cell-value (ccv-ref env-1 2)) ;; num-accounts-cell
              (1+ (cell-value (ccv-ref env-1 2)))) ;; num-accounts-cell
        (list
         ;; withdrawl function
         (make-closure
          (close-over-vars env-1 account-name balance-cell) ;; env-2
          (lambda* (env-2 amount)
            (list (ccv-ref (ccv-ref env-2 0) 1) ;; link user
                  (ccv-ref env-2 1) ;; account-name
                  (setf (cell-value (ccv-ref env-2 2)) ;; balance-cell
                        (- (cell-value (ccv-ref env-2 2)) ;; balance-cell
                           amount)))))
         ;; deposit function
         (make-closure
          (close-over-vars env-1 account-name balance-cell) ;; env-3
          (lambda* (env-3 amount)
            (list (ccv-ref (ccv-ref env-3 0) 1) ;; link user
                  (ccv-ref env-3 1) ;; account-name
                  (setf (cell-value (ccv-ref env-3 2)) ;; balance-cell
                        (+ (cell-value (ccv-ref env-3 2)) ;; balance-cell
                           amount)))))
         ;; check-balance function
         (make-closure
          (close-over-vars env-1 account-name balance-cell) ;; env-4
          (lambda* (env-4)
            (list (ccv-ref (ccv-ref env-4 0) 1) ;; link user
                  (ccv-ref env-4 1) ;; account-name
                  (cell-value (ccv-ref env-4 2))))) ;; balance-cell
         ;; check how many accounts function
         (make-closure
          (close-over-vars env-1) ;; env-5
          (lambda* (env-5)
            (list (ccv-ref (ccv-ref env-5 0) 1) ;; link user
                  (cell-value (ccv-ref (ccv-ref env-5 0) 2))) ;; link num-accounts-cell
            ))))))))


;; "flat closures".
;;
;; Working.
;; NOTE: This required the mutable cells transform as above.
(defun bar-0-flat ()
  (make-closure
   (no-free-vars) ;; env-0
   (lambda* (env-0 user num-accounts-cell)
     (make-closure
      (close-over-vars env-0 user num-accounts-cell) ;; env-1
      (lambda* (env-1 account-name balance-cell)
        (setf (cell-value (ccv-ref env-1 2)) ;; num-accounts-cell
              (1+ (cell-value (ccv-ref env-1 2)))) ;; num-accounts-cell
        (list
         ;; withdrawl function
         (make-closure
          (close-over-vars env-1
                           (ccv-ref env-1 1) ;; copy user
                           account-name
                           balance-cell) ;; env-2
          (lambda* (env-2 amount)
            (list (ccv-ref env-2 1) ;; user
                  (ccv-ref env-2 2) ;; account-name
                  (setf (cell-value (ccv-ref env-2 3)) ;; balance-cell
                        (- (cell-value (ccv-ref env-2 3)) ;; balance-cell
                           amount)))))
         ;; deposit function
         (make-closure
          (close-over-vars env-1
                           (ccv-ref env-1 1) ;; copy user
                           account-name
                           balance-cell) ;; env-3
          (lambda* (env-3 amount)
            (list (ccv-ref env-3 1) ;; user
                  (ccv-ref env-3 2) ;; account-name
                  (setf (cell-value (ccv-ref env-3 3)) ;; balance-cell
                        (+ (cell-value (ccv-ref env-3 3)) ;; balance-cell
                           amount)))))
         ;; check-balance function
         (make-closure
          (close-over-vars env-1
                           (ccv-ref env-1 1) ;; copy user
                           account-name
                           balance-cell) ;; env-4
          (lambda* (env-4)
            (list (ccv-ref env-4 1) ;; user
                  (ccv-ref env-4 2) ;; account-name
                  (cell-value (ccv-ref env-4 3))))) ;; balance-cell
         ;; check how many accounts function
         (make-closure
          (close-over-vars env-1
                           (ccv-ref env-1 1) ;; copy user
                           (ccv-ref env-1 2) ;; copy num-accounts-cell
                           ) ;; env-5
          (lambda* (env-5)
            (list (ccv-ref env-5 1) ;; user
                  (cell-value (ccv-ref env-5 2))) ;; num-accounts-cell
            ))))))))



;; TODO Turn this into "safely linked closures"
(defun bar-0-safely-linked ()
  (make-closure
   (no-free-vars)
   (lambda* (env-0 user num-accounts)
     (make-closure
      (no-free-vars)
      (lambda* (env-1 account-name balance)
        ;; TODO: Hrm, prolly wrong--uses env-0! Needs to use env-1!
        (setf XXX (1+ XXX))
        ;; TODO: Implement bodies.
        (list
         ;; withdrawl function
         (make-closure
          (no-free-vars)
          (lambda* (env-2 amount)
            nil))
         ;; deposit function
         (make-closure
          (no-free-vars)
          (lambda* (env-3 amount)
            nil))
         ;; check-balance function
         (make-closure
          (no-free-vars)
          (lambda* (env-4 amount)
            nil))
         ;; check how many accounts function
         (make-closure
          (no-free-vars)
          (lambda* (env-5 amount)
            nil))))))))

(defun bar-0-test ()
  (let* ((acc-psi (bar :psilord 0))

         (acc-psi/home (funcall acc-psi :home 1000))
         (acc-psi/home/withdrawl (first acc-psi/home))
         (acc-psi/home/deposit (second acc-psi/home))
         (acc-psi/home/balance (third acc-psi/home))
         (acc-psi/home/num-accounts (fourth acc-psi/home))

         (acc-psi/work (funcall acc-psi :work 5000))
         (acc-psi/work/withdrawl (first acc-psi/work))
         (acc-psi/work/deposit (second acc-psi/work))
         (acc-psi/work/balance (third acc-psi/work))
         (acc-psi/work/num-accounts (fourth acc-psi/work)))

    (funcall acc-psi/home/deposit 100)
    (funcall acc-psi/home/withdrawl 50)
    (format t "home/balance = ~(~S~), num-accounts = ~A~%"
            (funcall acc-psi/home/balance)
            (funcall acc-psi/home/num-accounts))

    (funcall acc-psi/work/deposit 1000)
    (funcall acc-psi/work/withdrawl 500)
    (format t "work/balance = ~(~S~), num-accounts = ~A~%"
            (funcall acc-psi/work/balance)
            (funcall acc-psi/work/num-accounts))

    (if (and (= 1050 (third (funcall acc-psi/home/balance)))
             (equal '(:psilord 2) (funcall acc-psi/home/num-accounts))
             (= 5500 (third (funcall acc-psi/work/balance)))
             (equal '(:psilord 2) (funcall acc-psi/work/num-accounts)))
        (format t "Good!~%")
        (format t "Bad!~%"))))



(defun bar-0-closure-test (bar-closure-variant)
  (let* ((bar (funcall bar-closure-variant))

         ;; NOTE: For now, we explicitly make cells for mutable variables.

         (acc-psi (funcall-closure bar :psilord (make-cell 0)))

         (acc-psi/home (funcall-closure acc-psi :home (make-cell 1000)))
         (acc-psi/home/withdrawl (first acc-psi/home))
         (acc-psi/home/deposit (second acc-psi/home))
         (acc-psi/home/balance (third acc-psi/home))
         (acc-psi/home/num-accounts (fourth acc-psi/home))

         (acc-psi/work (funcall-closure acc-psi :work (make-cell 5000)))
         (acc-psi/work/withdrawl (first acc-psi/work))
         (acc-psi/work/deposit (second acc-psi/work))
         (acc-psi/work/balance (third acc-psi/work))
         (acc-psi/work/num-accounts (fourth acc-psi/work)))

    (funcall-closure acc-psi/home/deposit 100)
    (funcall-closure acc-psi/home/withdrawl 50)
    (format t "home/balance = ~(~S~), num-accounts = ~A~%"
            (funcall-closure acc-psi/home/balance)
            (funcall-closure acc-psi/home/num-accounts))

    (funcall-closure acc-psi/work/deposit 1000)
    (funcall-closure acc-psi/work/withdrawl 500)
    (format t "work/balance = ~(~S~), num-accounts = ~A~%"
            (funcall-closure acc-psi/work/balance)
            (funcall-closure acc-psi/work/num-accounts))

    (if (and (= 1050 (third (funcall-closure acc-psi/home/balance)))
             (equal '(:psilord 2) (funcall-closure acc-psi/home/num-accounts))
             (= 5500 (third (funcall-closure acc-psi/work/balance)))
             (equal '(:psilord 2) (funcall-closure acc-psi/work/num-accounts)))
        (format t "Good!~%")
        (format t "Bad!~%"))))
