(in-package :paren-psos-tests)

(defsuite paren-conditions-tests nil
  (with-js-context (context)
    (compile-and-evaluate-js "var window = this;")
    (compile-and-evaluate-js (system-js))
    (run-child-tests)))

(in-suite paren-conditions-tests)

(deftest test-conditions-metaobjects ()
  (ps-forms-equal-lisp-forms
    (t (and condition t))))

(deftest test-restart-case ()
  (declare (optimize (debug 3)))
  (ps-forms-equal-lisp-forms
    (5 (funcall
        (lambda ()
          (restart-case (progn (invoke-restart 'thingoo 4) 10)
            (thingoo (num)
              (+ num 1))))))
    (5 (funcall
        (lambda ()
          (restart-case (progn (+ (invoke-restart 'thingoo 5) 10))
            (thingoo (num)
              num)))))
    ("enter-okay-unwind-restart-exit" 
     (funcall
      (lambda ()
        (let ((r "enter-"))
          (restart-case (try
                         (progn
                           (setf r (+ r "okay-"))
                           (+ (invoke-restart 'thingoo 5) 10)
                           (setf r (+ r "NEVER_GET_HERE")))
                         (:finally
                          (setf r (+ r "unwind-"))
                          ))
            (thingoo (num)
              (setf r (+ r "restart-"))
              num))
          (setf r (+ r "exit"))
          r))))
    ("start-enter1-enter2-unwhc2-restart2-unwind2-unwhc1-unwind1-exit"
     (funcall
      (lambda ()
        (let ((r "start-"))
          (labels ((myfn (invokep level)
                     (unwind-protect
                          (restart-case
                              (unwind-protect
                                   (progn
                                     (incf r (+ "enter" level "-"))
                                     (if invokep
                                         (invoke-restart 'myrestart "invoked")
                                         (myfn t "2")))
                                (incf r (+ "unwhc" level "-")))
                            (myrestart (thing)
                              (incf r (+ "restart" level "-"))
                              (when (eql "2" level)
                                #+nil
                                (invoke-restart 'myrestart))))
                       (incf r (+ "unwind" level "-")))))
            (myfn nil "1")
            (incf r "exit"))
          r))))))

(deftest test-restart-bind ()
  (declare (optimize (debug 3)))
  (ps-forms-equal-lisp-forms
    (10 (funcall
         (lambda ()
           (restart-bind ((xxx (lambda (thing)
                                 20)))
                         (invoke-restart 'xxx 4) 10))))
    (12 (funcall
         (lambda ()
           (+ 1
              (funcall 
               (lambda ()
                 (restart-bind ((xxx (lambda (thing)
                                       20)))
                               (invoke-restart 'xxx 4) 11)))))))))


(deftest test-handler-bind ()
  (declare (optimize (debug 3)))
  (ps-forms-equal-lisp-forms
    (6 (funcall
         (lambda ()
           (handler-bind ((condition (lambda (x)
                                       1)))
             (+ (signal (make-instance 'condition))
                5)))))
    (4 (progn
         (defclass mine (condition)
           ((x :initarg :x :initform 1 :accessor mine-x)))

         (funcall
          (lambda ()
            (handler-bind ((mine (lambda (m)
                                   (mine-x m)))
                           (condition (lambda (x)
                                        10)))
              (+ (signal (make-instance 'mine))
                 3))))))
    (13 (progn
          (funcall
           (lambda ()
             (handler-bind ((condition (lambda (x)
                                         10)))
               (+ (signal (make-instance 'mine))
                  3))))))))



(deftest test-handler-case ()
  (declare (optimize (debug 3)))
  (ps-forms-equal-lisp-forms
    (17 (funcall
         (lambda ()
           (handler-case (signal (make-instance 'condition))
             (condition (err) 17)))))
    ("abcd" (funcall
             (lambda ()
               (let ((r "a"))
                 (handler-case 
                     (try
                      (progn
                        (setf r (+ r "b"))
                        ;; signal should invoke a non-local exit, and
                        ;; the handler should execute after unwinding
                        (signal (make-instance 'condition))
                        (setf r (+ r "x")))
                      (:finally
                        (setf r (+ r "c"))))
                   (condition (err)
                     (setf r (+ r "d"))
                     17))
                 r))))
    ("start-enter1-enter2-unwhc2-handler2-unwind2-unwhc1-unwind1-exit"
     (funcall
      (lambda ()
        (let ((r "start-"))
          (labels ((myfn (errp level)
                     (unwind-protect
                          (handler-case
                              (unwind-protect
                                   (progn
                                     (incf r (+ "enter" level "-"))
                                     (if errp
                                         (signal (make-instance 'condition))
                                         (myfn t "2")))
                                (incf r (+ "unwhc" level "-")))
                            (condition (err)
                              (incf r (+ "handler" level "-"))))
                       (incf r (+ "unwind" level "-")))))
            (myfn nil "1")
            (incf r "exit"))
          r))))
    ("start-enter1-enter2-unwhc2-handler2-unwind2-unwhc1-unwind1-exit"
     (funcall
      (lambda ()
        (let ((r "start-"))
          (labels ((myfn2 (errp level)
                     (unwind-protect
                          (handler-case
                              (unwind-protect
                                   (progn
                                     (incf r (+ "enter" level "-"))
                                     (if errp
                                         (signal (make-instance 'condition))
                                         (myfn2 t "2")))
                                (incf r (+ "unwhc" level "-")))
                            (condition (err)
                              (incf r (+ "handler" level "-"))))
                       (incf r (+ "unwind" level "-"))))
                   (myfn1 (errp level)
                     (unwind-protect
                          (handler-case
                              (unwind-protect
                                   (progn
                                     (incf r (+ "enter" level "-"))
                                     (if errp
                                         (signal (make-instance 'condition))
                                         (myfn2 t "2")))
                                (incf r (+ "unwhc" level "-")))
                            (condition (err)
                              (incf r (+ "handler" level "-"))))
                       (incf r (+ "unwind" level "-")))))
            (myfn1 nil "1")
            (incf r "exit"))
          r))))))

             