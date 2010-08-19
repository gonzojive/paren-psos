(in-package :psos)

(defpsmacro handler-bind (bindings &body body)
  (with-ps-gensyms (local-handlers)
    `(let ((,local-handlers (list ,@(mapcar #'(lambda (binding)
                                                (destructuring-bind (type handler)
                                                    binding
                                                  `(create 'type ,type
                                                           'fn ,handler)))
                                            bindings))))
       (let ((*original-handlers* *active-handlers*))
         (declare (special *original-handlers*))
         (let ((*active-handlers* (append ,local-handlers *original-handlers*)))
           (declare (special *active-handlers*))
           ,@body)))))

(defpsmacro restart-bind (bindings &body body)
  (with-ps-gensyms (local-restarts)
    `(let ((,local-restarts (list ,@(mapcar #'(lambda (binding)
                                                (destructuring-bind (name handler)
                                                    binding
                                                  `(create 'name ',name
                                                           'fn ,handler)))
                                            bindings))))
       (let ((*active-restarts* (append ,local-restarts *active-restarts*)))
         (declare (special *active-restarts*))
         ,@body))))

(defpsmacro restart-case (form &rest clauses)
  (with-ps-gensyms (restart-block local-restarts)
    `(block ,restart-block
       (let ((,local-restarts (list ,@(mapcar #'(lambda (clause)
                                                  (destructuring-bind (name lambda-list &body body)
                                                      clause
                                                    ;; todo strip out :report, :interactive, :test
                                                    `(create 'name ',name
                                                             'fn (lambda ()
                                                                   (return-from ,restart-block
                                                                     (apply (lambda ,lambda-list
                                                                              ,@body)
                                                                            arguments))))))
                                                                   
                                              clauses))))
         (let ((*active-restarts* (append ,local-restarts *active-handlers*)))
           (declare (special *active-restarts*))
           ,form)))))

(defpsmacro restart-case (form &rest clauses)
  (with-ps-gensyms (args-to-restart local-restarts)
    `(let ((,local-restarts
            (list ,@(mapcar #'(lambda (clause)
                                (destructuring-bind (name lambda-list &body body)
                                    clause
                                  ;; todo strip out :report, :interactive, :test
                                  `(create 'name ',name
                                           'fn (lambda ()
                                                 (let ((,args-to-restart arguments))
                                                   (throw (create 'ps-signal-p t
                                                                  'continuation (lambda ()
                                                                                  (apply (lambda ,lambda-list
                                                                                           ,@body)
                                                                                         ,args-to-restart)))))))))
                            clauses))))
       
       (ps:try (let ((*active-restarts* (append ,local-restarts *active-restarts*)))
                 (declare (special *active-restarts*))
                 ,form)
               (:catch (err)
                 (if (and err (getprop err 'ps-signal-p))
                     (funcall (getprop err 'continuation))
                     (throw err)))))))

(defpsmacro handler-case (form &rest clauses)
  (with-ps-gensyms (local-handlers args-to-handler)
    `(let ((,local-handlers 
            (list ,@(mapcar #'(lambda (clause)
                                (destructuring-bind (type lambda-list &body body)
                                    clause
                                  ;; todo strip out :report, :interactive, :test
                                  `(create 'type ,type
                                           'fn (lambda ()
                                                 (let ((,args-to-handler arguments))
                                                   (throw (create 'ps-signal-p t
                                                                  'continuation (lambda ()
                                                                                  (apply (lambda ,lambda-list
                                                                                           ,@body)
                                                                                         ,args-to-handler)))))))))
                              clauses))))
       (ps:try (let ((*active-handlers* (append ,local-handlers *active-handlers*)))
                 (declare (special *active-handlers*))
                 ,form)
               (:catch (err)
                 (if (and err (getprop err 'ps-signal-p))
                     (funcall (getprop err 'continuation))
                     (throw err)))))))



;; the following is a lot more like handler-case
;       (ps:try 
;        (progn ,@body)))))
;        (:catch (err)
;          (invoke-handler-or-rethrow err ,activated-handlers ,original-handlers))))))
          