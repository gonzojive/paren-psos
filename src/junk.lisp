
(defun write-rjson (object &optional (xref-session *json-session*))
  (js:js-to-strings
   (js:js-compile 
    '(macrolet ((represent (obj)
		 (+ 1 obj)))
      (represent 3)))
   0)
  (or `((gethash object xref-session)
	(encode-xref-json object)))

(defgeneric encode-xref-json (object &optional xref-session)
  (:documentation "Called to encode an object into xref json"))

(defmethod encode-xref-json :after (object  &optional (xref-session *json-session*))
  (setf (gethash object xref-session) object))

(defmethod encode-xref-json :around (object  &optional (xref-session *json-session*))
  (or 

(defmethod encode-xref-json (object  &optional (xref-session *json-session*))
  "Default behavior is to return the object and let parenscript handle it"
  (declare (ignore xref-session))
  object)

(defmethod encode-xref-json ((object standard-object)  &optional (xref-session *json-session*))
  "There is special functionality for transmitting classes.
right now, it is a simple hash."
  (declare (ignore xref-session))
  object)

(defmethod encode-xref-json ((object hash-table)  &optional (xref-session *json-session*))
  "There is special functionality for transmitting classes.
right now, it is a simple hash."
  (declare (ignore xref-session))
  (let ((create-args (list)))
    (maphash #'(lambda (key value)
		 (push key create-args)
		 (push `(represent ,value) create-args))
	     object)
    (apply #'list 'create (nreverse create-args))))

