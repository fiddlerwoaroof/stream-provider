(fwoar.sp-user:define-package :stream-provider
  (:use :fw.lu)
  (:export #:get-stream-for
	   #:stream-provider
	   #:string-provider
	   #:file-provider
	   #:stream-key
	   #:root
	   #:streams
	   #:with-storage-stream))
(cl:in-package :stream-provider)

(defclass stream-provider ()
  ())

(defvar *store-root* *default-pathname-defaults*)

(defgeneric stream-key (provider item)
  (:documentation "return a key for an item: must be able to be compared with EQUAL

If you override on provider, make sure to CALL-NEXT-METHOD"))

(defgeneric get-stream-for (provider streamable &rest extra-args)
  (:documentation "get a stream for a given streamable object"))

(defgeneric root (provider)
  (:documentation "get the base path for the streams"))


(defclass string-provider (stream-provider)
  ((%streams :reader streams :initform (make-hash-table :test 'equal))))

(defmethod root ((provider string-provider))
  #p "/")

(defmethod stream-key :around (provider item)
  (let ((key (call-next-method)))
    (check-type key (or string pathname))
    (uiop:enough-pathname key (root provider))))

(defmethod get-stream-for ((provider string-provider) streamable &rest extra-args)
  (declare (ignore extra-args))
  (with-accessors* (streams) provider
    (vector-update-stream:make-update-stream 
     (setf (gethash (stream-key provider streamable) streams)
	   (make-array 10
		       :element-type 'octet
		       :adjustable t
		       :fill-pointer 0)))))

(defclass file-provider (stream-provider)
  ((%root :initarg :root :initform (error "need a root for a file-provider") :reader root)
   (%if-exists :initarg :if-exists :initform :supersede :reader if-exists)))

(defmethod get-stream-for ((provider file-provider) streamable &rest extra-args)
  (declare (ignore extra-args))
  (with-accessors* (if-exists root) provider
    (let ((stream-key (merge-pathnames (stream-key provider streamable)
				       root)))
      (when (eql if-exists :if-exists)
	(ensure-directories-exist stream-key))
      (open stream-key :direction :output :if-exists if-exists
	    :element-type 'octet))))

(defmacro with-storage-stream ((stream-sym object provider &rest extra-args) &body body)
  (once-only (object)
    `(let ((,stream-sym (flexi-streams:make-flexi-stream (get-stream-for ,provider ,object ,@extra-args)
							 :external-format :utf-8)))
       (unwind-protect (progn ,@body)
	 (close ,stream-sym)))))
