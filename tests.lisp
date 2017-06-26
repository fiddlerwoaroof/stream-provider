(fwoar.sp-user:define-package :stream-provider.tests
  (:use :cl :stream-provider :should-test))
(in-package :stream-provider.tests)

(defparameter *test-string-provider* (make-instance 'string-provider))
(defparameter *test-file-provider* (make-instance 'file-provider :root #p"/nowhere/"))

(defmethod stream-key ((provider (eql *test-string-provider*)) (item string))
  item)

(defmethod stream-key ((provider (eql *test-file-provider*)) (item string))
  item)

(deftest stream-key-returns-specified-pathname ()
    (let* ((stored-item "foo")
	   (expected-pathname (pathname "foo")))
      (should be equal
	      (stream-key *test-string-provider* stored-item)
	      expected-pathname)))



(deftest stream-key-for-file-provider-is-relative-to-root ()
    (let* ((stored-item "/nowhere/foo")
	   (expected-pathname (pathname "foo")))
      (should be equal
	      expected-pathname
	      (stream-key *test-file-provider* stored-item))))

(defclass object-to-store ()
  ((%name :reader name :initarg :name :initform (error "need a name"))
   (%value :reader value :initarg :value :initform (error "need a value"))))

(defclass test-provider (string-provider)
  ())

(defmethod stream-key ((provider test-provider) (item object-to-store))
  (name item))

(defun store (provider item)
  (with-storage-stream (s item provider)
    (write-sequence (value item) s)))

(deftest with-storage-stream-writes-to-right-place ()
    (let* ((name "foo")
	   (value "bar bar")
	   (object (make-instance 'object-to-store
				  :name name
				  :value value))
	   (provider (make-instance 'test-provider)))
      (should be equal
	      (store provider object)
	      value
	      (babel:octets-to-string
	       (gethash (stream-key provider object)
			(streams provider))))))
