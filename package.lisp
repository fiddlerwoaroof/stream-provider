;;;; package.lisp

(in-package :fwoar.sp-user)

(defmacro define-package (name &body arguments)
  (let ((uses (cons :use
		    (union '(:cl :alexandria :serapeum)
			   (cdr (assoc :use arguments))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defpackage ,name
	 ,uses
	 ,@(remove :use arguments :key 'car)))))

(define-package :stream-provider
  (:use :fw.lu)
  (:export
   #:get-stream-for
   #:stream-provider
   #:string-provider
   #:file-provider
   #:stream-key
   #:root
   #:streams
   #:with-storage-stream))

