;;;; stream-provider.asd
(cl:defpackage :fwoar.sp-system
  (:use :cl :asdf))
(in-package :fwoar.sp-system)

(asdf:defsystem #:stream-provider
  :description "A class library for interacting with groups of related resources"
  :author "Edward Langley <el-streamprovider@elangley.org"
  :license "MIT"
  :pathname #p"PROJECTS:stream-provider;"
  :in-order-to ((test-op (test-op "stream-provider.test")))
  :depends-on (#:fwoar-lisputils
               #:alexandria
               #:serapeum
	             #:uiop
	             #:flexi-streams
	             #:vector-update-stream)
  :serial t
  :components ((:file "package")
               (:file "stream-provider")))

(asdf:defsystem #:stream-provider.test
  :depends-on (#:stream-provider
	             #:should-test)
  :serial t
  :pathname #p"PROJECTS:stream-provider;"
  :components ((:file "package")
	             (:file "tests"))
  :perform (asdf:test-op (o s)
		                     (let ((*package* (find-package :stream-provider.tests)))
		                       (uiop:symbol-call :should-test
					                                   :test-for-xunit
					                                   *standard-output*))))

(defpackage :fwoar.sp-user
  (:use cl)
  (:export #:define-package))
