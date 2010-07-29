(defpackage cl-event
  (:nicknames ev)
  (:use common-lisp)
  (:export #:event
	   #:add-observer
	   #:delete-observer
	   #:fire-event

	   ;; macros
	   #:insensitive)
  (:documentation "A package of various helper functions and macros."))
  