(in-package #:cl-event)

;;;----------------------------------------------------------
;;; 
;;;----------------------------------------------------------

(defmacro insensitive ((observer event) &body body)
  "Causes the observer to be 'insensitive' (not receive notifications
  of) the given event, while the code in BODY is executed."
  (ut:once-only (observer event)
    `(progn
       (delete-observer ,event ,observer)
       (unwind-protect (progn
			 ,@body)
	 (add-observer ,event ,observer)))))

;;;----------------------------------------------------------