(in-package #:cl-event)

;;;-------------------------------------------------------------------------
;;; Provides the bulk of the event framework
;;;-------------------------------------------------------------------------

(defclass event ()
  ((observers :initform ()
	      :type list
	      :documentation "A list of objects interested in being notified of the event."))
  (:documentation "Provides the event framework."))

;;;-------------------------------------------------------------------------

(defgeneric add-observer (event observer)
  (:documentation "Schedules the given observer to receive updates from the given event.")
  (:method ((event event) observer)
    (with-slots (observers) event
      (push observer observers))
    event))

(defgeneric delete-observer (event observer &key test)
  (:documentation "Removes the given observer from the event.")
  (:method ((event event) observer &key (test #'eql))
    (with-slots (observers) event
      (setf observers (delete observer observers :test test)))
    event))

(defgeneric fire-event (event &rest arguments)
  (:documentation "Fires the given event.")
  (:method ((event event) &rest arguments)
    (with-slots (observers) event
      (mapcar #'(lambda (observer)
		  (apply observer arguments))
	      observers))
    nil))

;;;-------------------------------------------------------------------------

(defun create-event-name (symbol)
  (intern (concatenate 'string (symbol-name symbol) "-CHANGED") (symbol-package symbol)))

;;;-------------------------------------------------------------------------