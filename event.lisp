;;; cl-event: a simple event library
;;; Copyright (C) 2010-2012 Rudy Neeser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Additional permission under GNU GPL version 3 section 7
;;; 
;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library. Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.
;;; 
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module. An independent module is a module which is not derived from or
;;; based on this library. If you modify this library, you may extend this
;;; exception to your version of the library, but you are not obligated to
;;; do so. If you do not wish to do so, delete this exception statement
;;; from your version.

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
  (:documentation "Schedules the given observer to receive updates from the given event. Returns the observer.")
  (:method ((event event) observer)
    (with-slots (observers) event
      (push observer observers))
    observer))

(defgeneric delete-observer (event observer &key test)
  (:documentation "Removes the given observer from the event. Returns the observer.")
  (:method ((event event) observer &key (test #'eql))
    (with-slots (observers) event
      (setf observers (delete observer observers :test test)))
    observer))

(defgeneric fire-event (event &rest arguments)
  (:documentation "Fires the given event. Returns the event.")
  (:method ((event event) &rest arguments)
    (with-slots (observers) event
      (mapcar #'(lambda (observer)
		  (apply observer arguments))
	      observers))
    event))

;;;-------------------------------------------------------------------------

(defun create-event-name (symbol)
  (intern (concatenate 'string (symbol-name symbol) "-CHANGED") (symbol-package symbol)))

;;;-------------------------------------------------------------------------