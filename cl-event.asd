;;; -*- Mode: Lisp; -*-

(defpackage #:cl-event-asdf
  (:use :common-lisp :asdf))

(in-package #:cl-event-asdf)

(defsystem :cl-event
  :description "A simple event framework."
  :version "10.07"
  :author "Rudy Neeser <rudy.neeser@gmail.com>"
  :depends-on ("utility")
  :serial t
  :components ((:file "package")
	       (:file "event")
	       (:file "macros")))
