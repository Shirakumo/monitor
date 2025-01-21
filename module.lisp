(in-package #:modularize-user)
(define-module #:monitor
  (:use #:cl #:radiance)
  (:export)
  (:local-nicknames))
(in-package #:monitor)

(define-trigger startup ()
  (defaulted-config (make-random-string 32) :private-key))
