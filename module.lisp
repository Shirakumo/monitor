(in-package #:modularize-user)
(define-module #:monitor
  (:use #:cl #:radiance)
  (:export)
  (:local-nicknames
   (#:measurements #:org.shirakumo.machine-state.measurements)
   (#:machine-state #:org.shirakumo.machine-state)
   (#:precise-time #:org.shirakumo.precise-time)))
(in-package #:monitor)

(define-trigger startup ()
  (defaulted-config (make-random-string 32) :private-key))
