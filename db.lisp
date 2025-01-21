(in-package #:monitor)

(define-trigger db:connected ()
  (db:create 'datapoints
             '((series (:id series))
               (time (:integer 5))
               (value :float))
             :indices '(series time))

  (db:create 'series
             '((title (:varchar 32))
               (interval (:integer 4))
               (type (:integer 1)))
             :indices '(title))

  (db:create 'series/disk
             '((series (:id series))
               (path (:varchar 256)))
             :indices '(series))

  (db:create 'series/cpu
             '((series (:id series))
               (core (:integer 4)))
             :indices '(series))

  (db:create 'series/net
             '((series (:id series))
               (device (:varchar 32)))
             :indices '(series))

  (db:create 'alerts
             '((series (:id series))
               (threshold :float)
               (streak (:integer 4))
               (email (:varchar 128)))
             :indices '(series)))

(defparameter *series-type-map*
  #(:cpu
    :ram-percentage
    :ram-used-bytes
    :ram-free-bytes
    :disk-percentage
    :disk-used-bytes
    :disk-free-bytes
    :disk-io
    :net-io
    :uptime))

(defun series-type->id (type)
  (position type *series-type-map*))

(defun id->series-type (id)
  (elt id *series-type-map*))

(defgeneric measure (type &key))

(defun decode-type-args (type dm)
  (case type
    (:cpu
     (list :core-mask (dm:field dm "core-mask")))
    ((:disk-io :disk-percentage :disk-used-bytes :disk-free-bytes)
     (list :path (dm:field dm "path")))
    (:net
     (list :device (dm:field dm "device")))))

(defmethod measure ((dm dm:data-model) &key)
  (let ((type (id->series-type (dm:field dm "type"))))
    (apply #'measure type (decode-type-args type dm))))

(defmacro define-measurement (type args &body body)
  `(defmethod measure ((type (eql ,type)) &key ,@args)
     ,@body))

(define-measurement :cpu ((core T))
  (machine-state:core-utilization core))

(define-measurement :ram-percentage ()
  (multiple-value-bind (used total) (machine-state:machine-room)
    (* 100f0 (/ used total))))

(define-measurement :ram-used-bytes ()
  (values (machine-state:machine-room)))

(define-measurement :ram-free-bytes ()
  (multiple-value-bind (used total) (machine-state:machine-room)
    (- total used)))

(define-measurement :disk-percentage ((path "/"))
  (multiple-value-bind (free total) (machine-state:storage-room path)
    (* 100f0 (/ (- total free) total))))

(define-measurement :disk-used-bytes ((path "/"))
  (multiple-value-bind (free total) (machine-state:storage-room path)
    (- total free)))

(define-measurement :disk-free-bytes ((path "/"))
  (values (machine-state:storage-room path)))

(define-measurement :disk-io ((path "/"))
  )

(define-measurement :net-io ((device "enp1s0"))
  )

(define-measurement :uptime ()
  (machine-state:machine-uptime))
