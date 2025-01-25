(in-package #:monitor)

(defvar *measurements* (make-hash-table :test 'eql))
(defparameter *series-type-map*
  (map 'vector (lambda (x)
                 (or (find-symbol (string x) '#:org.shirakumo.machine-state.measurements)
                     (error "No measurement named ~a" x)))
       #(storage-io
         storage-read
         storage-write
         storage-%
         storage-free
         storage-used
         storage-total
         network-io
         network-read
         network-write
         memory-%
         memory-free
         memory-used
         memory-total
         uptime
         cpu-%
         cpu-idle
         cpu-busy
         heap-%
         heap-free
         heap-used
         heap-total
         process-busy
         process-size
         process-io
         process-read
         process-write
         gc-busy
         gpu-%
         gpu-free
         gpu-used
         gpu-busy)))

(define-trigger db:connected ()
  (db:create 'datapoints
             '((series (:id series))
               (time (:integer 5))
               (value :float))
             :indices '(series time))

  (db:create 'series
             '((title (:varchar 64))
               (interval :float)
               (type (:integer 2))
               (arguments (:varchar 256)))
             :indices '(title))

  (db:create 'alerts
             '((title (:varchar 64))
               (series (:id series))
               (threshold :float)
               (duration :float)
               (trigger-time :float)
               (last-check :float))
             :indices '(series title))

  (db:create 'alert/subscribers
             '((alert (:id alerts))
               (email (:varchar 128)))
             :indices '(alert email)))

(defun measurement->id (type)
  (or (position type *series-type-map* :test #'string-equal)
      (error "No such measurement type ~s" type)))

(defun id->measurement (id)
  (elt id *series-type-map*))

(defun load-measurement (series)
  (setf (gethash (dm:id series) *measurements*)
        (apply (id->measurement (dm:field series "type"))
               (read-from-string (dm:field series "arguments")))))

(defun list-series ()
  (dm:get 'series (db:query :all) :order '(("title" . :DESC))))

(defun ensure-series (series-ish &optional (errorp T))
  (or (typecase series-ish
        (db:id
         (dm:get-one 'series (db:query (:= '_id series-ish))))
        (dm:data-model
         (ecase (dm:collection series-ish)
           (series series-ish)
           (alerts (ensure-series (dm:field series-ish "series")))
           (datapoints (ensure-series (dm:field series-ish "series")))))
        (string
         (dm:get-one 'series (db:query (:= 'title series-ish)))))
      (when errorp (error "No such series ~a" series-ish))))

(defun add-series (type &key (title (string-downcase type)) (interval 1.0) arguments)
  (let ((series (dm:hull 'series)))
    (setf (dm:field series "title") title)
    (setf (dm:field series "interval") (float interval 1f0))
    (setf (dm:field series "type") (measurement->id type))
    (setf (dm:field series "arguments") (prin1-to-string arguments))
    (dm:insert series)
    (values series (load-measurement series))))

(defun remove-series (series)
  (let ((id (ensure-id series)))
    (db:with-transaction ()
      (db:delete 'datapoints (db:query (:= 'series id)))
      (db:delete 'alerts (db:query (:= 'series id)))
      (db:delete 'series (db:query (:= '_id id))))
    (remhash id *measurements*)))

(defun perform-measurement (series)
  (let* ((ensure-series series)
         (measurement (gethash (dm:id series) *measurements*))
         (value (measurements:measure measurement)))
    (db:insert 'datapoints `(("series" . ,(dm:id series))
                             ("time" . ,(float (precise-time:get-precise-time/double) 0f0))
                             ("value" . ,(float value 0f0))))))

(defun load-measurements ()
  (mapcar #'load-measurement (list-series)))

(defun perform-measurements ()
  (mapcar #'perform-measurement (list-series)))

(defun list-alerts ()
  (dm:get 'alert (db:query :all) :order '(("title" . :DESC))))

(defun ensure-alert (alert-ish &optional (errorp T))
  (or (typecase alert-ish
        (db:id
         (dm:get-one 'alert (db:query (:= '_id alert-ish))))
        (dm:data-model
         (ecase (dm:collection alert-ish)
           (alert alert-ish)
           (alert/subscribers (ensure-alert (dm:field alert-ish "alert")))))
        (string
         (dm:get-one 'alert (db:query (:= 'title alert-ish)))))
      (when errorp (error "No such alert ~a" alert-ish))))

(defun add-alert (series threshold &key title (duration 0.0) emails)
  (let ((alert (dm:hull 'alert)))
    (setf (dm:field alert "series") (ensure-id series))
    (setf (dm:field alert "title") (or title (dm:field (ensure-series series) "title")))
    (setf (dm:field alert "threshold") (float threshold 0f0))
    (setf (dm:field alert "duration") (float duration 0f0))
    (dm:insert alert)
    (dolist (email emails alert)
      (add-subscription alert email))))

(defun remove-alert (alert)
  (let ((id (ensure-id alert)))
    (db:with-transction ()
      (db:delete 'alert/subscribers (db:query (:= 'alert id)))
      (db:delete 'alerts (db:query (:= '_id id))))))

(defun add-subscription (alert email)
  (db:insert 'alert/subscribers `(("alert" . ,(ensure-id alert))
                                  ("email" . ,(string-downcase email)))))

(defun remove-subscription (alert email)
  (db:delete 'alert/subscribers (db:query (:and (:= alert (ensure-id alert))
                                                (:= email (string-downcase email))))))
