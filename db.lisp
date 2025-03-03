(in-package #:monitor)

(defvar *measurements* (make-hash-table :test 'eql))
(defparameter *series-type-map*
  (map 'vector (lambda (x)
                 (list* (or (find-symbol (string (first x)) '#:org.shirakumo.machine-state.measurements)
                            (error "No measurement named ~a" x))
                        (rest x)))
       '((storage-io "Storage IO" "fa-hard-drive" "B")
         (storage-read "Storage Read" "fa-hard-drive" "B")
         (storage-write "Storage Write" "fa-hard-drive" "B")
         (storage-% "Storage Use" "fa-hard-drive" "%")
         (storage-free "Storage Free" "fa-hard-drive" "B")
         (storage-used "Storage Used" "fa-hard-drive" "B")
         (storage-total "Storage Total" "fa-hard-drive" "B")
         (network-io "Network IO" "fa-network-wired" "B")
         (network-read "Network Read" "fa-network-wired" "B")
         (network-write "Network Write" "fa-network-wired" "B")
         (memory-% "Memory Use" "fa-memory" "%")
         (memory-free "Memory Free" "fa-memory" "B")
         (memory-used "Memory Used" "fa-memory" "B")
         (memory-total "Memory Total" "fa-memory" "B")
         (uptime "Uptime" "fa-heart" "s")
         (cpu-% "CPU Use" "fa-microchip" "%")
         (cpu-idle "CPU Idle" "fa-microchip" "s")
         (cpu-busy "CPU Busy" "fa-microchip" "s")
         (heap-% "Heap Use" "fa-user" "%")
         (heap-free "Heap Free" "fa-user" "B")
         (heap-used "Heap Used" "fa-user" "B")
         (heap-total "Heap Total" "fa-user" "B")
         (process-busy "Process Busy" "fa-user" "s")
         (process-size "Process Size" "fa-user" "s")
         (process-io "Process IO" "fa-user" "B")
         (process-read "Process Read" "fa-user" "B")
         (process-write "Process Write" "fa-user" "B")
         (gc-busy "GC Busy" "fa-user" "s")
         (gpu-% "GPU Use" "fa-cube" "%")
         (gpu-free "GPU Free" "fa-cube" "B")
         (gpu-used "GPU Used" "fa-cube" "B")
         (gpu-busy "GPU Busy" "fa-cube" "s")
         (battery-% "Battery" "fa-battery-half" "%")
         (battery-charge "Battery Charge" "fa-battery-half" "W"))))

(define-trigger db:connected ()
  (db:create 'series
             '((title (:varchar 64))
               (machine (:varchar 64))
               (interval :float)
               (type (:integer 2))
               (arguments (:varchar 256)))
             :indices '(title))

  (db:create 'datapoints
             '((series (:id series))
               (time :float)
               (value :float))
             :indices '(series time))

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
               (name (:varchar 64))
               (email (:varchar 128)))
             :indices '(alert email)))

(defun measurement->id (type)
  (or (position type *series-type-map* :key #'first :test #'string-equal)
      (error "No such measurement type ~s" type)))

(defun id->measurement (id)
  (first (elt *series-type-map* id)))

(defun load-measurement (series)
  (setf (gethash (dm:id series) *measurements*)
        (apply (id->measurement (dm:field series "type"))
               (when (string/= "" (dm:field series "arguments"))
                 (read-from-string (dm:field series "arguments"))))))

(defun id->unit (id)
  (fourth (elt *series-type-map* id)))

(defun id->icon (id)
  (third (elt *series-type-map* id)))

(defun list-types ()
  (coerce *series-type-map* 'list))

(defun ensure-id (thing)
  (typecase thing
    (db:id thing)
    (dm:data-model (dm:id thing))
    (T (db:ensure-id thing))))

(defun list-series (&key local)
  (dm:get 'series (if local
                      (db:query (:= 'machine (config :machine)))
                      (db:query :all))
          :sort '(("title" :DESC))))

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
         (or (dm:get-one 'series (db:query (:= 'title series-ish)))
             (ensure-series (db:ensure-id series-ish) errorp))))
      (when errorp (error "No such series ~a" series-ish))))

(defun add-series (type &key title (interval 1.0) arguments (machine (config :machine)))
  (db:with-transaction ()
    (let ((series (dm:hull 'series)))
      (setf (dm:field series "title") (or* title (format NIL "~(~a~) ~a" type machine)))
      (setf (dm:field series "interval") (float interval 1f0))
      (setf (dm:field series "type") (measurement->id type))
      (setf (dm:field series "arguments") (prin1-to-string arguments))
      (setf (dm:field series "machine") machine)
      (dm:insert series)
      (load-measurement series)
      (perform-measurement series)
      series)))

(defun edit-series (series &key title interval arguments machine)
  (db:with-transaction ()
    (let ((series (ensure-series series)))
      (when title (setf (dm:field series "title") title))
      (when machine (setf (dm:field series "machine") machine))
      (when interval (setf (dm:field series "interval") (float interval 1f0)))
      (when arguments (setf (dm:field series "arguments") (prin1-to-string arguments)))
      (dm:save series)
      series)))

(defun remove-series (series)
  (let ((id (ensure-id series)))
    (db:with-transaction ()
      (db:remove 'datapoints (db:query (:= 'series id)))
      (db:remove 'alerts (db:query (:= 'series id)))
      (db:remove 'series (db:query (:= '_id id))))
    (remhash id *measurements*)))

(defun perform-measurement (series)
  (let* ((series (ensure-series series))
         (measurement (or (gethash (dm:id series) *measurements*)
                          (load-measurement series))))
    (db:insert 'datapoints `(("series" . ,(dm:id series))
                             ("time" . ,(precise-time:get-precise-time/double))
                             ("value" . ,(float (measurements:measure measurement) 0d0))))))

(defun load-measurements ()
  (mapcar #'load-measurement (list-series)))

(defun perform-measurements ()
  (mapcar #'perform-measurement (list-series)))

(defun list-measurements (series &key since count before)
  (let* ((series (ensure-series series))
         (before (or before (+ (precise-time:get-precise-time/double) 10.0)))
         (since (or since (- before (* (or count (* 60 60 24)) (dm:field series "interval"))))))
    (db:select 'datapoints (db:query (:and (:= 'series (dm:id series))
                                           (:<= (float since 0d0) 'time)
                                           (:<= 'time (float before 0d0))))
               :sort '(("time" :ASC))
               :amount count)))

(defun last-measurement (series)
  (let ((value (first (db:select 'datapoints (db:query (:= 'series (dm:id (ensure-series series))))
                                 :sort '(("time" :DESC))
                                 :amount 1))))
    (when value
      (values (gethash "value" value)
              (gethash "time" value)))))

(defun list-alerts ()
  (dm:get 'alerts (db:query :all) :sort '(("title" :DESC))))

(defun ensure-alert (alert-ish &optional (errorp T))
  (or (typecase alert-ish
        (db:id
         (dm:get-one 'alerts (db:query (:= '_id alert-ish))))
        (dm:data-model
         (ecase (dm:collection alert-ish)
           (alerts alert-ish)
           (alert/subscribers (ensure-alert (dm:field alert-ish "alert")))))
        (string
         (or (dm:get-one 'alerts (db:query (:= 'title alert-ish)))
             (ensure-alert (db:ensure-id alert-ish) errorp))))
      (when errorp (error "No such alert ~a" alert-ish))))

(defun add-alert (series threshold &key title (duration 0.0) subscribers)
  (let ((alert (dm:hull 'alerts)))
    (setf (dm:field alert "series") (ensure-id series))
    (setf (dm:field alert "title") (or* title (dm:field (ensure-series series) "title")))
    (setf (dm:field alert "threshold") (float threshold 0f0))
    (setf (dm:field alert "duration") (float duration 0f0))
    (setf (dm:field alert "trigger-time") -1.0)
    (setf (dm:field alert "last-check") 0.0)
    (dm:insert alert)
    (dolist (subscriber subscribers alert)
      (destructuring-bind (&optional email name) (destructure-subscriber subscriber)
        (when email
          (add-subscription alert email name))))))

(defun edit-alert (alert &key threshold title duration (subscribers NIL subscribers-p))
  (db:with-transaction ()
    (let ((alert (ensure-alert alert)))
      (when threshold (setf (dm:field alert "threshold") (float threshold 0f0)))
      (when duration (setf (dm:field alert "duration") (float duration 0f0)))
      (when (or* title) (setf (dm:field alert "title") title))
      (when subscribers-p
        (db:remove 'alert/subscribers (db:query (:= 'alert (dm:id alert))))
        (dolist (subscriber subscribers alert)
          (destructuring-bind (&optional email name) (destructure-subscriber subscriber)
            (when email
              (add-subscription alert email name)))))
      (dm:save alert))))

(defun alert-up-p (alert)
  (< 0 (or (dm:field (ensure-alert alert) "trigger-time") 0)))

(defun remove-alert (alert)
  (let ((id (ensure-id alert)))
    (db:with-transaction ()
      (db:remove 'alert/subscribers (db:query (:= 'alert id)))
      (db:remove 'alerts (db:query (:= '_id id))))))

(defun list-subscriptions (alert)
  (dm:get 'alert/subscribers (db:query (:= 'alert (dm:id (ensure-alert alert))))
          :sort '(("name" :DESC))))

(defun destructure-subscriber (subscriber)
  (let ((subscriber (string-trim " " subscriber)))
    (when (string/= "" subscriber)
      (or (cl-ppcre:register-groups-bind (name email) ("(.*?)<([^>]+)>" subscriber)
            (let ((name (string-trim " " name)))
              (list email (when (string/= "" name) name))))
          (list subscriber)))))

(defun add-subscription (alert email &optional name)
  (db:insert 'alert/subscribers `(("alert" . ,(ensure-id alert))
                                  ("name" . ,name)
                                  ("email" . ,(string-downcase email)))))

(defun remove-subscription (alert email)
  (db:remove 'alert/subscribers (db:query (:and (:= alert (ensure-id alert))
                                                (:= email (string-downcase email))))))
