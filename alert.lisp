(in-package #:monitor)

(defun compile-mail (subscriber &rest args &key &allow-other-keys)
  (apply #'r-clip:process
         (@template "email.ctml")
         :subscriber (dm:field subscriber "name")
         :email (dm:field subscriber "email")
         :copyright (config :copyright)
         :software-title (config :title)
         :software-version (load-time-value (asdf:component-version (asdf:find-system :monitor)))
         args))

(defun compile-alert-mail (subscriber &rest args &key (alert (ensure-alert subscriber)) datapoints (direction :trigger-up))
  (let* ((alert (ensure-alert alert))
         (title (dm:field alert "title"))
         (series (ensure-series alert)))
    (apply #'compile-mail
           subscriber
           :time (get-universal-time)
           :alert title
           :series (dm:field series "title")
           :machine (dm:field series "machine")
           :threshold (dm:field alert "threshold")
           :duration (dm:field alert "duration")
           :trigger-time (dm:field alert "trigger-time")
           :datapoints datapoints
           :body (plump:parse
                  (ecase direction
                    (:trigger-up (@template "alert-up.ctml"))
                    (:trigger-down (@template "alert-down.ctml"))))
           :subject (ecase direction
                      (:trigger-up (format NIL "Alert for ~a!" title))
                      (:trigger-down (format NIL "Alert for ~a resolved" title)))
           :series-url (series-url series)
           :alert-url (alert-url alert)
           args)))

(defun extract-plaintext (html)
  ;; KLUDGE: This is real dumb.
  ;; FIXME: links do not get retained
  (with-output-to-string (out)
    (lquery:$ html "body" (text) (each (lambda (text) (write-line text out))))))

(defun extract-subject (html)
  (lquery:$1 html "head title" (text)))

(defun send-alert (subscriber alert datapoints &key (direction :trigger-up))
  (let* ((html (compile-alert-email subscriber alert datapoints direction))
         (text (extract-plaintext html))
         (subject (extract-subject html))
         (email (gethash subscriber "email")))
    (l:trace :monitor.alert "Sending alert mail to ~a" email)
    (mail:send email subject text :html (plump:serialize html NIL))))

(defun send-alerts (alert datapoints &rest args &key &allow-other-keys)
  (dolist (subscriber (db:select 'alert/subscribers (db:query (:= 'alert (dm:id alert)))))
    (apply #'send-alert subscriber alert datapoints args)))

(defun longest-streak (sequence check)
  ;; Compute the longest consecutive subsequence of elements that passes CHECK
  (let ((start 0) (end 0) (current-start 0))
    (flet ((end-streak (i)
             (when (< (- end start) (- i current-start))
               (setf start current-start
                     end i))))
      (loop for i from 0
            for prev = NIL then cur
            for entry in sequence
            for cur = (funcall check entry)
            do (cond ((and cur (null prev))
                      (setf current-start i))
                     ((and (null cur) prev)
                      (end-streak i)
                      (setf current-start most-positive-fixnum)))
            finally (when prev (end-streak (length sequence)))))
    (subseq sequence start end)))

(defun get-alert-streak (alert)
  (let* ((alert (ensure-alert alert))
         (min (min (or (dm:field alert "last-check") 0d0)
                   (- (precise-time:get-precise-time/double)
                      (* 2 (dm:field alert "duration")))))
         (check (if (= 1 (float-sign (dm:field alert "threshold")))
                    (let ((lo-threshold (+ (dm:field alert "threshold"))))
                      (lambda (x) (<= lo-threshold (gethash "value" x))))
                    (let ((hi-threshold (- (dm:field alert "threshold"))))
                      (lambda (x) (<= (gethash "value" x) hi-threshold)))))
         (points (db:select 'datapoints (db:query (:and (:= 'series (dm:field alert "series"))
                                                        (:<= min 'time)))
                            :sort '(("time" :ASC))))
         (streak (longest-streak points check)))
    (when (and streak
               (<= (dm:field alert "duration")
                   (- (gethash "time" (car (last streak)))
                      (gethash "time" (first streak)))))
      streak)))

(defun check-alert (alert &key (send T) (save T))
  (let* ((alert (ensure-alert alert))
         (streak (get-alert-streak alert)))
    (setf (dm:field alert "last-check") (precise-time:get-precise-time/double))
    (when send
      (cond ((and streak (not (alert-up-p alert)))
             ;; We have a streak but our trigger is not set yet, so up it.
             (send-alerts alert streak :direction :trigger-up)
             (setf (dm:field alert "trigger-time") (precise-time:get-precise-time/double)))
            ((and (not streak) (alert-up-p alert))
             ;; Streak has ended but our trigger is set, so down it.
             (send-alerts alert streak :direction :trigger-down)
             (setf (dm:field alert "trigger-time") -1d0))))
    (when save
      (dm:save alert))
    (values alert streak)))

(defun check-alerts (&rest args &key &allow-other-keys)
  (loop for alert in (list-alerts)
        collect (apply #'check-alert alert args)))
