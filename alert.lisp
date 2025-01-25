(in-package #:monitor)

(defun send-alert (email alert datapoints &key (direction :trigger-up))
  ())

(defun send-alerts (alert datapoints &rest args &key &allow-other-keys)
  (dolist (email (db:select 'alert/subscribers (db:query (:= 'alert (dm:id alert)))
                            :fields '("email")))
    (apply #'send-alert (gethash "email" email) alert datapoints args)))

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
            finally (end-streak (length sequence))))
    (subseq sequence start end)))

(defun get-alert-streak (alert)
  (let* ((alert (ensure-alert alert))
         (min (min (dm:field alert "last-check")
                   (- (precise-time:get-precise-time/double)
                      (dm:field alert "duration"))))
         (check (if (= 1 (float-sign (dm:field alert "threshold")))
                    (let ((lo-threshold (+ (dm:field alert "threshold"))))
                      (lambda (x) (<= lo-threshold (dm:field x "value"))))
                    (let ((hi-threshold (- (dm:field alert "threshold"))))
                      (lambda (x) (<= (dm:field x "value") hi-threshold)))))
         (points (db:select 'datapoints (db:query (:and (:= 'series (dm:field alert "series"))
                                                        (:<= min 'time)))
                            :order '(("time" . :ASC))))
         (streak (longest-streak points check)))
    (when (and streak
               (<= (dm:field alert "duration")
                   (- (dm:field (car (last streak)) "time")
                      (dm:field (first streak) "time"))))
      streak)))

(defun check-alert (alert &key (send T) (save T))
  (let* ((alert (ensure-alert alert))
         (streak (get-alert-streak alert)))
    (setf (dm:field alert "last-check") (precise-time:get-precise-time/double))
    (when send
      (cond ((and streak (< (dm:field alert "trigger-time") 0))
             ;; We have a streak but our trigger is not set yet, so up it.
             (send-alerts alert streak :direction :trigger-up)
             (setf (dm:field alert "trigger-time") (precise-time:get-precise-time/double)))
            ((and (not streak) (< 0 (dm:field alert "trigger-time")))
             ;; Streak has ended but our trigger is set, so down it.
             (send-alerts alert streak :direction :trigger-down)
             (setf (dm:field alert "trigger-time") -1d0))))
    (when save
      (dm:save alert))
    (values alert streak)))

(defun check-alerts (&rest args &key &allow-other-keys)
  (loop for alert in (list-alerts)
        collect (apply #'check-alert alert args)))
