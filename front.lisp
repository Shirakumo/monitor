(in-package #:monitor)

(defun alert-url (alert)
  (uri-to-url (format NIL "monitor/alert/~a" (dm:id (ensure-alert alert)))
              :representation :external))

(defun series-url (series)
  (uri-to-url (format NIL "monitor/series/~a" (dm:field (ensure-series series) "title"))
              :representation :external))

(defun time? (var)
  (when (and var (string/= var ""))
    (org.shirakumo.fuzzy-dates:parse var)))

(define-page dashboard "monitor/^$" (:access (perm monitor)) :clip "dashboard.ctml"
  (r-clip:process T :series (list-series)
                    :alerts (list-alerts)))

(define-page series "monitor/^series/(.*)$" (:uri-groups (series) :access (perm monitor) :clip "series.ctml")
  (let ((series (ensure-series series)))
    (r-clip:process T :series alert
                      :measurements (list-measurements series
                                                       :since (time? (post/get "since"))
                                                       :before (time? (post/get "before"))))))

(define-page alert "monitor/^alert/(.*)$" (:uri-groups (alert) :access (perm monitor) :clip "alert.ctml")
  (let ((alert (ensure-alert alert)))
    (r-clip:process T :alert alert
                      :subscriptions (list-subscriptions alert))))
