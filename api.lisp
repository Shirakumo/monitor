(in-package #:monitor)

(defun api-output* (data &optional message url-format &rest url-args)
  (let ((target (when url-format
                  (uri-to-url (if (stringp url-format)
                                  (apply #'format NIL url-format url-args)
                                  url-format)
                              :representation :external
                              :query `(("message" . ,message))))))
    (if (and target (string= "true" (post/get "browser")))
        (redirect target)
        (api-output data :message (or message "Ok.") :target target))))

(define-api monitor/series (id) (:access (perm monitor))
  (api-output* (ensure-series id)))

(define-api monitor/series/list () (:access (perm monitor))
  (api-output* (list-series)))

(define-api monitor/series/new (type &optional machine title interval argument[]) (:access (perm monitor))
  (let ((series (add-series type :title (or* title (string-downcase type))
                                 :machine (or* machine (config :machine))
                                 :interval (parse-float:parse-float (or* interval "1.0"))
                                 :arguments (loop for argument in argument[]
                                                  when (string/= "" argument)
                                                  collect (read-from-string argument)))))
    (api-output* series
                 "Series created."
                 "monitor/series/~a" (dm:field series "title"))))

(define-api monitor/series/edit (id &optional machine title interval argument[]) (:access (perm monitor))
  (let ((series (edit-series id :title (or* title)
                                :machine (or* machine)
                                :interval (if (or* interval) (parse-float:parse-float interval))
                                :arguments (loop for argument in argument[]
                                                 when (string/= "" argument)
                                                 collect (read-from-string argument)))))
    (api-output* series
                 "Series updated."
                 "monitor/series/~a" (dm:field series "title"))))

(define-api monitor/series/remove (id) (:access (perm monitor))
  (remove-series id)
  (api-output* NIL
               "Series deleted."
               "monitor/"))

(define-api monitor/series/data (id &optional since before) (:access (perm monitor))
  (let* ((series (ensure-series id))
         (datapoints (list-measurements series :since (time? since) :before (time? before)))
         (data (make-hash-table :test 'equal))
         (type-info (elt *series-type-map* (dm:field series "type")))
         (times (make-array (length datapoints)))
         (values (make-array (length times))))
    (loop for i from 0
          for point in datapoints
          do (setf (aref times i) (gethash "time" point))
             (setf (aref values i) (gethash "value" point)))
    (setf (gethash "title" data) (second type-info))
    (setf (gethash "unit" data) (fourth type-info))
    (setf (gethash "data" data) (vector times values))
    (api-output* data)))

(define-api monitor/alert (id) (:access (perm monitor))
  (api-output* (ensure-alert id)))

(define-api monitor/alert/list () (:access (perm monitor))
  (api-output* (list-alerts)))

(define-api monitor/alert/new (series threshold &optional above title duration subscribers[]) (:access (perm monitor))
  (api-output* (add-alert series (* (parse-float:parse-float threshold)
                                    (if (string-equal above "T") +1.0 -1.0))
                         :title title
                         :duration (parse-float:parse-float (or* duration "0.0"))
                         :subscribers subscribers[])
              "Alert created."
               "monitor/"))

(define-api monitor/alert/edit (id &optional threshold above title duration subscribers[]) (:access (perm monitor))
  (api-output* (edit-alert id
                           :title (or* title)
                           :threshold (when (or* threshold)
                                        (* (parse-float:parse-float threshold)
                                           (if (string-equal above "T") +1.0 -1.0)))
                           :duration (when (or* duration) (parse-float:parse-float duration))
                           :subscribers subscribers[])
               "Alert updated."
               "monitor/alert/~a" id))

(define-api monitor/alert/remove (id) (:access (perm monitor))
  (remove-alert id)
  (api-output* NIL
               "Alert deleted."
               "monitor/"))

(define-api monitor/alert/subscribe (id email &optional name) (:access (perm monitor))
  (let ((alert (ensure-alert id)))
    (add-subscription alert email (or* name email))
    (api-output* NIL
                 "Subscription created."
                 "monitor/alert/~a" (dm:id alert))))

(define-api monitor/alert/unsubscribe (id email) (:access (perm monitor))
  (let ((alert (ensure-alert id)))
    (remove-subscription alert email)
    (api-output* NIL
                 "Subscription deleted."
                 "monitor/alert/~a" (dm:id alert))))

(define-api monitor/alert/test (id) (:access (perm monitor))
  (let ((alert (ensure-alert id)))
    (send-alerts alert ())
    (api-output* NIL
                 "Alert sent."
                 "monitor/alert/~a" (dm:id alert))))
