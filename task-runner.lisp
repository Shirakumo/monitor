(in-package #:monitor)

(defvar *task-runner* NIL)

(defun start-task-runner ()
  (when (and *task-runner*
             (bt:thread-alive-p *task-runner*))
    (error "The task runner is already running."))
  (flet ((task-runner-thunk ()
           (l:info :monitor "Starting task runner.")
           (with-simple-restart (kill "Kill the task runner")
             (unwind-protect
                  (handler-bind ((error (lambda (e)
                                          (v:debug :monitor e)
                                          (v:error :monitor "Error in task runner: ~a" e)
                                          (maybe-invoke-debugger e 'continue))))
                    (run-tasks))
               (l:info :monitor "Stopping task runner.")
               (setf *task-runner* NIL)))))
    (setf *task-runner* (bt:make-thread #'task-runner-thunk :name "monitor task runner"))))

(defun kill-task-runner ()
  (unless (and *task-runner*
               (bt:thread-alive-p *task-runner*))
    (error "The task runner is not running."))
  (bt:interrupt-thread *task-runner* (lambda () (invoke-restart 'kill))))

(defun run-tasks ()
  (let* ((series (list-series))
         (now (precise-time:get-precise-time/double))
         (interval (loop for s in series minimize (dm:field s "interval"))))
    (dolist (series series)
      (setf (dm:field series 'last-check) now))
    (loop (dolist (series series)
            (when (< (dm:field series "interval")
                     (- now (dm:field series 'last-check)))
              (with-simple-restart (continue "Abort the task")
                (perform-measurement series)
                (setf (dm:field series 'last-check) now))))
          (dolist (alert (list-alerts))
            (with-simple-restart (continue "Abort the alert")
              (check-alert alert)))
          (sleep interval)
          ;; Refresh data
          (let ((new-series (list-series)))
            (dolist (s new-series (setf series new-series))
              (let ((prior (find (dm:id s) series :key #'dm:id)))
                (setf (dm:field s 'last-check) (if prior (dm:field prior 'last-check) now)))))
          (setf interval (loop for s in series minimize (dm:field s "interval")))
          (setf now (precise-time:get-precise-time/double)))))

(define-trigger (radiance:startup-done start-task-runner) ()
  (unless (and *task-runner*
               (bt:thread-alive-p *task-runner*))
    (start-task-runner)))

(define-trigger (server-shutdown stop-task-runner) ()
  (kill-task-runner))
