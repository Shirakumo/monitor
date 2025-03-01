(in-package #:monitor)

(defvar *task-runner* NIL)

(defun start-task-runner ()
  (when (and *task-runner*
             (bt:thread-alive-p *task-runner*))
    (error "The task runner is already running."))
  (flet ((task-runner-thunk ()
           (l:info :monitor "Starting task runner.")
           (with-simple-restart (kill "Kill the task runner")
             (unwind-protect (run-tasks)
               (l:info :monitor "Stopping task runner.")
               (setf *task-runner* NIL)))))
    (setf *task-runner* (bt:make-thread #'task-runner-thunk :name "monitor task runner"))))

(defun kill-task-runner ()
  (unless (and *task-runner*
               (bt:thread-alive-p *task-runner*))
    (error "The task runner is not running."))
  (bt:interrupt-thread *task-runner* (lambda () (invoke-restart 'kill))))

(defun run-tasks ()
  (loop (with-simple-restart (abort "Abort the task")
          (perform-measurements)
          (check-alerts))
        (sleep 0.5)))

(define-trigger (radiance:startup-done start-task-runner) ()
  (unless (and *task-runner*
               (bt:thread-alive-p *task-runner*))
    (start-task-runner)))

(define-trigger (server-shutdown stop-task-runner) ()
  (kill-task-runner))
