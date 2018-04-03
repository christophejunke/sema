(defpackage :sem
  (:use :cl :sb-thread)
  (:import-from :alexandria
                alexandria:once-only))
(in-package :sem)

(let ((mutex (make-mutex :name "protect standard output")))
  (defun emit (string &optional context)
    (with-recursive-lock (mutex)
      (format t "~&~@[~A: ~]~a~%" context string))))

(defmacro with-info ((context) &body forms)
  (once-only (context)
    `(macrolet ((info (tag &optional form)
                  `(progn (emit ,tag ,',context)
                          ,form)))
       ,@forms)))

(defmacro info (&rest args)
  (declare (ignore args))
  (error "Not inside with-info."))

(defun controller (*standard-output* check-point wake-up)
  (with-info (:controller)
    (info :sync (wait-on-semaphore check-point :n 2))
    (info :wake (signal-semaphore wake-up 2))
    (info :done)))

(defun task (time)
  (lambda (*standard-output* check-point wake-up)
    (with-info (`(:task ,(thread-name *current-thread*)))
      (info `(:sleep ,time)     (sleep time))
      (info :reached-checkpoint (signal-semaphore check-point))
      (info :synchronize        (wait-on-semaphore wake-up))
      (info :done))))

(defun main ()
  (fresh-line)
  (terpri)
  (terpri)
  (let ((arguments (list *standard-output*
                         (make-semaphore :name "check-point")
                         (make-semaphore :name "wake-up"))))
    (flet ((thread (name function)
             (make-thread function :name name :arguments arguments)))
      (map ()
           #'join-thread
           (list (thread "seam/control" 'controller)
                 (thread "sema/task-1" (task (float 3/100)))
                 (thread "sema/task-2" (task (float 2/100))))))))

(main)
