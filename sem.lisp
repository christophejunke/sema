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
    `(macrolet ((info (tag &optional (form (list 'quote tag)))
                  `(progn (emit ,tag ,',context)
                          ,form)))
       ,@forms)))

(defmacro info (&rest args)
  (declare (ignore args))
  (error "Not inside WITH-INFO."))

(defun controller (nb-threads *standard-output* check-point wake-up)
  (with-info (:controller)
    (info :synchronize (wait-on-semaphore check-point :n nb-threads))
    (info :wake        (signal-semaphore wake-up nb-threads))
    (info :synchronize (wait-on-semaphore check-point :n nb-threads))
    (info :wake        (signal-semaphore wake-up nb-threads))))

(defun task (time)
  (lambda (*standard-output* check-point wake-up)
    (with-info (`(:task ,(thread-name *current-thread*)))
      (info `(:sleep ,time)     (sleep time))
      (info :reached-checkpoint (signal-semaphore check-point))
      (info :synchronize        (wait-on-semaphore wake-up))
      (info `(:sleep ,time)     (sleep time))
      (info :reached-checkpoint (signal-semaphore check-point))
      (info :synchronize        (wait-on-semaphore wake-up))
      (info :done))))

(defun make-threads (arguments)
  (loop for i below 10
        collect (make-thread (task (+ 0.01d0 (random 1.0d0)))
                             :name (format nil "sema/task-~a" i)
                             :arguments arguments)))

(defun main ()
  (let* ((arguments (list *standard-output*
                          (make-semaphore :name "check-point")
                          (make-semaphore :name "wake-up")))
         (threads (make-threads arguments)))
    (mapc #'join-thread (list* (make-thread #'controller
                                            :name "seam/control"
                                            :arguments (list* (length threads)
                                                              arguments))
                               threads))))

(with-open-file (*standard-output* #P"lisp.out"
                                   :direction :output
                                   :if-exists :supersede)
  (main))
