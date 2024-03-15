(in-package #:virality)

(global-vars:define-global-var =thread-pool= nil)

;; Implementation of THREAD-POOL

(defun make-thread-pool ()
  (let* ((worker-count (or =threads= =cpu-count=))
         (thread-pool (make-instance 'thread-pool :worker-count worker-count)))
    (setf lparallel:*kernel* (lparallel:make-kernel worker-count)
          =thread-pool= thread-pool)))

(defun destroy-thread-pool ()
  (lparallel:end-kernel)
  (when =thread-pool=
    (setf lparallel:*kernel* nil
          =thread-pool= nil)))

(defun ensure-channel (purpose)
  (let ((channels (channels =thread-pool=)))
    (u:ensure-gethash purpose channels (lparallel:make-channel))))

(defun ensure-queue (purpose)
  (let ((queues (queues =thread-pool=)))
    (u:ensure-gethash purpose queues (lparallel.queue:make-queue))))

(defun submit-job (purpose job &optional (priority :default))
  (when =thread-pool=
    (let ((channel (ensure-channel purpose))
          (lparallel:*task-priority* priority)
          (lparallel:*task-category* purpose))
      (lparallel:submit-task channel job))))

(defun get-job-results (purpose)
  (when =thread-pool=
    (let ((channel (ensure-channel purpose)))
      (lparallel:receive-result channel))))

(defun kill-jobs (purpose)
  (lparallel:kill-tasks purpose))

(defun enqueue (purpose data)
  (when =thread-pool=
    (let ((queue (ensure-queue purpose)))
      (lparallel.queue:push-queue data queue))))

(defun dequeue (purpose)
  (when =thread-pool=
    (let ((queue (ensure-queue purpose)))
      (lparallel.queue:pop-queue queue))))

(defun queue-empty-p (purpose)
  (let ((queue (ensure-queue purpose)))
    (lparallel.queue:queue-empty-p queue)))

;;; TODO: The below functions are not fully baked yet. The live recompilation
;;; queue needs some work to make it more generic and easier to work with. See
;;; how I did it in Pyx when it comes time to fix this later. ~axion 4/8/2020

(defun push-queue (purpose data)
  (when =thread-pool=
    (let ((queue (ensure-queue purpose)))
      (lparallel.queue:push-queue data queue))))

(defun pop-queue (purpose)
  (when =thread-pool=
    (let ((queue (ensure-queue purpose)))
      (unless (lparallel.queue:queue-empty-p queue)
        (let ((result (lparallel.queue:pop-queue queue)))
          (values result t))))))

(defun process-queue (purpose)
  (u:while (and =thread-pool= (not (queue-empty-p purpose)))
    (destructuring-bind (&optional event-type data) (dequeue purpose)
      (when event-type
        (handle-queued-event purpose event-type data)))))

(defgeneric handle-queued-event (purpose event-type data)
  (:method (purpose event-type data)
    (error "Unhandled queue event type ~s for queue purpose ~a."
           event-type purpose)))
