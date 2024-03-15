(in-package #:virality)

;;;; Implementation of datatype INPUT-DATA

(defun make-input-data (core)
  (let ((input-data (make-instance 'input-data))
        (motion-state (make-mouse-motion-state)))
    (setf (u:href (states input-data) '(:mouse :motion)) motion-state
          (slot-value core '%input-data) input-data)))
