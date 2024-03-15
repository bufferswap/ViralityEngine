(in-package #:virality)

;;;; Implementation of datatype BUTTON-STATE

(defun button-transition-in (data input)
  (symbol-macrolet ((state (u:href (states data) input)))
    (if state
        (setf (button-state-enter state) t
              (button-state-enabled state) t
              (button-state-exit state) nil)
        (setf state (make-button-state :enter t :enabled t)))
    (push input (u:href (entering data) :button))))

(defun button-transition-out (data input)
  (u:when-let ((state (u:href (states data) input)))
    (setf (button-state-enter state) nil
          (button-state-enabled state) nil
          (button-state-exit state) t)
    (push input (u:href (exiting data) :button))))

(defun button-enable-entering (data)
  (symbol-macrolet ((entering (u:href (entering data) :button)))
    (dolist (button entering)
      (let ((state (u:href (states data) button)))
        (setf (button-state-enter state) nil
              (button-state-enabled state) t
              (button-state-exit state) nil)))
    (setf entering nil)))

(defun button-disable-exiting (data)
  (symbol-macrolet ((exiting (u:href (exiting data) :button)))
    (dolist (button exiting)
      (let ((state (u:href (states data) button)))
        (setf (button-state-enter state) nil
              (button-state-enabled state) nil
              (button-state-exit state) nil)))
    (setf exiting nil)))

(defun on-button-enter (context &rest args)
  (u:when-let* ((data (input-data (core context)))
                (state (u:href (states data) args)))
    (button-state-enter state)))

(defun on-button-enabled (context &rest args)
  (u:when-let* ((data (input-data (core context)))
                (state (u:href (states data) args)))
    (button-state-enabled state)))

(defun on-button-exit (context &rest args)
  (u:when-let* ((data (input-data (core context)))
                (state (u:href (states data) args)))
    (button-state-exit state)))
