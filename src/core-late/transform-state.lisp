(in-package #:virality)

;; Implementation for the defstruct TRANSFORM-STATE.

(defun make-translate-state ()
  (make-transform-state
   :previous (v3:zero)
   :current (v3:zero)
   :incremental (v3:zero)
   :incremental-delta (v3:zero)
   :interpolated (v3:zero)))

(defun make-rotate-state ()
  (make-transform-state
   :previous (q:id)
   :current (q:id)
   :incremental (v3:zero)
   :incremental-delta (q:id)
   :interpolated (q:id)))

(defun make-scale-state ()
  (make-transform-state
   :previous (v3:ones)
   :current (v3:ones)
   :incremental (v3:zero)
   :incremental-delta (v3:zero)
   :interpolated (v3:zero)))

(defun initialize-translation (state &optional initial velocity)
  (when initial
    (setf (transform-state-current state) initial
          (transform-state-previous state)
          (v3:copy (transform-state-current state))))
  (when velocity
    (setf (transform-state-incremental state) velocity)))

(defun initialize-rotation (state &optional initial velocity)
  (when initial
    (setf (transform-state-current state) initial
          (transform-state-previous state)
          (q:copy (transform-state-current state))))
  (when velocity
    (setf (transform-state-incremental state) velocity)))

(defun initialize-scale (state &optional initial velocity)
  (when initial
    (setf (transform-state-current state)
	  (etypecase initial
	    (v3:vec initial)
	    (real (v3:uniform initial)))
          (transform-state-previous state)
          (v3:copy (transform-state-current state))))
  (when velocity
    (setf (transform-state-incremental state) velocity)))

(defun interpolate-vector (state factor)
  (v3:lerp! (transform-state-interpolated state)
            (transform-state-previous state)
            (transform-state-current state) factor))

(defun interpolate-quaternion (state factor)
  (q:slerp! (transform-state-interpolated state)
            (transform-state-previous state)
            (transform-state-current state) factor))

(defun transform-node/vector (state delta &optional (func #'identity))
  (v3:copy! (transform-state-previous state) (transform-state-current state))
  (v3:scale! (transform-state-incremental-delta state)
             (transform-state-incremental state) delta)
  (v3:+! (transform-state-current state)
         (transform-state-current state)
         (funcall func (transform-state-incremental-delta state))))

(defun transform-node/quaternion (state delta)
  (q:copy! (transform-state-previous state) (transform-state-current state))
  (q:from-velocity! (transform-state-incremental-delta state)
                    (transform-state-incremental state) delta)
  (q:rotate! (transform-state-current state)
             (transform-state-current state)
             (transform-state-incremental-delta state)))
