(in-package #:virality)

;;;; Implementation of the CONTEXT structure.

(defun make-context (core)
  (setf (slot-value core '%context) (make-instance 'context :core core)))

(defun run-prologue (core)
  (let ((context (context core)))
    (setf (state context) (prologue context))))

(defun run-epilogue (core)
  (epilogue (context core)))

;; NOTE: This function must have &rest arguments, but they are ignored. This is
;; because this function is shared with the material protocol which must pass
;; more than the context.
(defun total-time (context &rest ignored)
  "Return the total time in seconds that the engine has been running."
  (declare (ignore ignored))
  (float (clock-current-time (clock (core context))) 1f0))

(defun frame-time (context)
  "Return the amount of time in seconds of the last frame as a REAL."
  (float (clock-frame-time (clock (core context))) 1f0))

(defun frame-count (context)
  "Return the number of frames since the engine has started."
  (clock-frame-count (clock (core context))))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (float (clock-delta-time (clock (core context))) 1f0))

(defun screen-resolution (context &rest ignored)
  "Return a V2:VEC of the screen resolution."
  (declare (ignore context ignored))
  (v2:vec (float =window-width= 1f0)
          (float =window-height= 1f0)))

(defmethod refresh-rate (context)
  "Return the screen refresh rate."
  (refresh-rate (display (core context))))
