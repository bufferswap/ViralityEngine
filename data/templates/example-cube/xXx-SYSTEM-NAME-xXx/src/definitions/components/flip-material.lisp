(in-package #:xXx-SYSTEM-NAME-xXx)

;; Every interval seconds, change the renderer to the next material in the
;; array, wrapping to the beginning when the end is reached.
(v:define-component flip-material ()
  ((%renderer :accessor renderer
              :initarg :renderer
              :initform nil)
   (%material-array :accessor material-array
                    :initarg :material-array
                    :initform (vector 'x:missing-material))
   (%curr-idx :accessor curr-idx
              :initarg :curr-idx
              :initform 0)
   (%flip-hz :accessor flip-hz
             :initarg :flip-hz
             :initform 1f0)
   (%time-accumulated :accessor time-accumulated
                      :initarg :time-accumulated
                      :initform 0f0)))
