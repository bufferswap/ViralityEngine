(in-package #:virality)

(defclass oriented-bounding-box ()
  ((%center :reader center
            :initarg :center
            :initform (v3:zero))
   (%axes :reader axes
          :initarg :axes
          :initform (m3:zero))
   (%half-widths :reader half-widths
                 :initarg :half-widths
                 :initform (v3:zero))))
