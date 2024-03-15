(in-package #:cl-user)

;;;; A 2D circle primitive.

(defpackage #:vorigin.geometry.circle
  (:local-nicknames
   (#:point2d #:vorigin.geometry.point2d)
   (#:u #:vutils))
  (:use #:cl)
  (:export
   #:circle
   #:circle-p
   #:origin
   #:radius))

(in-package #:vorigin.geometry.circle)

(declaim (inline %circle))
(defstruct (circle
            (:copier nil)
            (:constructor %circle)
            (:conc-name nil))
  (origin (point2d:point) :type point2d:point)
  (radius 1f0 :type u:f32))

(u:fn-> circle (&key (:origin point2d:point) (:radius u:f32)) circle)
(declaim (inline circle))
(defun circle (&key (origin (point2d:point)) (radius 1f0))
  (declare (optimize speed))
  (%circle :origin origin :radius radius))
