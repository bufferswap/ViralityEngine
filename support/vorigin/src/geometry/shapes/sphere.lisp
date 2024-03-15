(in-package #:cl-user)

;;;; A 3D sphere primitive.

(defpackage #:vorigin.geometry.sphere
  (:local-nicknames
   (#:point3d #:vorigin.geometry.point3d)
   (#:u #:vutils))
  (:use #:cl)
  (:export
   #:origin
   #:radius
   #:sphere))

(in-package #:vorigin.geometry.sphere)

(declaim (inline %sphere))
(defstruct (sphere
            (:copier nil)
            (:constructor %sphere)
            (:conc-name nil))
  (origin (point3d:point) :type point3d:point)
  (radius 1f0 :type u:f32))

(u:fn-> sphere (&key (:origin point3d:point) (:radius u:f32)) sphere)
(declaim (inline sphere))
(defun sphere (&key (origin (point3d:point)) (radius 1f0))
  (declare (optimize speed))
  (%sphere :origin origin :radius radius))
