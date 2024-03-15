(in-package #:cl-user)

(defpackage #:vorigin.geometry.triangle
  (:local-nicknames
   (#:point3d #:vorigin.geometry.point3d)
   (#:u #:vutils)
   (#:v3 #:vorigin.vec3))
  (:use #:cl)
  (:export
   #:triangle
   #:triangle-p))

(in-package #:vorigin.geometry.triangle)

(declaim (inline %triangle))
(defstruct (triangle
            (:copier nil)
            (:constructor %triangle (a b c))
            (:conc-name nil))
  (a (point3d:point) :type point3d:point)
  (b (point3d:point) :type point3d:point)
  (c (point3d:point) :type point3d:point))

(u:fn-> triangle (&optional point3d:point point3d:point point3d:point) triangle)
(declaim (inline triangle))
(defun triangle (&optional (a (point3d:point)) (b (point3d:point)) (c (point3d:point)))
  (declare (optimize speed))
  (%triangle a b c))
