(in-package #:cl-user)

(defpackage #:vorigin.geometry.plane
  (:local-nicknames
   (#:point3d #:vorigin.geometry.point3d)
   (#:u #:vutils)
   (#:v3 #:vorigin.vec3))
  (:use #:cl)
  (:export
   #:distance
   #:normal
   #:plane
   #:plane-p))

(in-package #:vorigin.geometry.plane)

(declaim (inline %plane))
(defstruct (plane
            (:copier nil)
            (:constructor %plane)
            (:conc-name nil))
  (normal (v3:vec 0f0 1f0 0f0) :type v3:vec)
  (distance 0f0 :type u:f32))

(u:fn-> plane (&key (:normal v3:vec) (:distance u:f32)) plane)
(declaim (inline plane))
(defun plane (&key (normal (v3:vec 0f0 1f0 0f0)) (distance 0f0))
  (declare (optimize speed))
  (%plane :normal (v3:normalize normal) :distance distance))
