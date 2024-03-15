(in-package #:cl-user)

;;; NOTE: Line3d represents a 3D line segment, not an infinite line in the mathematical sense. Since
;;; line segments are so common in physics, we have chosen to use this convention (as many other
;;; game physics libraries do).

(defpackage #:vorigin.geometry.line3d
  (:local-nicknames
   (#:point3d #:vorigin.geometry.point3d)
   (#:u #:vutils)
   (#:v3 #:vorigin.vec3))
  (:use #:cl)
  (:shadow
   #:length)
  (:export
   #:direction
   #:end
   #:length
   #:length-squared
   #:line
   #:midpoint
   #:start))

(in-package #:vorigin.geometry.line3d)

(declaim (inline %line))
(defstruct (line
            (:predicate nil)
            (:copier nil)
            (:constructor %line (start end))
            (:conc-name nil))
  (start (point3d:point) :type point3d:point)
  (end (point3d:point) :type point3d:point))

(u:fn-> line (&key (:start point3d:point) (:end point3d:point)) line)
(declaim (inline line))
(defun line (&key (start (point3d:point)) (end (point3d:point)))
  (declare (optimize speed))
  (%line start end))

(u:fn-> length (line) u:f32)
(declaim (inline length))
(defun length (line)
  (declare (optimize speed))
  (v3:length (v3:- (end line) (start line))))

(u:fn-> length-squared (line) u:f32)
(declaim (inline length-squared))
(defun length-squared (line)
  (declare (optimize speed))
  (v3:length-squared (v3:- (end line) (start line))))

(u:fn-> midpoint (line) point3d:point)
(declaim (inline midpoint))
(defun midpoint (line)
  (declare (optimize speed))
  (v3:lerp (start line) (end line) 0.5f0))

(u:fn-> direction (line) v3:vec)
(declaim (inline direction))
(defun direction (line)
  (declare (optimize speed))
  (v3:normalize (v3:- (end line) (start line))))
