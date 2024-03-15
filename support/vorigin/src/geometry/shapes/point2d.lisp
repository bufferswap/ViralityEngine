(in-package #:cl-user)

;;; 2D points are just type aliased to be vec2, with a convenience constructor function.

(defpackage #:vorigin.geometry.point2d
  (:local-nicknames
   (#:u #:vutils)
   (#:v2 #:vorigin.vec2))
  (:use #:cl)
  (:import-from
   #:vorigin.vec2
   #:x
   #:y)
  (:export
   #:distance
   #:distance-squared
   #:find-min-max
   #:point
   #:translate
   #:x
   #:y))

(in-package #:vorigin.geometry.point2d)

(deftype point () 'v2:vec)

(u:fn-> point (&optional u:f32 u:f32) point)
(declaim (inline point))
(defun point (&optional (x 0f0) (y 0f0))
  (declare (optimize speed))
  (v2:vec x y))

(u:fn-> translate (point v2:vec u:f32) point)
(declaim (inline translate))
(defun translate (point direction distance)
  (declare (optimize speed))
  (v2:+ point (v2:scale direction distance)))

(u:fn-> distance-squared (point point) u:f32)
(declaim (inline distance-squared))
(defun distance-squared (point1 point2)
  (declare (optimize speed))
  (v2:length-squared (v2:- point2 point1)))

(u:fn-> distance (point point) u:f32)
(declaim (inline distance))
(defun distance (point1 point2)
  (declare (optimize speed))
  (sqrt (distance-squared point1 point2)))

(u:fn-> find-min-max (simple-vector) (values point point))
(defun find-min-max (points)
  "Find the minimum and maximum points (extents) of a 2D point cloud vector."
  (declare (optimize speed)
           (simple-vector points))
  (let ((min (v2:uniform most-positive-single-float))
        (max (v2:uniform most-negative-single-float)))
    (dotimes (i (length points))
      (let ((x (svref points i)))
        (v2:min! min min x)
        (v2:max! max max x)))
    (values min max)))
