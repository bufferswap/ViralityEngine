(in-package #:cl-user)

;;; NOTE: Line2d represents a 2D line segment, not an infinite line in the mathematical sense. Since
;;; line segments are so common in physics, we have chosen to use this convention (as many other
;;; game physics libraries do).

(defpackage #:vorigin.geometry.line2d
  (:local-nicknames
   (#:point2d #:vorigin.geometry.point2d)
   (#:u #:vutils)
   (#:v2 #:vorigin.vec2))
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

(in-package #:vorigin.geometry.line2d)

(declaim (inline %line))
(defstruct (line
            (:predicate nil)
            (:copier nil)
            (:constructor %line (start end))
            (:conc-name nil))
  (start (point2d:point) :type point2d:point)
  (end (point2d:point) :type point2d:point))

(u:fn-> line (&key (:start point2d:point) (:end point2d:point)) line)
(declaim (inline line))
(defun line (&key (start (point2d:point)) (end (point2d:point)))
  (declare (optimize speed))
  (%line start end))

(u:fn-> length (line) u:f32)
(declaim (inline length))
(defun length (line)
  (declare (optimize speed))
  (v2:length (v2:- (end line) (start line))))

(u:fn-> length-squared (line) u:f32)
(declaim (inline length-squared))
(defun length-squared (line)
  (declare (optimize speed))
  (v2:length-squared (v2:- (end line) (start line))))

(u:fn-> midpoint (line) point2d:point)
(declaim (inline midpoint))
(defun midpoint (line)
  (declare (optimize speed))
  (v2:lerp (start line) (end line) 0.5f0))

(u:fn-> direction (line) v2:vec)
(declaim (inline direction))
(defun direction (line)
  (declare (optimize speed))
  (v2:normalize (v2:- (end line) (start line))))
