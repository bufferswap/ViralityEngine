(in-package #:cl-user)

;;;; A 3D axis-aligned box primitive.

(defpackage #:vorigin.geometry.aabb
  (:local-nicknames
   (#:point3d #:vorigin.geometry.point3d)
   (#:u #:vutils)
   (#:v2 #:vorigin.vec2)
   (#:v3 #:vorigin.vec3))
  (:use #:cl)
  (:shadow
   #:max
   #:min)
  (:export
   #:aabb
   #:aabb-from-min/max
   #:aabb-p
   #:max
   #:min
   #:origin
   #:size))

(in-package #:vorigin.geometry.aabb)

(declaim (inline %aabb))
(defstruct (aabb
            (:copier nil)
            (:constructor %aabb)
            (:conc-name nil))
  (origin (point3d:point) :type point3d:point)
  (size (v3:uniform 1f0) :type v3:vec))

(u:fn-> aabb (&key (:origin point3d:point) (:size v3:vec)) aabb)
(declaim (inline aabb))
(defun aabb (&key (origin (point3d:point)) (size (v3:uniform 1f0)))
  (declare (optimize speed))
  (%aabb :origin origin :size size))

(u:fn-> aabb-from-min/max (&key (:min point3d:point) (:max point3d:point)) aabb)
(declaim (inline aabb-from-min/max))
(defun aabb-from-min/max (&key (min (point3d:point -0.5f0)) (max (point3d:point 0.5f0)))
  "Construct an AABB from a MINIMUM and MAXIMUM points. "
  (declare (optimize speed))
  (let ((origin (v3:scale (v3:+ min max) 0.5f0))
        (size (v3:scale (v3:- max min) 0.5f0)))
    (%aabb :origin origin :size size)))

(u:fn-> min (aabb) point3d:point)
(declaim (inline min))
(defun min (aabb)
  "Return the minimum point of an AABB."
  (declare (optimize speed))
  (let ((origin (origin aabb))
        (size (size aabb)))
    (v3:min (v3:+ origin size)
            (v3:- origin size))))

(u:fn-> max (aabb) point3d:point)
(declaim (inline max))
(defun max (aabb)
  "Return the maximum point of an AABB."
  (declare (optimize speed))
  (let ((origin (origin aabb))
        (size (size aabb)))
    (v3:max (v3:+ origin size)
            (v3:- origin size))))

(u:fn-> vertices (aabb) (simple-vector 8))
(declaim (inline vertices))
(defun vertices (aabb)
  "Get a vector of an AABB's vertices."
  (declare (optimize speed))
  (let* ((min (min aabb))
         (max (max aabb))
         (min-x (v3:x min))
         (min-y (v3:y min))
         (min-z (v3:z min))
         (max-x (v3:x max))
         (max-y (v3:y max))
         (max-z (v3:z max)))
    (vector min
            max
            (v3:vec min-x max-y max-z)
            (v3:vec min-x max-y min-z)
            (v3:vec min-x min-y max-z)
            (v3:vec max-x max-y min-z)
            (v3:vec max-x min-y max-z)
            (v3:vec max-x min-y min-z))))

(u:fn-> interval (aabb v3:vec) v2:vec)
(defun interval (aabb axis)
  "Get the interval of the AABB along the given AXIS. This is used to test if two intervals overlap
in a separating axis theorem test. AXIS is expected to be normalized."
  (declare (optimize speed))
  (let ((vertices (vertices aabb)))
    (v2:with-components ((r (v2:uniform (v3:dot axis (aref vertices 0)))))
      (dotimes (i 8)
        (let ((projection (v3:dot axis (aref vertices i))))
          (setf rx (u:clamp rx rx projection)
                ry (u:clamp ry projection ry))))
      r)))
