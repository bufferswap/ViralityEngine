(in-package #:cl-user)

;;; 3D points are just type aliased to be vec3, with a convenience constructor function.

(defpackage #:vorigin.geometry.point3d
  (:local-nicknames
   (#:u #:vutils)
   (#:v3 #:vorigin.vec3)
   (#:v4 #:vorigin.vec4)
   (#:m4 #:vorigin.mat4))
  (:use #:cl)
  (:import-from
   #:vorigin.vec3
   #:x
   #:y
   #:z)
  (:export
   #:distance
   #:distance-squared
   #:find-min-max
   #:point
   #:translate
   #:unproject
   #:x
   #:y
   #:z))

(in-package #:vorigin.geometry.point3d)

(deftype point () 'v3:vec)

(u:fn-> point (&optional u:f32 u:f32 u:f32) point)
(declaim (inline point))
(defun point (&optional (x 0f0) (y 0f0) (z 0f0))
  (declare (optimize speed))
  (v3:vec x y z))

(u:fn-> translate (point v3:vec u:f32) point)
(declaim (inline translate))
(defun translate (point direction distance)
  (declare (optimize speed))
  (v3:+ point (v3:scale direction distance)))

(u:fn-> distance-squared (point point) u:f32)
(declaim (inline distance-squared))
(defun distance-squared (point1 point2)
  (declare (optimize speed))
  (v3:length-squared (v3:- point2 point1)))

(u:fn-> distance (point point) u:f32)
(declaim (inline distance))
(defun distance (point1 point2)
  (declare (optimize speed))
  (sqrt (distance-squared point1 point2)))

(u:fn-> unproject (point m4:mat m4:mat v4:vec) point)
(declaim (inline unproject))
(defun unproject (point model projection viewport)
  (declare (optimize speed))
  (u:mvlet ((out (point))
            (inverse-pm success-p (m4:invert (m4:* projection model))))
    (unless success-p
      (return-from unproject out))
    (m4:with-components ((m inverse-pm))
      (v3:with-components ((p point)
                           (out out))
        (v4:with-components ((v viewport)
                             (o (v4:zero))
                             (i (v4:vec (1- (/ (* (- px vx) 2f0) vz))
                                        (1- (/ (* (- py vy) 2f0) vw))
                                        (1- (* pz 2f0))
                                        1f0)))
          (m4:*v4! o m i)
          (when (zerop ow)
            (return-from unproject out))
          (v3:scale! out (v3:vec (v4:x o) (v4:y o) (v4:z o)) (/ ow)))))
    (values out t)))

(u:fn-> find-min-max (simple-vector) (values point point))
(defun find-min-max (points)
  "Find the minimum and maximum points (extents) of a 3D point cloud vector."
  (declare (optimize speed)
           (simple-vector points))
  (let ((min (v3:uniform most-positive-single-float))
        (max (v3:uniform most-negative-single-float)))
    (dotimes (i (length points))
      (let ((x (svref points i)))
        (v3:min! min min x)
        (v3:max! max max x)))
    (values min max)))
