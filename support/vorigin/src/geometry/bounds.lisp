(in-package #:vorigin.geometry)

;;;; This file defines a package for constructing minimum bounding shapes.

;;;; A 2D minimum bounding shape is a primitve circle or rect that encompasses all of a set of
;;;; points. 3D minimum bounding shapes are symmetrical except using spheres and AABBs.

;;;; Minimum bounding shapes are useful in collision detection to simplify a collision region. It is
;;;; much less expensive to test a circle, rect, or their 3D variants, than it is to test a more
;;;; complex shape, such as a point cloud or concave polygon. They are an important part of a
;;;; broad-phase collision detection system to quickly check to see if more expensive checks are
;;;; needed, before actually performing these expensive checks first.

(u:fn-> bounding-circle (simple-vector) circle:circle)
(defun bounding-circle (points)
  "Construct a minimum bounding circle that encompasses all points in the supplied vector of 2D
points."
  (declare (optimize speed))
  (let ((point-count (length points))
        (center (point2d:point)))
    (dotimes (i point-count)
      (v2:+! center center (svref points i)))
    (v2:scale! center center (/ 1f0 point-count))
    (let ((radius (point2d:distance-squared (svref points 0) center)))
      (dotimes (i point-count)
        (let ((distance (point2d:distance-squared (svref points i) center)))
          (when (> distance radius)
            (setf radius distance))))
      (circle:circle :origin center :radius (sqrt (the (u:f32 0f0) radius))))))

(u:fn-> bounding-rect (simple-vector) rect:rect)
(defun bounding-rect (points)
  "Construct a minimum bounding rect that encompasses all points in the supplied vector of 2D
points."
  (declare (optimize speed))
  (let ((min (v2:copy (svref points 0)))
        (max (v2:copy (svref points 0))))
    (dotimes (i (length points))
      (let ((point (svref points i)))
        (v2:min! min point min)
        (v2:max! max point max)))
    (rect:rect-from-min/max :min min :max max)))
