(in-package #:cl-user)

(defpackage #:vorigin.geometry
  (:local-nicknames
   (#:aabb #:vorigin.geometry.aabb)
   (#:com #:vorigin.common)
   (#:circle #:vorigin.geometry.circle)
   (#:line2d #:vorigin.geometry.line2d)
   (#:line3d #:vorigin.geometry.line3d)
   (#:m2 #:vorigin.mat2)
   (#:m3 #:vorigin.mat3)
   (#:obb #:vorigin.geometry.obb)
   (#:orect #:vorigin.geometry.oriented-rect)
   (#:plane #:vorigin.geometry.plane)
   (#:point2d #:vorigin.geometry.point2d)
   (#:point3d #:vorigin.geometry.point3d)
   (#:ray #:vorigin.geometry.ray)
   (#:rect #:vorigin.geometry.rect)
   (#:shape-set-2d #:vorigin.geometry.shape-set-2d)
   (#:sphere #:vorigin.geometry.sphere)
   (#:u #:vutils)
   (#:v2 #:vorigin.vec2)
   (#:v3 #:vorigin.vec3))
  (:use #:cl)
  ;; bounding shapes
  (:export
   #:bounding-circle
   #:bounding-rect)
  ;; 2D intersection tests
  (:export
   #:circle/circle
   #:circle/line2d
   #:circle/oriented-rect
   #:circle/point2d
   #:circle/rect
   #:circle/shape-set-2d
   #:line2d/circle
   #:line2d/oriented-rect
   #:line2d/point2d
   #:line2d/rect
   #:line2d/shape-set-2d
   #:oriented-rect/circle
   #:oriented-rect/line2d
   #:oriented-rect/oriented-rect
   #:oriented-rect/point2d
   #:oriented-rect/rect
   #:oriented-rect/shape-set-2d
   #:point2d/circle
   #:point2d/line2d
   #:point2d/oriented-rect
   #:point2d/point2d
   #:point2d/rect
   #:point2d/shape-set-2d
   #:rect/circle
   #:rect/line2d
   #:rect/oriented-rect
   #:rect/point2d
   #:rect/rect
   #:rect/shape-set-2d
   #:shape-set-2d/circle
   #:shape-set-2d/line2d
   #:shape-set-2d/oriented-rect
   #:shape-set-2d/point2d
   #:shape-set-2d/rect
   #:shape-set-2d/shape-set-2d)
  ;; 3D intersection tests
  (:export
   #:aabb/aabb
   #:aabb/obb
   #:aabb/plane
   #:aabb/point3d
   #:aabb/sphere
   #:line3d/point3d
   #:obb/aabb
   #:obb/obb
   #:obb/plane
   #:obb/point3d
   #:obb/sphere
   #:plane/aabb
   #:plane/obb
   #:plane/plane
   #:plane/point3d
   #:plane/sphere
   #:point3d/aabb
   #:point3d/line3d
   #:point3d/obb
   #:point3d/plane
   #:point3d/point3d
   #:point3d/ray
   #:point3d/sphere
   #:ray/point3d
   #:sphere/aabb
   #:sphere/obb
   #:sphere/plane
   #:sphere/point3d
   #:sphere/sphere)
  ;; 3D closest point tests
  (:export
   #:closest-point-aabb
   #:closest-point-line3d
   #:closest-point-obb
   #:closest-point-plane
   #:closest-point-ray
   #:closest-point-sphere)
  ;; 3D raycast tests
  (:export
   #:raycast-aabb
   #:raycast-sphere))
