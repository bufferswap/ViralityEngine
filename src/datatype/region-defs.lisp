(in-package #:virality)

(defclass region ()
  ((%center :accessor center
            :initarg :center
            :initform (v3:zero))))

(defclass region-cuboid (region)
  ((%minx :accessor minx
          :initarg :minx
          :initform -.5f0)
   (%maxx :accessor maxx
          :initarg :maxx
          :initform .5f0)
   (%miny :accessor miny
          :initarg :miny
          :initform -.5f0)
   (%maxy :accessor maxy
          :initarg :maxy
          :initform .5f0)
   (%minz :accessor minz
          :initarg :minz
          :initform -.5f0)
   (%maxz :accessor maxz
          :initarg :maxz
          :initform .5f0)))

(defclass region-sphere (region)
  ;; A specific type to make math faster when it is KNOWN one is using a sphere
  ;; for something.
  ((%radius :accessor radius
            :initarg :radius
            :initform 1f0)))

(defclass region-ellipsoid (region)
  ;; positive distances of each principal axis.
  ;; Can be used to make 2d or 3d circles, ellipses, spheres, spheroids, etc.
  ((%x :accessor x
       :initarg :x
       :initform 1f0)
   (%y :accessor y
       :initarg :y
       :initform 1f0)
   (%z :accessor z
       :initarg :z
       :initform 1f0)))
