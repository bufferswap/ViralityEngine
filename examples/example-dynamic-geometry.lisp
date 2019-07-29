(in-package #:first-light.example)

;; Dynamic geometry

(fl:define-geometry-layout tile
  (:data (:format interleaved :divisor 0)
         (position :type float :count 3)
         (normal :type float :count 3)
         (uv :type float :count 3)))

(fl:define-geometry tile
  :layout tile
  :primitive :triangles
  :vertex-count 6
  :buffers
  (:data (((-0.5 -0.5 0) (0 0 1) (-1 -1 0))
          ((-0.5 0.5 0) (0 0 1) (-1 1 0))
          ((0.5 0.5 0) (0 0 1) (1 1 0))
          ((-0.5 -0.5 0) (0 0 1) (-1 -1 0))
          ((0.5 0.5 0) (0 0 1) (1 1 0))
          ((0.5 -0.5 0) (0 0 1) (1 -1 0)))))

;;; Prefabs

(fl:define-material dynamic-geometry
  (:profiles (fl.materials:u-mvp)
   :shader fl.shader.user:dynamic-geometry))

(fl:define-prefab "dynamic-geometry" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("plane")
   (fl.comp:transform :rotate/inc (q:orient :local :x pi)
                      :scale (v3:make 20 20 20))
   (fl.comp:dynamic-mesh :geometry 'tile)
   (fl.comp:render :material 'dynamic-geometry
                   :mode :dynamic-mesh)))

;;; Prefab descriptors

(fl:define-prefab-descriptor dynamic-geometry ()
  ("dynamic-geometry" fl.example:examples))