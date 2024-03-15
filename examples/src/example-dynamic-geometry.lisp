(in-package #:virality-examples)

;; Dynamic geometry

(v:define-geometry tile ()
  (:layout x:2d
   :vertex-count 4
   :primitive :triangle-strip
   :buffers
   (:data (((-0.5 0.5) (0 1))
           ((-0.5 -0.5) (0 0))
           ((0.5 0.5) (1 1))
           ((0.5 -0.5) (1 0))))))

;;; Prefabs

(v:define-material dynamic-geometry
  (:profiles (x:u-mvp)
   :shader ex/shd:dynamic-geometry))

(v:define-prefab "dynamic-geometry" (:library examples)
  ;; TODO: This test is actually broken, we shouldn't see the backside of
  ;; the geometry, but we do! Probably a shader error.
  (("camera" :copy "/cameras/perspective"))
  ("plane"
   (comp:transform :rotate/velocity (v3:velocity v3:+right+ o:pi)
                   :scale (v3:vec 20f0 20f0 20f0))
   (comp:geometry :name 'tile)
   (comp:render :material 'dynamic-geometry
                :slave (v:ref :self :component 'comp:geometry))))
