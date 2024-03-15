(in-package #:xXx-SYSTEM-NAME-xXx)

;; This acts like a scene, but is just a regular prefab.
(v:define-prefab "initial-scene" (:library lib/main)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0f0 0f0 5f0))
   (comp:camera :zoom 3f0))
  (("mesh" :copy "/mesh")))
