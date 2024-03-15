(in-package #:virality-examples)

;;; Prefabs

(v:define-prefab "isometric-view" (:library examples)
  ("camera-handle"
   (comp:transform :rotate/velocity (v3:velocity v3:+up+ o:pi/4))
   ("iso"
    (comp:transform :rotate (q:orient :local
                                      :x (float (- (atan (/ (sqrt 2)))) 1f0)
                                      :y (- o:pi/4)))
    ("camera"
     (comp:transform :translate (v3:vec 0f0 0f0 10f0))
     (comp:camera :active-p t
                  :clip-near .1f0
                  :clip-far 1024f0
                  :zoom 50f0
                  :mode :orthographic))))
  ;; TODO: Can't use this, because of transform propogation bug.
  ;; Once that is fixed I can bring this back in.
  #++(("camera" :copy "/cameras/iso")
      ("camera"
       (comp:camera (:policy :new-args) :clip-near .1
                                        :clip-far 1024
                                        :zoom 100)))
  ;; NOTE: cubes are on xz plane.
  (("cube-z-1" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 0f0 -4f0))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube"))
  (("cube-z-0" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 0f0 -2f0))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube"))
  (("cube-origin" :copy "/mesh")
   (comp:transform
    #++ :rotate/velocity #++ (p:angular-velocity (v3:vec -1f0 1f0 1f0) o:pi/2))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube"))
  (("cube-x-0" :copy "/mesh")
   (comp:transform :translate (v3:vec 2f0 0f0 0f0))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube"))
  (("cube-x-1" :copy "/mesh")
   (comp:transform :translate (v3:vec 4f0 0f0 0f0))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube")))
