(in-package #:virality-examples)

;;; Prefabs

(v:define-prefab "cameras" (:library ptp-base)
  ("ortho"
   (comp:camera :active-p t
                :mode :orthographic))
  ("perspective"
   (comp:camera :active-p t
                :mode :perspective))
  ("iso"
   (comp:transform :rotate (q:orient :local
                                     :x (float (- (atan (/ (sqrt 2)))) 1f0)
                                     :y (- o:pi/4)))
   ("camera"
    (comp:transform :translate (v3:vec 0f0 0f0 10f0))
    (comp:camera :active-p t
                 :mode :orthographic))))

(v:define-prefab "mesh" (:library ptp-base)
  (comp:mesh :asset '(v::meshes v::primitives)
             :name "plane")
  (sketch :material 'x:unlit-texture
          :slave (v:ref :self :component 'comp:mesh)))
