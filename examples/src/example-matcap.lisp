(in-package #:virality-examples)

;; TODO: The matcap-lookup shader had the (- 1 y) removed to be in favor
;; of just the y coord. Normally, it means that these two texture would have
;; to be flipped. However, the gltf2 uv coords have an upper left origin
;; instead of an upper right one. So that just complicates everything until
;; we finish proper cannonicalization of uv coords for different model vendors.
;; Also, the regular matcap shader still has the (- 1 y). So confusion all
;; around for a little bit.

(v:define-texture matcap/matcap-map (:texture-2d x:clamp-all-edges)
  ;; TODO: This is commented out because the gltf2 uvs have a different origin
  ;;(:flip-y t) ;; removed 1 - y from shader.
  (:texture-min-filter :nearest)
  (:texture-mag-filter :nearest)
  (:data #((textures matcap-map))))


;; TOOD: make basic-dark an RGB greyscale image. Check the others too.
(v:define-texture matcap/lookup (:texture-2d-array x:clamp-all-edges)
  ;; TODO: This we keep to "undo" the origin of the gltf coords.
  ;; Confusing, no? I think the "upside downness" of the matcap/matcap-map
  ;; and these are actually different on disk....
  (:flip-y t) ;; removed 1 - y from shader.
  (:data #(#((v:matcaps v::basic-dark)) ;; inside engine
           #((v:matcaps v::metal-carpaint)) ;; body base color
           #((v:matcaps v::jade)) ;; windshield
           #((v:matcaps v::basic-1))
           #((v:matcaps v::metal-shiny)) ;; engines
           #((v:matcaps v::basic-1))
           #((v:matcaps v::metal-lead)) ;; strip on wings, trim
           #((v:matcaps v::basic-1)))))

(v:define-material matcap-lookup
  (:shader ex/shd::matcap-lookup
   :profiles (x:u-mvp)
   :uniforms ((:matcaps 'matcap/lookup)
              (:normal-matrix (m3:id))
              (:id-map 'matcap/matcap-map))))


(v:define-prefab "default-artblob" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  (comp:transform :rotate (q:orient :local :x o:pi/2))
  (comp:mesh :asset '(meshes art-blob)
             :name "ArtBlob")
  (comp:render :material '(x:matcap ceramic-dark-copy
                           :uniforms ((:sampler x:matcap/ceramic-dark)))
               :slave (v:ref :self :component 'comp:mesh)))

(v:define-prefab "artblob" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("artblob" :copy "/default-artblob")
   (comp:transform :rotate/velocity (v3:velocity v3:+forward+ (- o:pi/6))
                   :scale 4f0)))

;; TODO: This prefab has a bug with the material copying and it cannot be
;; instantiated properly. But I'll solve that in a later checkin.
(v:define-prefab "artblob-group" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("artblob1" :copy "/default-artblob")
   (comp:transform :rotate/velocity (v3:velocity (v3:ones) (- o:pi/3))
                   :translate (v3:vec -15f0 0f0 0f0)
                   :scale 4f0))
  (("artblob2" :copy "/default-artblob")
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :scale 4f0)))

(v:define-prefab "artblob-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("artblob" :copy "/default-artblob")
   (comp:transform :scale 4f0)
   (simple-mouse-rotator :clamp-p t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(v:define-prefab "default-ship" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  (comp:transform :rotate (q:orient :local :x o:pi/2))
  (comp:mesh :asset '(meshes ships)
             :name "Ship_001") ;; 001 to 024
  ;; TODO: When this material name is nonexistant, it doesn't draw anything.
  (comp:render :material 'matcap-lookup
               :slave (v:ref :self :component 'comp:mesh)))


;; ;;;;;;;;;;;;;;;;;;;
;; The test prefabs
;; ;;;;;;;;;;;;;;;;;;;

(v:define-prefab "ship01" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("ship01" :copy "/default-ship")
   (comp:transform :rotate/velocity (v3:velocity v3:+forward+ (- o:pi/6))
                   :scale 4f0)))

(v:define-prefab "ship01-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("ship01" :copy "/default-ship")
   (comp:transform :scale 4f0)
   (comp:sphere :on-layer :ground)
   (simple-mouse-rotator :clamp-p t)))

(v:define-prefab "ship-group" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("ship011" :copy "/default-ship")
   (comp:mesh :name "Ship_011")
   (comp:transform :rotate/velocity (v3:velocity (v3:ones) (- o:pi/3))
                   :translate (v3:vec -15f0 0f0 0f0)
                   :scale 4f0))
  (("ship012" :copy "/default-ship")
   (comp:mesh :name "Ship_012")
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :scale 4f0)))
