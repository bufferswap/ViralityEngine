(in-package #:virality-examples)

;;; Materials

(v:define-material noise
  (:profiles (x:u-mvpt)
   :shader ex/shd:noise))

;;; Prefabs

(v:define-prefab "noise-tile" (:library examples)
  (comp:transform :scale (v3:vec 90f0 90f0 0f0))
  (comp:mesh :asset '(v::meshes v::primitives)
             :name "plane"))

(v:define-prefab "noise" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("perlin-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -540f0 202.5f0 0f0))
   (comp:render :material '(noise
                            noise/perlin-3d
                            :shader ex/shd:noise/perlin-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("perlin-surflet-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -325f0 202.5f0 0f0))
   (comp:render
    :material '(noise
                noise/perlin-surflet-3d
                :shader ex/shd:noise/perlin-surflet-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("perlin-improved-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -110f0 202.5f0 0f0))
   (comp:render
    :material '(noise
                noise/perlin-improved-3d
                :shader ex/shd:noise/perlin-improved-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("perlin-4d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 110f0 202.5f0 0f0))
   (comp:render :material '(noise
                            noise/perlin-4d
                            :shader ex/shd:noise/perlin-4d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("cellular-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 325f0 202.5f0 0f0))
   (comp:render :material '(noise
                            noise/cellular-3d
                            :shader ex/shd:noise/cellular-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("cellular-fast-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 540f0 202.5f0 0f0))
   (comp:render
    :material '(noise
                noise/cellular-fast-3d
                :shader ex/shd:noise/cellular-fast-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("hermite-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -540f0 -22.5f0 0f0))
   (comp:render :material '(noise
                            noise/hermite-3d
                            :shader ex/shd:noise/hermite-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("simplex-perlin-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -325f0 -22.5f0 0f0))
   (comp:render
    :material '(noise
                noise/simplex-perlin-3d
                :shader ex/shd:noise/simplex-perlin-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("simplex-cellular-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -110f0 -22.5f0 0f0))
   (comp:render
    :material '(noise
                noise/simplex-cellular-3d
                :shader ex/shd:noise/simplex-cellular-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("simplex-polkadot-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 110f0 -22.5f0 0f0))
   (comp:render
    :material '(noise
                noise/simplex-polkadot-3d
                :shader ex/shd:noise/simplex-polkadot-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("value-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 325f0 -22.5f0 0f0))
   (comp:render :material '(noise
                            noise/value-3d
                            :shader ex/shd:noise/value-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("value-4d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 540f0 -22.5f0 0f0))
   (comp:render :material '(noise
                            noise/value-4d
                            :shader ex/shd:noise/value-4d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("value-hermite-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -540f0 -247.5f0 0f0))
   (comp:render
    :material '(noise
                noise/value-hermite-3d
                :shader ex/shd:noise/value-hermite-3d)
    :slave (v:ref :self :component 'comp:mesh)))
  (("value-perlin-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -325f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/value-perlin-3d
                            :shader ex/shd:noise/value-perlin-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("polkadot-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -110f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/polkadot-3d
                            :shader ex/shd:noise/polkadot-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("polkadot-box-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 110f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/polkadot-box-3d
                            :shader ex/shd:noise/polkadot-box-3d)
                :slave (v:ref :self :component 'comp:mesh)))
  (("cubist-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 325f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/cubist-3d
                            :shader ex/shd:noise/cubist-3d)
                :slave (v:ref :self :component 'comp:mesh))))
