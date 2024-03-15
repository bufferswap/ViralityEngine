(in-package #:virality.extension)

(tex:define-texture debug-texture (:texture-2d clamp-all-edges)
  ;; I can put overrides in here too specific to this texture.
  (:data #((v::textures v::debug-0)
           (v::textures v::debug-1)
           (v::textures v::debug-2)
           (v::textures v::debug-3)
           (v::textures v::debug-4)
           (v::textures v::debug-5)
           (v::textures v::debug-6)
           (v::textures v::debug-7)
           (v::textures v::debug-8)
           (v::textures v::debug-9)
           (v::textures v::debug-10))))

(defmacro gen-define-textures (pool-name prefix options &body name-list)
  `(progn
     ,@(loop
         :for name :in name-list
         :collect
         `(tex:define-texture
              ,(u:format-symbol :virality.extension
                                "~A~A" prefix name)
              ,options

            (:data #((,pool-name ,(u:format-symbol :v "~A" name))))))))

;; These textures are CC-0 from the Blener Foundation.
(gen-define-textures v:matcaps matcap/ (:texture-2d clamp-all-edges)
  basic-1
  basic-2
  basic-dark
  basic-side
  ceramic-dark
  ceramic-lightbulb
  check-normal-y
  check-rim-dark
  check-rim-light
  clay-brown
  clay-muddy
  clay-studio
  jade
  metal-anisotropic
  metal-carpaint
  metal-lead
  metal-shiny
  pearl
  reflection-check-horizontal
  reflection-check-vertical
  resin
  skin
  toon)
