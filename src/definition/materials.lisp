(in-package #:virality.extension)

(v:define-material unlit-color
  (:shader shd:unlit-color
   :profiles (u-mvp)))

(v:define-material unlit-color-decal
  (:shader shd:unlit-color-decal
   :profiles (u-mvp)))

(v:define-material unlit-texture
  (:shader shd:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'x:debug-texture)
              (:mix-color (v4:ones)))))

(v:define-material unlit-texture-invert
  (:shader shd:unlit-texture-invert
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'x:debug-texture)
              (:mix-color (v4:ones)))))

(v:define-material unlit-texture-decal
  (:shader shd:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:zero))
              (:max-intensity (v4:ones))
              (:tex.sampler1 'x:debug-texture))))

(v:define-material unlit-texture-decal-bright
  (:shader shd:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:vec 0.1f0 0.1f0 0.1f0 0.1f0))
              (:max-intensity (v4:ones))
              (:tex.sampler1 'x:debug-texture))))

(v:define-material sprite
  (:profiles (u-mvp)
   :shader umbra.sprite:sprite
   :uniforms ((:sprite.sampler 'x:debug-texture)
              (:sprite.index 0)
              (:opacity 1.0))
   :blocks ((:block-name :spritesheet
             :storage-type :buffer
             :block-alias :spritesheet
             :binding-policy :manual))))

(v:define-material missing-material
  (:shader shd:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'x:debug-texture))))

(v:define-material collider/sphere
  (:shader shd:collider/sphere
   :profiles (u-mvp)
   :uniforms ((:collider-local-center (v3:zero))
              (:in-contact-color (v4:vec 1f0 0f0 0f0 1f0))
              (:not-in-contact-color (v4:vec 0f0 1f0 0f0 .5f0))
              (:in-contact-p nil)
              (:radius 0f0))))

(v:define-material collider/cuboid
  (:shader shd:collider/cuboid
   :profiles (u-mvp)
   :uniforms ((:collider-local-center (v3:zero))
              (:in-contact-color (v4:vec 1f0 0f0 0f0 1f0))
              (:not-in-contact-color (v4:vec 0f0 1f0 0f0 .5f0))
              (:in-contact-p nil)
              (:minx 0f0)
              (:maxx 0f0)
              (:miny 0f0)
              (:maxy 0f0)
              (:minz 0f0)
              (:maxz 0f0))))

;; These are CC-0 from blender.
(v:define-material matcap
  (:shader shd:matcap
   :profiles (u-mvp)
   :uniforms ((:sampler 'x:matcap/basic-1)
              (:normal-matrix (m3:id)))))
