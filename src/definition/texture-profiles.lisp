(in-package #:virality.extension)

(tex:define-texture-profile default-profile
  ;; texparameter stuff, opengl defaults
  ;; NOTE: This next one might be called :depth-texture-mode in cl-opengl. Or,
  ;; :dempth-texture-mode might have been removed and replaced with this. Or,
  ;; something else entirely.
  (:depth-stencil-texture-mode :depth-component) ;; note: opengl 4.3 or greater
  (:texture-base-level 0)
  (:texture-border-color (v4:zero))
  (:texture-compare-func :lequal)
  (:texture-compare-mode :none)
  (:texture-lod-bias 0f0)
  ;; The next one below is :nearest-mipmap-linear default in ogl. But, this one
  ;; is almost surely what you actually want. Also, it will downconvert to
  ;; :nearest or :linear as apprpriate if you turn of mipmaps for a texture.
  (:texture-min-filter :linear-mipmap-linear)
  (:texture-mag-filter :linear)
  (:texture-min-lod -1000)
  (:texture-max-lod 1000)
  (:texture-max-level 1000)
  (:texture-swizzle-r :red)
  (:texture-swizzle-g :green)
  (:texture-swizzle-b :blue)
  (:texture-swizzle-a :alpha)
  (:texture-wrap-s :repeat)
  (:texture-wrap-t :repeat)
  (:texture-wrap-r :repeat)
  ;; Potential other defaults for other aspects of textures.
  ;; If :use-mipmaps is t, and a single image supplied, we generate them.
  ;; If :use-mipmaps is t, and we supply mipmaps, we use the supplied ones.
  ;; If :use-mipmaps is nil, and no mipmaps are supplied, none are generated.
  ;; If :use-mipmaps is nil, and mipmaps are supplied, none are used.
  ;; NOTE: The images defined in the texture are exactly those that will be put
  ;; into the texture. Meaning, if base-level is 0, and max-level is 1000, and
  ;; there are 11 images defined, then the resolution of image 0 in the ordered
  ;; array must be such that 11 images are created via the mipmap process. If
  ;; base_level is 4 and max-level is 10, then there should be 6 mipmaps
  ;; following the rules of halving:
  ;; https://www.khronos.org/opengl/wiki/Texture#Texture_completeness
  ;; The layout of the mipamaps in the :data array is largest first to smallest
  ;; which is last.
  (:use-mipmaps t)
  ;; :immutable t means the texture attributes MAY NOT CHNAGE at runtime.
  ;; :immutable nil means the attributes can change at runtime.
  (:immutable t))

(tex:define-texture-profile clamp-all-edges
  (:texture-wrap-s :clamp-to-edge)
  (:texture-wrap-t :clamp-to-edge)
  (:texture-wrap-r :clamp-to-edge))
