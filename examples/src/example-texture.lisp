(in-package #:virality-examples)

;;; Textures

(v:define-texture 1d-gradient
    (:texture-1d x:clamp-all-edges)
  (:data #((textures texture-gradient-1d))))

(v:define-texture 2d-wood
    (:texture-2d x:clamp-all-edges)
  (:data #((textures wood))))

(v:define-texture 3d
    (:texture-3d x:clamp-all-edges)
  ;; TODO: Currently, these are the only valid origin and slices values. They
  ;; directly match the default of opengl.
  (:layout `((:origin :left-back-bottom)
             (:shape (:slices :back-to-front))))
  ;; TODO: Maybe I shuld implement pattern specification of mipmaps.
  (:data #(#((textures 3d-slice-0-0)
             (textures 3d-slice-1-0)
             (textures 3d-slice-2-0)
             (textures 3d-slice-3-0)
             (textures 3d-slice-4-0)
             (textures 3d-slice-5-0)
             (textures 3d-slice-6-0)
             (textures 3d-slice-7-0))
           #((textures 3d-slice-0-1)
             (textures 3d-slice-1-1)
             (textures 3d-slice-2-1)
             (textures 3d-slice-3-1))
           #((textures 3d-slice-0-2)
             (textures 3d-slice-1-2))
           #((textures 3d-slice-0-3)))))

(v:define-texture 1d-array
    (:texture-1d-array x:clamp-all-edges)
  ;; If there are multiple images in each list, they are mipmaps. Since this is
  ;; a test, each mip_0 image is 8 width x 1 height
  (:data #(#((textures red-line-0)
             (textures red-line-1)
             (textures red-line-2)
             (textures red-line-3))
           #((textures green-line-0)
             (textures green-line-1)
             (textures green-line-2)
             (textures green-line-3))
           #((textures blue-line-0)
             (textures blue-line-1)
             (textures blue-line-2)
             (textures blue-line-3))
           #((textures white-line-0)
             (textures white-line-1)
             (textures white-line-2)
             (textures white-line-3)))))

(v:define-texture 2d-array
    (:texture-2d-array x:clamp-all-edges)
  ;; Since this is a test, each mip_0 image is 1024x1024 and has 11 mipmaps.
  (:data #(#((textures blue-fur-0)
             (textures blue-fur-1)
             (textures blue-fur-2)
             (textures blue-fur-3)
             (textures blue-fur-4)
             (textures blue-fur-5)
             (textures blue-fur-6)
             (textures blue-fur-7)
             (textures blue-fur-8)
             (textures blue-fur-9)
             (textures blue-fur-10))
           #((textures bark-0)
             (textures bark-1)
             (textures bark-2)
             (textures bark-3)
             (textures bark-4)
             (textures bark-5)
             (textures bark-6)
             (textures bark-7)
             (textures bark-8)
             (textures bark-9)
             (textures bark-10))
           #((textures rock-0)
             (textures rock-1)
             (textures rock-2)
             (textures rock-3)
             (textures rock-4)
             (textures rock-5)
             (textures rock-6)
             (textures rock-7)
             (textures rock-8)
             (textures rock-9)
             (textures rock-10))
           #((textures wiggles-0)
             (textures wiggles-1)
             (textures wiggles-2)
             (textures wiggles-3)
             (textures wiggles-4)
             (textures wiggles-5)
             (textures wiggles-6)
             (textures wiggles-7)
             (textures wiggles-8)
             (textures wiggles-9)
             (textures wiggles-10)))))

(v:define-texture cubemap (:texture-cube-map)
  (:data
   ;; TODO: Only :six (individual images) is supported currently.
   #(((:layout :six) ;; :equirectangular, :skybox, etc, etc.
      #((:+x #((textures cube-map-right)))
        (:-x #((textures cube-map-left)))
        (:+y #((textures cube-map-top)))
        (:-y #((textures cube-map-bottom)))
        (:+z #((textures cube-map-back)))
        (:-z #((textures cube-map-front))))))))

(v:define-texture cubemaparray (:texture-cube-map-array)
  (:data
   #(((:layout :six)
      #((:+x #((textures cube-map-right)))
        (:-x #((textures cube-map-left)))
        (:+y #((textures cube-map-top)))
        (:-y #((textures cube-map-bottom)))
        (:+z #((textures cube-map-back)))
        (:-z #((textures cube-map-front)))))
     ((:layout :six)
      #((:+x #((textures cube-map-right-2)))
        (:-x #((textures cube-map-left-2)))
        (:+y #((textures cube-map-top-2)))
        (:-y #((textures cube-map-bottom-2)))
        (:+z #((textures cube-map-back-2)))
        (:-z #((textures cube-map-front-2))))))))

;;; Materials

(v:define-material 1d-gradient
  (:shader ex/shd:unlit-texture-1d
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 '1d-gradient)
    (:mix-color (v4:ones)))))

(v:define-material 2d-wood
  (:shader shd:unlit-texture
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 '2d-wood)
    (:mix-color (v4:ones)))))

(v:define-material 3d
  (:shader ex/shd:unlit-texture-3d
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 '3d)
    (:mix-color (v4:ones))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (float (/ (1+ (sin (* (v:total-time context) 1.5))) 2.0) 1f0))))))

(v:define-material 1d-array
  (:shader ex/shd:unlit-texture-1d-array
   :profiles (x:u-mvpt)
   :uniforms
   ((:tex.sampler1 '1d-array)
    (:mix-color (v4:ones))
    (:num-layers 4))))

(v:define-material 2d-array
  (:shader ex/shd:unlit-texture-2d-array
   :profiles (x:u-mvpt)
   :uniforms
   ((:tex.sampler1 '2d-array)
    (:mix-color (v4:ones))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (float (/ (1+ (sin (* (v:total-time context) 1.5))) 2.0) 1f0)))
    (:num-layers 4))))

(v:define-material 2d-sweep-input
  (:shader ex/shd:noise-2d/sweep-input
   :profiles (x:u-mvp)
   :uniforms
   ;; any old 2d texture here will do since we overwrite it with noise.
   ((:tex.sampler1 '2d-wood)
    (:tex.channel0 (v2:zero))
    (:mix-color (v4:ones)))))

(v:define-material cubemap
  (:shader ex/shd:unlit-texture-cube-map
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 'cubemap)
    (:mix-color (v4:ones)))))

(v:define-material cubemaparray
  (:shader ex/shd:unlit-texture-cube-map-array
   :profiles (x:u-mvpt)
   :uniforms
   ((:tex.sampler1 'cubemaparray)
    (:mix-color (v4:ones))
    (:cube-layer
     (lambda (context material)
       (declare (ignore material))
       ;; make sin in the range of 0 to 1 for texture coord.
       (float (/ (1+ (sin (* (v:total-time context) 1.5))) 2.0) 1f0)))
    (:num-layers 2))))

;;; Components

(v:define-component shader-sweep ()
  ((%renderer :reader renderer)
   (%material :reader material)
   (%material-retrieved-p :reader material-retrieved-p
                          :initform nil)
   (%channel0 :reader channel0
              :initform (v2:zero))))

(defmethod v:on-component-initialize ((self shader-sweep))
  (with-slots (%renderer) self
    (setf %renderer (v:component-by-type (v:actor self) 'comp:render))))

(defmethod v:on-component-update ((self shader-sweep))
  (with-slots (%material %material-retrieved-p) self
    (unless %material-retrieved-p
      (setf %material (comp:material (renderer self))
            %material-retrieved-p t))
    (u:mvlet* ((context (v:context self))
               (x y (v:get-mouse-position context)))
      (when (null x) (setf x (/ v:=window-width= 2f0)))
      (when (null y) (setf y (/ v:=window-height= 2f0)))
      (v2:with-components ((c (channel0 self)))
        ;; crappy, but good enough.
        (setf cx (float (/ x v:=window-width=) 1f0)
              cy (float (/ y v:=window-height=) 1f0)))
      ;; get a reference to the material itself (TODO: use MOP stuff to get
      ;; this right so I don't always have to get it here)
      (setf (v:uniform-ref %material :tex.channel0) (channel0 self)))))

;;; Prefabs

(v:define-prefab "texture" (:library examples :policy :new-type)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 3f0))
  (("1d-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec -4f0 3f0 0f0))
   (comp:render :material '1d-gradient
                :slave (v:ref :self :component 'comp:mesh)))
  (("2d-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec -2f0 3f0 0f0))
   (comp:render :material '2d-wood
                :slave (v:ref :self :component 'comp:mesh)))
  (("3d-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 3f0 0f0))
   (comp:render :material '3d
                :slave (v:ref :self :component 'comp:mesh)))
  (("1d-array-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec 2f0 3f0 0f0))
   (comp:render :material '1d-array
                :slave (v:ref :self :component 'comp:mesh)))
  (("2d-array-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec 4f0 3f0 0f0))
   (comp:render :material '2d-array
                :slave (v:ref :self :component 'comp:mesh)))
  (("swept-input" :copy "/mesh")
   (comp:transform :translate (v3:vec -4f0 1f0 0f0))
   (comp:render :material '2d-sweep-input
                :slave (v:ref :self :component 'comp:mesh))
   (shader-sweep))
  (("cube-map" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 -1f0 0f0)
                   :rotate (q:orient :world
                                     :x (float (asin (/ (sqrt 2))) 1f0)
                                     :z o:pi/4))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube")
   (comp:render :material 'cubemap
                :slave (v:ref :self :component 'comp:mesh)))
  (("cube-map-array" :copy "/mesh")
   (comp:transform :translate (v3:vec 3f0 -1f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi/4))
   (comp:mesh :asset '(v::meshes v::primitives)
              :name "cube")
   (comp:render :material 'cubemaparray
                :slave (v:ref :self :component 'comp:mesh))))
