(in-package #:virality-examples)

;; TODO: Remove uses of globals and replace with calls to cfg system.
;; This is because V should not provide any globals to the user, and also
;; because the screen size can change.

;;; Textures

(v:define-texture art5/texture (:texture-2d)
  (:data #((textures city))))

;;; Materials

(v:define-material art1
  (:profiles (x:u-mvptr)
   :shader ex/shd:art1))

(v:define-material art2
  (:profiles (x:u-mvptr)
   :shader ex/shd:art2))

(v:define-material art3
  (:profiles (x:u-mvptr)
   :shader ex/shd:art3))

(v:define-material art4
  (:profiles (x:u-mvptr)
   :uniforms ((:zoom 0.85f0)
              (:speed 1f0)
              (:strength 0.7f0)
              (:colorize nil)
              (:outline nil)
              (:detail 0.8f0))
   :shader ex/shd:art4))

(v:define-material art5
  (:profiles (x:u-mvptr)
   :uniforms ((:blur 7.5f0)
              (:speed 0.24f0)
              (:zoom 0.75f0)
              (:sampler 'art5/texture))
   :shader ex/shd:art5))

(v:define-material art6
  (:profiles (x:u-mvptr)
   :shader ex/shd:art6
   :uniforms ((:mouse (v2:zero)))))

;;; Components

(v:define-component mouse-shader-input ()
  ((%renderer :reader renderer)
   (%material :accessor material
              :initarg :material)
   (%material-retrieved-p :reader material-retrieved-p
                          :initform nil)
   (%mouse :reader mouse
           :initform (v2:zero))))

(defmethod v:on-component-initialize ((self mouse-shader-input))
  (with-slots (%renderer) self
    (setf %renderer (v:component-by-type (v:actor self) 'comp:render))))

(defmethod v:on-component-update ((self mouse-shader-input))
  (with-slots (%material %material-retrieved-p) self
    (unless %material-retrieved-p
      (setf %material (comp:material (renderer self))
            %material-retrieved-p t))
    (u:mvlet* ((context (v:context self))
               (x y (v:get-mouse-position context))
               (lmb-p (v:on-button-enabled context :mouse :left)))
      (when (null x) (setf x (/ v:=window-width= 2f0)))
      (when (null y) (setf y (/ v:=window-height= 2f0)))
      (when lmb-p
        (v2:with-components ((m (mouse self)))
          (setf mx (float (/ x v:=window-width=) 1f0)
                my (float (/ y v:=window-height=) 1f0)))
        (setf (v:uniform-ref %material :mouse) (mouse self))))))

;;; Prefabs

(v:define-prefab "art1" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ v:=window-width= 2f0)
                                  (/ v:=window-height= 2f0)
				  0f0))
   (comp:render :material 'art1
                :slave (v:ref :self :component 'comp:mesh))))

(v:define-prefab "art2" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ v:=window-width= 2f0)
                                  (/ v:=window-height= 2f0)
				  0f0))
   (comp:render :material 'art2
                :slave (v:ref :self :component 'comp:mesh))))

(v:define-prefab "art3" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ v:=window-width= 2f0)
                                  (/ v:=window-height= 2f0)
				  0f0))
   (comp:render :material 'art3
                :slave (v:ref :self :component 'comp:mesh))))

(v:define-prefab "art4" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ v:=window-width= 2f0)
                                  (/ v:=window-height= 2f0)
				  0f0))
   (comp:render :material 'art4
                :slave (v:ref :self :component 'comp:mesh))))

(v:define-prefab "art5" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ v:=window-width= 2f0)
                                  (/ v:=window-height= 2f0)
				  0f0))
   (comp:render :material 'art5
                :slave (v:ref :self :component 'comp:mesh))))

(v:define-prefab "art6" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (mouse-shader-input)
   (comp:transform :scale (v3:vec (/ v:=window-width= 2f0)
                                  (/ v:=window-height= 2f0)
				  0f0))
   (comp:render :material 'art6
                :slave (v:ref :self :component 'comp:mesh))))
