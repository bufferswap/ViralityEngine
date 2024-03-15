(in-package #:virality-examples)

;;; Textures

(v:define-texture sprites (:texture-2d)
  (:data #((textures sprites))))

;;; Components

(v:define-component simple-movement ()
  ((%transform :reader transform)))

(defmethod v:on-component-initialize ((self simple-movement))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp:transform))
    (v:translate self (v3:vec -400f0 0f0 0f0) :replace t :instant t)))

(defmethod v:on-component-update ((self simple-movement))
  (u:mvlet* ((context (v:context self))
             (lx ly (v:get-gamepad-analog
                     context :radial-scaled '(:gamepad1 :left-stick)))
             (rx ry (v:get-gamepad-analog
                     context :radial-scaled '(:gamepad1 :right-stick)))
             (instant-p (zerop (v:frame-count context))))
    (let ((vec (v3:vec lx ly 0f0)))
      (v3:scale! vec (if (> (v3:length vec) 1) (v3:normalize vec) vec) 150f0)
      (v:translate self
                   (v3:+ (v3:vec -400f0 0f0 0f0) vec)
                   :replace t
                   :instant instant-p
                   :space :inertial)
      (unless (= rx ry 0f0)
        (let* ((angle (atan (- rx) ry))
               (angle (if (minusp angle)
                          (+ o:pi (- o:pi (abs angle)))
                          angle)))
          (v:rotate self
                    (q:orient :local :z angle)
                    :replace t
                    :instant instant-p))))))

(v:define-component shot-mover ()
  ((%transform :reader transform)
   (%velocity :reader velocity
              :initarg :velocity
              :initform 0f0)))

(defmethod v:on-component-initialize ((self shot-mover))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp:transform))))

(defmethod v:on-component-update ((self shot-mover))
  (v:translate self
               (v3:scale v3:+up+
                         (* (velocity self) (v:frame-time (v:context self))))))

(v:define-component shot-emitter ()
  ((%transform :reader transform)))

(defmethod v:on-component-initialize ((self shot-emitter))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'comp:transform))))

(defmethod v:on-component-update ((self shot-emitter))
  (let ((context (v:context self)))
    (when (or (v:on-button-enter context :gamepad1 :a)
              (v:on-button-enter context :mouse :left))
      (let* ((parent-model (v:get-model-matrix self))
             (parent-translation (m4:get-translation parent-model))
             (parent-rotation (q:from-mat4 parent-model))
             (new-actor (v:make-actor context :display-id "Ship bullet"))
             (transform (v:make-component context
                                          'comp:transform
                                          :translate parent-translation
                                          :rotate parent-rotation))
             (shot-mover (v:make-component context 'shot-mover
                                           :velocity 1000f0))
             (sprite (v:make-component context
                                       'comp:sprite
                                       :spec '(metadata sprites)
                                       :name "bullet01"
                                       :frames 2))
             (render (v:make-component context
                                       'comp:render
                                       :material `(x:sprite
                                                   sprite
                                                   :uniforms
                                                   ((:sprite.sampler sprites)))
                                       :slave sprite)))
        (v:attach-components
         new-actor transform shot-mover sprite render)
        (v:spawn-actor new-actor)
        (v:destroy new-actor :ttl 2f0)))))

;;; Prefabs

(v:define-prefab "sprite" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("ship"
   (comp:transform :rotate (q:orient :local :z (- o:pi/2)))
   (simple-movement)
   (shot-emitter)
   ("ship-body"
    (comp:sprite :spec '(metadata sprites)
                 :name "ship29")
    (comp:render :material `(x:sprite
                             sprite
                             :uniforms ((:sprite.sampler sprites)))
                 :slave (v:ref :self :component'comp:sprite))
    ("exhaust"
     (comp:transform :translate (v3:vec 0f0 -140f0 0f0))
     (comp:sprite :spec '(metadata sprites)
                  :name "exhaust03-01"
                  :frames 8
                  :duration 0.75)
     (comp:render :material `(x:sprite
                              sprite
                              :uniforms ((:sprite.sampler sprites)))
                  :slave (v:ref :self :component'comp:sprite))))))
