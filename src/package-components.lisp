(in-package #:cl-user)

(defpackage #:first-light.components
  (:nicknames #:fl.comp)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:~ #:origin.swizzle)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4)
                    (#:q #:origin.quat))
  (:use #:cl #:%first-light)
  ;; camera
  (:export
   #:camera
   #:tracking-camera
   #:following-camera
   #:transform
   #:view
   #:projection
   #:zoom-camera
   #:find-active-camera
   #:compute-camera-view)
  ;; action
  (:export
   #:actions
   #:action
   #:action-list
   #:sprite-animate)
  ;; mesh
  (:export
   #:mesh)
  ;; render
  (:export
   #:render
   #:material
   #:draw-mesh)
  ;; sprite
  (:export
   #:sprite
   #:update-sprite-index)
  ;; transform
  (:export
   #:transform
   #:transform-add-child
   #:transform-remove-child
   #:model
   #:local
   #:translate
   #:rotate
   #:scale)
  ;; various colliders
  (:export
   #:collider/sphere
   #:collide-p
   #:referent
   #:on-layer
   #:center
   #:radius)
  ;; transform
  (:export
   #:transform
   #:transform-add-child
   #:transform-remove-child
   #:model
   #:local
   #:translate
   #:rotate
   #:scale
   #:transform-point
   #:inverse-transform-point
   #:transform-vector
   #:inverse-transform-vector
   #:transform-direction
   #:inverse-transform-direction
   #:transform-forward
   #:transform-backward
   #:transform-up
   #:transform-down
   #:transform-right
   #:transform-left))