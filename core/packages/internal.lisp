(in-package :defpackage+-user-1)

(defpackage+ #:%first-light
  (:nicknames #:%fl)
  (:use #:cl)
  (:export
   #:active-camera
   #:actor
   #:actor-component-by-type
   #:actor-components-by-type
   #:attach-component
   #:attach-multiple-components
   #:cameras
   #:component
   #:context
   #:copy-material
   #:core-state
   #:define-annotation
   #:define-component
   #:define-graph
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-resources
   #:define-scene
   #:define-texture
   #:define-texture-profile
   #:delta
   #:deploy-binary
   #:detach-component
   #:find-resource
   #:frame-manager
   #:frame-time
   #:general-data-format-descriptor
   #:id
   #:input-data
   #:lookup-material
   #:make-actor
   #:make-component
   #:mat-uniform-ref
   #:on-component-destroy
   #:on-component-initialize
   #:on-component-render
   #:on-component-update
   #:option
   #:print-all-resources
   #:project-data
   #:scene-tree
   #:shader
   #:spawn-actor
   #:ss-href
   #:start-engine
   #:state
   #:stop-engine
   #:total-time
   #:using-material
   #:with-shared-storage))
