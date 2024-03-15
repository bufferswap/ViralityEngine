(in-package #:xXx-SYSTEM-NAME-xXx)

;;; V DSL forms that explain to the engine things about the configuration for
;;; how to start the application, where to find physical game assets, and the
;;; definition of the execution order of the component protocol.

;;; Config for this project.

(v:define-config start-config-0 ()
  :default t
  :window-width 800
  :window-height 600
  :delta (float 1/60 1f0)
  :initial-scene '(("initial-scene" lib/main)))

(v:define-config start-config-1 ()
  :window-width 1920
  :window-height 1080
  :delta (float 1/60 1f0)
  :initial-scene '(("initial-scene" lib/main)))

;;; Asset Pool Definitions

(v:define-asset-pool metadata ()
  :path "data/metadata")

(v:define-asset-pool meshes ()
  :path "data/mesh"
  :filter "glb")

(v:define-asset-pool textures ()
  :path "data/texture"
  :filter "png")

(v:define-asset-pool mesh-textures ()
  :path "data/texture/mesh"
  :filter "png")

(v:define-asset-pool environments ()
  :path "data/texture/environment"
  :filter "hdr")

;;; Graphs

;; TODO: Figure out why the graph DSL can't parse syntax based on symbol-name.
;; The following in-package form is needed until this is fixed. Even though
;; this DSL is going to be evolved, we should at least fix this since it causes
;; great confusion when it goes wrong.

(in-package #:virality)

(define-graph :xXx-SYSTEM-NAME-xXx
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(define-graph :virality
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:comp -> :xXx-SYSTEM-NAME-xXx))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
