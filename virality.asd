;; Turn off a cl-opengl bug fix for buggy intel drivers that cause a severe
;; performance problem for those not using the buggy intel gpu. See the
;; See the README concerning this line.
(push :cl-opengl-no-masked-traps *features*)

(asdf:defsystem #:virality
  :description "An experimental game engine."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "AGPLv3"
  :homepage "https://github.com/mfiano/virality-engine"
  :encoding :utf-8
  :depends-on (#:3b-bmfont
               #:3b-bmfont/json
               #:babel
               #:cl-cpus
               #:cl-graph
               #:cl-opengl
               #:cl-ppcre
               #:cl-slug
               #:closer-mop
               #:fast-io
               #:global-vars
               #:jsown
               #:lparallel
               #:vutils
               #:vorigin
               #:vshadow
               #:vumbra
               #:pngload
               #:printv
               #:queues.simple-queue
               #:sdl2
               #:split-sequence
               #:static-vectors
               #:trivial-features
               #:uiop)
  :in-order-to ((asdf:test-op (asdf:test-op #:virality.test)))
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:module "datatype"
    :components
    ((:file "thread-pool-defs")
     (:file "uuid-defs")
     (:file "asset-defs")
     (:file "graph-defs")
     (:file "flow-defs")
     (:file "clock-defs")
     (:file "context-defs")
     (:file "core-defs")
     (:file "kernel-defs")
     (:file "actor-defs")
     (:file "component-mop-defs")
     (:file "data-defs")
     (:file "mouse-defs")
     (:file "gamepad-defs")
     (:file "button-defs")
     (:file "region-defs")
     (:file "bounding-volume-obb-defs")
     (:file "colliders-defs")
     (:file "spec-defs")
     (:file "attribute-defs")
     (:file "group-defs")
     (:file "layout-defs")
     (:file "texture-defs")
     (:file "common-defs")
     (:file "component-support-defs")
     (:file "reference-defs")
     (:file "display-defs")
     (:file "image-defs")
     (:file "gltf-defs")
     (:file "material-defs")
     (:file "make-project-defs")))
   (:module "core-early"
    :components
    ((:file "general")
     (:file "metadata")
     (:file "config")
     (:file "hardware")    ;; Put into new GPU abstraction support lib.
     (:file "thread-pool")
     (:file "live-coding")
     (:file "debug")
     (:file "parser")
     (:file "uuid")
     (:file "make-project")
     (:file "deployment")
     (:file "asset")
     (:file "graph")
     (:file "flow") ;; maybe lift binding-pattern if used elsewhere.
     (:file "protocol")
     (:file "resource-cache")
     (:file "clock")
     (:file "shaders")
     (:file "annotations")
     (:file "context")
     (:file "core")))
   ;; This module houses files that are still being worked out and not
   ;; integrated into the core yet.
   (:module "kernel"
    :components
    ((:file "kernel")
     (:file "actor")
     (:file "component-mop")
     (:file "component")
     (:file "storage")))
   (:module "input"       ;;;; KEEP GOING for vutils refactor.
    :components
    ((:file "data")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "button")
     (:file "input")))
   (:module "collision-detection"
    :components
    ((:file "region")
     (:file "bounding-volume-obb")
     (:file "colliders")))
   (:module "geometry"
    :components
    ((:file "spec")
     (:file "attribute")
     (:file "group")
     (:file "layout")
     (:file "buffer")
     (:file "geometry")))
   (:module "texture"
    :components
    ((:file "common")
     (:file "texture")
     (:file "1d")
     (:file "2d")
     (:file "3d")
     (:file "1d-array")
     (:file "2d-array")
     (:file "cube-map")
     (:file "cube-map-array")
     (:file "rectangle")
     (:file "buffer")))
   (:module "components"
    :components
    ((:file "transform")
     (:file "camera")
     (:file "camera-following")
     (:file "camera-tracking")
     (:file "render")
     (:file "geometry")
     (:file "mesh")
     (:file "sprite")
     (:file "font")
     (:file "collider-sphere")
     (:file "collider-cuboid")
     (:file "collider-collide-p")))
   (:module "prefab"
    :components
    ((:file "common")
     (:file "checks")
     (:file "parser")
     (:file "loader")
     (:file "reference")
     (:file "prefab")))
   ;; datatypes in module "shader" are not CL datatypes. They stay here.
   (:module "shader"
    :components
    ((:file "texture")
     (:file "collider")
     (:file "matcap")))
   (:module "core-late"
    :components
    ((:file "opengl")
     (:file "display")
     (:file "image")
     (:file "image-png")
     (:file "image-hdr")
     (:file "gltf")
     (:file "material-values")
     (:file "material")
     (:file "materials-table")
     (:file "material-meta")
     (:file "framebuffer")
     (:file "transform-state")
     (:file "transform-protocol")
     (:file "free-look-state")
     (:file "spritesheet")
     (:file "font")
     (:file "object-picking")
     (:file "engine")))
   (:module "definition"
    :components
    ((:file "graphs")
     (:file "flows")
     (:file "texture-profiles")
     (:file "textures")
     (:file "material-profiles")
     (:file "materials")
     (:file "geometry-layouts")))))
