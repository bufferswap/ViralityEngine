(in-package #:virality)

;;;; This file sets up at compile time a collection of hash tables stored in
;;;; global variables used for storing metadata that are used during development
;;;; only.

;; Game asset metadata, stored by `DEFINE-ASSET-POOL`.
(global-vars:define-global-var =meta/asset-pools= (u:dict #'eq))

;; Call flows for defining the entity-component call flow, storing by
;; `DEFINE-CALL-FLOW`.
(global-vars:define-global-var =meta/call-flows= (u:dict #'eq))

;; Configuration options, stored by `DEFINE-CONFIG`. We have two tables, one for
;; engine defaults, and one for per config overlays. The per-config
;; configurations are the result of a left-to-right merge of the defaults and
;; the overlays.
(global-vars:define-global-var =meta/config/default= (u:dict #'eq))
(global-vars:define-global-var =meta/config/config= (u:dict #'eq))

;; Framebuffer specifications, stored by `DEFINE-FRAMEBUFFER`.
(global-vars:define-global-var =meta/framebuffers= (u:dict #'eq))

;; Dynamic geometry objects, stored by `DEFINE-GEOMETRY`.
(global-vars:define-global-var =meta/geometry= (u:dict #'eq))

;; Dynamic geometry layouts, which can be re-used across different dynamic
;; geometry definitions. This metadata is stored by `DEFINE-GEOMETRY-LAYOUT`.
(global-vars:define-global-var =meta/geometry-layouts= (u:dict #'eq))

;; A brief description of this data structure would be better written by
;; psilord.
;; TODO: Wait for psilord to fill this description in. ~axion 4/8/2020.
(global-vars:define-global-var =meta/graphs= (u:dict #'eq))

;; Material definitions, stored by `DEFINE-MATERIAL`.
(global-vars:define-global-var =meta/materials= (u:dict #'eq))

;; Material profiles, which can be re-used across different material
;; definitions. This metadata is stored by `DEFINE-MATERIAL-PROFILE`.
(global-vars:define-global-var =meta/material-profiles= (u:dict #'eq))

;; Prefabs for specifying arbitrary game object hierarchies to be instantiated
;; and placed into the game world at runtime. This metadata is stored by
;; `DEFINE-PREFAB`.
(global-vars:define-global-var =meta/prefabs= (u:dict #'eq))

;; Textures for mapping image assets to GPU data structures for rendering. This
;; metadata is stored by `DEFINE-TEXTURE`.
(global-vars:define-global-var =meta/textures= (u:dict #'eq))

;; Texture profiles, which can be re-used across different texture definitions.
;; This metadata is stored by `DEFINE-TEXTURE-PROFILE`.
(global-vars:define-global-var =meta/texture-profiles= (u:dict #'eq))
