(in-package #:virality)

;;; NOTE: These protocols should all be generic functions, without any :METHODs
;;; defined for them, and never DEFMETHODs. Additionally, do not define any
;;; DEFMETHODs for any protocol functions before this file in load order, or
;;; they will be overwritten by these DEFGENERICs. This file is loaded very
;;; early to define protocol functions so that we do not waste anymore time
;;; debugging why methods are getting overwritten due to DEFGENERICs defined
;;; later. All methods should be defined in the file for the respective
;;; functionality, sometime after the class definition so specializations can be
;;; computed.

;;; Kernel

(defgeneric destroy (kernel &key ttl)
  (:documentation "The keyword argument :TTL supplied in real seconds, is how
long the thing has yet to live, with NIL meaning infinity."))

;;; Component

(defgeneric on-component-initialize (component))

(defgeneric on-component-attach (component actor))

(defgeneric on-component-detach (component actor))

(defgeneric on-component-physics-update (component))

(defgeneric on-component-update (component))

(defgeneric on-component-render (component))

(defgeneric on-component-slave-render (renderer component))

(defgeneric on-component-destroy (component))

;;; Collider

(defgeneric on-collision-enter (collider other))

(defgeneric on-collision-continue (collider other))

(defgeneric on-collision-exit (collider other))

(defgeneric collide-p (collider other))

;;; Resource Cache

(defgeneric resource-cache-layout (entry-type))

(defgeneric resource-cache-peek (context entry-type &rest keys))

(defgeneric resource-cache-lookup (context entry-type &rest keys))

(defgeneric resource-cache-construct (context entry-type &rest keys))

(defgeneric resource-cache-remove (context entry-type &rest keys))

(defgeneric resource-cache-dispose (context entry-type removed-value))

;;; Prologue/Epilogue

(defgeneric prologue (context)
  (:method (context)))

(defgeneric epilogue (context)
  (:method (context)))
