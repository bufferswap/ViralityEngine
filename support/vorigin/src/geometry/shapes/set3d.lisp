(in-package #:cl-user)

;;;; This file defines a package for constructing 3D shape sets.

;;;; A 3D shape set is a collection of multiple spheres and AABBs that can be tested against other
;;;; primitives or shape sets for a collision.

;;;; Shape sets are useful because sometimes a minimum bounding shape is not accurate enough for the
;;;; object which needs a collision region, or they can be used in a later phase of collision
;;;; detection after the broad phase using a minimum bounding shape. With a shape set, there is a
;;;; single object with N spheres and M AABBs to approximate the shape of a complex piece of
;;;; geometry. This single shape set can be tested against other shape sets, points, lines, and
;;;; other primitives with a single function call.

(defpackage #:vorigin.geometry.shape-set-3d
  (:local-nicknames
   (#:u #:vutils))
  (:use #:cl)
  (:export
   #:aabbs
   #:shape-set
   #:spheres))

(in-package #:vorigin.geometry.shape-set-3d)

(declaim (inline %shape-set))
(defstruct (shape-set
            (:predicate nil)
            (:copier nil)
            (:constructor %shape-set)
            (:conc-name nil))
  (spheres (vector) :type simple-vector)
  (aabbs (vector) :type simple-vector))

(u:fn-> shape-set (&key (:spheres sequence) (:aabbs sequence)) shape-set)
(declaim (inline shape-set))
(defun shape-set (&key spheres aabbs)
  ;; TODO: Add assertions to check if every element is a sphere or AABB when those structs are
  ;; defined.
  (%shape-set :spheres (map 'vector #'identity spheres)
              :aabbs (map 'vector #'identity aabbs)))
