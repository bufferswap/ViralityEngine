(in-package #:cl-user)

;;;; This file defines a package for constructing 2D shape sets.

;;;; A 2D shape set is a collection of multiple circles, rects and/or oriented
;;;; rects, that can be tested against other primitives or shape sets for a
;;;; collision.

;;;; Shape sets are useful because sometimes a minimum bounding shape is not accurate enough for the
;;;; object which needs a collision region, or they can be used in a later phase of collision
;;;; detection after the broad phase using a minimum bounding shape. With a shape set, there is a
;;;; single object with multiple circles, rects and oriented rects to approximate the shape of a
;;;; complex piece of geometry. This single shape set can be tested against other shape sets,
;;;; points, lines, and other primitives with a single function call.

(defpackage #:vorigin.geometry.shape-set-2d
  (:local-nicknames
   (#:circle #:vorigin.geometry.circle)
   (#:orect #:vorigin.geometry.oriented-rect)
   (#:rect #:vorigin.geometry.rect)
   (#:u #:vutils))
  (:use #:cl)
  (:export
   #:circles
   #:oriented-rects
   #:rects
   #:shape-set))

(in-package #:vorigin.geometry.shape-set-2d)

(declaim (inline %shape-set))
(defstruct (shape-set
            (:predicate nil)
            (:copier nil)
            (:constructor %shape-set)
            (:conc-name nil))
  (circles (vector) :type simple-vector)
  (oriented-rects (vector) :type simple-vector)
  (rects (vector) :type simple-vector))

(u:fn-> shape-set (&key (:circles sequence)
                        (:oriented-rects sequence)
                        (:rects sequence))
        shape-set)
(declaim (inline shape-set))
(defun shape-set (&key circles rects oriented-rects)
  (assert (every #'circle:circle-p circles))
  (assert (every #'rect:rect-p rects))
  (assert (every #'orect:rect-p oriented-rects))
  (%shape-set :circles (map 'vector #'identity circles)
              :rects (map 'vector #'identity rects)
              :oriented-rects (map 'vector #'identity oriented-rects)))
