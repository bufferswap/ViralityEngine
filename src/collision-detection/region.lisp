(in-package #:virality)

;;;; Implementation of datatypes:
;;;; REGION, REGION-CUBOID, REGION-SPHERE, REGION-ELLIPSOID

(defun make-region-cuboid (center minx maxx miny maxy minz maxz)
  (make-instance 'region-cuboid
                 :center center
                 :minx (float minx -.5f0)
                 :maxx (float maxx .5f0)
                 :miny (float miny -.5f0)
                 :maxy (float maxy .5f0)
                 :minz (float minz -.5f0)
                 :maxz (float maxz .5f0)))

(defun make-region-sphere (center radius)
  (make-instance 'region-sphere :center center
                                :radius (float radius 1f0)))

(defun make-region-ellipsoid (center x y z)
  (make-instance 'region-ellipsoid :center center
                                   :x (float x 1f0)
                                   :y (float y 1f0)
                                   :z (float z 1f0)))

(defun clip-movement-vector (movement-vector current-translation region-cuboid)
  "Clip the MOVEMENT-VECTOR by an amount that will cause it to not violate the
REGION-CUBOID when MOVEMENT-VECTOR is added to the CURRENT-TRANSLATION.
Return a newly allocated and adjusted MOVEMENT-VECTOR."

  (with-accessors ((center center)
                   (minx minx) (maxx maxx) (miny miny) (maxy maxy)
                   (minz minz) (maxz maxz))
      region-cuboid
    (v3:with-components ((c current-translation)
                         (m movement-vector))
      ;; add the movement-vector to the current-translation
      (let* ((nx (+ cx mx))
             (ny (+ cy my))
             (nz (+ cz mz))
             ;; And the default adjustments to the movement-vector
             (adj-x 0)
             (adj-y 0)
             (adj-z 0))
        ;; Then if it violates the boundary cube, compute the adjustment we
        ;; need to the movement vector to fix it.
        (let ((offset-minx (+ (v3:x center) minx))
              (offset-maxx (+ (v3:x center) maxx))
              (offset-miny (+ (v3:y center) miny))
              (offset-maxy (+ (v3:y center) maxy))
              (offset-minz (+ (v3:z center) minz))
              (offset-maxz (+ (v3:z center) maxz)))
          (when (< nx offset-minx)
            (setf adj-x (- offset-minx nx)))
          (when (> nx offset-maxx)
            (setf adj-x (- offset-maxx nx)))
          (when (< ny offset-miny)
            (setf adj-y (- offset-miny ny)))
          (when (> ny offset-maxy)
            (setf adj-y (- offset-maxy ny)))
          (when (< nz offset-minz)
            (setf adj-z (- offset-minz nz)))
          (when (> nz offset-maxz)
            (setf adj-z (- offset-maxz nz)))
          ;; NOTE: Allocates memory.
          (v3:vec (+ mx adj-x) (+ my adj-y) (+ mz adj-z)))))))
