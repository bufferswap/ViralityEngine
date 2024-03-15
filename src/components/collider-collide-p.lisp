(in-package #:virality.component)

;; All colliders define a COLLIDE-P method appropriate for any combination that
;; could be computed.
;; TODO: Currently these COLLIDE-P are discrete. There needs to be a slot in the
;; collider which indicates :discrete or :continuous, and then these collider
;; functions should do the right thing if at all possible.

(defmethod collide-p ((fist sphere) (face sphere))
  "Return T if the two spheres actually collided."
  (if (not (and (v:actor fist) (v:actor face)))
      ;; NOTE: This code path is here for testing the collider system when
      ;; not running V properly. It should be removed when we figure out how
      ;; to test V's collision system better.
      (let ((distance/2 (/ (p3:distance (v:center fist)
                                        (v:center face)) 2f0)))
        (or (<= distance/2 (v:radius fist))
            (<= distance/2 (v:radius face))))

      ;; This is the REAL path through this code, which transforms the collider
      ;; into world space appropriately.
      (let* (;; Figure out where the center for these colliders are in world
             ;; space.
             (fist-collider-world-center (v:transform-point fist
                                                            (v:center fist)))
             (face-collider-world-center (v:transform-point face
                                                            (v:center face)))
             ;; Figure out the size of the radius in world space. We treat the
             ;; radius as a vector and rotate/scale (but no translate!) it by
             ;; the world matrix.
             (fist-world-radius (v:transform-vector
                                 fist (v3:vec (v:radius fist) 0f0 0f0)))
             (face-world-radius (v:transform-vector
                                 face (v3:vec (v:radius face) 0f0 0f0)))

             ;; TODO: Allow the gamedev an ability to ensure that the world
             ;; matrix must be uniform for a sphere collider.  This involves
             ;; calculating the magnitude of each of the rotation axes and
             ;; checking they are within a small tolerance of each other.  Don't
             ;; check it here, but it should be checked somewhere.

             ;; Compute the half way point between the two colliders.
             (distance (p3:distance fist-collider-world-center
                                    face-collider-world-center)))
        ;; Now, compute the collision is the common world space we converted
        ;; everything into.
        (<= distance (+ (v3:length fist-world-radius)
                        (v3:length face-world-radius))))))


(defun closest-pt-point-obb (world-pt obb-axes world-obb-center obb-half-widths)
  (let* ((d (v3:- world-pt world-obb-center))
         (q (v3:copy world-obb-center))
         (vec-accessor
           (make-array 3 :initial-contents (list #'v3:x #'v3:y #'v3:z))))

    (dotimes (i 3)
      (let* ((dist (v3:dot d (aref obb-axes i))))

        (when (> dist (funcall (aref vec-accessor i) obb-half-widths))
          (setf dist (funcall (aref vec-accessor i) obb-half-widths)))

        (when (< dist (- (funcall (aref vec-accessor i) obb-half-widths)))
          (setf dist (- (funcall (aref vec-accessor i) obb-half-widths))))

        (v3:+! q q (v3:scale (aref obb-axes i) dist))))
    q))

(defun %collide-p-sphere/cuboid (fist face)
  ;; NOTE!!!! fist is sphere, face is cuboid. ALWAYS.
  ;;
  ;; We're going to treat the cuboid as an OBB in world space and collide
  ;; it with the sphere also in world space.
  (let* (;; Compute on the cuboid objects
         ;; Create an OBB from the cuboid
         (l-min (v3:+ (v:center face)
                      (v3:vec (v:minx face) (v:miny face) (v:minz face))))
         (l-max (v3:+ (v:center face)
                      (v3:vec (v:maxx face) (v:maxy face) (v:maxz face))))
         (w-min (v:transform-point face l-min))
         (w-max (v:transform-point face l-max))
         ;; center of OBB in world space.
         (w-center (v3:lerp w-min w-max .5f0))
         ;; Now get rotation axes as rotated in world-space. Page 101 of RTCD
         ;; was vague on if u in the struct OBB was a basis vector or not.
         (x-axis (v3:normalize (m4:rotation-axis-to-vec3
                                (v:get-model-matrix face) :x)))
         (y-axis (v3:normalize (m4:rotation-axis-to-vec3
                                (v:get-model-matrix face) :y)))
         (z-axis (v3:normalize (m4:rotation-axis-to-vec3
                                (v:get-model-matrix face) :z)))
         (obb-axes (make-array 3 :element-type 'v3:vec
                                 :initial-contents (list x-axis y-axis z-axis)))
         (diagonal (v3:- w-max w-center))
         ;; Compute the half widths of the OBB.
         ;; TODO: Understand the effect of scaled transformation matricies.
         (half-widths (v3:vec (v3:dot diagonal x-axis)
                              (v3:dot diagonal y-axis)
                              (v3:dot diagonal z-axis)))

         ;; Get the important parts of the sphere into world space.
         (fist-collider-world-center (v:transform-point fist (v:center fist)))
         (fist-world-radius (v3:length
                             (v:transform-vector
                              fist (v3:vec (v:radius fist) 0f0 0f0)))))

    (let* ((p (closest-pt-point-obb fist-collider-world-center
                                    obb-axes w-center half-widths))
           (v (v3:- p fist-collider-world-center)))

      (<= (v3:dot v v) (expt fist-world-radius 2f0)))))


(defmethod collide-p ((fist sphere) (face cuboid))
  (%collide-p-sphere/cuboid fist face))


(defmethod collide-p ((fist cuboid) (face sphere))
  (%collide-p-sphere/cuboid face fist))

;; The original obb/obb code from RTCD.
#++
(defmethod collide-p ((fist cuboid) (face cuboid))
  (u:mvlet* ((obb1 (obb fist))
             (obb2 (obb face))
             (r r-abs (v::make-obb-obb-rotation obb1 obb2)))
    (m3:with-components ((r r) (ar r-abs))
      (v3:with-components ((tr (v::make-obb-obb-translation obb1 obb2))
                           (h1 (v::half-widths obb1))
                           (h2 (v::half-widths obb2)))
        (let ((b (+ (* h2x ar00) (* h2y ar01) (* h2z ar02))))
          (when (> (abs trx) (+ h1x b))
            (return-from collide-p)))
        (let ((b (+ (* h2x ar10) (* h2y ar11) (* h2z ar12))))
          (when (> (abs try) (+ h1y b))
            (return-from collide-p)))
        (let ((b (+ (* h2x ar20) (* h2y ar21) (* h2z ar22))))
          (when (> (abs trz) (+ h1z b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar00) (* h1y ar10) (* h1z ar20))))
          (when (> (abs (+ (* trx r00) (* try r10) (* trz r20))) (+ a h2x))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar01) (* h1y ar11) (* h1z ar21))))
          (when (> (abs (+ (* trx r01) (* try r11) (* trz r21))) (+ a h2y))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar02) (* h1y ar12) (* h1z ar22))))
          (when (> (abs (+ (* trx r02) (* try r12) (* trz r22))) (+ a h2z))
            (return-from collide-p)))
        (let ((a (+ (* h1y ar20) (* h1z ar10)))
              (b (+ (* h2y ar02) (* h2z ar01))))
          (when (> (abs (- (* trz r10) (* try r20))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1y ar21) (* h1z ar11)))
              (b (+ (* h2x ar02) (* h2z ar00))))
          (when (> (abs (- (* trz r11) (* try r21))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1y ar22) (* h1z ar12)))
              (b (+ (* h2x ar01) (* h2y ar00))))
          (when (> (abs (- (* trz r12) (* try r22))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar20) (* h1z ar00)))
              (b (+ (* h2y ar12) (* h2z ar11))))
          (when (> (abs (- (* trx r20) (* trz r00))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar21) (* h1z ar01)))
              (b (+ (* h2x ar12) (* h2z ar10))))
          (when (> (abs (- (* trx r21) (* trz r01))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar22) (* h1z ar02)))
              (b (+ (* h2x ar11) (* h2y ar10))))
          (when (> (abs (- (* trx r22) (* trz r02))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar10) (* h1y ar00)))
              (b (+ (* h2y ar22) (* h2z ar21))))
          (when (> (abs (- (* try r00) (* trx r10))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar11) (* h1y ar01)))
              (b (+ (* h2x ar22) (* h2z ar20))))
          (when (> (abs (- (* try r01) (* trx r11))) (+ a b))
            (return-from collide-p)))
        (let ((a (+ (* h1x ar12) (* h1y ar02)))
              (b (+ (* h2x ar21) (* h2y ar20))))
          (when (> (abs (- (* try r02) (* trx r12))) (+ a b))
            (return-from collide-p)))
        t))))

;; When this confirms to work remove the above.
;; Refactored code from RTCD for obb/obb collisions.
(defmethod collide-p ((fist cuboid) (face cuboid))
  (u:mvlet* ((obb1 (obb fist))
             (obb2 (obb face))
             (r r-abs (v::make-obb-obb-rotation obb1 obb2)))
    (m3:with-components ((r r) (ar r-abs))
      (v3:with-components ((tr (v::make-obb-obb-translation obb1 obb2))
                           (h1 (v::half-widths obb1))
                           (h2 (v::half-widths obb2)))
        (not (or (> (abs trx)
                    (+ h1x (* h2x ar00) (* h2y ar01) (* h2z ar02)))
                 (> (abs try)
                    (+ h1y (* h2x ar10) (* h2y ar11) (* h2z ar12)))
                 (> (abs trz)
                    (+ h1z (* h2x ar20) (* h2y ar21) (* h2z ar22)))
                 (> (abs (+ (* trx r00) (* try r10) (* trz r20)))
                    (+ (* h1x ar00) (* h1y ar10) (* h1z ar20) h2x))
                 (> (abs (+ (* trx r01) (* try r11) (* trz r21)))
                    (+ (* h1x ar01) (* h1y ar11) (* h1z ar21) h2y))
                 (> (abs (+ (* trx r02) (* try r12) (* trz r22)))
                    (+ (* h1x ar02) (* h1y ar12) (* h1z ar22) h2z))
                 (> (abs (- (* trz r10) (* try r20)))
                    (+ (* h1y ar20) (* h1z ar10) (* h2y ar02) (* h2z ar01)))
                 (> (abs (- (* trz r11) (* try r21)))
                    (+ (* h1y ar21) (* h1z ar11) (* h2x ar02) (* h2z ar00)))
                 (> (abs (- (* trz r12) (* try r22)))
                    (+ (* h1y ar22) (* h1z ar12) (* h2x ar01) (* h2y ar00)))
                 (> (abs (- (* trx r20) (* trz r00)))
                    (+ (* h1x ar20) (* h1z ar00) (* h2y ar12) (* h2z ar11)))
                 (> (abs (- (* trx r21) (* trz r01)))
                    (+ (* h1x ar21) (* h1z ar01) (* h2x ar12) (* h2z ar10)))
                 (> (abs (- (* trx r22) (* trz r02)))
                    (+ (* h1x ar22) (* h1z ar02) (* h2x ar11) (* h2y ar10)))
                 (> (abs (- (* try r00) (* trx r10)))
                    (+ (* h1x ar10) (* h1y ar00) (* h2y ar22) (* h2z ar21)))
                 (> (abs (- (* try r01) (* trx r11)))
                    (+ (* h1x ar11) (* h1y ar01) (* h2x ar22) (* h2z ar20)))
                 (> (abs (- (* try r02) (* trx r12)))
                    (+ (* h1x ar12) (* h1y ar02) (* h2x ar21) (* h2y ar20)))))))))
