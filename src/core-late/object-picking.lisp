(in-package #:virality)

;; TODO: Make this picking code general to support arbitrary line segments in
;; world space etc.

;; TODO: last-picked-actor in core should be a collection of some sort, 1 picked
;; actor per camera. This implies a picking component that is attached to any
;; camera that wants to pick from view space.

(defclass line-segment ()
  ((%start-point :reader start-point
                 :initform (v3:zero))
   (%end-point :reader end-point
               :initform (v3:zero))))

(defgeneric line-segment-cast (line-segment shape)
  (:method (line-segment shape)))

(defmethod line-segment-cast (line-segment (collider comp:sphere))
  (let* ((transform (component-by-type (actor collider) 'comp:transform))
         (start (start-point line-segment))
         (end (end-point line-segment))
         (vec (v3:- end start))
         (d (v3:normalize vec))
         (m (v3:- start (transform-point transform (center collider))))
         (b (v3:dot m d))
         (world-radius (v3:length
                        (transform-vector transform
                                          (v3:vec (radius collider) 0f0 0f0))))
         (c (- (v3:dot m m) (expt world-radius 2))))
    (unless (and (plusp c) (plusp b))
      (let ((discriminant (- (expt b 2) c)))
        (unless (minusp discriminant)
          (let ((x (max 0.0 (- (- b) (sqrt discriminant)))))
            (when (<= x (v3:length vec))
              x)))))))

(defmethod line-segment-cast (line-segment (collider comp:cuboid))
  (let* ((transform (component-by-type (actor collider) 'comp:transform))
         (center (center collider))
         (start (transform-point transform
                                 (start-point line-segment)
                                 :space :model))
         (end (transform-point transform
                               (end-point line-segment)
                               :space :model))
         (start (v3:- start center))
         (end (v3:- end center))
         (vec (v3:- end start))
         (d (v3:normalize vec))
         ;; Don't do v3:+ with the center point because we are working in the OBB space
         (obb-min (v3:vec (minx collider) (miny collider) (minz collider)))
         (obb-max (v3:vec (maxx collider) (maxy collider) (maxz collider)))
         (t-min 0f0)
         (t-max (v3:length vec)))
    (dotimes (i 3)
      (let ((p-i (aref start i))
            (min-i (aref obb-min i))
            (max-i (aref obb-max i)))
        (if (< (abs (aref d i)) 1e-7)
            (when (or (< p-i min-i) (> p-i max-i))
              (return-from line-segment-cast nil))
            (let* ((ood (/ (aref d i)))
                   (t1 (* (- min-i p-i) ood))
                   (t2 (* (- max-i p-i) ood)))
              (when (> t1 t2)
                (rotatef t1 t2))
              (setf t-min (max t-min t1)
                    t-max (min t-max t2))
              (when (> t-min t-max)
                (return-from line-segment-cast nil))))))
    ;; original code from RTCD pg. 181 returns q = p + d * tmi
    ;; TODO: We want the intersection point in world space as a second return
    ;; value
    t-min))

(defun update-line-segment (context line-segment)
  (u:mvlet ((core-state (core context))
            (x y dx dy (get-mouse-position context)))
    (u:when-let* ((start (start-point line-segment))
                  (end (end-point line-segment))
                  (camera (comp:find-active-camera context))
                  (view (comp::view camera))
                  (proj (comp::projection camera)))
      (v2:with-components ((r (resolution (display core-state))))
        (let ((viewport (v4:vec 0f0 0f0 rx ry)))
          (v3:copy! start
                    (p3:unproject (v3:vec* x y 0f0) view proj viewport))
          (v3:copy! end
                    (p3:unproject (v3:vec* x y 1f0) view proj viewport))
          t)))))

(defun pick-actor (context line-segment)
  (let ((core-state (core context))
        (picked nil))
    (update-line-segment context line-segment)
    (u:do-hash-values (layer (stable-colliders (collider-system core-state)))
      (u:do-hash-values (v layer)
        (u:when-let ((x (line-segment-cast line-segment v)))
          (push (cons x v) picked))))
    (when picked
      (let* ((collider (cdar (stable-sort picked #'< :key #'car)))
             (actor (actor collider)))
        (setf (last-picked-actor core-state) actor)
        actor))))

(defun unpick-actor (context)
  (setf (last-picked-actor (core context)) nil))

(defun actor-picked-p (actor)
  (eq actor (last-picked-actor (core (context actor)))))
