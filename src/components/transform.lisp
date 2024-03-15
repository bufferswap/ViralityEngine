(in-package #:virality.component)

(v:define-component transform ()
  ((%parent :accessor parent
            :initform nil)
   (%children :accessor children
              :initform nil)
   (%translation :reader translation
                 :initform (v::make-translate-state))
   (%rotation :reader rotation
              :initform (v::make-rotate-state))
   (%scale :reader scale
           :initform (v::make-scale-state))
   (%local :reader local
           :initform (m4:id))
   (%model :reader model
           :initform (m4:id))
   (%normal-matrix :reader normal-matrix
                   :initform (m4:id))))

(defun add-child (parent child)
  (pushnew child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if
                           (lambda (x)
                             (eq x child))
                           (children parent))
        (parent child) nil))

(defun transform-node (node)
  (v::transform-node/vector (scale node) v:=delta=)
  (v::transform-node/quaternion (rotation node) v:=delta=)
  (v::transform-node/vector
   (translation node) v:=delta=
   (lambda (incremental-translation-delta)
     ;; TODO: Remove memory allocation. Add a mat3 to transform-state we can
     ;; reuse at opportune times--but could mess up threading...
     (let ((rot (m4:rotation-to-mat3 (local node))))
       ;; View the incremental delta in terms of a fly vector in the rotated
       ;; local space of this transform.
       (m3:*v3! incremental-translation-delta rot
                incremental-translation-delta)))))

(defun resolve-local (node factor)
  (with-slots (%local) node
    (let ((translation (translation node))
          (rotation (rotation node))
          (scale (scale node)))
      (v::interpolate-vector scale factor)
      (v::interpolate-quaternion rotation factor)
      (v::interpolate-vector translation factor)
      (q:to-mat4! %local (v::transform-state-interpolated rotation))
      ;; TODO: This next code needs a temporary to reduce garbage, but I don't
      ;; want to store it in the transform since that causes that amount of
      ;; space addition for each transform used only for this. See if there is
      ;; another way before doing that option.
      (m4:*! %local %local
             (m4:set-scale m4:+id+ (v::transform-state-interpolated scale)))
      (m4:set-translation! %local %local
                           (v::transform-state-interpolated translation)))))

(defun resolve-model (node factor)
  (u:when-let ((parent (parent node)))
    (resolve-local node factor)
    (m4:*! (model node) (model parent) (local node))))

(defun resolve-normal-matrix (node)
  ;; Computes a normal matrix from the camera's view matrix and the supplied
  ;; transform component's model matrix. Writes the result into the
  ;; normal-matrix slot of this transform component.
  ;; NOTE: This is completely a non-consing operation except for the last part
  ;; to convert the result into a mat3. See if we can do something about this
  ;; sometime when if it matters. The "something to do" is write a mat3 invert
  ;; for origin which will allow us to reduce the memory load and operations
  ;; further.
  (let ((result (normal-matrix node)))
    ;; Only compute if there is an active camera.
    (u:when-let ((camera (v::active-camera (v:context node))))
      (m4:set-translation! result (model node) v3:+zero+)
      (m4:*! result (view camera) result)
      (m4:invert! result result)
      (m4:transpose! result result))
    ;; Regardless of whether there is an active camera, convert the stored
    ;; normal-matrix to a mat3, returning this result to the caller.
    (m4:rotation-to-mat3 result)))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

;; TODO: This needs to live in a better place, but I'm not sure where.
;; It IS technically a feature of the transform component since it keeps track
;; if the actual links between actors.
(defun map-actors (func parent &optional context)
  "Call the FUNC on the actor PARENT and on all of its actor children in a root
to leaf ordering. If PARENT is the keyword :universe then the CONTEXT reference
MUST be also supplied. If so, then this will map the FUNC across all actors
in the scene tree EXCEPT the universe actor itself."
  (flet ((apply-as-actor (transform)
           (funcall func (v:actor transform))))
    (if (eq parent :universe)
        (let* ((core (v::core context))
               (universe (v::scene-tree core))
               (universe-transform
                 (v:component-by-type universe 'comp:transform)))
          ;; NOTE: Don't apply function to universe itself!
          (dolist (child (children universe-transform))
            (map-nodes #'apply-as-actor child)))
        (map-nodes #'apply-as-actor
                   (v:component-by-type parent 'comp:transform)))))

(defun interpolate-transforms (core)
  (let ((factor (float (v::clock-interpolation-factor (v::clock core)) 1f0)))
    (map-nodes
     (lambda (x)
       (resolve-model x factor)
       (reset-transform-replace-count x))
     (v:component-by-type (v::scene-tree core) 'transform))))

(defmethod v:make-component (context (component-type (eql 'transform))
                             &rest args)
  (let ((instance (make-instance component-type
                                 :type component-type
                                 :context context)))
    (apply #'reinitialize-instance instance args)
    instance))

(defmethod shared-initialize :after ((instance transform) slot-names
                                     &key
                                       actor
                                       translate
                                       translate/velocity
                                       rotate
                                       rotate/velocity
                                       scale
                                       scale/velocity)
  (with-slots (%translation %rotation %scale) instance
    (setf (v:actor instance) actor
          (v::state instance) :initialize)
    (v::initialize-translation %translation translate translate/velocity)
    (v::initialize-rotation %rotation rotate rotate/velocity)
    (v::initialize-scale %scale scale scale/velocity)))

(defun update-replace-count (transform state transform-type)
  (let ((actor (v::actor transform)))
    (when (and (plusp (v::transform-state-replace-count state))
               (null (v::transform-state-replace-warned-p state)))
      (warn (format nil "Attempted to replace transform state ~s more than ~
                         once in a physics step or frame, for actor ~s."
                    transform-type
                    actor))
      (setf (v::transform-state-replace-warned-p state) t))
    (incf (v::transform-state-replace-count state))))

(defun reset-transform-replace-count (transform)
  (setf (v::transform-state-replace-count (translation transform)) 0
        (v::transform-state-replace-count (rotation transform)) 0
        (v::transform-state-replace-count (scale transform)) 0)
  (values))

(defun process-deferred-instant-transform-updates (core)
  (map nil (lambda (x) (funcall x core))
       (nreverse (v::end-of-frame-work core)))
  (setf (v::end-of-frame-work core) nil))


;;; User protocol helper functions The user protocol functions are generic and
;;; live in the VIRALITY package, but they call out to the helper functions
;;; below.

(defun %get-model-matrix (transform copy)
  (let ((model (model transform)))
    (if copy (m4:copy model) model)))

(defun %get-translation (transform space)
  (m4:get-translation
   (ecase space
     (:local (local transform))
     (:model (model transform)))))

(defun %get-rotation (transform space)
  (q:from-mat4
   (ecase space
     (:local (local transform))
     (:model (model transform)))))

(defun %get-scale (transform space)
  (m4:get-scale
   (ecase space
     (:local (local transform))
     (:model (model transform)))))

(defun %translate (transform vec space replace instant)
  (let ((state (translation transform)))
    (ecase space
      (:inertial
       ;; NOTE: The interial frame is always in reference to the parent and
       ;; does not take into consideration (by design) the local orientation of
       ;; the transform.
       (v3:+! (v::transform-state-current state)
              (cond
                (replace
                 (update-replace-count transform state :translate)
                 v3:+zero+)
                (t (v::transform-state-current state)))
              vec)
       (when instant
         (push
          (lambda (core)
            (declare (ignore core))
            (v3:copy! (v::transform-state-previous state)
                      (v::transform-state-current state)))
          (v::end-of-frame-work (v::core transform)))))

      (:local
        ;; NOTE: The movement vector is always rotated to match the local
        ;; orientation of the transform before translating in that direction.
        (let ((local-vec (m3:*v3 (m4:rotation-to-mat3 (local transform)) vec)))
          (v3:+! (v::transform-state-current state)
                 (cond
                   (replace
                    (update-replace-count transform state :translate)
                    v3:+zero+)
                   (t (v::transform-state-current state)))
                 local-vec)
          (when instant
            (push
             (lambda (core)
               (declare (ignore core))
               (v3:copy! (v::transform-state-previous state) (v::transform-state-current state)))
             (v::end-of-frame-work (v::core transform))))))
      (:model (error "TRANSLATE not yet implemented for world space.")))
    ;; side-effects only; return T
    t))

(defun %translate/velocity (transform axis rate)
  (let ((state (translation transform)))
    (setf (v::transform-state-incremental state) (v3:velocity axis rate))))

(defun %rotate (transform quat space replace instant)
  (let ((state (rotation transform)))
    (ecase space
      (:local
        (q:rotate! (v::transform-state-current state)
                   (cond
                     (replace
                      (update-replace-count transform state :rotate)
                      q:+id+)
                     (t (v::transform-state-current state)))
                   quat)
        (when instant
          (push
           (lambda (core)
             (declare (ignore core))
             (q:copy! (v::transform-state-previous state) (v::transform-state-current state)))
           (v::end-of-frame-work (v::core transform)))))
      (:model (error "ROTATE not yet implemented for world space.")))
    ;; side-effects only; return T
    t))

(defun %rotate/velocity (transform axis rate)
  (let ((state (rotation transform)))
    (setf (v::transform-state-incremental state) (v3:velocity axis rate))))

(defun %scale (transform vec space replace instant)
  (let ((state (scale transform)))
    (ecase space
      (:local
        (v3:+! (v::transform-state-current state)
               (cond
                 (replace
                  (update-replace-count transform state :scale)
                  v3:+zero+)
                 (t (v::transform-state-current state)))
               vec)
        (when instant
          (push
           (lambda (core)
             (declare (ignore core))
             (v3:copy! (v::transform-state-previous state) (v::transform-state-current state)))
           (v::end-of-frame-work (v::core transform)))))
      (:model (error "SCALE not yet implemented for world space.")))
    ;; side-effects only; return T
    t))

(defun %scale/velocity (transform axis rate)
  (let ((state (scale transform)))
    (setf (v::transform-state-incremental state) (v3:velocity axis rate))))

(defun %scale-around (target-transform pivot-in-world-space scale-diff
                      &key (min-scale (v3:vec 0f0 0f0 0f0))
                        (max-scale
                         (v3:vec most-positive-single-float
                                 most-positive-single-float
                                 most-positive-single-float)))
  (let* (;; We need the translation of the target transform from its parent.
         (a (%get-translation target-transform :local))
         ;; We need the pivot point in the coordinte space of the _parent_
         ;; of the target.
         (parent-transform (parent target-transform))
         (b (%transform-point parent-transform pivot-in-world-space :model))
         ;; And the vector from the pivot point to the target in the parent's
         ;; coordinate frame.
         (c (v3:- a b))
         ;; Get the local scale of the target
         (s (%get-scale target-transform :local))
         ;; And what that scale would be if we performed the scale-diff.
         (asd (v3:+ s scale-diff)))
    ;; If any component of the scale dips down below or above the extents,
    ;; don't attempt to scale. There are different choices in how this can
    ;; be enforced, for now this is probably good enough.
    (v3:with-components ((asd asd) (mins min-scale) (maxs max-scale))
      (unless (or (< asdx minsx) ;; TODO: Need any< and any> in origin.
                  (< asdy minsy)
                  (< asdz minsz)
                  (> asdx maxsx)
                  (> asdy maxsy)
                  (> asdz maxsz))
        (let* (;; Compute the relative scale vector
               (rs (v3:/ asd s))
               ;; And then use it to compute where the new location position
               ;; is going to be for the target frame.
               (scaled-c (v3:* c rs))
               (fp (v3:+ b scaled-c)))
          ;; Now scale up or down the target's frame by the requested amount.
          (%scale target-transform scale-diff :local nil nil)
          ;; And adjust the target's translation vector from the parent to
          ;; put the pivot point into the same place as the target scales.
          (%translate target-transform fp :inertial t nil))))
    ;; side-effects only; return T
    t))

(defun %transform-point (transform point space)
  (let ((model (model transform)))
    (ecase space
      (:local (m4:*v3 model point))
      (:model (m4:*v3 (m4:invert model) point)))))

(defun %transform-vector (transform vec space)
  (let ((model (m4:copy (model transform))))
    (m4:set-translation! model model v3:+zero+)
    (ecase space
      (:local (m4:*v3 model vec))
      (:model (m4:*v3 (m4:invert model) vec)))))

(defun %transform-direction (transform direction space)
  (let ((model (m4:copy (model transform))))
    (m4:set-translation! model model v3:+zero+)
    (m4:normalize-rotation! model model)
    (ecase space
      (:local (m4:*v3 model direction))
      (:model (m4:*v3 (m4:invert model) direction)))))

(defun %transform-forward (transform)
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :z)))

(defun %transform-backward (transform)
  (m4:rotation-axis-to-vec3 (model transform) :z))

(defun %transform-up (transform)
  (m4:rotation-axis-to-vec3 (model transform) :y))

(defun %transform-down (transform)
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :y)))

(defun %transform-right (transform)
  (m4:rotation-axis-to-vec3 (model transform) :x))

(defun %transform-left (transform)
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :x)))
