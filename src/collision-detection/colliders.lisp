(in-package #:virality)

;;;; Implementation of datatype COLLIDER-SYSTEM

;; TODO: This is a naive collider resolution system that doesn't even take into
;; consideration or provide the feature of a sleeping collider. Also the main
;; algorithm in N^2 worst case. We take great care in handling the entrance and
;; exits of colliders as they enter this system to prevent multiple events from
;; being sent to referents of colliders and to correctly handle the removal
;; (via whatever means, like destruction or detachment) of colliders from thier
;; actors.

(defun make-collider-system (&rest init-args)
  (apply #'make-instance 'collider-system init-args))

;; TODO: Add a new component Cfs-NNN state for computing physics collisions that
;; happens just after disabling a component. NOTE: Think about this more.
(defun register-collider (context collider)
  "Add a new collider that may participate in the collision system."
  (let* ((cs (collider-system (core context)))
         (registering-colliders (registering-colliders cs)))
    ;; Insert the request for processing.
    (setf (u:href registering-colliders (comp:on-layer collider) collider)
          collider)))

(defun deregister-collider (context collider)
  "Mark that a collider is ready to leve the collision system."
  (let* ((cs (collider-system (core context)))
         (deregistering-colliders (deregistering-colliders cs)))
    ;; Insert the request for processing.
    (setf (u:href deregistering-colliders (comp:on-layer collider) collider)
          collider)))

;;; Contacts are symmetric in the internal data structures.

(defun contact-p (collider-system fist-collider face-collider)
  "Return a generalized boolean if the FIST-COLLIDER and the FACE-COLLIDER are
currently in contact."
  (assert (not (eq fist-collider face-collider)))
  ;; since there is a symmetric link, I can check any one and be satisfied.
  (let ((contacts (contacts collider-system)))
    (when (u:href contacts fist-collider)
      (u:when-let ((found-p (u:href contacts fist-collider face-collider)))
        ;; generalized-boolean.
        found-p))))

(defun enter-contact (collider-system fist-collider face-collider)
  (assert (not (eq fist-collider face-collider)))
  (let ((contacts (contacts collider-system)))
    ;; First, we add a link: fist -> face.
    (unless (u:href contacts fist-collider)
      (setf (u:href contacts fist-collider) (u:dict #'eq)))
    (setf (u:href contacts fist-collider face-collider) face-collider)
    ;; Then we add a symmetric back link: face -> fist.
    (unless (u:href contacts face-collider)
      (setf (u:href contacts face-collider) (u:dict #'eq)))
    (setf (u:href contacts face-collider fist-collider) fist-collider)
    ;; Now that the contact has been added we'll invoke the enter protocol for
    ;; the contact.
    (on-collision-enter fist-collider face-collider)
    (on-collision-enter face-collider fist-collider)
    :enter))

(defun continue-contact (collider-system fist-collider face-collider)
  "Assuming the contact already exists, call the continue protocol for the
FIST-COLLIDER and FACE-COLLIDER."
  (declare (ignore collider-system))
  (assert (not (eq fist-collider face-collider)))
  ;; If we're continuing to collide, run the protocol!
  (on-collision-continue fist-collider face-collider)
  (on-collision-continue face-collider fist-collider)
  :continue)

(defun exit-contact (collider-system fist-collider face-collider)
  "Remove the contact between the FIST and the FACE."
  (assert (not (eq fist-collider face-collider)))
  (let ((contacts (contacts collider-system)))
    ;; Remove the link: fist -> face
    (u:when-let ((face-set (u:href contacts fist-collider)))
      (remhash face-collider face-set)
      ;; If the fist is colliding with nothing now, remove its table.
      (when (zerop (hash-table-count face-set))
        (remhash fist-collider contacts)))
    ;; Remove the link: face -> fist
    (u:when-let ((fist-set (u:href contacts face-collider)))
      (remhash fist-collider fist-set)
      ;; If the face is colliding with nothing now, remove its table.
      (when (zerop (hash-table-count fist-set))
        (remhash face-collider contacts)))
    ;; Now that the contact has been removed, invoke the exit protocol for the
    ;; exiting contacts.
    (on-collision-exit fist-collider face-collider)
    (on-collision-exit face-collider fist-collider)
    :exit))

(defun remove-all-contacts (collider-system fist-collider)
  "Remove the FIST-COLLIDER from the contacts db and any contacts it might have
had--and update all other faces too."
  (let ((contacts (contacts collider-system)))
    ;; Look up the fist to see if anything at all is contacting it.
    (u:when-let ((face-set (u:href contacts fist-collider)))
      ;; NOTE: get a list of the faces, since we'll be altering the hash tables
      ;; while iterating over the faces
      (let ((face-colliders (u:hash-keys face-set)))
        ;; Now force exit each symmetric contact, if present. This will handle
        ;; removing hash tables we don't need anymore.
        (dolist (face-collider face-colliders)
          (when (contact-p collider-system fist-collider face-collider)
            #++(:printv "remove-all-contacts: attempting to remove ~a from ~
                        contacting ~a."
                        fist-collider face-collider)
            (exit-contact collider-system fist-collider face-collider)))))))

(defun compute-contact-state (collider-system fist-collider face-collider)
  ;; 1. Compute if a collision happend.
  ;; 2. look into the contacts table and see what to do.
  (let ((collided-p (comp:collide-p fist-collider face-collider))
        (contact-p (contact-p collider-system fist-collider face-collider)))
    ;; split up into clauses this way for easier understanding. The clauses
    ;; happen in order of what I speculate will be the most common to least
    ;; common (but it likely depends on the game a lot I'm sure.)
    (cond
      ((and (not collided-p) (not contact-p))
       ;; nothing to do cause nothing continues to happen with these two
       ;; colliders!
       nil)
      ((and collided-p contact-p)
       ;; The collision is continuing. Inform both colliders they are in
       ;; a continuing collision.
       (continue-contact collider-system fist-collider face-collider))
      ((and (not collided-p) contact-p)
       ;; We just stopped colliding with something.
       (exit-contact collider-system fist-collider face-collider))
      ((and collided-p (not contact-p))
       ;; We just started colliding with something.
       (enter-contact collider-system fist-collider face-collider)))))

;; a test funtion for now, normally the internal functions will be called at
;; different times in the flow.
(defun compute-all-collisions  (collider-system)
  ;; NOTE: This order is not arbitrary! I need to thikn hard about how
  ;; registering and deregistering colliders work in the same frame. I may have
  ;; to deregister first, then stable, then register. It depends on how I treat
  ;; a registering collider showing up in a frame with a deregistering
  ;; collider... do I register first and then deregister? Or vice versa? We
  ;; compute these first....
  (compute-stable-collisions collider-system)
  ;; and then add in the imcoming "diffs" next.
  (compute-registering-collisions collider-system)
  ;; We do deregistering last to catch the case where a collider existed for one
  ;; frame. We'll get an enter/exit protocol on it properly.
  (compute-deregistering-collisions collider-system))

;;; Stable collisions

(defun compute-stable-collisions (collider-system)
  ;; Update the collision status for stable colliders in the order of the
  ;; layers. N^2 for now cause it is fast to write. There is no spatial
  ;; partitioning.
  (dolist (fist-layer (physics-layers collider-system))
    (let ((face-layers (u:href (collision-plan collider-system) fist-layer)))
      (dolist (face-layer face-layers)
        (compute-stable-layer-collisions collider-system
                                         fist-layer face-layer)))))

(defun compute-stable-layer-collisions (collider-system fist-layer face-layer)
  ;; NOTE: we have to be very careful about sending duplicate events for the
  ;; same contact. If A and B collide, A gets a message about B
  ;; colliding with it, and B gets a message about A colliding with it, and
  ;; we better not later collide B with A and reinvoke the protocol! This
  ;; is why we do the two different methods since if we did
  ;; do-hash-keys-pairwise with the same hash table for both entries, we'll
  ;; get duplicate contacts. Hence, we use map-combinations in that case.
  (if (eq fist-layer face-layer)
      ;; In this scenario, we map-combinations over the hash table containing
      ;; the stable-colliders that are going to collide with themselves. We do
      ;; it a little strangely to avoid producing even more garbage caused by
      ;; asking for the hash-keys or hash-values as a list--instead we use a
      ;; dynamically growing reusable buffer.
      ;; However this does two passes over the keys, so there's that....
      (u:when-let ((fists-and-faces
                    (u:href (stable-colliders collider-system) fist-layer)))
        (setf (fill-pointer (buffer collider-system)) 0)
        (u:do-hash-keys (fist/face fists-and-faces)
          (vector-push-extend fist/face (buffer collider-system)))
        ;; compute collisions between each _unique_ pair of fists-and-faces
        (when (>= (length (buffer collider-system)) 2)
          (u:map-combinations
           (lambda (x)
             (compute-contact-state collider-system (aref x 0) (aref x 1)))
           (buffer collider-system)
           :length 2
           ;; NOTE: :copy nil will reuse the same vector for each invocation
           ;; of the function, so very little garbage produced.
           :copy nil)))
      ;; ELSE simply iterate pairwise each fist collider over all the face
      ;; colliders. No chance of duplicate invocation of protocol here.
      (u:when-let ((fists (u:href (stable-colliders collider-system) fist-layer))
                   (faces (u:href (stable-colliders collider-system) face-layer)))
        (when (and (plusp (hash-table-count fists))
                   (plusp (hash-table-count faces)))
          (do-hash-keys-pairwise
              (lambda (fist face)
                (compute-contact-state collider-system fist face))
            fists faces)))))

;;; Registering collisions

(defun compute-registering-collisions (collider-system)
  ;; For each registering collider, collide it against every appropriate
  ;; collision layer in the stable colliders--computing the contact state as
  ;; necessary. Then, move the registering collider into the stable collider
  ;; state. This should only allow non-duplicate contact discovery. We can go
  ;; out of order, but choose to process the registering colliders in the order
  ;; of the physics-layers.
  (let ((registering-colliders (registering-colliders collider-system))
        (stable-colliders (stable-colliders collider-system)))
    ;; First, we process each physics-layer in order.
    ;; TODO: This is O(n) each time, we could do a better job of knowing what
    ;; registering colliders we actually have when there are a LOT of
    ;; physics-layers.
    (dolist (fist-layer (physics-layers collider-system))
      ;; We will be processing each registering fist collider of the current
      ;; fist-layer.
      (let ((fist-layer-registering-colliders
              (u:href registering-colliders fist-layer)))
        (unless (zerop (hash-table-count fist-layer-registering-colliders))
          ;; Now for each fist in our specified fist-layer, we'll collide it
          ;; against each collider in every single face physics layer its
          ;; on-layer slot implies.
          (u:do-hash-keys (fist fist-layer-registering-colliders)
            ;; we're processing it right now, so no matter what happens we
            ;; remove it from the registering-colliders.
            (remhash fist fist-layer-registering-colliders)
            ;; If somehow it is also currently stable, we totally ignore it.
            (unless (u:href stable-colliders fist-layer fist)
              ;; The FIST is good to go! collide it and stabilize it!
              (let ((face-layers
                      (u:href (collision-plan collider-system) fist-layer)))
                #++(:printv "Checking registering fist: ~s, [~s: ~s]"
                            (display-id fist) (comp:on-layer fist)
                            face-layers)
                (cond
                  ((null face-layers)
                   ;; If no face layers to collide against AT ALL, automatically
                   ;; stabilize the fist and we're done with it.
                   #++(:printv " Stabilizing[0]: ~s" (display-id fist))
                   (setf (u:href stable-colliders fist-layer fist) fist))
                  (t
                   ;; Else, we collide the fist against each face in each
                   ;; layer.
                   (dolist (face-layer face-layers)
                     #++(:printv " Checking contacts between layers: ~s <=> ~s"
                                 fist-layer face-layer)
                     ;; Find all the face-layer colliders to which we need to
                     ;; collide.
                     (let ((face-layer-stable-colliders
                             (u:href stable-colliders face-layer)))
                       ;; Do the work of colliding the single fist to all
                       ;; faces in this face-layer.
                       (unless (zerop (hash-table-count
                                       face-layer-stable-colliders))
                         (u:do-hash-keys (face face-layer-stable-colliders)
                           #++(:printv "  compute-contact-state: [reg: ~s <-> ~
                                      stable: ~s]"
                                       (display-id fist) (display-id face))
                           (compute-contact-state collider-system fist face)))))
                   ;; And when we *FINISH* colliding the specific registering
                   ;; fist against all of the stable faces in all face-layers
                   ;; its on-layer implied, *THEN* we stabilize the fist. This
                   ;; is so the next registering fist can collide against it if
                   ;; need be.
                   ;; NOTE: We CANNOT stabilize until AFTER the registering fist
                   ;; has been collided with all stable faces.
                   #++(:printv " Stabilizing[1]: ~s" (display-id fist))
                   (setf (u:href stable-colliders fist-layer fist)
                         fist)))))))))))

(defun compute-deregistering-collisions (collider-system)
  ;; For each deregistering collider, remove it from the stable-colliders and
  ;; remove it from registering-colliders. Then remove all contacts for it. We
  ;; don't need to do this in any particular order but we choose the physics
  ;; layer ordering for convenience.
  (let ((registering-colliders (registering-colliders collider-system))
        (stable-colliders (stable-colliders collider-system))
        (deregistering-colliders (deregistering-colliders collider-system)))
    (dolist (fist-layer (physics-layers collider-system))
      (let ((fist-layer-deregistering-colliders
              (u:href deregistering-colliders fist-layer)))
        (unless (zerop (hash-table-count fist-layer-deregistering-colliders))
          (u:do-hash-keys (fist fist-layer-deregistering-colliders)
            ;; 1. Remove from stable-colliders, cause it is leaving.
            (remhash fist (u:href stable-colliders fist-layer))
            ;; 2. Remove from registering-colliders. Either they have been
            ;; processed by now and this is empty or they will never be
            ;; processed cause the collider is leaving.
            (remhash fist (u:href registering-colliders fist-layer))
            ;; 3. Break all current fist contacts (if any)
            (remove-all-contacts collider-system fist)
            ;; 4. Now remove from myself, cause I just processed it.
            (remhash fist fist-layer-deregistering-colliders)))))))

;; TODO: This needs to be in a DSL like define-physics or something. The
;; collision plan describes a lower left triangle of a collision matrix with the
;; indicies in both axes being in the same order as the :layers list. @ means
;; that isn't a valid spot to put either a space (for these two do not collide)
;; or an X (which means these two do collide).
;;
;; Here is a collision system: (notice :scenery collides with nothing)
;; (NOTE: :p represents the keyword :planet, it was too long to fit)
;;
;;              :ground :player :player-bullet :enemy :enemy-bullet :scenery :p
;;               --------------------------------------------------------------
;; :ground       | X       @           @          @         @           @     @
;; :player       | X                   @          @         @           @     @
;; :player-bullet| X                              @         @           @     @
;; :enemy        | X       X           X                    @           @     @
;; :enemy-bullet | X       X           X                                @     @
;; :scenery      |                                                            @
;; :planet       |                                X
;;
;; Then, the collision plan is an sparse representation of which columns for
;; each row are marked X.

(defun initialize-collider-system (core)
  (let* ((collider-system-desc
           ;; TODO: This should be a DSL in a define-physics macro or something.
           `(:physics-layers
             (:ground :player :player-bullet :enemy :enemy-bullet :scenery
              :planet)
             ;; NOTE: Format and legality described in comment above.
             :collision-plan
             ;; The KEY is the row header, the VALUE is the X locations that
             ;; indicate a collision situation.
             ,(u:dict
               #'eq
               :ground (list :ground)
               :player (list :ground)
               :player-bullet (list :ground)
               :enemy (list :ground :player :player-bullet)
               :enemy-bullet (list :ground :player :player-bullet)
               :scenery (list)
               :planet (list :enemy))))
         (new-collider-system
           (apply #'make-collider-system collider-system-desc)))
    (setf (collider-system core) new-collider-system)
    (with-accessors ((registering-colliders registering-colliders)
                     (stable-colliders stable-colliders)
                     (deregistering-colliders deregistering-colliders))
        new-collider-system
      (dolist (physics-layer (physics-layers new-collider-system))
        ;; Set up the htables to receive the collider references for each
        ;; physics-layer
        (setf (u:href registering-colliders physics-layer) (u:dict #'eq)
              (u:href stable-colliders physics-layer) (u:dict #'eq)
              (u:href deregistering-colliders physics-layer) (u:dict #'eq))))))

;; TODO: Move to utility file, maybe put in goldern-utils. Possibly extend to
;; take &rest and perform all combinations of keys/values/etc in hash table.
;; transfer-func deals with moving a from ht0 to ht1.
(defun do-hash-keys-pairwise (func ht0 ht1)
  (u:do-hash-keys (a ht0)
    (u:do-hash-keys (b ht1)
      (funcall func a b))))

(defun test-collider-system ()
  "Manually test the basic functionality of the collider system. To be run at
the repl when the game is NOT running."
  (let* ((core (make-instance 'core))
         (context (make-instance 'context :core core)))
    (setf (slot-value core '%context) context)
    (let* ((c0 (make-component context 'comp:sphere
                               :display-id "Ground"
                               :on-layer :ground
                               :center (v3:zero)
                               :radius 1))
           (c1 (make-component context 'comp:sphere
                               :display-id "Player"
                               :on-layer :player
                               :center (v3:vec -20f0 5f0 0f0)
                               :radius 1))
           (c2 (make-component context 'comp:sphere
                               :display-id "Player-Bullet"
                               :on-layer :player-bullet
                               :center (v3:vec -10f0 5f0 0f0)
                               :radius 1))
           (c3 (make-component context 'comp:sphere
                               :display-id "Enemy"
                               :on-layer :enemy
                               :center (v3:vec 20f0 5f0 0f0)
                               :radius 1))
           (c4 (make-component context 'comp:sphere
                               :display-id "Enemy-Bullet"
                               :on-layer :enemy-bullet
                               :center (v3:vec 10f0 5f0 0f0)
                               :radius 1))
           (c5 (make-component context 'comp:sphere
                               :display-id "Scenery 1"
                               :on-layer :scenery
                               :center (v3:vec 0f0 5f0 0f0)
                               :radius 1))
           (c6 (make-component context 'comp:sphere
                               :display-id "Scenery 2"
                               :on-layer :scenery
                               :center (v3:vec 1f0 5f0 0f0)
                               :radius 1)))
      ;; Set referent to the same component for
      (dolist (c (list c0 c1 c2 c3 c4 c5 c6))
        (setf (comp:referent c) c))
      (initialize-collider-system core)
      (register-collider context c0)
      (register-collider context c1)
      (register-collider context c2)
      (register-collider context c3)
      (register-collider context c4)
      (register-collider context c5)
      (register-collider context c6)
      (format t "Collider Pass 0: no colliding~%")
      (compute-all-collisions (collider-system core))
      (format t "Collider Pass 1: enter~%")
      (format t "Moving enemy-bullet.~%")
      (setf (center c4) (v3:vec -9f0 5f0 0f0))
      (compute-all-collisions (collider-system core))
      (format t "Collider Pass 2: continue~%")
      (format t "Moving enemy-bullet.~%")
      (setf (center c4) (v3:vec -10f0 5f0 0f0))
      (compute-all-collisions (collider-system core))
      (format t "Collider Pass 2a: continue~%")
      (format t "Moving enemy-bullet.~%")
      (setf (center c4) (v3:vec -11f0 5f0 0f0))
      (compute-all-collisions (collider-system core))
      (format t "Collider Pass 2b: continue~%")
      (format t "Moving enemy-bullet.~%")
      (setf (center c4) (v3:vec -12f0 5f0 0f0))
      (compute-all-collisions (collider-system core))
      (format t "Moving enemy-bullet.~%")
      (setf (center c4) (v3:vec -13f0 5f0 0f0))
      (format t "Collider Pass 3: exit~%")
      (compute-all-collisions (collider-system core))
      (format t "Collider Pass 4: no colliding~%")
      (compute-all-collisions (collider-system core))
      (deregister-collider context c0)
      (deregister-collider context c1)
      (deregister-collider context c2)
      (deregister-collider context c3)
      (deregister-collider context c4)
      (deregister-collider context c5)
      (deregister-collider context c6)
      (collider-system core))))

;;; Protocol methods

(defmethod on-collision-enter ((self component) (other component)))

(defmethod on-collision-continue ((self component) (other component)))

(defmethod on-collision-exit ((self component) (other component)))
