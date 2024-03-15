(in-package #:virality)

;;;; Implementation of datatype ACTOR

(defmethod register-kernel-id ((kernel actor))
  (u:when-let ((table (actors-by-id (tables (core kernel)))))
    (register-kernel-id-in-table kernel table)))

(defmethod deregister-kernel-id ((kernel actor))
  (u:when-let ((table (actors-by-id (tables (core kernel)))))
    (deregister-kernel-id-from-table kernel table)))

(defun make-actor (context &rest args &key &allow-other-keys)
  (let ((actor (apply #'make-instance 'actor :context context args)))
    (register-kernel-uuid actor)
    (register-kernel-id actor)
    actor))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE.
The actor is not yet in the scene and the main loop protocol will not be called
on it or its components. If keyword argument :PARENT is supplied it is an actor
reference which will be the parent of the spawning actor. It defaults to
:universe, which means make this actor a child of the universe actor."
  (let* ((core (core actor))
         (tables (tables core))
         (transform (component-by-type actor 'comp:transform)))
    (cond
      ((eq parent :universe)
       (unless (comp::parent transform)
         (comp:add-child
          (component-by-type (scene-tree core) 'comp:transform)
          (component-by-type actor 'comp:transform))))
      ((typep parent 'actor)
       (comp:add-child
        (component-by-type parent 'comp:transform)
        (component-by-type actor 'comp:transform)))
      ((null parent)
       (u:noop))
      (t
       (error "Cannot parent actor ~s to unknown parent ~s" actor parent)))
    ;; Populate book-keeping tables
    (let ((preinit-by-type (component-preinit-by-type-view tables)))
      (setf (u:href (actor-preinit-db tables) actor) actor)
      (u:do-hash-values (v (components actor))
        (let ((key (canonicalize-component-type (component-type v) core)))
          (unless (u:href preinit-by-type key)
            (setf (u:href preinit-by-type key) (u:dict #'eq)))
          (setf (u:href preinit-by-type key v) v))))))

(defmethod destroy ((kernel actor) &key (ttl 0))
  (let* ((core (core kernel))
         (table (actor-predestroy-view (tables core))))
    (when (eq (id kernel) 'universe)
      (error "Cannot destroy the scene tree root."))
    ;; TODO: this needs fixing because TTL is never nil
    (setf (ttl kernel) (and ttl (max 0 ttl)))
    ;; TODO: Same for this
    (if ttl
        (setf (u:href table kernel) kernel)
        ;; If the TTL is stopped, we want to remove the actor from the
        ;; pre-destroy view!
        (remhash kernel table))))

(defun actor/preinit->init (actor)
  (let ((tables (tables (core actor))))
    (remhash actor (actor-preinit-db tables))
    (setf (u:href (actor-init-db tables) actor) actor)))

(defun actor/init->active (actor)
  (let ((tables (tables (core actor))))
    (remhash actor (actor-init-db tables))
    (setf (state actor) :active
          (u:href (actor-active-db tables) actor) actor)))

(defun actor/init-or-active->destroy (actor)
  ;; TODO: A different logic error (that of a destroyed object not having its
  ;; components also destroyed), and a destroyed object being destroyed
  ;; twice--which is legal but not explicitly handled) caused this UNLESS to be
  ;; here. Replace with new flow.
  (unless (or (eq (state actor) :destroy)
              (plusp (ttl actor)))
    (let ((tables (tables (core actor))))
      (setf (u:href (actor-destroy-db tables) actor) actor
            (state actor) :destroy)
      (remhash actor (actor-predestroy-view tables))
      (unless (remhash actor (actor-active-db tables))
        (remhash actor (actor-preinit-db tables)))
      (u:do-hash-values (v (components actor))
        (setf (ttl v) 0)
        (enqueue-detach-event v actor)
        (component/init-or-active->destroy v)))))

(defun actor/destroy-descendants (actor)
  (flet ((destroy-actor (actor)
           (setf (ttl actor) 0)
           (actor/init-or-active->destroy actor)
           (deregister-kernel-uuid actor)
           (deregister-kernel-id actor)))
    (when actor
      (comp::map-nodes
       (lambda (x) (destroy-actor (actor x)))
       (component-by-type actor 'comp:transform)))))

(defun actor/disconnect (actor)
  (when (eq (id actor) 'universe)
    (error "Cannot disconnect the top-level universe node."))
  (let ((transform (component-by-type actor 'comp:transform)))
    (comp:remove-child (comp::parent transform) transform)))

(defun actor/destroy->released (actor)
  (let ((core (core actor)))
    (unless (zerop (hash-table-count (components actor)))
      (error "Destroyed actor still has components."))
    (remhash actor (actor-destroy-db (tables core)))))

(defun actor/countdown-to-destruction (actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (frame-time (context actor)))))
