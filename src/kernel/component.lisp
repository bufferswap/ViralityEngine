(in-package #:virality)

;;;; Implementation of the datatype COMPONENT and how it moves through the
;;;; core book keeping tables.

;; NOTE: This is a toplevel form for this class!
(clear-annotations 'component)

(defmethod register-kernel-id ((kernel component))
  (u:when-let ((table (components-by-id (tables (core kernel)))))
    (register-kernel-id-in-table kernel table)))

(defmethod deregister-kernel-id ((kernel component))
  (u:when-let ((table (components-by-id (tables (core kernel)))))
    (deregister-kernel-id-from-table kernel table)))

(defmethod initialize-instance :after ((instance component) &key)
  (register-kernel-uuid instance)
  (register-kernel-id instance))

(defun qualify-component (core component-type)
  "This function tries to resolve the COMPONENT-TYPE symbol into a potentially
different packaged symbol of the same name that corresponds to a component
definition in that package. The packages are searched in the order they are are
defined in a toposort of the graph category COMPONENT-PACKAGE-ORDER. The result
should be a symbol suitable for MAKE-INSTANCE in all cases, but in the case of
mixin superclasses, it might not be desireable.
NOTE: If the component-type is a mixin class/component that is a superclass to a
component, then the first external to the package superclass definition found in
the package search order will be returned as the package qualified symbol.
NOTE: This function can not confirm that a symbol is a component defined by
DEFINE-COMPONENT. It can only confirm that the symbol passed to it is a
superclass of a DEFINE-COMPONENT form (up to but not including the COMPONENT
superclass type all components have), or a component created by the
DEFINE-COMPONENT form."
  (let ((search-table (component-search-table (tables core)))
        (component-type/class (find-class component-type nil))
        (base-component-type/class (find-class 'component))
        (graph (u:href (analyzed-graphs core) 'component-package-order)))
    (u:when-found (pkg-symbol (u:href search-table component-type))
      (return-from qualify-component pkg-symbol))
    (if (or (null component-type/class)
            (not (subtypep (class-name component-type/class)
                           (class-name base-component-type/class))))
        (dolist (potential-package (toposort graph))
          (let ((potential-package-name (second potential-package)))
            (dolist (pkg-to-search (u:href (pattern-matched-packages
                                            (annotation graph))
                                           potential-package-name))
              (u:mvlet ((symbol kind (find-symbol (symbol-name component-type)
                                                  pkg-to-search)))
                (when (and (eq kind :external)
                           (find-class symbol nil))
                  (setf (u:href search-table component-type) symbol)
                  (return-from qualify-component symbol))))))
        component-type)))

(defmethod make-component (context type &rest args)
  (u:if-let ((new-type (qualify-component (core context) type)))
    (apply #'make-instance new-type :type new-type :context context args)
    (error "Could not qualify the component type ~s." type)))

(defun attach-component (actor component)
  (let ((type (qualify-component (core actor) (component-type component))))
    (detach-component actor component)
    (enqueue-attach-event component actor)
    (setf (actor component) actor
          (u:href (components actor) component) component)
    (push component (u:href (%components-by-type actor) type))))

(defun attach-components (actor &rest components)
  (dolist (component components)
    (attach-component actor component)))

(defun detach-component (actor component)
  "If COMPONENT is contained in the ACTOR. Remove it. Otherwise, do nothing."
  (symbol-macrolet ((components (u:href (%components-by-type actor)
                                        (component-type component))))
    (when (remhash component (components actor))
      (enqueue-detach-event component actor)
      (setf (actor component) nil
            components (remove-if (lambda (x) (eq x component)) components)))))

(defmethod destroy ((kernel component) &key (ttl 0))
  (let ((table (u:href (component-predestroy-view (tables (core kernel))))))
    ;; TODO: Fix this. TTL is never nill so this won't do what we expect.
    (setf (ttl kernel) (and ttl (max 0 ttl)))
    (if ttl
        (setf (u:href table kernel) kernel)
        ;; If the TTL is stopped, we want to remove the component from the
        ;; pre-destroy view!
        (remhash kernel table))))

(defun component-by-type (actor type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns the rest of the components as a secondary value if there are more than
one of the same type."
  (let* ((core (core actor))
         (new-type (qualify-component core type))
         (components (u:href (%components-by-type actor) new-type)))
    (values (first components)
            (rest components))))

(defun component/countdown-to-destruction (component)
  (when (plusp (ttl component))
    (decf (ttl component) (frame-time (context component)))))

(defun enqueue-attach-event (component actor)
  (queues:qpush (attach/detach-event-queue component) (list :attached actor)))

(defun enqueue-detach-event (component actor)
  (queues:qpush (attach/detach-event-queue component) (list :detached actor)))

(defun dequeue-attach/detach-event (component)
  ;; NOTE: Returns NIL, which is not in our domain, when empty.
  (queues:qpop (attach/detach-event-queue component)))

(defun component/invoke-attach/detach-events (component)
  (loop :for event = (queues:qpop (attach/detach-event-queue component))
        :for (event-kind actor) = event
        :while event-kind
        :do (ecase event-kind
              (:attached (on-component-attach component actor))
              (:detached (on-component-detach component actor)))))

(defun component/preinit->init (component)
  (u:when-let ((thunk (initializer component)))
    (funcall thunk)
    (setf (initializer component) nil))
  (let* ((core (core component))
         (type (canonicalize-component-type (component-type component) core))
         (tables (tables core))
         (preinit-view (component-preinit-by-type-view tables))
         (init-view (component-init-by-type-view tables)))
    (remhash component (u:href preinit-view type))
    (unless (u:href init-view type)
      (setf (u:href init-view type) (u:dict #'eq)))
    (setf (u:href init-view type component) component)))

(defun component/init->active (component)
  (let* ((core (core component))
         (type (canonicalize-component-type (component-type component) core))
         (tables (tables core))
         (init-view (component-init-by-type-view tables))
         (active-view (component-active-by-type-view tables)))
    (remhash component (u:href init-view type))
    (unless (u:href active-view type)
      (setf (u:href active-view type) (u:dict #'eq)))
    (setf (u:href active-view type component) component
          (state component) :active)))

(defun component/init-or-active->destroy (component)
  (let* ((core (core component))
         (type (canonicalize-component-type (component-type component) core))
         (tables (tables core))
         (preinit-view (component-preinit-by-type-view tables))
         (active-view (component-active-by-type-view tables))
         (predestroy-view (component-predestroy-view tables))
         (destroy-view (component-destroy-by-type-view tables)))
    (unless (plusp (ttl component))
      (unless (u:href destroy-view type)
        (setf (u:href destroy-view type) (u:dict #'eq)))
      (setf (u:href destroy-view type component) component)
      (setf (state component) :destroy)
      (remhash component predestroy-view)
      (unless (remhash component (u:href active-view type))
        (remhash component (u:href preinit-view type))))))

(defun component/destroy->released (component)
  (let* ((core (core component))
         (type (canonicalize-component-type (component-type component) core))
         (destroy-view (component-destroy-by-type-view (tables core))))
    (remhash component (u:href destroy-view type))
    (detach-component (actor component) component)
    (deregister-kernel-uuid component)
    (deregister-kernel-id component)))

;;; Protocol methods

(defmethod on-component-initialize ((self component)))

(defmethod on-component-attach ((self component) (actor actor)))

(defmethod on-component-detach ((self component) (actor actor)))

(defmethod on-component-physics-update ((self component)))

(defmethod on-component-update ((self component)))

(defmethod on-component-render ((self component)))

(defmethod on-component-destroy ((self component)))
