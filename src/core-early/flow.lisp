(in-package #:virality)

;;;; Implementation of structure FLOW-STATE

(defun make-flow-state (&rest initargs)
  (apply #'make-instance 'flow-state initargs))

(defun canonicalize-binding (binding)
  (if (symbolp binding)
      (list binding nil)
      (list (first binding) (second binding))))

(defun binding-partition (bindings)
  (loop :for (b v) :in bindings
        :collect b :into bs
        :collect v :into vs
        :finally (return (values bs vs))))

(defun generate-reset-function (symbols values)
  (let* ((tmp-symbols
           (loop :for symbol :in symbols
                 :collect (u:format-symbol nil "~a-ONCE-ONLY"
                                           (symbol-name symbol))))
         (once-only-bindings (mapcar
                              (lambda (tsym value) `(,tsym ,value))
                              tmp-symbols
                              values)))
    `(let ,once-only-bindings
       (lambda ()
         ,(when (plusp (length tmp-symbols))
            `(setf
              ,@(mapcan
                 (lambda (symbol value)
                   `(,symbol ,value))
                 symbols
                 tmp-symbols)))))))

(defun ensure-matched-symbol (symbol name)
  (assert (string= (symbol-name symbol) (string-upcase name))))

(defun parse-flow-state-functions (name funcs)
  "Parse the selection, action, and transition form from the FUNCS list.
They can be in any order, but return them as a values in the specific order of
selector, action, and transition."
  (let ((ht (u:dict #'eq)))
    (dolist (func-form funcs)
      (when func-form
        (setf (u:href ht (first func-form)) (second func-form))))
    (u:mvlet ((selector selector-present-p (u:href ht 'selector))
              (action action-present-p (u:href ht 'action))
              (transition transition-present-p (u:href ht 'transition)))
      (unless selector-present-p
        (error "Missing selector function in flow-state: ~a" name))
      (unless action-present-p
        (error "Missing action function in flow-state: ~a" name))
      (unless transition-present-p
        (error "Missing transition function in flow-state: ~a" name))
      (values selector action transition))))

(defun parse-flow-state (form)
  "Parse a single flow-state DSL form and return a form which creates the
flow-state CLOS instance for it."
  (destructuring-bind (match name policy binds . funcs) form
    (let ((binds (mapcar #'canonicalize-binding binds)))
      (u:mvlet ((selector action transition (parse-flow-state-functions
                                             name funcs))
                (bind-syms bind-vals (binding-partition binds)))
        (ensure-matched-symbol match "flow-state")
        (let ((reset-function (generate-reset-function bind-syms bind-vals)))
          ;; Generate the instance maker for this flow-state.
          `(,name
            (let ,binds
              ;; TODO: Add a generated function to get the binding values out in
              ;; order of definition.
              (make-flow-state :name ',name
                               :policy ,policy
                               :exiting-p ,(null transition)
                               :selector ,selector
                               :action ,action
                               :transition ,transition
                               :reset ,reset-function))))))))

(defun get-flow-state-variables (flow-states)
  (loop :for (nil name . nil) :in flow-states
        :collect `(,name ',name) :into binds
        :collect `(declare (ignorable ,name)) :into ignores
        :finally (return (values binds ignores))))

(defun parse-flow (form)
  "Parse a single flow DSL node into a form that evaluates to a hash table
containing each flow-state indexed by name."
  (destructuring-bind (match flow-name . flow-states) form
    (multiple-value-bind (binds ignores) (get-flow-state-variables flow-states)
      (let ((flow-table (u:make-gensym '#:flow-table)))
        (ensure-matched-symbol match "flow")
        `(,flow-name
          (let ((,flow-table (u:dict #'eq))
                ,@binds)
            ,@ignores
            ,@(loop :for (name state) :in (mapcar #'parse-flow-state
                                                  flow-states)
                    :collect `(setf (u:href ,flow-table ',name) ,state))
            ,flow-table))))))

(defun parse-call-flows (form)
  "Parse an entire call-flow and return a list of the name of it and a form
which evaluates to a hash table of flows keyed by their name."
  (let ((call-flows (u:make-gensym '#:call-flows)))
    `(let ((,call-flows (u:dict #'eq)))
       ,@(loop :for (name flow) :in (mapcar #'parse-flow form)
               :collect `(setf (u:href ,call-flows ',name) ,flow))
       ,call-flows)))

(defun execute-flow (core call-flow-name flow-name flow-state-name
                     &key come-from-state-name)
  "Find the CALL-FLOW-NAME call flow in CORE, then lookup the flow FLOW-NAME,
and then the state FLOW-STATE-NAME inside of that flow. Start execution of the
flow from FLOW-STATE-NAME. Return a values of two states, the one which executed
before the currnet one which led to an exit of the call-flow.
COME-FROM-STATE-NAME is an arbitrary symbol that indicates the previous
flow-state name. This is often a symbolic name so execute-flow can determine how
the flow exited. Return two values The previous state name and the current state
name which resulted in the exiting of the flow."
  #++(:printv "Entering flow: (~a ~a ~a)"
              call-flow-name flow-name flow-state-name)
  (loop :with call-flow = (get-call-flow call-flow-name core)
        :with flow = (get-flow flow-name call-flow)
        :with flow-state = (get-flow-state flow-state-name flow)
        :with current-state-name = come-from-state-name
        :with last-state-name
        :with selections
        :with policy = :identity-policy
        :do
        #++(:printv "Processing flow-state: ~a, exiting: ~a"
                    (name flow-state) (exiting-p flow-state))
        ;; Step 1: Record state transition and update to current.
           (setf last-state-name current-state-name
                 current-state-name (name flow-state))
           ;; Step 2: Perform execution policy
           (ecase (policy flow-state)
             (:reset
              (funcall (reset flow-state))))
           ;; Step 3: Run Selector Function
           (multiple-value-bind (the-policy the-selections)
               (if (selector flow-state)
                   (funcall (selector flow-state) core)
                   (values :identity-policy nil))
             (setf selections the-selections
                   policy the-policy))
           ;; Step 4: Iterate the action across everything in the selections.
           ;; NOTE: This is not a map-tree or anything complex. It just assumes
           ;; selection is going to be one of:

           ;; If the policy is :identity-policy, then the selection can be: a
           ;; single hash table instance a single instance of some class a list
           ;; of things that are either hash tables or class instances. If the
           ;; policy is :type-policy, then the selection can be: a single
           ;; type-table instance (more semantics for :type-policy could be
           ;; added at a later date).
           (labels ((act-on-item (item)
                      (u:when-let ((action (action flow-state)))
                        (etypecase item
                          (hash-table (u:do-hash-values (v item)
                                        (funcall action v)))
                          (atom (funcall action item)))))
                    (act-on-type-table (type-key type-table)
                      ;; Get the hash of components for the type-key
                      (u:when-found (component-table
                                     (u:href type-table type-key))
                        (act-on-item component-table))))
             (ecase policy
               ;; TODO: :type-policy is in this branch until I write the code
               ;; path that it is supposed to take with that policy.
               ((:identity-policy)
                (if (listp selections)
                    (dolist (item selections)
                      (act-on-item item))
                    (act-on-item selections)))
               ((:type-policy)
                (let* ((component-dependency-graph
                         (u:href (analyzed-graphs core) 'component-dependency))
                       (annotation (annotation component-dependency-graph))
                       (dependency-type-order
                         (toposort component-dependency-graph)))
                  ;; Now, walk the list of dependency order and run the action
                  ;; in the topologically sorted type order.
                  (dolist (dependency-type dependency-type-order)
                    (cond
                      ;; If it is a regular type, then act on all available
                      ;; components for that type.
                      ((is-syntax-form-p '(component-type) dependency-type)
                       (act-on-type-table (second dependency-type) selections))
                      ;; If it is the unknown type, then run all components in
                      ;; the unknown type set.
                      ((is-syntax-form-p '(unknown-types) dependency-type)
                       (act-on-type-table (unknown-type-id annotation)
                                          selections))
                      (t
                       (error "EXECUTE-FLOW :type-policy is broken."))))))))
           ;; Step 5: Exit if reached exiting state.
           (when (exiting-p flow-state)
             #++(:printv "Exiting flow: (~a ~a ~a)"
                         call-flow-name flow-name current-state-name)
             (return-from execute-flow
               (values last-state-name
                       current-state-name)))
           ;; Step 6: Run the transition function to determine the next
           ;; flow-state. Currently, a transition can only go into the SAME
           ;; flow.
           (flet ((act-on-transition (transition)
                    (etypecase transition
                      (function (funcall transition core))
                      (symbol transition))))
             (setf flow-state (u:href flow (act-on-transition
                                            (transition flow-state)))))))

(defun get-call-flow (call-flow-name core)
  (u:href (call-flows core) call-flow-name))

(defun get-flow (flow-name call-flow)
  (u:href call-flow flow-name))

(defun get-flow-state (flow-state-name flow)
  (u:href flow flow-state-name))

(defmacro define-call-flow (name () &body body)
  `(setf (u:href =meta/call-flows= ',name) ,(parse-call-flows body)))

(defun load-call-flows (core)
  (u:do-hash (k v =meta/call-flows=)
    (setf (u:href (call-flows core) k) v)))
