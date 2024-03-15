(in-package #:virality.prefab)

(defmacro preprocess-spec (prefab-name context policy spec)
  (labels ((rec (data)
             (u:mvlet ((name components children (split-spec data)))
               `(list ',name
                      ,@(mapcar #'thunk components)
                      ,@(mapcar #'rec children))))
           (thunk (data)
             (destructuring-bind (type . options/args) data
               (let ((options-p (listp (first options/args))))
                 `(list ',type
                        ',(when options-p (first options/args))
                        ,@(loop :with args = (if options-p
                                                 (rest options/args)
                                                 options/args)
                                :for (key value) :on args :by #'cddr
                                :collect key
                                :collect
                                `(make-injectable-ref-value-thunk
                                  :thunk (lambda (,context)
                                           (declare (ignorable ,context))
                                           ,value)
                                  :env-injection-control-func
                                  (u:dlambda
                                    (:actors
                                     (x)
                                     (setf actor-table x))
                                    (:components
                                     (x)
                                     (setf component-table x))
                                    (:current-actor
                                     (x)
                                     (setf current-actor x))
                                    (:current-component
                                     (x)
                                     (setf current-component x))))))))))
    `(list
      ,@(mapcar
         #'rec
         (list (cons (list prefab-name :policy policy) spec))))))

(defmacro inject-ref-environment (&body body)
  `(let (actor-table component-table current-actor current-component)
     (flet ((ref (&rest args)
              (when args
                (lookup-reference args
                                  current-actor
                                  current-component
                                  actor-table
                                  component-table))))
       ;; NOTE: If a prefab doesn't make a ref, this just removes a DCE note on
       ;; SBCL.
       (ref)
       ,@body)))

(defmethod documentation ((object string) (doc-type symbol))
  (u:when-let ((prefab (%find-prefab object doc-type)))
    (doc prefab)))

(defun update-prefab/interactively (spec)
  (v::push-queue
   :recompile
   (list
    :prefab
    (lambda (core)
      (dolist (entity (u:href (v::prefab-entities core) spec))
        (let* ((transform (v:component-by-type entity 'comp:transform))
               (parent (comp::parent transform)))
          (declare (ignore parent))
          ;; TODO: Figure out why parenting isn't working
          (v::destroy entity)
          (load-prefab core spec nil)))))))

(defmacro define-prefab (name (&key library (context 'context) policy)
                         &body body)
  (let ((prefabs `(u:href v::=meta/prefabs= ',library)))
    (u:with-gensyms (prefab data)
      (u:mvlet ((body decls doc (u:parse-body body :documentation t)))
        `(progn
           (ensure-prefab-name-string ',name)
           (ensure-prefab-name-valid ',name)
           (ensure-prefab-library-set ',name ',library)
           (ensure-prefab-library-symbol ',name ',library)
           (unless ,prefabs
             (setf ,prefabs (u:dict #'equalp)))
           ;; NOTE: This prefab-wide ref environment is accessible via a
           ;; pandoric function in the INJECTABLE-REF-VALUE-THUNK instance
           ;; created for each component initarg value. We use it later to
           ;; adjust which actors and components are available for V:REF when
           ;; forcing the argument thunk. We COULD have made an
           ;; INJECTABLE-REF-VALUE-THUNK for EACH argument value, but that would
           ;; generate more garbage than this method. So, unless we find we have
           ;; to do that, we'll just do it for all the components in this prefab
           ;; itself.
           (inject-ref-environment
             (u:mvlet* ((,data (preprocess-spec
                                ,name ,context ,policy ,body))
                        (,prefab (make-prefab ',name ',library ,doc ,data)))
               (setf (u:href ,prefabs ',name) ,prefab
                     (func ,prefab) (make-factory ,prefab))
               (parse-prefab ,prefab)))
           (update-prefab/interactively '(,name ,library)))))))
