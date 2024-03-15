(in-package #:virality.prefab)

(defun parse-prefab (prefab)
  (let (success-p)
    (with-slots (%name %library %data) prefab
      (unwind-protect
           (progn
             (make-parse-tree prefab %data)
             (verify-components prefab)
             (insert-missing-transforms prefab)
             (copy-source-nodes prefab)
             (make-relationships prefab)
             (make-component-table prefab)
             (update-links prefab)
             (setf success-p t))
        (unless success-p
          (remhash %name (u:href v::=meta/prefabs= %library)))))))

(defun parse-copy/link (library path target options)
  (destructuring-bind (&key id (display-id target) (copy nil copy-p)
                         (link nil link-p) policy)
      options
    (flet ((check-source (source)
             (ensure-copy/link-source-valid path source)
             (ensure-copy/link-source-absolute path source)
             (ensure-copy/link-source-no-trailing-slash path source))
           (make-path-options (source from)
             (values path
                     id
                     display-id
                     (list :mode (cond (copy-p 'copy) (link-p 'link))
                           :source source
                           :from from
                           :policy policy))))
      (let ((form (or copy link)))
        (typecase form
          (string
           (check-source form)
           (make-path-options form library))
          (list
           (destructuring-bind (source &key (from library)) form
             (check-source source)
             (ensure-copy/link-source-string path source)
             (ensure-copy/link-library path from)
             (make-path-options source from))))))))

(defun parse-path-spec (parent library path-spec)
  (labels ((check-path (path)
             (ensure-path-string path)
             (ensure-path-length parent path)
             (ensure-path-valid path)
             (ensure-path-relative path)
             (ensure-path-no-trailing-slash path)
             (ensure-path-not-parent path))
           (direct-path (path &key id display-id policy)
             (check-path path)
             (values (make-node-path parent path)
                     id
                     display-id
                     (list :mode 'direct :policy policy))))
    (typecase path-spec
      (string
       (direct-path path-spec :display-id path-spec))
      (list
       (destructuring-bind (target . options) path-spec
         (check-path target)
         (let ((path (make-node-path parent target)))
           (ensure-path-options-plist path options)
           (ensure-path-options-valid path options)
           (destructuring-bind (&key id (display-id target) copy link policy)
               options
             (ensure-path-options-keys path options)
             (u:if-let ((copy/link-form (or copy link)))
               (parse-copy/link library path target options)
               (direct-path target :id id
                                   :display-id display-id
                                   :policy policy)))))))))

(defun make-node (prefab path &optional options)
  (symbol-macrolet ((node (u:href (parse-tree prefab) path)))
    (unless node
      (let ((name (first (last (explode-path path))))
            (options (if options
                         (append options (list :mode :direct))
                         (list :mode :direct))))
        (setf node (make-instance 'node
                                  :name name
                                  :prefab prefab
                                  :path path
                                  :options options))))
    node))

(defun make-parse-tree (prefab data)
  (with-slots (%library %name %root %parse-tree) prefab
    (labels ((%make-nodes (parent data)
               (dolist (node-spec data)
                 (u:mvlet* ((name components children (split-spec node-spec))
                            (path id display-id options
                                  (parse-path-spec parent %library name)))
                   (with-slots (%id %display-id %options %components)
                       (make-node prefab path options)
                     (setf %id id
                           %display-id display-id
                           %options options)
                     (u:appendf %components components)
                     (%make-nodes path children)))))
             (%expand-path (parts)
               (u:when-let* ((parts (butlast parts))
                             (path (format nil "/~{~a~^/~}" parts)))
                 (make-node prefab path)
                 (%expand-path parts))))
      (let ((root-path (make-node-path nil %name)))
        (%make-nodes nil data)
        (setf %root (find-node root-path %library))
        (u:do-hash-keys (path %parse-tree)
          (%expand-path (explode-path path)))))))

(defun verify-components (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (dolist (component (components node))
      (ensure-component-list component path)
      (ensure-component-form component path)
      (destructuring-bind (type options . args) component
        (ensure-component-type-symbol type path)
        (ensure-component-type-exists type path)
        (ensure-component-options-plist type options path)
        (ensure-component-options-valid type options path)
        (ensure-component-policy type options path)
        (ensure-component-args-plist type args path)
        (ensure-component-args-valid type args path)))))

(defun insert-missing-transforms (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (with-slots (%components) node
      (unless (find 'comp:transform %components :key #'car)
        (push '(comp:transform (:policy :old-type)) %components))
      (ensure-path-single-transform %components path))))

(defun collect-source-components (node)
  (let (components)
    (u:do-hash (type table (components-table node))
      (u:do-hash-values (data table)
        (destructuring-bind (&key merge-id args &allow-other-keys) data
          (push `(,type (:merge-id ,merge-id) ,@args) components))))
    components))

(defun insert-source-components (source target)
  (with-slots (%components) target
    (setf %components (append (collect-source-components source) %components))))

(defun copy-source-nodes (prefab)
  (u:do-hash (path node (u:copy-hash-table (parse-tree prefab)))
    (destructuring-bind (&key source from &allow-other-keys) (options node)
      (when source
        (map-nodes
         (lambda (x)
           (if (string= source (path x))
               (insert-source-components x node)
               (let* ((sub-path (subseq (path x) (1+ (length source))))
                      (new-path (make-node-path path sub-path))
                      (new-node (make-node prefab new-path)))
                 (insert-source-components x new-node))))
         (find-node source from))))))

(defun make-relationships (prefab)
  (flet ((get-parent (path)
           (u:when-let ((path-parts (butlast (explode-path path))))
             (make-node-path-from-parts path-parts))))
    (with-slots (%library) prefab
      (u:do-hash (path node (parse-tree prefab))
        (u:when-let ((parent-path (get-parent path)))
          (setf (slot-value node '%parent) (find-node parent-path %library)
                (u:href (children (parent node)) path)
                (find-node path %library)))))))

(defun get-source-prefab (node)
  (destructuring-bind (&key source from &allow-other-keys) (options node)
    (let ((name (first (explode-path source))))
      (find-prefab name from))))

(defun make-links (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (destructuring-bind (&key mode source &allow-other-keys) (options node)
      (when (eq mode 'link)
        (let* ((source-prefab (get-source-prefab node))
               (target (cons (library prefab) path))
               (links (links source-prefab)))
          (symbol-macrolet ((targets (u:href links :source->targets source)))
            (setf (u:href links :target->source target) source)
            (unless targets
              (setf targets (u:dict #'equalp)))
            (setf (u:href targets target) node)))))))

(defun remove-broken-links (prefab)
  (let ((source->targets (u:href (links prefab) :source->targets))
        (target->source (u:href (links prefab) :target->source)))
    (u:do-hash (target source target->source)
      (destructuring-bind (library . path) target
        (unless (%find-node path library)
          (remhash target target->source)
          (remhash target (u:href source->targets source))
          (unless (u:href source->targets source)
            (remhash source source->targets)))))
    (u:do-hash-keys (source source->targets)
      (unless (%find-node source (library prefab))
        (remhash source source->targets)
        (u:do-hash (k v target->source)
          (when (string= v source)
            (remhash k target->source)))))))

(defun reinitialize-prefab (prefab)
  (clrhash (parse-tree prefab))
  (parse-prefab prefab)
  prefab)

(defun update-links-recursively (prefab)
  (u:do-hash-keys (target (u:href (links prefab) :target->source))
    (destructuring-bind (library . path) target
      (let* ((name (first (explode-path path)))
             (target-prefab (find-prefab name library)))
        (reinitialize-prefab target-prefab)
        (update-links-recursively target-prefab)))))

(defun update-links (prefab)
  (make-links prefab)
  (remove-broken-links prefab)
  (update-links-recursively prefab))

(defgeneric merge-component (policy node type id args))

;; new-type: replace previous type completely
;; old-type: no matter what, throw out the new type and keep the old one.
;; new-args: Intersecting initargs resolve to new ones, new initargs are taken.
;; old-args: Intersecting initargs resolve to old ones, new initargs are taken.

;; change names to:

;; new-type -> overwrite-type
;; old-type -> previous-type
;; new-args -> overlay-args
;; old-args -> previous-args

(defmethod merge-component ((policy (eql :new-type)) node type id args)
  (let ((table (components-table node)))
    (setf (u:href table type id)
          (list :merge-id id :policy policy :args args))))

(defmethod merge-component ((policy (eql :old-type)) node type id args)
  (let ((table (components-table node)))
    (u:unless-found (components (u:href table type id))
      (setf (u:href table type id)
            (list :merge-id id :policy policy :args args)))))

(defmethod merge-component ((policy (eql :new-args)) node type id args)
  (let* ((table (components-table node))
         (old-args (u:plist->hash
                    (getf (u:href table type id) :args)))
         (new-args (u:hash->plist
                    (u:hash-merge old-args (u:plist->hash args)))))
    (setf (u:href table type id)
          (list :merge-id id :policy policy :args new-args))))

(defmethod merge-component ((policy (eql :old-args)) node type id args)
  (let* ((table (components-table node))
         (old-args (u:plist->hash
                    (getf (u:href table type id) :args)))
         (new-args (u:hash->plist
                    (u:hash-merge (u:plist->hash args) old-args))))
    (setf (u:href table type id)
          (list :merge-id id :policy policy :args new-args))))

(defun make-component-table (prefab)
  (u:do-hash-values (node (parse-tree prefab))
    (dolist (component (components node))
      (destructuring-bind (type (&key merge-id policy) . args)
          component
        (let ((policy (or policy (get-node-option node :policy) :new-args)))
          (unless (u:href (components-table node) type)
            (setf (u:href (components-table node) type) (u:dict #'equalp)))
          (merge-component policy node type merge-id args))))))
