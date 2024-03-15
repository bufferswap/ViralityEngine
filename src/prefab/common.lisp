(in-package #:virality.prefab)

;;;; Implementation of datatypes: PREFAB, NODE, INJECTABLE-REF-VALUE-THUNK

(u:define-printer (prefab stream :type t)
  (format stream "~a" (name prefab)))

(u:define-printer (node stream :type t)
  (format stream "~a" (path node)))

(defun make-injectable-ref-value-thunk (&rest init-args)
  (apply #'make-instance 'injectable-ref-value-thunk init-args))

(u:eval-always
  (defun split-spec (spec)
    (destructuring-bind (name . body) spec
      (loop :for tail :on body
            :for item = (first tail)
            :while (symbolp (first item))
            :collect item :into components
            :finally (return (values name components tail))))))

(defun explode-path (path)
  (split-sequence:split-sequence #\/ path :remove-empty-subseqs t))

(defun make-node-path (parent name)
  (let ((path (u:string-merge parent "/" name)))
    (string-right-trim "/" path)))

(defun make-node-path-from-parts (path-parts)
  (format nil "/~{~a~^/~}" path-parts))

(defun find-library (name)
  (u:if-found (library (u:href v::=meta/prefabs= name))
    library
    (error "Prefab library ~s does not exist." name)))

(defun %find-prefab (name library)
  (let ((library (find-library library)))
    (u:href library name)))

(defun find-prefab (name library)
  (or (%find-prefab name library)
      (error "Prefab ~s not found in library ~s." name library)))

(defun %find-node (path library)
  (let* ((name (first (explode-path path)))
         (prefab (%find-prefab name library)))
    (when prefab
      (u:href (parse-tree prefab) path))))

(defun find-node (path library)
  (or (%find-node path library)
      (error "Prefab node ~s not found in library ~s." path library)))

(defun map-nodes (func node)
  (funcall func node)
  (u:do-hash-values (child (children node))
    (map-nodes func child)))

(defun make-prefab (name library doc data)
  (let ((prefab (or (%find-prefab name library)
                    (make-instance 'prefab :name name
                                           :library library
                                           :doc doc))))
    (with-slots (%data %parse-tree) prefab
      (setf %data data)
      (clrhash %parse-tree))
    prefab))

(defun get-node-option (node option &optional (inherit-p t))
  (let ((parent (parent node)))
    (or (getf (options node) option)
        (and inherit-p
             parent
             (getf (options parent) option)))))
