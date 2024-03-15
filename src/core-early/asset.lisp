(in-package #:virality)

;;; implementation of ASSET-SPEC

(u:define-printer (asset-spec stream :type nil)
  (format stream "~s (pool: ~s)" (name asset-spec) (pool asset-spec)))

(defun find-asset-pool (name)
  (u:href =meta/asset-pools= name))

(defun make-asset-spec (pool-name base-path data)
  (destructuring-bind (name path) data
    (let* ((pool (find-asset-pool pool-name))
           (base-path (uiop:ensure-directory-pathname base-path))
           (path (uiop:merge-pathnames* path base-path))
           (asset (make-instance 'asset-spec
                                 :pool pool-name
                                 :name name
                                 :path path)))
      (setf (u:href pool name) asset)
      asset)))

(defun find-asset-spec (pool-name spec-name)
  (u:if-let ((pool (find-asset-pool pool-name)))
    (or (u:href pool spec-name)
        (error "Asset ~s not found in pool ~s." spec-name pool-name))
    (error "Asset pool ~s does not exist." pool-name)))

(defun get-asset-pool-system (pool-name)
  (let ((package-name (package-name (symbol-package pool-name))))
    (or (asdf:find-system (u:make-keyword package-name) nil)
        (error "Asset pool ~s must be defined in a package with the same name ~
                as its ASDF system."
               pool-name))))

(defun make-asset-symbol (path)
  (intern
   (string-upcase
    (cl-slug:slugify
     (pathname-name path)))))

(defun asset-path-collect-p (path filter)
  (flet ((normalize-type (type)
           (string-downcase (string-left-trim "." type))))
    (let ((path-type (string-downcase (pathname-type path))))
      (some
       (lambda (x)
         (string= path-type (normalize-type x)))
       (u:ensure-list filter)))))

(defun update-asset-pool (pool-name path filter)
  (let ((pool (find-asset-pool pool-name)))
    (let* ((path (uiop:ensure-directory-pathname path))
           (system (get-asset-pool-system pool-name))
           (resolved-path (%resolve-path system path)))
      (clrhash pool)
      (u:map-files
       resolved-path
       (lambda (x)
         (let* ((asset-name (make-asset-symbol x))
                (file-name (file-namestring x))
                (spec (list asset-name file-name)))
           (u:if-found (existing (u:href pool asset-name))
             (error "Asset pool ~s has ambiguously named assets:~%~
                               File 1: ~a~%File 2: ~a~%Normalized name: ~a"
                    pool-name
                    file-name
                    (file-namestring (path existing))
                    asset-name)
             (make-asset-spec pool-name path spec))))
       :test (lambda (x) (if filter (asset-path-collect-p x filter) t))
       :recursive-p nil))))

(defun make-asset-pool (name path filter)
  (let ((pool (u:dict #'eq)))
    (setf (u:href =meta/asset-pools= name) pool)
    (update-asset-pool name path filter)
    pool))

(defmacro define-asset-pool (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key path filter) body
    `(if (u:href =meta/asset-pools= ',name)
         (update-asset-pool ',name ,path ',filter)
         (make-asset-pool ',name ,path ',filter))))

;;; implementation

;;; TODO: The below code feels really wrong and has a fundamental flaw. It needs
;;; inspection wrt to live coding vs deployed binary contexts.

(defun find-asset (context type key)
  (u:href (assets (core context)) type key))

(defun delete-asset (context type key)
  (remhash key (u:href (assets (core context)) type)))

(defun %resolve-path (system path)
  (if =release=
      #+sbcl
      (uiop:merge-pathnames*
       path
       (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
      #-sbcl
      (error "Release must be deployed on SBCL to load assets.")
      (asdf:system-relative-pathname system path)))

(defun resolve-system-path (path &optional (system :virality))
  (let* ((system (asdf:find-system system))
         (path (uiop:merge-pathnames*
                path
                (uiop:ensure-directory-pathname "data")))
         (resolved-path (%resolve-path system path)))
    resolved-path))

(defgeneric resolve-path (asset))

(defmethod resolve-path ((asset list))
  (destructuring-bind (pool-name spec-name) asset
    (let* ((spec (find-asset-spec pool-name spec-name))
           (system (get-asset-pool-system pool-name))
           (path (%resolve-path system (path spec))))
      (if (uiop:file-exists-p path)
          (values path spec)
          (error "File path not found for asset ~s of pool ~s.~%Path: ~s."
                 spec-name pool-name path)))))

(defmethod resolve-path ((asset string))
  (resolve-system-path asset :virality))

(defmacro with-asset-cache (context type key &body body)
  (u:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (assets (core ,context)) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (u:ensure-gethash ,key ,table (progn ,@body)))))

(define-asset-pool meshes ()
  :path "data/mesh"
  :filter "glb")

(define-asset-pool textures ()
  :path "data/texture"
  :filter "png")

(define-asset-pool matcaps ()
  :path "data/texture/matcaps"
  :filter "png")
