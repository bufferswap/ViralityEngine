(in-package #:virality)

;;;; all code dealing with the material (and profile) DSL parsing and inserting
;;;; into the metadata, reification and integration of the material metadata
;;;; into a running instance of core, and live recompilation handling of
;;;; materials.

(defun parse-material-uniforms (matvar uniforms)
  `(setf ,@(loop :for (var val . options) :in uniforms
                 :append
                 `((u:href (uniforms ,matvar) ,var)
                   (%make-material-uniform-value
                    :material ,matvar
                    :semantic-value ,val
                    :transformer
                    (or ,(getf options :transformer)
                        #'identity/for-material-custom-functions)
                    :copier
                    (or ,(getf options :copier)
                        #'identity/for-material-custom-functions)
                    ;; force-copy is nil by default.
                    :force-copy ,(getf options :force-copy))))))

(defun parse-material-blocks (matvar blocks)
  `(setf ,@(loop :for form :in blocks
                 :append (let ((block-name (getf form :block-name))
                               (storage-type (getf form :storage-type))
                               (block-alias (getf form :block-alias))
                               (binding-buffer (getf form :binding-buffer))
                               (binding-policy (getf form :binding-policy)))
                           `((u:href (blocks ,matvar) ,block-alias)
                             (%make-material-block-value
                              :material ,matvar
                              :block-name ,block-name
                              :storage-type ,storage-type
                              :binding-policy (or ,binding-policy :repeat)
                              :binding-buffer ,binding-buffer
                              :bound-once-p nil))))))

(defun parse-material-profile (name uniforms blocks)
  (u:with-gensyms (matprof)
    `(let* ((,matprof (%make-material-profile :name ',name)))
       ,(parse-material-uniforms matprof uniforms)
       ;; TODO: We prevent processing of blocks in material-profiles until we
       ;; discover if it is a good idea or not.
       ;; ,(parse-material-blocks matprof blocks)
       (when ',blocks
         (error "Interface blocks are not supported in material profiles: ~a"
                ',blocks))
       ,matprof)))

(defmacro define-material-profile (name &body (body))
  "Define a set of uniform and block shader attribute defaults that can be
applied in an overlay manner while defining a material."
  (u:with-gensyms (profile)
    (destructuring-bind (&key uniforms blocks) body
      `(let ((,profile ,(parse-material-profile name uniforms blocks)))
         (setf (u:href =meta/material-profiles= (name ,profile))
               ,profile)))))

(defun apply-material-profile-overlays (mat core)
  ;; Mix in the profiles, in order specified.
  (when (profile-overlay-names mat)
    ;; convert the overlay names to concrete overlay instances
    (let ((concrete-profiles
            (loop :for po-name :in (profile-overlay-names mat)
                  :for inst = (u:href (profiles (materials core)) po-name)
                  :if inst
                    :collect inst
                  :else
                    :do (error "Material profile name: ~s doesn't exist."
                               po-name))))
      ;; Insert the uniforms in the profile, overwriting whatever was present
      ;; for that uniform if it existed in a previous uniform.
      (dolist (concrete-profile concrete-profiles)
        (u:do-hash (uniform-name material-value (uniforms concrete-profile))
          (setf (u:href (uniforms mat) uniform-name)
                ;; NOTE: We copy here so there is no shared structure between
                ;; material-values and profiles.
                (%deep-copy-material-uniform-value material-value mat)))))))

(defun parse-material (name shader instances attributes profiles uniforms
                       blocks)
  "Return a function which creates a partially complete material instance.
It is partially complete because it does not yet have the shader binder
function available for it so BIND-UNIFORMS cannot yet be called on it."
  (u:with-gensyms (matvar)
    `(lambda (core)
       (let ((,matvar (%make-material ',name ',shader ,instances ',attributes
                                      ',profiles core)))
         ;; First, insert in order the profile overlays for this material.
         (apply-material-profile-overlays ,matvar core)
         ;; Then, overlay whatever uniforms and blocks the user specified over
         ;; over the profile supplied information, if any. This must come last.
         ,(parse-material-uniforms matvar uniforms)
         ,(parse-material-blocks matvar blocks)
         ,matvar))))

(defun resolve-all-materials (core)
  "Convert all semantic-values to computed-values for all materials. This must
be executed after all the shader programs have been compiled."
  ;; TODO: Check that all :block-alias names are actually unique between
  ;; materials.
  (%map-materials
   (lambda (mat)
     (resolve-material mat))
   core))

(defun load-materials (core)
  (u:do-hash-values (profile =meta/material-profiles=)
    (%add-material-profile profile core))
  (u:do-hash-values (material-func =meta/materials=)
    (%add-material (funcall material-func core) core))
  (resolve-all-materials core))


(defun update-material (old-material new-material)
  (with-slots ((old-tex %active-texture-unit)
               (old-attrs %attributes)
               (old-blocks %blocks)
               (old-instances %instances)
               (old-overlays %profile-overlay-names)
               (old-shader %shader)
               (old-uniforms %uniforms))
      old-material
    (with-slots ((new-tex %active-texture-unit)
                 (new-attrs %attributes)
                 (new-blocks %blocks)
                 (new-instances %instances)
                 (new-overlays %profile-overlay-names)
                 (new-shader %shader)
                 (new-uniforms %uniforms))
        new-material
      (setf old-tex new-tex
            old-attrs new-attrs
            old-blocks new-blocks
            old-overlays new-overlays
            old-shader new-shader
            old-uniforms new-uniforms))))


(defun update-material/interactively (name func)
  (push-queue
   :recompile
   (list
    :material
    (lambda (core)
      (u:mvlet ((old-material found-p (%lookup-material name core))
                (new-material (funcall func core)))
        (resolve-material new-material)
        (if found-p
            (update-material old-material new-material)
            (%add-material new-material core)))))))

(defmacro define-material (name &body (body))
  ;; TODO: better parsing and type checking of material forms...
  (u:with-gensyms (func)
    (destructuring-bind (&key shader profiles (instances 1) attributes uniforms
                           blocks)
        body
      `(let ((,func ,(parse-material name shader instances attributes profiles
                                     uniforms blocks)))
         (setf (u:href =meta/materials= ',name) ,func)
         (update-material/interactively ',name ,func)))))


(defun material-annotator (mat-val component)
  (with-accessors ((context context)) component
    (typecase mat-val
      (symbol
       (lookup-material mat-val context))
      (cons
       (destructuring-bind (base-mat-sym new-mat-sym
                            &key shader instances uniforms blocks)
           mat-val
         (let* ((new-mat-sym (u:make-gensym new-mat-sym))
                (base-mat (lookup-material base-mat-sym context))
                (copy-mat (copy-material base-mat new-mat-sym)))
           (when blocks
             (error "Material override: :blocks not implemented yet."))
           ;; First, change to the new shader
           (when shader
             (setf (shader copy-mat) shader))
           (when instances
             (setf (instances copy-mat) instances))
           ;; Then process the initargs for the new shader.
           (when uniforms
             (unless (every (lambda (x) (= (length x) 2)) uniforms)
               (error "Material override: :uniforms entries must have a length ~
                       of 2 ~a~%"
                      uniforms))
             (loop :for (uniform-name value) :in uniforms
                   :do (setf (uniform-ref copy-mat uniform-name) value)))
           ;; and return the newly minted material with all the overrides.
           copy-mat)))
      (t
       mat-val))))

;; TODO: Why must this be `material` even though `material` is the name of
;; the class in this package?
(define-annotation material
  :getter material-annotator
  :setter material-annotator)
