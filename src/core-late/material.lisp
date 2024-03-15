(in-package #:virality)


;; material-profile impl

;; NOTE: will be replaced by defstruct constructor
(defun %make-material-profile (&rest init-args)
  (apply #'make-instance 'material-profile init-args))

(defun %add-material-profile (profile core)
  (setf (u:href (profiles (materials core)) (name profile)) profile))


;; material impl

(defun %make-material (id shader instances attributes profiles core)
  (make-instance 'material :id id
                           :shader shader
                           :instances instances
                           :attributes attributes
                           :profile-overlay-names profiles
                           :core core))

(defmethod (setf instances) (value (mat material))
  (with-slots (%instances) mat
    (setf %instances value)))

(defmethod (setf attributes) (value (mat material))
  (with-slots (%attributes) mat
    (setf %attributes value)))


(defun %deep-copy-material (current-mat new-mat-name)
  (let* ((new-id new-mat-name)
         (new-shader (shader current-mat))
         (new-shader-program (shader-program current-mat))
         (new-instances (instances current-mat))
         (new-attributes
           (copy-seq (attributes current-mat)))
         (new-uniforms (u:dict #'eq))
         (new-blocks (u:dict #'eq))
         (new-profile-overlay-names
           (copy-seq (profile-overlay-names current-mat)))
         (new-active-texture-unit (active-texture-unit current-mat))
         (new-mat
           ;; TODO: Fix %make-material so I don't have to do this.
           (make-instance 'material
                          :id new-id
                          :shader new-shader
                          :shader-program new-shader-program
                          :instances new-instances
                          :attributes new-attributes
                          :core (core current-mat)
                          :uniforms new-uniforms
                          :blocks new-blocks
                          :profile-overlay-names new-profile-overlay-names
                          :active-texture-unit new-active-texture-unit)))
    ;; Now we copy over the uniforms
    (u:do-hash (k v (uniforms current-mat))
      (setf (u:href new-uniforms k)
            (%deep-copy-material-uniform-value v new-mat)))
    ;; Now we copy over the blocks.
    (u:do-hash (k v (blocks current-mat))
      (setf (u:href new-blocks k)
            (%deep-copy-material-block-value v new-mat)))
    new-mat))

(defun bind-material-uniforms (mat)
  (when mat
    (u:do-hash (k v (uniforms mat))
      (when (functionp (semantic-value v))
        (execute-composition/semantic->computed v))
      (funcall (binder v) (shader-program mat) k (computed-value v)))))

(defun bind-material-buffers (mat)
  (when mat
    (u:do-hash (k v (blocks mat))
      (declare (ignore k v))
      ;; TODO: we probably need to call into core buffer binding management
      ;; services to perform the binding right here.
      nil)))

;; export PUBLIC API
(defun bind-material (mat)
  (bind-material-uniforms mat)
  (bind-material-buffers mat))

(defun uniform-ref-p (mat uniform-var)
  "Return a generalized boolean if the specified uniform ref exists or not."
  (u:href (uniforms mat) uniform-var))

;; TODO: these modify the semantic-buffer which then gets processed into a new
;; computed buffer.
(defun uniform-ref (mat uniform-var)
  (u:if-let ((material-uniform-value (u:href (uniforms mat) uniform-var)))
    (semantic-value material-uniform-value)
    (error "Material ~s does not have the referenced uniform ~s.~%~
            Please add a uniform to the material, and/or check your material ~
            profile settings."
           (id mat) uniform-var)))

;; We can only set the semantic-value, which gets automatically upgraded to the
;; computed-value upon setting.
(defun (setf uniform-ref) (new-val mat uniform-var)
  (u:if-let ((material-uniform-value (u:href (uniforms mat) uniform-var)))
    (progn
      ;; TODO: Need to do something with the old computed value since it might
      ;; be consuming resources like when it is a sampler on the GPU.
      (setf (semantic-value material-uniform-value) new-val)
      (execute-composition/semantic->computed material-uniform-value)
      (semantic-value material-uniform-value))
    (error "Material ~s does not have the referenced uniform ~s.~%~
            Please add a uniform to the material, and/or check your material ~
            profile settings."
           (id mat) uniform-var)))

;; export PUBLIC API
;; This is read only, it is the computed value in the material.
(defun mat-computed-uniform-ref (mat uniform-var)
  (computed-value (u:href (uniforms mat) uniform-var)))

;;; Default conversion functions for each uniform type.

(defun determine-binder-function (material glsl-type)
  (typecase glsl-type
    (symbol
     (if (sampler-p glsl-type)
         (let ((unit (active-texture-unit material)))
           (incf (active-texture-unit material))
           (lambda (shader uniform-name texture)
             (gl:active-texture unit)
             (gl:bind-texture (sampler-type->texture-type glsl-type)
                              (tex::texid texture))
             (shadow:uniform-int shader uniform-name unit)))
         (ecase glsl-type
           (:bool (lambda (shader uniform value)
                    (shadow:uniform-int shader uniform (if value 1 0))))
           (:int #'shadow:uniform-int)
           (:float #'shadow:uniform-float)
           (:vec2 #'shadow:uniform-vec2)
           (:vec3 #'shadow:uniform-vec3)
           (:vec4 #'shadow:uniform-vec4)
           (:mat2 #'shadow:uniform-mat2)
           (:mat3 #'shadow:uniform-mat3)
           (:mat4 #'shadow:uniform-mat4))))
    (cons
     (if (sampler-p (car glsl-type))
         (let* ((units
                  (loop :for i :below (cdr glsl-type)
                        :collect (prog1 (active-texture-unit material)
                                   (incf (active-texture-unit material)))))
                (units (coerce units 'vector)))
           (lambda (shader uniform-name texture-array)
             ;; Bind all of the textures to their active units first
             (loop :for texture :across texture-array
                   :for unit :across units
                   :do (gl:active-texture unit)
                       (gl:bind-texture
                        (sampler-type->texture-type (car glsl-type))
                        (tex::texid texture)))
             (shadow:uniform-int-array shader uniform-name units)))
         (ecase (car glsl-type)
           (:bool #'shadow:uniform-int-array)
           (:int #'shadow:uniform-int-array)
           (:float #'shadow:uniform-float-array)
           (:vec2 #'shadow:uniform-vec2-array)
           (:vec3 #'shadow:uniform-vec3-array)
           (:vec4 #'shadow:uniform-vec4-array)
           (:mat2 #'shadow:uniform-mat2-array)
           (:mat3 #'shadow:uniform-mat3-array)
           (:mat4 #'shadow:uniform-mat4-array))))
    (t
     (error "Cannot determine binder function for glsl-type: ~s~%"
            glsl-type))))


(defun annotate-material-uniform (uniform-name uniform-value material
                                  shader-program)
  (u:if-found (type-info (u:href (shadow:uniforms shader-program)
                                 uniform-name))
    (let ((uniform-type (u:href type-info :type)))
      ;; 1. Find the uniform in the shader-program and get its
      ;; type-info. Use that to set the binder function.
      (setf (binder uniform-value) (determine-binder-function
                                    material uniform-type))
      ;; 2. Figure out the semantic->computed composition function
      ;; for this specific uniform type. This will be the last
      ;; function executed and will convert the in-flight semantic
      ;; value into a real computed value for use by the binder
      ;; function.
      ;; NOTE: We set it to NIL here because if we're changing the
      ;; shader on a material, we'll push multiple copies of the this
      ;; sequence into the list when we resolve-material on that copy
      ;; with the changed shader--which is wrong. This will now do
      ;; the right thing in any (I believe) situation.
      (setf (semantic->computed uniform-value) nil)
      (push (if (sampler-p uniform-type)
                (gen-sampler/sem->com)
                (if (force-copy uniform-value)
                    (gen-default-copy/sem->com)
                    #'identity/for-material-custom-functions))
            (semantic->computed uniform-value))
      ;; 3. Put the user specified semantic transformer function into
      ;; the composition sequence before the above.
      (push (transformer uniform-value)
            (semantic->computed uniform-value))
      ;; 4. Execute the composition function sequence to produce the computed
      ;; value. NOTE: This will cause a texture to be looked up in the resource
      ;; cache right here (which may bring it in from disk and put it on the
      ;; GPU if that has already not been done).
      (execute-composition/semantic->computed uniform-value))
    (error "Material ~s uses unknown uniform ~s in shader ~s."
           (id material) uniform-name (shader material))))

(defun annotate-material-uniforms (material shader-program)
  (u:do-hash (k v (uniforms material))
    (annotate-material-uniform k v material shader-program)))

(defun annotate-material-block (alias-name block-value material shader-program
                                core)
  (declare (ignore shader-program core))
  ;; 1. Validate that this material-block-value is present in the shaders in
  ;; core
  ;; TODO: 2. Create the block-name-alias, but only once.
  (unless (shadow:find-block alias-name)
    (shadow:create-block-alias (storage-type block-value)
                               (block-name block-value)
                               (shader material)
                               alias-name)))

(defun annotate-material-blocks (material shader-program core)
  ;; TODO: Ensure that all :block-alias names are unique in the material
  (u:do-hash (k v (blocks material))
    (annotate-material-block k v material shader-program core)))

(defun resolve-material (material-instance)
  "Convert semantic-values to computed-values. Type check the uniforms against
the shader program in the material."
  (let ((core (core material-instance)))
    (u:if-found (shader-program (u:href (shaders core)
                                        (shader material-instance)))
      (progn
        (with-slots (%shader-program) material-instance
          (setf %shader-program shader-program))
        (annotate-material-uniforms material-instance shader-program)
        (annotate-material-blocks
         material-instance shader-program core))
      (error "Material ~s uses an undefined shader: ~s."
             (id material-instance) (shader material-instance)))))

;; An accessor for the material type pops up all the way over here since we
;; need to understand that it requires a large amount of high level work to
;; perform its function of changing the shader program associated with the
;; amterial..
(defmethod (setf shader) (value (mat material))
  (with-slots (%shader) mat
    (setf %shader value)
    (resolve-material mat))
  value)
