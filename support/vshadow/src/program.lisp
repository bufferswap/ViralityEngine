(in-package #:vshadow)

(defclass program ()
  ((%id :reader id
        :initform 0)
   (%name :reader name
          :initarg :name)
   (%version :reader version
             :initarg :version)
   (%translated-stages :reader translated-stages
                       :initform nil)
   (%source :reader source
            :initform (u:dict #'eq))
   (%primitive :reader primitive
               :initarg :primitive)
   (%stage-specs :reader stage-specs
                 :initarg :stage-specs)
   (%attributes :reader attributes
                :initform (u:dict #'eq))
   (%uniforms :reader uniforms
              :initform (u:dict #'eq))
   (%blocks :reader blocks
            :initform (u:dict #'equal))))

(defun find-program (program-name)
  (u:href (meta :programs) program-name))

(defun get-program-id (program-name)
  (id (find-program program-name)))

(defun view-source (program-name stage)
  (u:when-let ((program (find-program program-name)))
    (format t "~a" (u:href (source program) stage))))

(defun compile-stages (program)
  (let ((shaders nil))
    (u:do-hash (k v (source program))
      (let* ((type (stage-type->shader-type k))
             (shader (gl:create-shader type)))
        (gl:shader-source shader v)
        (gl:compile-shader shader)
        (push shader shaders)
        (unless (gl:get-shader shader :compile-status)
          (error "Failed to compile ~a shader stage:~%~a~%" type (gl:get-shader-info-log shader)))))
    shaders))

(defun link-program (shaders)
  (let ((program (gl:create-program)))
    (case program
      (0
       (dolist (shader shaders)
         (gl:delete-shader shader))
       (error "Failed to create program: ~a." (gl:get-error)))
      (t
       (dolist (shader shaders)
         (gl:attach-shader program shader))
       (gl:link-program program)
       (unless (gl:get-program program :link-status)
         (error "Failed to link shader program: ~a." (gl:get-program-info-log program)))
       (dolist (shader shaders)
         (gl:detach-shader program shader)
         (gl:delete-shader shader))))
    program))

(defun build-shader-program (name)
  "Compile the shader stages of NAME, linking them into a program. NAME refers to a previously
defined shader program using MAKE-SHADER-PROGRAM."
  (let* ((program (find-program name))
         (id (link-program (compile-stages program))))
    (setf (slot-value program '%id) id)
    (store-attribute-locations program)
    (store-uniforms program)
    (store-uniform-locations program)
    id))

(defun build-shader-dictionary ()
  "Compile all shader programs defined with MAKE-SHADER-PROGRAM."
  (let ((programs (meta :programs)))
    (u:maphash-keys #'build-shader-program programs)
    programs))

(defun store-stage-program-dependencies (program)
  (let ((stage-fn->programs (meta :stage-fn->programs)))
    (dolist (stage-spec (stage-specs program))
      (destructuring-bind (stage-type func-spec) stage-spec
        (declare (ignore stage-type))
        (pushnew (name program) (u:href stage-fn->programs func-spec))))))

(defun translate-program (program)
  (with-slots (%name %version %primitive %stage-specs %translated-stages) program
    (let ((stages (translate-stages %version %primitive %stage-specs)))
      (dolist (stage stages)
        (store-source program stage)
        (store-blocks program stage))
      (setf %translated-stages stages))))

(defun %make-shader-program (name version primitive stage-specs)
  (let ((program (make-instance 'program
                                :name name
                                :version version
                                :primitive primitive
                                :stage-specs stage-specs)))
    (setf (u:href (meta :programs) name) program)
    (translate-program program)
    (store-attributes program)
    (store-uniforms program)
    (store-stage-program-dependencies program)
    program))

(defun translate-shader-programs (program-list)
  "Re-translate a collection of shader programs."
  (dolist (program-name program-list)
    (translate-program (find-program program-name))))

(defun build-shader-programs (program-list)
  "Recompile a collection of shader programs."
  (dolist (program-name program-list)
    (build-shader-program program-name)))

(defun set-modify-hook (function)
  "Specify a function to be called when shader programs need to be updated."
  (setf (meta :modify-hook) function))

(defmacro with-shader (name &body body)
  "Run a body of code which uses (as in glUseProgram) the program identified by NAME."
  `(unwind-protect
        (progn
          (gl:use-program (id (find-program ,name)))
          ,@body)
     (gl:use-program 0)))
