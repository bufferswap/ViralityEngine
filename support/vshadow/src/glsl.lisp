(in-package #:vshadow.glsl)

(cl:defmacro defun (name args &body body)
  "Define a GPU function."
  (u:with-gensyms (split-details deps fn spec)
    (let ((split-args (varjo.utils:split-arguments args '(&uniforms &context))))
      (destructuring-bind (in-args uniforms context) split-args
        `(varjo:with-constant-inject-hook #'s::lisp-constant->glsl-constant
           (varjo:with-stemcell-infer-hook #'s::lisp-symbol->glsl-type
             (let* ((,fn (varjo:add-external-function ',name ',in-args ',uniforms ',body)))
               (when (s::meta :track-dependencies-p)
                 (let* ((,spec (s::get-function-spec ,fn))
                        (,split-details
                          (varjo:test-translate-function-split-details
                           ',name ',in-args ',uniforms ',context ',body))
                        (,deps (varjo:used-external-functions (first ,split-details))))
                   (s::store-function-dependencies ,spec ,deps)
                   (funcall (s::meta :modify-hook) (s::compute-outdated-programs ,spec))))
               ,fn)))))))

(cl:defmacro defstruct (name &body slots)
  "Define a GPU structure."
  `(varjo:define-vari-struct ,name () ,@slots))

(cl:defmacro defmacro (name lambda-list &body body)
  "Define a GPU macro."
  `(varjo:define-vari-macro ,name ,lambda-list ,@body))

(cl:defmacro define-shader (name (&key (version :430) (primitive :triangles)) &body body)
  "Create a new shader program using the stage-specs defined in BODY.

VERSION: The default version shader stages use, and can be overridden on a per-function basis.

PRIMITIVE: The drawing primitive to use for the vertex stage."
  `(u:eval-always
     (setf (u:href (s::meta :shader-definitions) ',name)
           (lambda ()
             (s::%make-shader-program ',name ,version ,primitive ',body)))))
