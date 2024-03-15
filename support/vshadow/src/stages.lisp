(in-package #:vshadow)

(defun stage-type (stage)
  (varjo:stage-kind stage))

(defun stage-type->shader-type (stage-type)
  (ecase stage-type
    (:vertex :vertex-shader)
    (:tessellation-control :tess-control-shader)
    (:tessellation-evaluation :tess-evaluation-shader)
    (:geometry :geometry-shader)
    (:fragment :fragment-shader)
    (:compute :compute-shader)))

(defun make-stage (version primitive stage-spec)
  (destructuring-bind (stage-type func-spec) stage-spec
    (u:if-let ((func (find-gpu-function func-spec)))
      (varjo:make-stage stage-type
                        (varjo.internals:in-args func)
                        (varjo.internals:uniforms func)
                        `(,(ensure-keyword version))
                        (varjo.internals:code func)
                        t
                        (when (eq stage-type :vertex)
                          (varjo.internals:primitive-name-to-instance primitive)))
      (error "No function found for stage ~S with signature ~s."
             stage-type func-spec))))

(defun translate-stages (version primitive stage-specs)
  (varjo:with-constant-inject-hook #'lisp-constant->glsl-constant
    (varjo:with-stemcell-infer-hook #'lisp-symbol->glsl-type
      (varjo:rolling-translate (mapcar
                                (lambda (x) (make-stage version primitive x))
                                stage-specs)))))
