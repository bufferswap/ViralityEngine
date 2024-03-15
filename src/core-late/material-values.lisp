(in-package #:virality)

;; material-uniform-value impl

(defun %make-material-uniform-value (&rest init-args)
  (apply #'make-instance 'material-uniform-value init-args))

(defun execute-composition/semantic->computed (material-uniform-value)
  ;; Execute a compositional sequence of functions whose inputs start with the
  ;; semantic-value and end with the computed-value. If the semantic-value is
  ;; actually a function, then invoke it and get the juice it produced and
  ;; shove it down the pipeline.
  (let ((context (context (core (material material-uniform-value))))
        (mat (material material-uniform-value))
        (sv (semantic-value material-uniform-value)))
    (loop :with value = (if (functionp sv)
                            (funcall sv context mat)
                            sv)
          :for transformer :in (semantic->computed material-uniform-value)
          :do (setf value (funcall transformer value context mat))
          :finally (setf (computed-value material-uniform-value) value))))

(defun %deep-copy-material-uniform-value (material-uniform-value new-mat)
  (let ((copy-mat-value
          (%make-material-uniform-value
           :material new-mat
           ;; Copier func lambda list is (semantic-value context new-mat)
           :semantic-value (funcall (copier material-uniform-value)
                                    (semantic-value material-uniform-value)
                                    (context (core new-mat))
                                    new-mat)
           :transformer (transformer material-uniform-value)
           :copier (copier material-uniform-value)
           :semantic->computed (copy-seq (semantic->computed
                                          material-uniform-value))
           :force-copy (force-copy material-uniform-value)
           :computed-value nil
           :binder (binder material-uniform-value))))
    ;; NOTE: Repair the nil computed-value!
    ;;
    ;; TODO: It is the case that the re-execution of this pipeline might not
    ;; result in an identical computed-value from the original (depending on
    ;; edge cases like gamedev supplied uniform value functions). This should
    ;; be documented and possibly a new flag (that we must implement) specified
    ;; in the define-material form if you want this pipeline to re-execute on
    ;; deep copies.
    (execute-composition/semantic->computed copy-mat-value)
    copy-mat-value))

(defun sampler-type->texture-type (sampler-type)
  "Given a SAMPLER-TYPE, like :sampler-2d-array, return the kind of
texture-type that is appropriate for it, such as :texture-2d-array. Do this for
all sampler types and texture types."
  (u:if-found (texture-type (u:href tex::+sampler-type->texture-type+
                                    sampler-type))
    texture-type
    (error "Unknown sampler-type: ~a~%" sampler-type)))

(defun sampler-p (glsl-type)
  "Return T if the GLSL-TYPE is a sampler like :sampler-2d or :sampler-buffer,
or if it a vector of the same. Return NIL otherwise."
  (let ((raw-type (if (symbolp glsl-type)
                      glsl-type
                      (car glsl-type))))
    (member raw-type
            '(:sampler-1d :isampler-1d :usampler-1d
              :sampler-2d :isampler-2d :usampler-2d
              :sampler-3d :isampler-3d :usampler-3d
              :sampler-cube :isampler-cube :usampler-cube
              :sampler-2d-rect :isampler-2d-rect :usampler-2d-rect
              :sampler-1d-array :isampler-1d-array :usampler-1d-array
              :sampler-2d-array :isampler-2d-array :usampler-2d-array
              :sampler-cube-array :isampler-cube-array :usampler-cube-array
              :sampler-buffer :isampler-buffer :usampler-buffer
              :sampler-2d-ms :isampler-2d-ms :usamplers-2d-ms
              :sampler-2d-ms-array :isampler-2d-ms-array :usampler-2d-ms-array)
            :test #'eq)))

(defun gen-sampler/sem->com ()
  "Generates a function that: Returns a function that takes three arguments:
SEMANTIC-VALUE, CONTEXT, and MAT. SEMANTIC-VALUE must be a symbol naming a
DEFINE-TEXTURE form, or a vector of such names. CONTEXT, the context object,
and MAT the material that contains this semantic value are ignored in the
returned function. The returned function will convert the symbolic names to
actual TEXTURE objects and load all the images from the texture into the
GPU. It will only do this once per TEXTURE name no matter how many times this
is called to perform a conversion. The result is a TEXTURE object, or a vector
of them corresponding in order to the input."
  (lambda (semantic-value context mat)
    (declare (ignore mat))
    (etypecase semantic-value
      ((or cons symbol)
       (resource-cache-lookup
        context
        :texture
        (tex::canonicalize-texture-name semantic-value)))
      (vector
       (map 'vector
            (lambda (sv)
              (resource-cache-lookup
               context :texture (tex::canonicalize-texture-name sv)))
            semantic-value)))))

;; TODO: Check the performance of this returned function, especially related to
;; the use of COPY-SEQUENCE-TREE.
(defun gen-default-copy/sem->com ()
  (lambda (semantic-value context mat)
    (declare (ignore context mat))
    (u:copy-sequence-tree semantic-value)))

(defun identity/for-material-custom-functions (semval context material)
  "This is effectively IDENTITY in that it returns SEMVAL unchanged, but
accepts and ignores the CONTEXT and MATERIAL arguments."
  (declare (ignore context material))
  semval)


;; material-block-value impl

(defun %make-material-block-value (&rest init-args)
  (apply #'make-instance 'material-block-value init-args))

(defun %deep-copy-material-block-value (material-block-value new-mat)
  (%make-material-block-value
   :material new-mat
   :block-name (block-name material-block-value)
   :storage-type (storage-type material-block-value)
   :binding-policy (binding-policy material-block-value)
   :binding-buffer (binding-buffer material-block-value)
   :bound-once-p (bound-once-p material-block-value)))
