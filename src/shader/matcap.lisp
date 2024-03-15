(in-package #:virality.shader)

(defun matcap/v ((mesh-attrs mesh-attrs)
                 &uniforms
                 (model :mat4)
                 (view :mat4)
                 (proj :mat4)
                 (normal-matrix :mat3))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (let ((normal (normalize (* normal-matrix mesh/normal))))
      (values (* proj view model (vec4 mesh/pos 1))
              normal))))

(defun matcap/f ((normal :vec3)
                 &uniforms
                 (view :mat4)
                 (sampler :sampler-2d))
  (let ((uv (+ 0.5 (* 0.5 (vec2 (* view (vec4 (normalize normal) 0)))))))
    ;; TODO: flipping y is a problem since gltf, webgl, blender, whatever, are
    ;; not interchangable and we need to deal with it in some agnostic way.
    ;; Probably when loading the specific interchange model format (gltf,
    ;; blender, FBX, etc), we should use the context of the load time of those
    ;; formats to indicate in the texture descriptor expansions if we should
    ;; flip the y in the loading of the texture data or not.
    (texture sampler (vec2 (.x uv) (- 1 (.y uv))))))

(define-shader matcap ()
  (:vertex (matcap/v mesh-attrs))
  (:fragment (matcap/f :vec3)))
