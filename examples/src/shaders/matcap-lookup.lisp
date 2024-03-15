(in-package #:virality-examples.shader)

;; Using a map

(defun matcap-lookup/v ((mesh-attrs mesh-attrs)
                        &uniforms
                        (model :mat4)
                        (view :mat4)
                        (proj :mat4)
                        (normal-matrix :mat3))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (let ((normal (normalize (* normal-matrix mesh/normal))))
      (values (* proj view model (vec4 mesh/pos 1))
              mesh/pos
              mesh/uv1
              normal))))

(defun rgba-to-vec4 ((red :float) (green :float) (blue :float)  (alpha :float))
  (vec4 (/ red 255f0) (/ green 255f0) (/ blue 255f0) (/ alpha 255f0)))


(defun matcap-lookup/f ((pos :vec3)
                        (uv :vec2)
                        (normal :vec3)
                        &uniforms
                        (view :mat4)
                        (matcaps :sampler-2d-array)
                        (id-map :sampler-2d))
  (let* ((muv (+ 0.5 (* 0.5 (vec2 (* view (vec4 (normalize normal) 0))))))
         ;; TODO: reconsider the y flipping.
         (id-color (.rgb (texture id-map (vec2 (.x uv) (.y uv)))))
         (id (+ (* (.x id-color) 4) (* (.y id-color) 2) (* (.z id-color) 1)))
         (material (.rgb (texture matcaps (vec3 muv id))))
         (noise (vec3 (umbra.noise:simplex-perlin (* 250 pos))))
         (final (mix material noise 0.15)))
    (vec4 final 1)
    ;; TODO: This is debugging code to test when we read the gltf UVs properly.
    ;; Otherwise it could just be removed if it is some far time into the
    ;; future.
    #++(cond
         ((and (> (.x uv) .75) (> (.y uv) .75))
          (rgba-to-vec4 74 65 42 255))
         ((and (< (.x uv) .25) (< (.y uv) .25))
          (rgba-to-vec4 219 111 105 255))
         (t
          (vec4 id-color 1)))))


(define-shader matcap-lookup ()
  (:vertex (matcap-lookup/v mesh-attrs))
  (:fragment (matcap-lookup/f :vec3 :vec2 :vec3)))
