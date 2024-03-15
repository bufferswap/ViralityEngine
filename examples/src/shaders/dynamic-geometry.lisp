(in-package #:virality-examples.shader)

(defun dynamic-geometry/vert ((pos :vec2)
                              (uv :vec2)
                              &uniforms
                              (model :mat4)
                              (view :mat4)
                              (proj :mat4))
  (values (* proj view model (vec4 pos 0 1))
          uv))

(defun dynamic-geometry/frag ((uv :vec2))
  (vec4 1 0 0 1))

(define-shader dynamic-geometry ()
  (:vertex (dynamic-geometry/vert :vec2 :vec2))
  (:fragment (dynamic-geometry/frag :vec2)))
