(in-package #:virality-examples.shader)

(defun font/distance ((color :vec3))
  (let ((r (.r color))
        (g (.g color))
        (b (.b color)))
    (- (max (min r g) (min (max r g) b)) 0.5)))

(defun font/vert ((pos :vec2)
                  (uv :vec2)
                  &uniforms
                  (model :mat4)
                  (view :mat4)
                  (proj :mat4)
                  (sampler :sampler-2d))
  (values (* proj view model (vec4 pos 0 1))
          uv))

(defun font/frag ((uv :vec2)
                  &uniforms
                  (color :vec4)
                  (sampler :sampler-2d))
  (let* ((sample (.rgb (texture sampler uv)))
         (distance (font/distance sample))
         (alpha (clamp (+ (/ distance (fwidth distance)) 0.5) 0 1))
         (frag-color (vec4 (.rgb color) (* alpha (.a color)))))
    (if (zerop (.a frag-color))
        (discard)
        frag-color)))

(define-shader font ()
  (:vertex (font/vert :vec2 :vec2))
  (:fragment (font/frag :vec2)))
