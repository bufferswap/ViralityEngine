(in-package #:vumbra.sprite)

(defstruct sprite-data
  (sampler :sampler-2d :accessor sampler)
  (index :int :accessor index))

(defstruct spritesheet-data
  (pos (:ivec2 2048) :accessor pos)
  (size (:ivec2 2048) :accessor size))

(defun make-vertex-data ((sprite sprite-data)
                         (spritesheet spritesheet-data))
  (let* ((texture-size (.xyxy (texture-size (sampler sprite) 0)))
         (sprite-size (aref (size spritesheet) (index sprite)))
         (sprite-pos (aref (pos spritesheet) (index sprite)))
         (pos (vec4 (* sprite-size 0.5) (* sprite-size -0.5)))
         (uv (vec4 sprite-pos (+ sprite-pos (1- sprite-size)))))
    (case gl-vertex-id
      (0 (values (.xy pos) (.zw uv)))
      (1 (values (.zy pos) (.xw uv)))
      (2 (values (.xw pos) (.zy uv)))
      (otherwise (values (.zw pos) (.xy uv))))))

(defun sprite/v (&uniforms
                 (model :mat4)
                 (view :mat4)
                 (proj :mat4)
                 (sprite sprite-data)
                 (spritesheet spritesheet-data :ssbo :std-430))
  (mvlet* ((pos uv (make-vertex-data sprite spritesheet)))
    (values (* proj view model (vec4 pos 0 1))
            uv)))

(defun sprite/f ((uv :vec2)
                 &uniforms
                 (sprite sprite-data)
                 (opacity :float))
  (let* ((texture-size (.xy (texture-size (sampler sprite) 0)))
         (color (texture (sampler sprite) (/ (+ uv 0.5) texture-size))))
    (vec4 (.rgb color) (* (.a color) opacity))))

(define-shader sprite (:primitive :triangle-strip)
  (:vertex (sprite/v))
  (:fragment (sprite/f :vec2)))
