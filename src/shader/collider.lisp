(in-package #:virality.shader)

(defun collider/sphere/vert ()
  (values))

(defun collider/sphere/geom (&uniforms
                             (model :mat4)
                             (view :mat4)
                             (proj :mat4)
                             (collider-local-center :vec3)
                             (radius :float))
  (declare (output-primitive :kind :line-strip :max-vertices 256))
  (let ((mvp (* proj view model))
        (iterations 256))
    (dotimes (i iterations)
      (let* ((coeff (/ i (1- (float iterations))))
             (theta (float (mix 0 (* 24 pi) coeff)))
             (phi (float (mix 0 pi coeff)))
             (x (* radius (cos theta) (sin phi)))
             (y (* radius (sin theta) (sin phi)))
             (z (* radius (cos phi)))
             (local-point (vec4 (+ collider-local-center (vec3 x y z)) 1f0))
             (spiral-world-point (* mvp local-point)))
        (emit ()
          spiral-world-point)))
    (end-primitive))
  (values))

(defun collider/frag (&uniforms
                      (in-contact-color :vec4)
                      (not-in-contact-color :vec4)
                      (in-contact-p :bool))
  (if in-contact-p
      in-contact-color
      not-in-contact-color))

(define-shader collider/sphere (:primitive :points)
  (:vertex (collider/sphere/vert))
  (:geometry (collider/sphere/geom))
  (:fragment (collider/frag)))


;;;; --------------------------------------------------------------------

(defun collider/cuboid/vert ()
  (values))

(defun collider/cuboid/geom (&uniforms
                             (model :mat4)
                             (view :mat4)
                             (proj :mat4)
                             (collider-local-center :vec3)
                             (minx :float)
                             (maxx :float)
                             (miny :float)
                             (maxy :float)
                             (minz :float)
                             (maxz :float))
  (declare (output-primitive :kind :line-strip :max-vertices 16))
  (let* ((mvp (* proj view model))
         (center collider-local-center)

         ;; FIXME: Move the mvp and such to the vertex shader. Do some
         ;; refactoring to do less math in this shader.

         ;; local space computation of the cuboid verticies
         (a (+ center (vec3 minx maxy maxz)))
         (b (+ center (vec3 maxx maxy maxz)))
         (c (+ center (vec3 maxx miny maxz)))
         (d (+ center (vec3 minx miny maxz)))
         (e (+ center (vec3 minx maxy minz)))
         (f (+ center (vec3 maxx maxy minz)))
         (g (+ center (vec3 maxx miny minz)))
         (h (+ center (vec3 minx miny minz)))

         ;; recompute into world space
         (a (* mvp (vec4 a 1f0)))
         (b (* mvp (vec4 b 1f0)))
         (c (* mvp (vec4 c 1f0)))
         (d (* mvp (vec4 d 1f0)))
         (e (* mvp (vec4 e 1f0)))
         (f (* mvp (vec4 f 1f0)))
         (g (* mvp (vec4 g 1f0)))
         (h (* mvp (vec4 h 1f0))))

    ;; FIXME: emit a crappy line strip to draw the cuboid which overdraws,
    ;; add more primitives to draw only once.
    (emit () a)
    (emit () b)
    (emit () c)
    (emit () d)
    (emit () a)
    (emit () e)
    (emit () f)
    (emit () g)
    (emit () h)
    (emit () e)
    (emit () f)
    (emit () b)
    (emit () c)
    (emit () g)
    (emit () h)
    (emit () d)
    (end-primitive))

  (values))

(define-shader collider/cuboid (:primitive :points)
  (:vertex (collider/cuboid/vert))
  (:geometry (collider/cuboid/geom))
  (:fragment (collider/frag)))
