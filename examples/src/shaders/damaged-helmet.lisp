(in-package #:virality-examples.shader)

(defconstant +albedo+ 0)
(defconstant +ao+ 1)
(defconstant +emissive+ 2)
(defconstant +metal-roughness+ 3)
(defconstant +normal+ 4)
(defconstant +diffuse+ 0)
(defconstant +specular+ 1)

(defstruct material-info
  (perceptual-roughness :float :accessor perceptual-roughness)
  (reflectance0 :vec3 :accessor reflectance0)
  (alpha-roughness :float :accessor alpha-roughness)
  (diffuse-color :vec3 :accessor diffuse-color)
  (reflectance90 :vec3 :accessor reflectance90)
  (specular-color :vec3 :accessor specular-color))

(defstruct angular-info
  (n-dot-l :float :accessor n-dot-l)
  (n-dot-v :float :accessor n-dot-v)
  (n-dot-h :float :accessor n-dot-h)
  (l-dot-h :float :accessor l-dot-h)
  (v-dot-h :float :accessor v-dot-h))

(defstruct light-info
  (direction :vec3 :accessor direction)
  (color :vec3 :accessor color)
  (intensity :float :accessor intensity))

(defun damaged-helmet/get-ibl-contribution
    ((material-info material-info)
     (normal :vec3)
     (view-dir :vec3)
     (brdf-lut :sampler-2d)
     (environment-sampler :sampler-cube-array))
  (with-slots (perceptual-roughness diffuse-color specular-color) material-info
    (let* ((n-dot-v (clamp (dot normal view-dir) 0 1))
           (mipmaps (texture-query-levels environment-sampler))
           (lod (clamp (* perceptual-roughness mipmaps)
                       0
                       mipmaps))
           (reflection (normalize (reflect (- view-dir) normal)))
           (brdf-sample-point (clamp (vec2 n-dot-v perceptual-roughness)
                                     (vec2 0)
                                     (vec2 1)))
           (brdf (.rg (texture brdf-lut brdf-sample-point)))
           (diffuse-sample (texture environment-sampler
                                    (vec4 normal +diffuse+)))
           (specular-sample (texture environment-sampler
                                     (vec4 reflection +specular+)
                                     lod))
           (diffuse-light (.rgb diffuse-sample))
           (specular-light (.rgb specular-sample))
           (diffuse (* diffuse-light diffuse-color))
           (specular (* specular-light (+ (* specular-color (.x brdf))
                                          (.y brdf)))))
      (+ diffuse specular))))

(defun damaged-helmet/get-angular-info ((point-to-light :vec3)
                                        (normal :vec3)
                                        (view :vec3))
  (let* ((n (normalize normal))
         (v (normalize view))
         (l (normalize point-to-light))
         (h (normalize (+ l v))))
    (make-angular-info (clamp (dot n l) 0 1)
                       (clamp (dot n v) 0 1)
                       (clamp (dot n h) 0 1)
                       (clamp (dot l h) 0 1)
                       (clamp (dot v h) 0 1))))

(defun damaged-helmet/diffuse ((material-info material-info))
  (/ (diffuse-color material-info) +pi+))

(defun damaged-helmet/specular-reflection ((material-info material-info)
                                           (angular-info angular-info))
  (with-slots (reflectance0 reflectance90) material-info
    (+ reflectance0
       (* (- reflectance90 reflectance0)
          (pow (clamp (- 1 (v-dot-h angular-info)) 0 1) 5)))))

(defun damaged-helmet/visibility-occlusion ((material-info material-info)
                                            (angular-info angular-info))
  (with-slots (n-dot-l n-dot-v) angular-info
    (with-slots (alpha-roughness) material-info
      (let* ((alpha-roughness-squared (* alpha-roughness alpha-roughness))
             (ggxv (* n-dot-l
                      (sqrt (+ (* n-dot-v n-dot-v (- 1 alpha-roughness-squared))
                               alpha-roughness-squared))))
             (ggxl (* n-dot-v
                      (sqrt (+ (* n-dot-l n-dot-l (- 1 alpha-roughness-squared))
                               alpha-roughness-squared))))
             (ggx (+ ggxv ggxl)))
        (if (plusp ggx)
            (/ 0.5 ggx)
            0.0)))))

(defun damaged-helmet/microfacet-distribution ((material-info material-info)
                                               (angular-info angular-info))
  (with-slots (alpha-roughness) material-info
    (with-slots (n-dot-h) angular-info
      (let* ((alpha-roughness-squared (* alpha-roughness alpha-roughness))
             (f (1+ (* (- (* n-dot-h alpha-roughness-squared)
                          n-dot-h)
                       n-dot-h))))
        (/ alpha-roughness-squared (* +pi+ f f))))))

(defun damaged-helmet/get-point-shade ((point-to-light :vec3)
                                       (material-info material-info)
                                       (normal :vec3)
                                       (view :vec3))
  (let ((angular-info (damaged-helmet/get-angular-info
                       point-to-light normal view)))
    (if (or (plusp (n-dot-l angular-info))
            (plusp (n-dot-v angular-info)))
        (let* ((f (damaged-helmet/specular-reflection
                   material-info angular-info))
               (vis (damaged-helmet/visibility-occlusion
                     material-info angular-info))
               (d (damaged-helmet/microfacet-distribution
                   material-info angular-info))
               (diffuse (* (- 1 f) (damaged-helmet/diffuse material-info)))
               (specular (* f vis d)))
          (* (n-dot-l angular-info) (+ diffuse specular)))
        (vec3 0))))

(defun damaged-helmet/get-range-attenuation ((range :float)
                                             (distance :float))
  (if (<= range 0)
      1.0
      (/ (max (min (- 1 (pow (/ distance range) 4)) 1) 0)
         (pow distance 2))))

(defun damaged-helmet/get-spot-attenuation ((point-to-light :vec3)
                                            (spot-direction :vec3)
                                            (outer-cone-cos :float)
                                            (inner-cone-cos :float))
  (let ((actual-cos (dot (normalize spot-direction)
                         (normalize (- point-to-light)))))
    (if (> actual-cos outer-cone-cos)
        (if (< actual-cos inner-cone-cos)
            (smoothstep outer-cone-cos inner-cone-cos actual-cos)
            1.0)
        0.0)))

(defun damaged-helmet/apply-directional-light ((light light-info)
                                               (material-info material-info)
                                               (normal :vec3)
                                               (view-dir :vec3))
  (let* ((point-to-light (- (normalize (direction light))))
         (shade (damaged-helmet/get-point-shade
                 point-to-light material-info normal view-dir)))
    (* (intensity light) (color light) shade)))

(defun damaged-helmet/get-normal ((sampler :sampler-2d-array)
                                  (normal-scale :float)
                                  (uv :vec2)
                                  (world-position :vec3)
                                  (normal :vec3))
  (let* ((pos-dx (d-fdx world-position))
         (pos-dy (d-fdy world-position))
         (tex-dx (d-fdx (vec3 uv 0)))
         (tex-dy (d-fdy (vec3 uv 0)))
         (tv (/ (- (* (.t tex-dy) pos-dx)
                   (* (.t tex-dx) pos-dy))
                (- (* (.s tex-dx) (.t tex-dy))
                   (* (.s tex-dy) (.t tex-dx)))))
         (ng (normalize normal))
         (tv (normalize (- tv (* ng (dot ng tv)))))
         (b (normalize (cross ng tv)))
         (tbn (mat3 tv b ng))
         (n (.rgb (texture sampler (vec3 uv +normal+)))))
    (normalize (* tbn
                  (1- (* 2 n))
                  (vec3 normal-scale normal-scale 1)))))

(defun damaged-helmet/vert ((mesh-attrs mesh-attrs)
                            &uniforms
                            (model :mat4)
                            (view :mat4)
                            (proj :mat4)
                            (normal-matrix :mat3))
  (with-slots (mesh/pos mesh/normal mesh/tangent mesh/uv1) mesh-attrs
    (let* ((pos (* model (vec4 mesh/pos 1)))
           (world-pos (/ (.xyz pos) (.w pos)))
           (normal (normalize
                    (* normal-matrix mesh/normal)))
           (camera-pos (.xyz (aref (inverse view) 3)))
           (view-dir (normalize (- camera-pos world-pos))))
      (values (* proj view pos)
              world-pos
              view-dir
              normal
              mesh/uv1))))

(defun damaged-helmet/frag ((world-pos :vec3)
                            (view-dir :vec3)
                            (normal :vec3)
                            (uv :vec2)
                            &uniforms
                            (light light-info)
                            (sampler :sampler-2d-array)
                            (base-color-factor :vec4)
                            (metallic-factor :float)
                            (roughness-factor :float)
                            (normal-scale :float)
                            (occlusion-strength :float)
                            (emissive-factor :float)
                            (brdf-lut :sampler-2d)
                            (environment-sampler :sampler-cube-array)
                            (use-punctual :bool)
                            (use-ibl :bool))
  (let* ((uv (vec2 (.x uv) (- 1 (.y uv))))
         (f0 (vec3 0.04))
         (mr-sample (texture sampler (vec3 uv +metal-roughness+)))
         (perceptual-roughness (* (.g mr-sample) roughness-factor))
         (metallic (* (.b mr-sample) metallic-factor))
         (base-color (* (umbra.color:srgb->rgb
                         (texture sampler (vec3 uv +albedo+)))
                        base-color-factor))
         (diffuse-color (* (.rgb base-color)
                           (- (vec3 1) f0)
                           (- 1 metallic)))
         (specular-color (mix f0 (.rgb base-color) metallic))
         (perceptual-roughness (clamp perceptual-roughness 0 1))
         (metallic (clamp metallic 0 1))
         (alpha-roughness (* perceptual-roughness perceptual-roughness))
         (reflectance (max (max (.r specular-color) (.g specular-color))
                           (.b specular-color)))
         (specular-environment-r0 (.rgb specular-color))
         (specular-environment-r90 (vec3 (clamp (* reflectance 50) 0 1)))
         (material-info (make-material-info perceptual-roughness
                                            specular-environment-r0
                                            alpha-roughness
                                            diffuse-color
                                            specular-environment-r90
                                            specular-color))
         (color (vec3 0))
         (normal-w (damaged-helmet/get-normal
                    sampler normal-scale uv world-pos normal))
         (ao (.r (texture sampler (vec3 uv +ao+))))
         (emissive (* (.rgb (umbra.color:srgb->rgb
                             (texture sampler (vec3 uv +emissive+))))
                      emissive-factor)))
    (when use-punctual
      (incf color (damaged-helmet/apply-directional-light light
                                                          material-info
                                                          normal-w
                                                          view-dir)))
    (when use-ibl
      (incf color (damaged-helmet/get-ibl-contribution material-info
                                                       normal-w
                                                       view-dir
                                                       brdf-lut
                                                       environment-sampler)))
    (setf color (mix color (* color ao) occlusion-strength))
    (incf color emissive)
    (vec4 (umbra.color:rgb->srgb (umbra.color:tone-map/aces color 1))
          (.a base-color))))

(define-shader damaged-helmet ()
  (:vertex (damaged-helmet/vert mesh-attrs))
  (:fragment (damaged-helmet/frag :vec3 :vec3 :vec3 :vec2)))
