(in-package #:vumbra.color)

;;; Exposure

(defun set-exposure ((color :vec3)
                     (exposure :int))
  (* color (expt 2 exposure)))

(defun set-exposure ((color :vec4)
                     (exposure :int))
  (vec4 (set-exposure (.rgb color) exposure) (.a color)))

;;; Saturation

(defun set-saturation ((color :vec3)
                       (saturation :float))
  (mix (rgb->grayscale color) color saturation))

(defun set-saturation ((color :vec4)
                       (saturation :float))
  (vec4 (set-saturation (.rgb color) saturation) (.a color)))

;;; Contrast

(defun set-contrast ((color :vec3)
                     (contrast :float))
  (+ (* (- (.rgb color) 0.5) contrast) 0.5))

(defun set-contrast ((color :vec4)
                     (contrast :float))
  (vec4 (set-contrast (.rgb color) contrast) (.a color)))

;;; Brightness

(defun set-brightness ((color :vec3)
                       (brightness :float))
  (+ color brightness))

(defun set-brightness ((color :vec4)
                       (brightness :float))
  (vec4 (set-brightness (.rgb color) brightness) (.a color)))

;;; Gamma

(defun set-gamma ((color :vec3)
                  (gamma :float))
  (vec3 (expt (abs (.r color)) (/ gamma))
        (expt (abs (.g color)) (/ gamma))
        (expt (abs (.b color)) (/ gamma))))

(defun set-gamma ((color :vec4)
                  (gamma :float))
  (vec4 (set-gamma (.rgb color) gamma) (.a color)))

;;; Color filter

(defun color-filter ((color :vec3)
                     (filter :vec3)
                     (exposure :int))
  (let ((exposure (set-exposure color exposure)))
    (* color filter exposure)))

(defun color-filter ((color :vec4)
                     (filter :vec3)
                     (exposure :int))
  (vec4 (color-filter (.rgb color) filter exposure) (.a color)))

;;; Tone mapping
;;; http://filmicworlds.com/blog/filmic-tonemapping-operators/

(defun tone-map/linear ((color :vec3)
                        (exposure :int))
  (set-gamma (set-exposure color exposure) +gamma+))

(defun tone-map/linear ((color :vec4)
                        (exposure :int))
  (vec4 (tone-map/linear (.rgb color) exposure) (.a color)))

(defun tone-map/reinhard ((color :vec3)
                          (exposure :int))
  (let ((color (set-exposure color exposure)))
    (set-gamma (/ color (1+ color)) +gamma+)))

(defun tone-map/reinhard ((color :vec4)
                          (exposure :int))
  (vec4 (tone-map/reinhard (.rgb color) exposure) (.a color)))

(defun tone-map/haarm-peter-duiker ((color :vec3)
                                    (exposure :int)
                                    (film-lut :sampler-2d))
  (let* ((color (set-exposure color exposure))
         (log-color (saturate (/ (+ (* (/ (log10 (* 0.4 color)) 0.002) 0.45)
                                    444)
                                 1023.0)))
         (padding 0.001953125)
         (r (vec2 (mix padding (- 1 padding) (.r color)) 0.5))
         (g (vec2 (mix padding (- 1 padding) (.g color)) 0.5))
         (b (vec2 (mix padding (- 1 padding) (.b color)) 0.5)))
    (vec3 (.r (texture film-lut r))
          (.r (texture film-lut g))
          (.r (texture film-lut b)))))

(defun tone-map/haarm-peter-duiker ((color :vec4)
                                    (exposure :int)
                                    (film-lut :sampler-2d))
  (vec4 (tone-map/haarm-peter-duiker (.rgb color) exposure film-lut)
        (.a color)))

(defun tone-map/hejl-burgess-dawson ((color :vec3)
                                     (exposure :int))
  (let* ((color (set-exposure color exposure))
         (x (max (vec3 0) (- color 0.004)))
         (y (* 6.2 x)))
    (/ (* x (+ y 0.5))
       (+ (* x (+ y 1.7)) 0.06))))

(defun tone-map/hejl-burgess-dawson ((color :vec4)
                                     (exposure :int))
  (vec4 (tone-map/hejl-burgess-dawson (.rgb color) exposure) (.a color)))

(defun tone-map/uncharted2 ((color :vec3)
                            (exposure :int))
  (flet ((tone-map ((x :vec3))
           (- (/ (+ (* x (+ (* 0.15 x) 0.05)) 0.004)
                 (+ (* x (+ (* 0.15 x) 0.5)) 0.06))
              0.006)))
    (expt (* (tone-map (* 2 (set-exposure color exposure)))
             (/ (tone-map (vec3 11.2))))
          (vec3 +gamma+))))

(defun tone-map/uncharted2 ((color :vec4)
                            (exposure :int))
  (vec4 (tone-map/uncharted2 (.rgb color) exposure) (.a color)))

(defun tone-map/aces ((color :vec3)
                      (exposure :int))
  (let ((color (set-exposure color exposure))
        (a 2.51)
        (b 0.03)
        (c 2.43)
        (d 0.59)
        (e 0.14))
    (saturate (/ (* color (+ (* color a) b))
                 (+ (* color (+ (* color c) d)) e)))))

(defun tone-map/aces ((color :vec4)
                      (exposure :int))
  (vec4 (tone-map/aces (.rgb color) exposure) (.a color)))
