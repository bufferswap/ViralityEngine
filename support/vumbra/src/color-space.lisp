(in-package #:vumbra.color)

;;;; Color space conversion
;;;; Credits:
;;;; Ian Taylor http://www.chilliant.com/rgb2hsv.html
;;;; Jason Summers http://entropymine.com/imageworsener/srgbformula
;;;; https://en.wikipedia.org/wiki/SRGB
;;;; https://en.wikipedia.org/wiki/CIE_1931_color_space

(defconstant +gamma+ (/ 2.2))
(defconstant +gamma-inverse+ 2.2)

;;; Grayscale
;;; Uses the ITU-R Recommendation BT.709 standard for its luma coefficients.

(defun rgb->grayscale ((color :vec3))
  (vec3 (+ (* (.r color) 0.2126)
           (* (.g color) 0.7152)
           (* (.b color) 0.0722))))

(defun rgb->grayscale ((color :vec4))
  (vec4 (rgb->grayscale (.rgb color)) (.a color)))

;;; Hue

(defun hue->rgb ((hue :float))
  (let ((v (* hue 6)))
    (saturate
     (vec3 (1- (abs (- v 3)))
           (- 2 (abs (- v 2)))
           (- 2 (abs (- v 4)))))))

;;; HCV (hue/chroma/value)

(defun rgb->hcv ((color :vec3))
  (let* ((k (vec4 0 (/ -1 3.0) (/ 2 3.0) -1))
         (p (if (< (.g color) (.b color))
                (vec4 (.bg color) (.wz k))
                (vec4 (.gb color) (.xy k))))
         (q (if (< (.r color) (.x p))
                (vec4 (.xyw p) (.r color))
                (vec4 (.r color) (.yzx p))))
         (d (- (.x q) (min (.w q) (.y q))))
         (h (abs (+ (/ (- (.w q) (.y q))
                       (+ (* 6 d) +epsilon+))
                    (.z q)))))
    (vec3 h d (.x q))))

(defun rgb->hcv ((color :vec4))
  (vec4 (rgb->hcv (.rgb color)) (.a color)))

;;; HSV (hue/saturation/value)

(defun rgb->hsv ((color :vec3))
  (let* ((hcv (rgb->hcv color))
         (s (/ (.y hcv) (+ (.z hcv) +epsilon+))))
    (vec3 (.x hcv) s (.z hcv))))

(defun rgb->hsv ((color :vec4))
  (vec4 (rgb->hsv (.rgb color)) (.a color)))

(defun hsv->rgb ((color :vec3))
  (let ((rgb (hue->rgb (.x color))))
    (* (1+ (* (1- rgb) (.y color))) (.z color))))

(defun hsv->rgb ((color :vec4))
  (vec4 (hsv->rgb (.xyz color)) (.a color)))

;;; HCY (hue/saturation/luminance)

(defun rgb->hcy ((color :vec3))
  (let* ((hcy-weights (vec3 0.299 0.587 0.114))
         (hcv (rgb->hcv color))
         (y (dot color hcy-weights))
         (z (dot (hue->rgb (.x hcv)) hcy-weights)))
    (if (< y z)
        (multf (.y hcv) (/ z (+ +epsilon+ y)))
        (multf (.y hcv) (/ (- 1 z) (- (1+ +epsilon+) y))))
    (vec3 (.xy hcv) y)))

(defun rgb->hcy ((color :vec4))
  (vec4 (rgb->hcy (.rgb color)) (.a color)))

(defun hcy->rgb ((color :vec3))
  (let* ((hcy-weights (vec3 0.299 0.587 0.114))
         (rgb (hue->rgb (.x color)))
         (z (dot rgb hcy-weights)))
    (cond
      ((< (.z color) z)
       (multf (.y color) (/ (.z color) z)))
      ((< z 1)
       (multf (.y color) (/ (- 1 (.z color)) (- 1 z)))))
    (+ (* (- rgb z) (.y color)) (.z color))))

(defun hcy->rgb ((color :vec4))
  (vec4 (hcy->rgb (.xyz color)) (.a color)))

;;; HSL (hue/saturation/lightness)

(defun rgb->hsl ((color :vec3))
  (let* ((hcv (rgb->hcv color))
         (l (- (.z hcv) (* (.y hcv) 0.5)))
         (s (/ (.y hcv) (- 1 (+ (abs (1- (* l 2)))) +epsilon+))))
    (vec3 (.x hcv) s l)))

(defun rgb->hsl ((color :vec4))
  (vec4 (rgb->hsl (.rgb color)) (.a color)))

(defun hsl->rgb ((color :vec3))
  (let ((rgb (hue->rgb (.x color)))
        (c (* (- 1 (abs (1- (* 2 (.z color))))) (.y color))))
    (+ (* (- rgb 0.5) c) (.z color))))

(defun hsl->rgb ((color :vec4))
  (vec4 (hsl->rgb (.xyz color)) (.a color)))

;;; SRGB

(defun rgb->srgb-approx ((color :vec3))
  (expt (.rgb color) (vec3 +gamma+)))

(defun rgb->srgb-approx ((color :vec4))
  (vec4 (rgb->srgb-approx (.rgb color)) (.a color)))

(defun rgb->srgb ((color :vec3))
  (mix (* 12.92 color)
       (- (* 1.055 (expt color (vec3 (/ 2.4)))) 0.055)
       (step (vec3 0.0031308) color)))

(defun rgb->srgb ((color :vec4))
  (vec4 (rgb->srgb (.rgb color)) (.a color)))

(defun srgb->rgb-approx ((color :vec3))
  (expt color (vec3 +gamma-inverse+)))

(defun srgb->rgb-approx ((color :vec4))
  (vec4 (srgb->rgb-approx (.rgb color)) (.a color)))

(defun srgb->rgb ((color :vec3))
  (mix (/ color 12.92)
       (expt (/ (+ color 0.055) 1.055) (vec3 2.4))
       (step (vec3 0.04045) color)))

(defun srgb->rgb ((color :vec4))
  (vec4 (srgb->rgb (.rgb color)) (.a color)))

;;; CIE XYZ 1931

(defun rgb->xyz ((color :vec3))
  (let ((transform (mat3 0.4124564 0.3575761 0.1804375
                         0.2126729 0.7151522 0.072175
                         0.0193339 0.119192 0.9503041)))
    (* transform color)))

(defun rgb->xyz ((color :vec4))
  (vec4 (rgb->xyz (.rgb color)) (.a color)))

(defun xyz->rgb ((color :vec3))
  (let ((transform (mat3 3.2404542 -1.5371385 -0.4985314
                         -0.969266 1.8760108 0.041556
                         0.0556434 -0.2040259 1.0572252)))
    (* transform color)))

(defun xyz->rgb ((color :vec4))
  (vec4 (xyz->rgb (.xyz color)) (.a color)))

;;; CIE xyY

(defun xyy->xyz ((color :vec3))
  (let* ((y (.z color))
         (x (/ (* y (.x color)) (.y color)))
         (z (/ (* y (- 1 (.x color) (.y color))) (.y color))))
    (vec3 x y z)))

(defun xyy->xyz ((color :vec4))
  (vec4 (xyy->xyz (.xyz color)) (.a color)))

(defun xyz->xyy ((color :vec3))
  (let* ((v (+ (.x color) (.y color) (.z color)))
         (x (/ (.x color) v))
         (y (/ (.y color) v)))
    (vec3 x y (.y color))))

(defun xyz->xyy ((color :vec4))
  (vec4 (xyz->xyy (.xyz color)) (.a color)))

(defun rgb->xyy ((color :vec3))
  (xyz->xyy (rgb->xyz color)))

(defun rgb->xyy ((color :vec4))
  (xyz->xyy (rgb->xyz color)))

(defun xyy->rgb ((color :vec3))
  (xyz->rgb (xyy->xyz color)))

(defun xyy->rgb ((color :vec4))
  (xyz->rgb (xyy->xyz color)))
