(in-package #:virality.component)

(v:define-component font ()
  ((%asset :reader asset
           :initarg :asset
           :initform nil)
   (%text :accessor text
          :initarg :text
          :initform "")
   (%position :reader font-position
              :initarg :position
              :initform nil)
   (%offset :reader offset
            :initarg :offset
            :initform (v2:zero))
   (%rate :reader rate
          :initarg :rate
          :initform 0.5)
   (%spec :reader spec
          :initform nil)
   (%update-time :accessor update-time
                 :initform 0)
   (%buffer-data :accessor buffer-data
                 :initform nil)
   (%dimensions :accessor dimensions
                :initform (v2:zero))))

(v:define-geometry text ()
  (:layout x:2d
   :vertex-count 6
   :primitive :triangles))

(defun load-font-spec (font)
  (with-slots (%asset %spec) font
    (unless %asset
      (error "Font component ~s does not have an asset specified." font))
    (let ((context (v:context font))
          (path (v::resolve-path %asset)))
      (setf %spec (v:with-asset-cache context :font path
                    (with-open-file (in path)
                      (font:read-bmfont in)))))))

(defun resolve-font-text (font)
  (with-slots (%text) font
    (typecase %text
      (string %text)
      ((or function symbol)
       (let ((text (funcall %text)))
         (unless (stringp text)
           (error "Font component ~s has text that is not a string." font))
         text)))))

(defun generate-font-data (font)
  (lambda (x- y- x+ y+ u- v- u+ v+)
    (push `((,x- ,y+ ,u- ,v+)
            (,x- ,y- ,u- ,v-)
            (,x+ ,y+ ,u+ ,v+)
            (,x+ ,y+ ,u+ ,v+)
            (,x- ,y- ,u- ,v-)
            (,x+ ,y- ,u+ ,v-))
          (buffer-data font))))

(defmethod v:on-component-initialize ((self font))
  (load-font-spec self))

(defmethod v:on-component-update ((self font))
  (let* ((context (v:context self))
         (actor (v:actor self))
         (geometry (v:component-by-type actor 'geometry))
         (time (v:total-time context)))
    (when (or (zerop (v:frame-count context))
              (>= time (+ (update-time self) (rate self))))
      (u:mvlet* ((text (resolve-font-text self))
                 (func (funcall #'generate-font-data self))
                 (width height (v::map-font-glyphs (spec self) func text)))
        (v3:with-components ((s (v:get-scale actor)))
          (v2:with-components ((fd (dimensions self)))
            (setf fdx width fdy height))
          (v:update-geometry (geometry geometry) :data (buffer-data self))
          (v:translate actor
                       (v3:vec (- (* sx width)) (* sy height) 0f0)
                       :replace t)))
      (setf (buffer-data self) nil
            (update-time self) time))))
