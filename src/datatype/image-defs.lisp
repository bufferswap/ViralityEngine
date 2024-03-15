(in-package #:virality)

(defclass image ()
  ((%path :reader path
          :initarg :path
          :initform nil)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data
          :initform nil)))

(defclass hdr-image-buffer ()
  ((%stream :reader buffer-stream
            :initarg :stream)
   (%position :accessor buffer-position
              :initform 0)
   (%end :accessor end
         :initform 0)
   (%data :reader data
          :initarg :data)))
