(in-package #:virality)

;;;; Implementation of IMAGE structure for PNG images

(defun get-png-channel-count (image)
  (ecase (pngload:color-type image)
    (:greyscale 1)
    (:greyscale-alpha 2)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defun get-png-pixel-format (image)
  (ecase (pngload:color-type image)
    (:greyscale :red)
    (:greyscale-alpha :rg)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun get-png-pixel-type (image)
  (ecase (pngload:bit-depth image)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun get-png-internal-format (image)
  (let ((channel-count (get-png-channel-count image))
        (bit-depth (pngload:bit-depth image)))
    (u:format-symbol :keyword "~a~d"
                     (subseq "RGBA" 0 channel-count)
                     bit-depth)))

(defmethod %load-image ((type (eql :png)) path &key flip-y)
  (let ((image (pngload:load-file path :flatten t :flip-y flip-y)))
    (make-instance 'image
                   :path path
                   :width (pngload:width image)
                   :height (pngload:height image)
                   :pixel-format (get-png-pixel-format image)
                   :pixel-type (get-png-pixel-type image)
                   :internal-format (get-png-internal-format image)
                   :data (pngload:data image))))
