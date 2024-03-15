(in-package #:virality.texture)


(defmethod load-texture-data ((texture-type (eql :texture-1d-array))
                              texture context)
  (let* ((use-mipmaps-p
           (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p
           (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         (num-layers (length data))
         (reshaped-layers (reshape-image-array-layout data))
         (all-layers (read-mipmap-images context
                                         reshaped-layers
                                         use-mipmaps-p
                                         :1d-array
                                         flip-y))
         (first-image (aref (aref all-layers 0) 0))
         ;; TODO: Assert num-mipmaps is same for all layers.
         (num-mipmaps (length all-layers)))
    ;; Figure out the ideal mipmap count from the base resolution.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (v::width first-image)
                               (v::height first-image))
      ;; TODO: Fix this call for arrays
      #++(validate-mipmap-images images texture
                                 expected-mipmaps expected-resolutions)
      (potentially-degrade-texture-min-filter texture)
      ;; Allocate storage.
      (let ((num-mipmaps-to-generate
              (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
        (if immutable-p
            (%gl:tex-storage-2d texture-type
                                num-mipmaps-to-generate
                                (v::internal-format first-image)
                                (v::width first-image)
                                num-layers)
            (loop :for i :below num-mipmaps-to-generate
                  :for mipmap-resolution :in expected-resolutions
                  :do (gl:tex-image-2d texture-type
                                       (+ texture-base-level i)
                                       (v::internal-format first-image)
                                       (first mipmap-resolution)
                                       num-layers
                                       0
                                       (v::pixel-format first-image)
                                       (v::pixel-type first-image)
                                       (cffi:null-pointer)))))
      ;; Upload all of the mipmap images into the texture ram.
      ;; TODO: Make this higher order.
      (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
            :for level = (+ texture-base-level idx)
            :for image = (aref (aref all-layers idx) 0)
            ;; Construct the entire 2d array image of these 1d image pieces.
            :do (dotimes (i num-layers)
                  (gl:tex-sub-image-2d
                   texture-type
                   level
                   0
                   i
                   (v::width image)
                   1
                   (v::pixel-format image)
                   (v::pixel-type image)
                   (v::data (aref (aref all-layers idx) i)))))
      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))
