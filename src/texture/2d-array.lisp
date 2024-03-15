(in-package #:virality.texture)

(defmethod load-texture-data ((texture-type (eql :texture-2d-array))
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
         (num-layers (length data))
         (reshaped-layers (reshape-image-array-layout data))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         (all-layers
           (read-mipmap-images
            context reshaped-layers use-mipmaps-p :2d-array flip-y))
         (first-image (aref (aref all-layers 0) 0))
         ;; TODO: Assert num-mipmaps is same for all layers.
         (num-mipmaps (length all-layers)))

    ;; Type-checking (for all textures types, but notated here):
    ;;
    ;; TODO: Error under the condition where the first-image is a narrower
    ;; internal format than subsequent images. Example: first-image is RGBA8
    ;; but subsequent images are RGBA16. Add flag in define-texture to
    ;; make this condition an error and have it be error by default.
    ;;
    ;; TODO: Warn under the condition where the rest of the images (other than
    ;; the first) have a wider internal format then the first image. Example:
    ;; first-image is RGBA16 and subsequent images are RGBA8, this causes
    ;; additional space to be used unexpectedly. Stuck flag in define-texture
    ;; for warning or error in this case.

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
            (%gl:tex-storage-3d texture-type
                                num-mipmaps-to-generate
                                ;; TODO: Ensure that all images loaded to the
                                ;; texture array are the same internal-format.
                                ;; TODO: Ensure the internal-format is actually
                                ;; correct, since the storage-* functions
                                ;; require a SIZED internal format and the image
                                ;; loading code doesn't keep track of it.
                                (v::internal-format first-image)
                                (v::width first-image)
                                (v::height first-image)
                                num-layers)
            (loop :for i :below num-mipmaps-to-generate
                  :for (mipmap-width mipmap-height) :in expected-resolutions
                  :do (gl:tex-image-3d texture-type
                                       (+ texture-base-level i)
                                       (v::internal-format first-image)
                                       mipmap-width
                                       mipmap-height
                                       num-layers
                                       0
                                       (v::pixel-format first-image)
                                       (v::pixel-type first-image)
                                       (cffi:null-pointer)))))
      ;; Upload all of the mipmap images into the texture ram.
      ;; TODO: Make this higher order.
      (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
            :for level = (+ texture-base-level idx)
            ;; Construct the entire 2d array image of these 1d image pieces.
            :do (dotimes (i num-layers)
                  (let ((image (aref (aref all-layers idx) i)))
                    (gl:tex-sub-image-3d
                     texture-type
                     level
                     0
                     0
                     i
                     (v::width image)
                     (v::height image)
                     1
                     (v::pixel-format image)
                     (v::pixel-type image)
                     (v::data image)))))
      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))
