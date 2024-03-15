(in-package #:virality.texture)

(defmethod load-texture-data ((texture-type (eql :texture-3d)) texture context)
  ;; Determine if loading :images or :volume
  ;; TODO: Validating a 3d texture.
  ;; 0. Ensure that each mipmap level has the same number of slices.
  ;; 1. Ensure that all mipmap levels have the same number of slices.
  ;; 2. Ensure that all required mipmaps levels are present and correct.
  ;; 2. Ensure that each mipmap level fits into the current limits on the card.
  ;; TODO: Fix me. Need to support :layout properly. Currently, this form
  ;; represents opengl defaults.
  (let ((hardcoded-layout '((:origin :left-back-bottom)
                            (:shape (:slices :back-to-front))))
        (current-layout (get-computed-applied-attribute texture :layout)))
    (unless (equal current-layout hardcoded-layout)
      (error "3D Texture ~a has layout: ~s, but it can only have this as its ~
              layout:~%~s"
             (name texture) current-layout hardcoded-layout)))
  (let* ((use-mipmaps-p
           (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p
           (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (max-texture-3d-size (v::get-gpu-parameter :max-3d-texture-size))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         (num-mipmaps (length data)))
    #++(format t "Attempting to load 3d texture ~a onto GPU: immutable = ~a~%"
               (name texture) immutable-p)
    ;; Load all of our images for each mipmap level, if needed.
    (let* ((all-slices (read-mipmap-images
                        context data use-mipmaps-p :3d flip-y))
           (first-image (aref (aref all-slices 0) 0))
           (depth (length (aref all-slices 0))))
      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (v::width first-image)
                                 (v::height first-image)
                                 depth)
        #++(format t "expected mipmaps: ~a expected-resolutions: ~a~%"
                   expected-mipmaps expected-resolutions)
        ;; TODO: Fix this call.
        #++(validate-mipmap-images images texture
                                   expected-mipmaps expected-resolutions)
        (potentially-degrade-texture-min-filter texture)
        ;; Allocate immutable storage if required.
        (let ((num-mipmaps-to-generate
                (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
          (if immutable-p
              (%gl:tex-storage-3d texture-type num-mipmaps-to-generate
                                  (v::internal-format first-image)
                                  (v::width first-image)
                                  (v::height first-image)
                                  depth)
              (loop :for i :below num-mipmaps-to-generate
                    :for (mipmap-width mipmap-height mipmap-depth)
                      :in expected-resolutions
                    :do (gl:tex-image-3d texture-type (+ texture-base-level i)
                                         (v::internal-format first-image)
                                         mipmap-width mipmap-height mipmap-depth
                                         0
                                         (v::pixel-format first-image)
                                         (v::pixel-type first-image)
                                         (cffi:null-pointer)))))
        ;; Load all volumetric mipmaps into the gpu.
        (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
              :for level = (+ texture-base-level idx)
              :for (mipmap-width mipmap-height mipmap-depth)
                :in expected-resolutions
              :do (let ((image (aref (aref all-slices idx) 0)))
                    ;; TODO: I should move this error check to the validation
                    ;; stage above instead of being here.
                    (when (> (max mipmap-width mipmap-height mipmap-depth)
                             max-texture-3d-size)
                      (error "load-texture-data[:texture-3d]: Cannot load ~
                              texture-3d mipmap because one of its size ~
                              (width=~a, height=~a, depth=~a) is larger than ~
                              the maximum opengl 3d texture size: ~a"
                             mipmap-width mipmap-height mipmap-depth
                             max-texture-3d-size))
                    (dotimes (i (length (aref all-slices idx)))
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
                       (v::data (aref (aref all-slices idx) i))))))
        (potentially-autogenerate-mipmaps texture-type texture)))))
