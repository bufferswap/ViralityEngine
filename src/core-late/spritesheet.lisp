(in-package #:virality)

;;;; NOTE: Implementation of defstruct SPRITESHEET

(defun make-spritesheet-buffer (spritesheet)
  (let ((name (spritesheet-name spritesheet))
        (block-alias (spritesheet-block-alias spritesheet)))
    ;; TODO: This 1 is hardcoded because virality doesn't have an allocation
    ;; system for these integers.
    (shadow:bind-block block-alias 1)
    (shadow:create-buffer name block-alias)
    ;; TODO: Have materials automatically calculate a binding point instead of
    ;; hard-coding.
    (shadow:bind-buffer name 1)))

(defun update-spritesheet-buffer (spritesheet)
  (loop :with name = (spritesheet-name spritesheet)
        :with spec = (spritesheet-spec spritesheet)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :from 0
        :do (destructuring-bind (&key id x y w h y-flipped &allow-other-keys)
                sprite
              (when (and id x y w h)
                ;; TODO: Make sure y-flip math is correct
                (setf (aref pos i) (vector x (if y-flipped (- 1 y h) y))
                      (aref size i) (vector w h)
                      (u:href (spritesheet-sprites spritesheet) id) i)))
        :finally (shadow:write-buffer-path name :path :pos :index 0 :value pos)
                 (shadow:write-buffer-path name :path :size :index 0 :value size)))

(defun find-sprite (spritesheet name)
  (or (u:href (spritesheet-sprites spritesheet) name)
      (error "Sprite ~s not found in spritesheet ~s."
             name
             (spritesheet-name spritesheet))))

(defun make-spritesheet (context spec block-alias)
  (let ((path (v::resolve-path spec)))
    (v:with-asset-cache context block-alias spec
      (let ((spritesheet (%make-spritesheet
                          :name spec
                          :block-alias block-alias
                          :spec (u:safe-read-file-form path)
                          :geometry (gl:gen-vertex-array))))
        (make-spritesheet-buffer spritesheet)
        (update-spritesheet-buffer spritesheet)
        spritesheet))))
