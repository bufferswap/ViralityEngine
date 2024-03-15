(in-package #:virality)

;; NOTE: A general place to store structures used in the engine so they are
;; loaded before any use of them later. This preserves inlining of the slot
;; accessors.


;; NOTE: Used by V's transform component.
(defstruct transform-state
  (previous (v3:zero) :type (or v3:vec q:quat))
  (current (v3:zero) :type (or v3:vec q:quat))
  ;; TODO: Determine if we actually need FUTURE for the change about to
  ;; happen before the next physics update. Currently not implemented.
  ;;
  ;; (future (v3:zero) :type (or v3:vec q:quat))
  (incremental (v3:zero) :type v3:vec)
  (incremental-delta (v3:zero) :type (or v3:vec q:quat))
  (interpolated (v3:zero) :type (or v3:vec q:quat))
  (replace-count 0 :type fixnum)
  (replace-warned-p nil :type boolean))


;; NOTE: Used by V's sprite component.
(defstruct (spritesheet (:constructor %make-spritesheet))
  (name nil :type (or symbol number cons))
  (block-alias nil :type (or symbol number))
  (spec nil :type (or null cons))
  (geometry 0 :type fixnum)
  (sprites (u:dict #'equalp) :type hash-table))
