(in-package #:virality)

;;;; TODO: This file is currently not used by Virality Engine yet. The contents
;;;; of this file will become useful when we have a complete rendering pipeline
;;;; with multiple render passes. ~axion 4/8/2020

;; This is a list of the default OpenGL enabled capabilities. We store them here
;; as a constant so we have a known state to revert back to when we render
;; per-material capabilities.
(u:define-constant +enabled-capabilities+
    '(:blend :cull-face :depth-test :dither :multisample)
  :test #'equal)

;; This is a list of the default OpenGL disabled capabilities. We store them
;; here as a constant so we have a known state to revert back to when we render
;; per-material capabilities.
(u:define-constant +disabled-capabilities+
    '(:clip-distance0 :clip-distance1 :clip-distance2 :clip-distance3
      :clip-distance4 :clip-distance5 :clip-distance6 :clip-distance7
      :color-logic-op :debug-output :debug-output-synchronous :depth-clamp
      :framebuffer-srgb :line-smooth :polygon-offset-fill :polygon-offset-line
      :polygon-offset-point :polygon-smooth :primitive-restart
      :primitive-restart-fixed-index :rasterizer-discard
      :sample-alpha-to-coverage :sample-alpha-to-one :sample-coverage
      :sample-shading :sample-mask :scissor-test :stencil-test
      :texture-cube-map-seamless :program-point-size)
  :test #'equal)

;; This is a specification of the default OpenGL blend function to apply when a
;; per-material option is not active.
(u:define-constant +blend-mode+ '(:src-alpha :one-minus-src-alpha)
  :test #'equal)

;; This is a specification of the default OpenGL depth test to apply when a
;; per-material option is not active.
(u:define-constant +depth-mode+ :less)

;; This is a specification of the default OpenGL polygon mode to apply when a
;; per-material option is not active.
(u:define-constant +polygon-mode+ '(:front-and-back :fill) :test #'equal)

;; This macro will push an OpenGL debug group around surrounding code. This is
;; used to annotate graphical debugging tools such as RenderDoc with a tree of
;; human readable annotations of the call flow.
(defmacro with-debug-group (name &body body)
  (if (find :virality.release *features*)
      `(progn ,@body)
      (u:once-only (name)
        `(progn
           (cffi:with-foreign-string (s ,name)
             (%gl:push-debug-group
              :debug-source-application 0 (length ,name) s))
           (unwind-protect (progn ,@body)
             (%gl:pop-debug-group))))))
