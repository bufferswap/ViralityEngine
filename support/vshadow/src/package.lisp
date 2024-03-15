(in-package #:cl-user)

(defpackage #:vshadow
  (:local-nicknames
   (#:u #:vutils))
  (:use #:cl)
  (:export
   #:bind-block
   #:bind-buffer
   #:buffer-name
   #:build-shader-dictionary
   #:build-shader-program
   #:clear-buffer
   #:create-block-alias
   #:create-buffer
   #:define-shader
   #:defmacro
   #:defstruct
   #:defun
   #:delete-buffer
   #:find-block
   #:find-buffer
   #:find-program
   #:find-shader-definition
   #:get-program-id
   #:load-shaders
   #:program
   #:read-buffer-path
   #:recompile-shaders
   #:unbind-block
   #:unbind-buffer
   #:unload-shaders
   #:uniforms
   #:uniform-bool
   #:uniform-bool-array
   #:uniform-float
   #:uniform-float-array
   #:uniform-int
   #:uniform-int-array
   #:uniform-mat2
   #:uniform-mat2-array
   #:uniform-mat3
   #:uniform-mat3-array
   #:uniform-mat4
   #:uniform-mat4-array
   #:uniform-vec2
   #:uniform-vec2-array
   #:uniform-vec3
   #:uniform-vec3-array
   #:uniform-vec4
   #:uniform-vec4-array
   #:view-source
   #:with-shader
   #:write-buffer-path))

(defpackage #:vshadow.glsl
  (:local-nicknames
   (#:s #:vshadow)
   (#:u #:vutils))
  (:use #:cl #:vari)
  (:shadow
   #:defun
   #:defstruct
   #:defmacro)
  ;; export external CL and VARI symbols
  #.(cons
     :export
     (flet ((find-symbols (&rest packages)
              (let (symbols)
                (dolist (package packages)
                  (do-external-symbols (x package)
                    (push x symbols)))
                (nreverse symbols))))
       (loop :for symbol :in (find-symbols '#:cl '#:vari)
             :unless (member (symbol-name symbol)
                             '("DEFUN" "DEFSTRUCT" "DEFMACRO"))
               :collect symbol)))
  (:export
   #:defun
   #:defstruct
   #:defmacro
   #:define-shader))
