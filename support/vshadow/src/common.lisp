(in-package #:vshadow)

(defun ensure-keyword (x)
  (etypecase x
    ((or number string symbol)
     (u:make-keyword (format nil "~a" x)))))

(defun parts->string (parts &optional (filter #'identity))
  (with-output-to-string (s)
    (flet ((convert (parts)
             (mapcar
              (lambda (part)
                (etypecase part
                  ((or symbol string) (funcall filter part))
                  (integer part)))
              parts)))
      (loop :for (part . rest) :on (convert parts)
            :for separator = "" :then "."
            :do (etypecase part
                  ((or symbol string) (format s "~a~a" separator part))
                  (integer (format s "[~a]" part)))))))

(defgeneric get-qualifiers (type)
  (:method ((type varjo:v-type))
    (varjo.internals:qualifiers type))
  (:method ((type varjo.internals:shader-variable))
    (varjo:qualifiers (varjo.internals:v-type-of type))))

(defun has-qualifier-p (type qualifier)
  (member qualifier (get-qualifiers type) :test #'varjo.internals:qualifier=))

(defun lisp-symbol->glsl-type (symbol)
  (when (and (boundp symbol)
             (not (constantp symbol)))
    (etypecase (symbol-value symbol)
      (boolean :bool)
      (u:f32 :float)
      (u:f64 :double)
      (u:b32 :int)
      (u:ub32 :uint)
      ((u:f32a 2) :vec2)
      ((u:f32a 3) :vec3)
      ((u:f32a 4) :vec4)
      ((u:f32a 9) :mat3)
      ((u:f32a 16) :mat4))))

(defun lisp-constant->glsl-constant (constant)
  (when (constantp constant)
    (let ((value (symbol-value constant)))
      (typecase value
        (null 0)
        (boolean 1)
        (u:f32 value)
        (u:f64 value)
        (u:b32 value)
        (u:ub32 value)))))
