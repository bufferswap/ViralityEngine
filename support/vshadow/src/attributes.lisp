(in-package #:vshadow)

(defun store-attributes (program)
  (dolist (stage (translated-stages program))
    (when (eq (stage-type stage) :vertex)
      (loop :for attr :in (varjo:input-variables stage)
            :for id = (ensure-keyword (varjo:name attr))
            :for type = (varjo:v-type-of attr)
            :do (setf (u:href (attributes program) id)
                      (u:dict #'eq
                              :name (varjo:glsl-name attr)
                              :type (varjo:type->type-spec type)))))))

(defun store-attribute-locations (program)
  (let ((id (id program)))
    (gl:use-program id)
    (u:do-hash-values (v (attributes program))
      (setf (u:href v :location) (gl:get-attrib-location id (u:href v :name))))
    (gl:use-program 0)))
