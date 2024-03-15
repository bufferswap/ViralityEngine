(in-package #:vshadow)

(defun pack-container (type)
  (etypecase type
    (varjo:v-vector :vec)
    (varjo:v-matrix :mat)
    (varjo:v-array :array)))

(defun pack-type (type)
  (etypecase type
    (varjo:v-bool '(:bool))
    (varjo:v-int '(:int 32))
    (varjo:v-uint '(:uint 32))
    (varjo:v-float '(:float 32))
    (varjo:v-struct (varjo:type->type-spec type))
    (varjo:v-container
     (let ((element-type (varjo:v-element-type type)))
       (typecase element-type
         ((or varjo:v-user-struct varjo:v-array)
          (error "Shader blocks containing arrays of aggregates are not currently supported."))
         (t (list* (pack-container type)
                   (pack-type element-type)
                   (varjo:v-dimensions type))))))))

(defun pack-struct (struct)
  (loop :with name = (varjo:type->type-spec struct)
        :for (slot-name slot-type) :in (varjo.internals:v-slots struct)
        :for type = (pack-type slot-type)
        :collect (list slot-name type) :into members
        :finally (return `(,name (:struct () ,@members)))))

(defun pack-block (layout)
  (loop :with uniform = (uniform layout)
        :with name = (varjo:name uniform)
        :with layout-type = (layout-type layout)
        :with slots = (varjo.internals:v-slots (varjo:v-type-of uniform))
        :for (slot-name slot-type) :in slots
        :for packed-type = (pack-type slot-type)
        :collect (list slot-name packed-type) :into members
        :finally (return `(,name (:block (:packing ,layout-type) ,@members)))))

(defun pack-layout (layout)
  (let ((structs (collect-layout-structs layout)))
    (glsl-packing:pack-structs
     `(,@(mapcar #'pack-struct structs)
       ,(pack-block layout)))))

(defun unpack-type (layout-type type)
  (destructuring-bind ((spec &optional x (y 1) (z 1)) &key &allow-other-keys) type
    (labels ((get-container (x)
               (unpack-type layout-type (list x)))
             (get-stride (count)
               (ecase layout-type
                 (:std140 4)
                 (:std430 (if (= count 3) 4 count))))
             (get-result (&rest args)
               (destructuring-bind (&key (dimensions '(1 1)) element-type count type
                                    &allow-other-keys)
                   args
                 (list :dimensions dimensions
                       :element-type element-type
                       :element-stride (get-stride (car dimensions))
                       :count (or count 1)
                       :type type))))
      (ecase spec
        ((:bool :uint) (get-result :type :scalar :element-type '(unsigned-byte 32)))
        (:int (get-result :type :scalar :element-type '(signed-byte 32)))
        (:float (get-result :type :scalar :element-type 'u:f32))
        ((:vec :mat) (apply #'get-result :type spec :dimensions (list y z) (get-container x)))
        (:array (apply #'get-result :count y (get-container x)))))))
