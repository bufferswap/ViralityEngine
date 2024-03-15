(in-package #:virality)

;;; The value of a uniform is this type in the material. It holds the original
;;; semantic value and any transformation of it that is actually the usable
;;; material value.
;;; NOTE: :initforms are supplied here because if how profiles are implemented.
;;; Basically, we need to be able to copy pre-annotated material values from
;;; profiles BEFORE we annotate them for real when reaolving all the materials.

;; TODO: Refactor the %material field out into a base material-value structure.
(defclass material-uniform-value ()
  (
   ;; This is a back reference to the material that owns this
   ;; material-uniform-value.
   (%material :accessor material
              :initarg :material)

   ;; This is the semantic value for a uniform. In the case of a :sampler-2d it
   ;; is a string to a texture found on disk, etc.
   (%semantic-value :accessor semantic-value
                    :initarg :semantic-value
                    :initform nil)
   ;; A function the user supplies that performs some a-prioi conversion of the
   ;; semantic value to something apprpropriate for the next stages of
   ;; conversion. This will be added to the below composition chain at the
   ;; right time to form the complete transform of the semantic to the computed
   ;; value.  The function in this slot is always FIRST in the composition.
   (%transformer :reader transformer
                 :initarg :transformer
                 :initform #'identity/for-material-custom-functions)
   ;; When materials get copied, material-uniform-values get copied, and if the
   ;; user is using some custom semantic value, they need to supply the
   ;; function which will deep copy it.
   (%copier :reader copier
            :initarg :copier
            :initform (lambda (sv context mat)
                        (declare (ignore context mat))
                        sv))
   ;; This is a composition of functions, stored as a list, where the first
   ;; function on the left's result is passed to the next function until the
   ;; end. The last function should be the final converter which produces
   ;; something suitable for the computed-value.
   (%semantic->computed :accessor semantic->computed
                        :initarg :semantic->computed
                        :initform nil)
   ;; When we're converting the semantic-value to to computed-value, do we
   ;; attempt to force a copy between the semantic-value and the computed value
   ;; even though they COULD be the same in common circumstances?
   (%force-copy :accessor force-copy
                :initarg :force-copy
                :initform nil)
   ;; This is the final processed value that is suitable (or nearly suitable)
   ;; to bind to a uniform.
   (%computed-value :accessor computed-value
                    :initarg :computed-value
                    :initform nil)
   ;; The function that knows how to bind the computed value to a shader.
   (%binder :accessor binder
            :initarg :binder
            :initform (constantly nil))))

;; TODO: Refactor the %material field out into a base material-value structure.
;; Shader interface blocks get a wildly different material-value implementation
;; and semantics.
(defclass material-block-value ()
  (
   ;; This is a back reference to the material that owns this
   ;; material-uniform-value.
   (%material :accessor material
              :initarg :material)

   ;; Store which block-name this originated from for debugging
   (%block-name :reader block-name
                :initarg :block-name)
   ;; The storage type of the block-alias for which this is a value. Useful for
   ;; error messages.
   (%storage-type :reader storage-type
                  :initarg :storage-type)
   ;; each time I request bindings on the material to be performed, how shall I
   ;; deal with it?
   (%binding-policy :accessor binding-policy
                    :initarg :binding-policy
                    ;; next value can be :repeat, :once, or :manual
                    :initform :repeat)
   ;; The buffer-name to bind with the block-alias for which this is a value.
   (%binding-buffer :accessor binding-buffer
                    :initarg :binding-buffer)
   ;; TODO: Add in range information for range binding. If the binding-policy
   ;; is :once, then this slot represents if we did the binding work. Whenever
   ;; this goes nil, and the policy is once, we'll rebind the block-alias to
   ;; the binding-buffer name and set bound-once-p to T again.
   (%bound-once-p :accessor bound-once-p
                  :initarg :bound-once-p
                  :initform nil)))

(defclass material ()
  ((%id :reader id
        :initarg :id)
   ;; This backreference simplifies when we need to change the texture at
   ;; runtime or do something else that makes us grovel around in the core.
   (%core :reader core
          :initarg :core)
   ;; This is the name of the shader as its symbol.
   (%shader :reader shader ;; :writer defined below.
            :initarg :shader)
   ;; This is the actual compiled shader program for this material.
   (%shader-program :reader shader-program
                    :initarg :shader-program)
   (%instances :reader instances
               :initarg :instances)
   (%attributes :reader attributes
                :initarg :attributes
                :initform nil)
   (%profile-overlay-names :reader profile-overlay-names
                           :initarg :profile-overlay-names
                           :initform nil)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is material-uniform-value
              :initform (u:dict #'eq))
   (%blocks :reader blocks
            :initarg :blocks
            ;; Hash tables:
            ;; key1 = <block-alias>,
            ;; value = material-block-value
            :initform (u:dict #'eq))
   (%active-texture-unit :accessor active-texture-unit
                         :initarg :active-texture-unit
                         :initform 0)))

(defclass material-profile ()
  ((%name :reader name
          :initarg :name)
   (%uniforms :reader uniforms
              :initarg :uniforms
              :initform (u:dict #'eq))
   (%blocks :reader blocks
            :initarg :blocks
            :initform (u:dict #'eq))))

;; Held in core, the material database for all materials everywhere.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (u:dict #'eq))
   (%profiles :reader profiles
              :initarg :profiles
              :initform (u:dict #'eq))))
