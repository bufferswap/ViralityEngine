(in-package #:virality.prefab)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%library :reader library
             :initarg :library)
   (%doc :reader doc
         :initarg :doc)
   (%data :reader data
          :initarg :data)
   (%parse-tree :reader parse-tree
                :initform (u:dict #'equalp))
   (%root :reader root)
   (%links :reader links
           :initform (u:dict :source->targets (u:dict #'equalp)
                             :target->source (u:dict #'equalp)))
   (%func :accessor func
          :initform (constantly nil))))

(defclass node ()
  ((%name :reader name
          :initarg :name)
   (%id :reader id
        :initarg :id
        :initform nil)
   (%display-id :reader display-id
                :initarg :display-id
                :initform nil)
   (%prefab :reader prefab
            :initarg :prefab)
   (%path :reader path
          :initarg :path)
   (%options :reader options
             :initarg :options
             :initform nil)
   (%components :reader components
                :initform nil)
   (%components-table :reader components-table
                      :initform (u:dict #'eq))
   (%parent :reader parent
            :initarg :parent
            :initform nil)
   (%children :reader children
              :initform (u:dict #'equalp))))

;; Each component initialization argument is converted temporarily to an
;; instance of this class which, after we figure out which argument values are
;; actually valid and present due to component merge policies, we then
;; initialize the env and force the thunk. This is usef to implement the V:REF
;; function in the value form of the components.
(defclass injectable-ref-value-thunk ()
  (;; A lambda function wrapped around the value lexically supplying a CONTEXT
   ;; variable. This function also exists in a injection ref environment (a
   ;; closure), which means the REF function is available in the value of the
   ;; argument.
   (%thunk :reader thunk
           :initarg :thunk)
   ;; Each injection ref environment has a secret back door to fill in the
   ;; lexical variables that the V:REF lexicaly scoped call needs to process
   ;; each argument. We use this to poke in the values to the lexical closure
   ;; before evaluating the thunk.
   (%env-injection-control-func :reader env-injection-control-func
                                :initarg :env-injection-control-func
                                :initform (constantly nil))))
