(in-package #:virality)

(defclass actor (kernel)
  ((%components :reader components
                :initform (u:dict #'eq))
   (%components-by-type :reader %components-by-type
                        :initform (u:dict #'eq))
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node
                 :initform nil)))
