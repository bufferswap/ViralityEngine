(in-package #:virality.prefab)

(defclass reference ()
  ((%id :reader id
        :initarg :id)
   (%current-actor :reader current-actor
                   :initarg :current-actor)
   (%current-component :reader current-component
                       :initarg :current-component)
   (%actors :reader actors
            :initarg :actors)
   (%components :reader components
                :initarg :components)
   (%component :reader component
               :initarg :component)
   (%merge-id :reader merge-id
              :initarg :merge-id)))
