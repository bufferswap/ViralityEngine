(in-package #:virality)

(defclass thread-pool ()
  ((%worker-count :reader worker-count
                  :initarg :worker-count)
   (%channels :reader channels
              :initform (u:dict #'eq))
   (%queues :reader queues
            :initform (u:dict #'eq))))
