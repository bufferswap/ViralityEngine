(in-package #:virality)

(defstruct (uuid
            (:constructor %make-uuid)
            (:predicate nil)
            (:copier nil))
  version
  (variant :rfc-4122)
  (low 0 :type u:ub64)
  (high 0 :type u:ub64))
