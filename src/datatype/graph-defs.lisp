(in-package #:virality)

(defclass analyzed-graph ()
  ((%category :accessor category
              :initarg :category)
   (%graph :accessor graph
           :initarg :graph
           :initform nil)
   (%toposort :accessor toposort
              :initarg :toposort
              :initform nil)
   (%annotation :accessor annotation
                :initarg :annotation
                :initform nil)
   (%graphdefs :accessor graphdefs
               :initarg :graphdefs
               :initform  (u:dict #'eq))))

(defclass graphdef ()
  ((%name :accessor name
          :initarg :name)
   (%original-form :reader original-form
                   :initarg :original-form)
   (%enabled :accessor enabled
             :initarg :enabled)
   (%category :accessor category
              :initarg :category)
   ;; This starts out as a list grabbed from the graph-definition dsl, but we
   ;; transform it later into a hash table with values of graphdef-depends-on
   ;; instances to hold a richer semantic understanding of original depends-on
   ;; forms in the graph-definition dsl.
   (%depends-on :accessor depends-on
                :initarg :depends-on)
   (%roots :accessor roots
           :initarg :roots)
   (%subforms :accessor subforms
              :initarg :subforms)))

(defclass graphdef-depends-on ()
  ((%name :accessor name
          :initarg :name)
   ;; when we transmute the original form into this class type, we reserve it
   ;; for future debugging.
   (%original-form :accessor original-form
                   :initarg :original-form)
   ;; a reference to the real graphdef containing the referenced subforms.
   (%graphdef :accessor graphdef
              :initarg :graphdef)
   ;; a hash table keyed by subform name, and value is the subform itself in the
   ;; appropriate graphdef instance.
   (%subforms :accessor subforms
              :initarg :subforms
              :initform (u:dict #'eq))))

(defclass subform ()
  ((%name :accessor name
          :initarg :name)
   (%kind :accessor kind
          :initarg :kind)
   (%depforms :accessor depforms
              :initarg :depforms)))

(defclass depform ()
  ;; the original depform for debugging/error output
  ((%original-form :accessor original-form
                   :initarg :original-form)
   ;; the lifted form with the new symbols, if any
   (%canonical-form :accessor canonical-form
                    :initarg :canonical-form)
   ;; :empty, :vertex, or :hyperedges
   (%kind :accessor kind
          :initarg :kind)))


;; For category COMPONENT-DEPENDENCY
(defclass graph-annotation/component-dependency ()
  ((%unknown-type-id :accessor unknown-type-id
                     :initarg :unknown-type-id)
   (%referenced-types :accessor referenced-types
                      :initarg :referenced-types
                      :initform (u:dict #'eq))))

;; For category COMPONENT-PACKAGE-ORDER
(defclass graph-annotation/component-package-order ()
  ((%pattern-matched-packages :accessor pattern-matched-packages
                              :initarg :pattern-matched-packages
                              :initform (u:dict #'eq))))
