(in-package #:virality)

;; In the component-class we keep annotation values for annotations that are
;; identified by symbol.
(defclass annotation-value ()
  ((%serialnum :accessor serialnum
               :initarg :serialnum)
   (%name :accessor name
          :initarg :name)
   (%state :accessor state ;; :initialized or :forward-reference
           :initarg :state)
   (%setter :accessor setter
            :initarg :setter)
   (%getter :accessor getter
            :initarg :getter)))

;; The meta-class for the component class.
(defclass component-class (standard-class)
  (;; Each created annotation gets a serialnumber gotten from here.
   (%annotation-serialnum :accessor annotation-serialnum
                          :initarg :annotation-serialnum
                          :initform 0)
   ;; The annotation database for all annotations
   (%annotations :accessor annotations
                 :initarg :annotations
                 ;; key: symbol, value: component-class/annotation-value
                 :initform (u:dict #'eq))
   (%annotations-dirty-p :accessor annotations-dirty-p
                         :initarg :annotations-dirty-p
                         :initform nil)
   ;; The optimized annotations for easy funcalling.
   (%annotation-array :accessor annotation-array
                      :initarg :annotation-array
                      :initform #())
   ;; A database of which slots on which classes are annotated.
   ;; key: component-name, value: list of (slot-name  (initarg ..) (anno ..))
   ;; Note: I'm storing the EFFECTIVE slot data in thie db. This means that
   ;; all annotation inheritance rules are in effect and we can track
   ;; annotations across inherited components.
   (%annotated-slots :accessor annotated-slots
                     :initarg :annotated-slots
                     :initform (u:dict #'eq))))

(defclass annotatable-slot ()
  (;; A list of symbols like: (material transform ... texture)
   (%annotation :accessor annotation
                :initarg :annotation
                :initform nil)
   ;; An array of indexes that map the symbols, in the same order, to locations
   ;; in an optimized annotation array in the 'component metaclass slots.
   (%annotation-indexes :accessor annotation-indexes
                        :initarg :annotation-indexes
                        :initform nil)))

;; NOTE: First we define the direct and effective slot class we desire for the
;; component-class metaclass. Then the point of having two classes, annotated,
;; and not annotated, is to make slot-value-using-class ONLY execute on
;; _actually_ annotated slots.

;; component-class has its own definition of standard-direct slots
(defclass component-direct-slot-definition
    (c2mop:standard-direct-slot-definition)
  ())

;; annotated slot definitions for direct slots
(defclass component-annotated-direct-slot-definition
    (component-direct-slot-definition annotatable-slot)
  ())

;; component-class has its own definition of standard-effective slots
(defclass component-effective-slot-definition
    (c2mop:standard-effective-slot-definition)
  ())

;; annotated slot defitions for effective slots.
(defclass component-annotated-effective-slot-definition
    (component-effective-slot-definition annotatable-slot)
  ())


;; NOTE: The MOP chicanery we're doing quires this method to be defined
;; before we can declare a defclass with the component-class metaclass.
(defmethod c2mop:validate-superclass ((class component-class)
                                      (super standard-class))
  t)

;; The venerable COMPONENT class from which all user components are derived.
;; It uses the above machinery to deal with annotable slots. Since this is so
;; intertwined with the MOP stuff defined above, we keep it in the same file.
(defclass component (kernel)
  ((%type :reader component-type
          :initarg :type)
   (%actor :accessor actor
           :initarg :actor
           :initform nil)
   (%initializer :accessor initializer
                 :initarg :initializer
                 :initform nil)
   (%attach/detach-event-queue :accessor attach/detach-event-queue
                               :initarg :attach/detach-event-queue
                               :initform (queues:make-queue :simple-queue)))
  (:metaclass component-class))
