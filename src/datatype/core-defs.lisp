(in-package #:virality)

;; Used in the CORE to manage a pile of component flow state in a state
;; machine.
(defclass bookkeeping-tables ()
  ((%component-search-table :reader component-search-table
                            :initform (u:dict #'eq))
   (%component-preinit-by-type-view :reader component-preinit-by-type-view
                                    :initform (u:dict #'eq))
   (%component-init-by-type-view :reader component-init-by-type-view
                                 :initform (u:dict #'eq))
   (%component-active-by-type-view :reader component-active-by-type-view
                                   :initform (u:dict #'eq))
   (%component-predestroy-view :reader component-predestroy-view
                               :initform (u:dict #'eq))
   (%component-destroy-by-type-view :reader component-destroy-by-type-view
                                    :initform (u:dict #'eq))
   (%components-by-id :reader components-by-id
                      :initform (u:dict #'equalp))
   (%actor-predestroy-view :reader actor-predestroy-view
                           :initform (u:dict #'eq))
   (%actor-preinit-db :reader actor-preinit-db
                      :initform (u:dict #'eq))
   (%actor-init-db :reader actor-init-db
                   :initform (u:dict #'eq))
   (%actor-active-db :reader actor-active-db
                     :initform (u:dict #'eq))
   (%actor-destroy-db :reader actor-destroy-db
                      :initform (u:dict #'eq))
   (%actors-by-id :reader actors-by-id
                  :initform (u:dict #'equalp))
   (%kernels-by-uuid :reader kernels-by-uuid
                     :initform (u:dict #'equalp))))

;; The main structure holding the information for a single executing game
;; instance.
(defclass core ()
  ((%config :reader config
             :initarg :config)
   (%assets :reader assets
            :initform (u:dict #'eq))
   (%running-p :accessor running-p
               :initform t)
   (%resource-cache :reader resource-cache
                    :initform (u:dict #'eq))
   (%clock :reader clock
           :initform nil)
   (%display :reader display
             :initform nil)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%input-data :reader input-data)
   (%shaders :accessor shaders)
   (%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%materials :accessor materials
               :initform (make-materials-table))
   (%textures :accessor textures
              :initform (tex::make-textures-table))
   (%context :reader context)
   (%tables :reader tables
            :initarg :tables)
   (%call-flows :reader call-flows
                :initform (u:dict #'eq))
   (%collider-system :accessor collider-system
                     :initform nil)
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (u:dict #'equalp))
   (%end-of-frame-work :accessor end-of-frame-work
                       :initform nil)
   (%prefab-entities :reader prefab-entities
                     :initform (u:dict #'equalp))
   (%last-picked-actor :accessor last-picked-actor
                       :initform nil)))
