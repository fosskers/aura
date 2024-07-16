(defpackage aura
  (:use :cl :fn-macro :arrow-macros)
  (:local-nicknames (:t :transducers)
                    (:j :transducers/jzon)
                    (:p :filepaths)))

(in-package :aura)
