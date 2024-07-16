(defsystem "aura"
  :version "0.0.0"
  :depends-on (:transducers
               :transducers/jzon
               :filepaths
               :fn-macro
               :arrow-macros
               :str)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "localisations")
                 (:file "security")))))
