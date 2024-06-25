(defsystem "aura"
  :version "0.0.0"
  :depends-on (:transducers
               :filepaths
               :fn-macro
               :arrow-macros
               :str)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "localisations")))))
