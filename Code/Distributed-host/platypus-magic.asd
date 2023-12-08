(asdf:defsystem :platypus-magic
  :depends-on (:bordeaux-threads :cl-who :hunchentoot :numpy-file-format
               :platypus-games-without-frontiers :shasht :sqlite)
  :serial t
  :components ((:file "package")
               (:file "fast-popcount")
               (:file "work-distribution")
               (:file "report")
               (:file "graphviz")))
