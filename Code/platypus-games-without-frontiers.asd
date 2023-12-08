(asdf:defsystem :platypus-games-without-frontiers
  :depends-on (:amb :atomics :cl-cpus :sqlite :lparallel :numpy-file-format :sb-simd)
  :serial t
  :components ((:file "vector")
               (:file "package")
               (:file "vm")
               (:file "representation")
               (:file "equivalences")
               (:file "run-game")
               (:file "layout")
               (:file "report-diagrams")
               (:file "results")))
