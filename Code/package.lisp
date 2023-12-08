(defpackage :platypus-games-without-frontiers
  (:use :cl)
  (:export  #:dead-code-elimination #:rename-states #:dfa-minimise
            #:normalise #:equivalence-sets #:differential-testing
            #:generate-diagrams)
  (:nicknames :pgwf)
  (:local-nicknames (#:v8 #:pgwf-simd8)
                    (#:v32 #:pgwf-simd32)))
