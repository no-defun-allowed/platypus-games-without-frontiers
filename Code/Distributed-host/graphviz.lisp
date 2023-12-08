(in-package :platypus-magic)

(defun graphviz-for-machine (machine)
  (with-output-to-string (buffer)
    (format buffer "digraph {~%  rankdir=LR; node[fontname=\"Open Sans\"];~% edge[fontname=\"Open Sans\"];~%")
    (format buffer "  Start[label=\"\", color=white]; Start -> K;")
    (pgwf::walk-machine
     (lambda (state colour)
       (let ((transition (pgwf::transition machine state colour)))
         (format buffer "  ~A -> ~A[label=\"~A → ~A, ~A\"];~%"
                 (pgwf::capital pgwf::*animals* state)
                 (pgwf::capital pgwf::*animals* (pgwf::next-animal transition))
                 (pgwf::capital pgwf::*colours* colour)
                 (pgwf::capital pgwf::*colours* (pgwf::next-colour transition))
                 (pgwf::capital pgwf::*directions* (pgwf::next-direction transition)))))
     machine)
    (format buffer "}")))

(defun graphviz->svg (graphviz-code)
  (let ((process (sb-ext:run-program "/usr/bin/dot"
                                     '("-Tsvg" "-o" "/dev/stdout")
                                     :input (make-string-input-stream graphviz-code)
                                     :output :stream)))
    (assert (zerop (sb-ext:process-exit-code process)))
    (alexandria:read-stream-content-into-string (sb-ext:process-output process))))

(hunchentoot:define-easy-handler (handle-graphviz :uri "/draw")
    ((id :parameter-type 'integer))
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (graphviz->svg (graphviz-for-machine (aref (state-machine-data *state*) id))))

(defvar *page-size* 50)

(defun machine-page (page)
  (loop with statement = (sqlite:prepare-statement
                          (state-database *state*)
                          (format nil "SELECT id, wins, points FROM platypus ORDER BY id ASC LIMIT ~D, ~D"
                                  (* page *page-size*) *page-size*))
        while (sqlite:step-statement statement)
        collect (list (sqlite:statement-column-value statement 0)
                      (sqlite:statement-column-value statement 1)
                      (sqlite:statement-column-value statement 2))
        finally (sqlite:finalize-statement statement)))

(hunchentoot:define-easy-handler (handle-machines :uri "/machines")
    ((page :parameter-type 'integer))
  (cl-who:with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:title "View machines")
      (:link :rel "stylesheet" :href "style.css"))
     (:body
      (:h1 "View machines")
      (:p
       (unless (zerop page) (cl-who:htm (:a :href (format nil "/machines?page=~D" (1- page)) "Previous page")))
       " "
       (:a :href (format nil "/machines?page=~D" (1+ page)) "Next page"))
      (loop for (index wins points) in (machine-page page)
            do (cl-who:htm
                (:p
                 (cl-who:esc (format nil "Equivalence set #~D got ~D wins and ~D points."
                                     index wins points))
                 (:img :src (format nil "/draw?id=~D" index)))))))))
