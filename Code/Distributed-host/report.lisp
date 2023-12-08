(in-package :platypus-magic)

(setf (cl-who:html-mode) :html5)

(defun top-players ()
  (loop with statement = (sqlite:prepare-statement
                          (state-database *state*)
                          "SELECT id, wins, points FROM platypus ORDER BY wins DESC, points DESC LIMIT 200")
        while (sqlite:step-statement statement)
        collect (list (sqlite:statement-column-value statement 0)
                      (sqlite:statement-column-value statement 1)
                      (sqlite:statement-column-value statement 2))
        finally (sqlite:finalize-statement statement)))

(hunchentoot:define-easy-handler (handle-report :uri "/") ()
  (cl-who:with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:title "Status report")
      (:link :rel "stylesheet" :href "style.css"))
     (:body
      (:h1 "Platypus Games Without Frontiers")
      (:p (cl-who:esc (format nil "~D of ~D machines have been tested."
                              (min (* (state-chunk-size *state*) (fast-count-1 (state-bitmap *state*)))
                                   (state-count *state*))
                              (state-count *state*))))
      (:h2 "Top 200 machines")
      (:table
       (:thead
        (:tr (:th "ID") (:th "Wins") (:th "Points")))
       (:tbody
        (loop for (id wins points) in (top-players)
              do (cl-who:htm
                  (:tr (:td (:a :class "machine-link"
                                :href (format nil "/draw?id=~D" id)
                                :target "_blank"
                                (cl-who:esc (write-to-string id))))
                       (:td (cl-who:esc (write-to-string wins)))
                       (:td (cl-who:esc (write-to-string points))))))))
      (:h2 "Telemetry")
      (:table
       (:thead
        (:tr (:th "Name") (:th "Work time") (:th "GET time") (:th "POST time") (:th "Last seen") (:th "Throughput")))
       (:tbody
        (loop for name being the hash-keys of *telemetry*
              for table being the hash-values of *telemetry*
              do (cl-who:htm
                  (:tr
                   (:td (cl-who:esc name))
                   (:td (cl-who:esc (format nil "~$" (gethash "compute" table))))
                   (:td (cl-who:esc (format nil "~$" (gethash "fetch" table))))
                   (:td (cl-who:esc (format nil "~$" (gethash "post" table))))
                   (:td (cl-who:esc (format nil "~$"
                                            (/ (- (get-internal-real-time) (gethash "time" table))
                                               internal-time-units-per-second))))
                   (:td (cl-who:esc (format nil "~$" (/ (state-chunk-size *state*) (gethash "compute" table))))))))))))))

(hunchentoot:define-easy-handler (handle-style :uri "/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  "
h1, h2 { font-family: Chicago, sans-serif; }
body {
  max-width: 50em;
  margin: 8pt auto;
  font-family: Open Sans, sans-serif;
}

tbody tr:nth-child(odd) { background: rgba(0, 0, 0, 0.1); }
thead { background: rgba(0, 0, 255, 0.1); }
td, th { padding: 0 1em; }
td { text-align: right; }
table {
    border-collapse: collapse;
    box-shadow: 2px 2px rgba(0, 0, 0, 0.1);
}

.machine-link { color: black !important; }
")
