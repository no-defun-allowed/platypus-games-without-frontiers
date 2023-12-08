(in-package :pgwf)

;;; Heat maps for normalisation algorithms.

(defun tikz-heatmap (stream table)
  (declare (result-table table))
  (format stream "~&\\begin{tikzpicture}[yscale=-1, scale=0.4]
  \\node[anchor=west] at (16, -1.5) {$\\delta (\\q{K}, \\q{Y})$};
  \\node[anchor=west] at (16, -0.5) {$\\delta (\\q{K}, \\q{G})$};
  \\node[anchor=west, rotate=-90] at (-1.5, 16) {$\\delta (\\q{E}, \\q{Y})$};
  \\node[anchor=west, rotate=-90] at (-0.5, 16) {$\\delta (\\q{E}, \\q{G})$};
")
  (let ((counts (make-array '(16 16) :initial-element 0)))
    ;; Compute densities.
    (dotimes (m (expt 2 28))
      (let ((ky (next-animal (transition m +kangaroo+ +yellow+)))
            (kg (next-animal (transition m +kangaroo+ +green+)))
            (ey (next-animal (transition m +emu+ +yellow+)))
            (eg (next-animal (transition m +emu+ +green+))))
        (unless (zerop (aref table m)) (incf (aref counts (+ (* 4 ky) kg) (+ (* 4 ey) eg))))))
    ;; Draw labels.
    (loop for name across *animals*
          for char = (char-upcase (char name 0))
          for index from 0
          do (format stream "~&  \\node at (~D.5, -1.5) {~C}; \\node at (-1.5, ~D.5) {~C};"
                     (* 4 index) char (* 4 index) char)
             (dotimes (n 4)
               (let ((position (+ (* 4 n) index)))
                 (format stream
                         "~&  \\node at (~D.5, -0.5) {~C}; \\node at (-0.5, ~D.5) {~C};"
                         position char position char))))
    ;; Draw heatmap.
    (dotimes (x 16)
      (dotimes (y 16)
        (let* ((density (floor (/ (aref counts x y) (expt 2 20)) 0.01)))
          (format stream "~&  \\fill[black!~D!white] (~d, ~d) rectangle +(1, 1);" density x y)))))
  (format stream "~&  \\draw (0, 0) rectangle (16, 16);~&\\end{tikzpicture}"))

(defun generate-diagrams ()
  (labels ((path (name)
             (asdf:system-relative-pathname
              :platypus-games-without-frontiers
              (make-pathname :directory '(:relative :up "Report") :name name :type "tex")))
           (write-results (name filename function)
             (with-open-file (stream (path filename) :direction :output :if-exists :supersede)
               (let* ((start-time (get-internal-run-time))
                      (table (normalise function))
                      (time-taken (/ (- (get-internal-run-time) start-time)
                                     internal-time-units-per-second))
                      (remaining (count 0 table :test #'/=)))
                 (format t "~&~A yields ~:D machines (~$%, took ~$ CPU.s)"
                         name remaining
                         (* 100 (/ remaining (expt 2 28)))
                         time-taken)
                 (tikz-heatmap stream table)))))
    (write-results "DCE" "dce" #'dead-code-elimination)
    (write-results "Renaming" "rename" #'rename-states)
    (write-results "DFA minimisation + renaming" "dfa" (alexandria:compose #'rename-states #'dfa-minimise))))

;;; Render a machine as a graph.

(defun sum-of-squares (lines indices)
  (loop for (a . b) in lines
        sum (expt (- (aref indices a) (aref indices b)) 2)))

(defun least-sum-of-squares-layout (machines)
  (let ((lines '())
        best-layout
        (best-sum most-positive-fixnum))
    (dolist (machine machines)
      (dotimes (animal 4)
        (dotimes (colour 2)
          (unless (and (= colour +green+) (= animal +platypus+))
            (push (cons animal (next-animal (transition machine animal colour))) lines)))))
    (print lines)
    (alexandria:map-permutations
     (lambda (perm)
       (let ((indices (make-array 4)))
         ;; Populate indices
         (setf (aref indices +kangaroo+) 0)
         (loop for i from 1
               for animal in perm
               do (setf (aref indices animal) i))
         (let ((sum (sum-of-squares lines indices)))
           (when (< sum best-sum)
             (setf best-sum sum
                   best-layout (cons +kangaroo+ perm))))))
     (list +platypus+ +wombat+ +emu+))
    (values best-layout best-sum)))

(defun draw-machine (machine
                     &key
                       (stream *standard-output*)
                       (layout (least-sum-of-squares-layout (list machine))))
  "Generate boilerplate for a TikZ diagram of a machine.
The layout is most likely going to be wrong - the user has to tweak that by themself."
  (format stream "~&\\begin{tikzpicture}")
  (loop for x from 0 by 3
        for index in layout
        for letter = (capital *animals* index)
        do (format stream "~&\\node[draw, circle] (~C) at (~D, 0) {\\sffamily ~C};"
                   letter x letter))
  (format stream "~&\\draw[->] (-1, 0) to (K);")
  (dotimes (animal 4)
    (dotimes (colour 2)
      (unless (and (= colour +green+) (= animal +platypus+))
        (let ((transition (transition machine animal colour)))
          (format stream "~&\\draw[->] (~C) to[~A] node[midway, above] {$\\q{~C} \\rightarrow \\q{~C}, \\q{~C}$} (~C);"
                  (capital *animals* animal)
                  ;; Generate curves to avoid collisions.
                  (cond
                    ((= animal (next-animal transition))
                     ;; Generate larger and smaller loops.
                     (format nil "in=135, out=45, looseness=~D"
                             (if (= colour +green+) 5 10)))
                    ((= colour +green+) "bend left")
                    (t "bend right"))
                  (capital *colours* colour)
                  (capital *colours* (next-colour transition))
                  (capital *directions* (next-direction transition))
                  (capital *animals* (next-animal transition)))))))
  (format stream "~&\\end{tikzpicture}"))

;;; Tables for heuristics.

(defun fewer-sets-table (sets &key (slices 100))
  (loop with counts = (equivalence-sets-counts sets)
        with slice-size = (floor (length counts) slices)
        for n below slices
        for end from 0 by slice-size
        for amount = (/ (loop for n below end
                              sum (aref counts n))
                        (expt 2 28))
        do (format t "~&~4$, ~4$" (/ n slices) amount)))

(defun match-length-table (sets &key (tests 1000000))
  (let ((results (make-array (1+ +move-limit+) :initial-element 0))
        (machines (equivalence-sets-machines sets)))
    (dotimes (i tests)
      (let ((m1 (alexandria:random-elt machines))
            (m2 (alexandria:random-elt machines)))
        (incf (aref results (nth-value 3 (run-game m1 m2))))))
    (loop for n from 0
          for count across results
          do (format t "~&~D, ~D, ~D"
                     n
                     count
                     (reduce #'+ results :end (1+ n))))))
