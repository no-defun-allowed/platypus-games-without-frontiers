(in-package :pgwf)

;;; Rendering a machine as a graph.

;;; Lay out the states by minimizing the sum-of-squares of the
;;; taken by all transitions.

(defun sum-of-squares (lines indices)
  (loop for (a b) in lines
        sum (expt (- (aref indices a) (aref indices b)) 2)))

(defun lines-from-machine (machine)
  (flet ((format-label (from to direction)
           (format nil "$~A \\rightarrow \\q{~A}, \\q{~A}$" from to direction)))
    (loop for animal below 4
          ;; Handle two transitions having the same effect.
          if (and (/= animal +platypus+)
                  (= (transition machine animal +green+)
                     (transition machine animal +yellow+)))
            collect (let ((transition (transition machine animal +green+)))
                      (list animal
                            (next-animal transition)
                            (format-label "\\Sigma"
                                          (capital *colours* (next-colour transition))
                                          (capital *directions* (next-direction transition)))))
          else
            append (loop for colour below 2
                          unless (and (= colour +green+) (= animal +platypus+))
                            collect (let ((transition (transition machine animal colour)))
                                      (list animal
                                            (next-animal transition)
                                            (format-label (format nil "\\q{~A}" (capital *colours* colour))
                                                          (capital *colours* (next-colour transition))
                                                          (capital *directions* (next-direction transition)))))))))

(defun least-sum-of-squares-layout (machines)
  (let ((lines (reduce #'append machines :key #'lines-from-machine))
        best-layout
        (best-sum most-positive-fixnum))
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
    (values (coerce best-layout 'vector) best-sum)))

;;; Pick angles for arrows.

(defvar *l->r-angles*
  '((20 . 160)
    (-20 . -160)
    (70 . 110)
    (-70 . -110)
    (45 . 135)
    (-45 . -135))
  "A list of angles for arcs between a state on the left and a state on the right.")
(defvar *r->l-angles*
  (loop for (l . r) in *l->r-angles* collect (cons r l)))

(defun layout-lines (machine state-layout)
  (labels ((next (lines solution used)
             (if (endp lines)
                 solution
                 (destructuring-bind ((from to name) &rest rest) lines
                   (amb:amb ((angles
                              (if (< (position from state-layout) (position to state-layout))
                                  *l->r-angles*
                                  *r->l-angles*)))
                   ;; Check that we haven't already used a location from this line.
                   (amb:constrain (not (member (cons (car angles) from) used :test #'equal)))
                   (amb:constrain (not (member (cons (cdr angles) to) used :test #'equal)))
                   (next rest
                         (cons (list from (car angles) to (cdr angles) name) solution)
                         (list* (cons (car angles) from) (cons (cdr angles) to) used))))))
           (line-length (line)
             (destructuring-bind (from to name) line
               (declare (ignore name))
               (abs (- (position from state-layout) (position to state-layout))))))
    (next (sort (lines-from-machine machine) #'< :key #'line-length) '() '())))

;;; Generate TikZ from a layout and machine.

(defun draw-machine (machine
                     &key
                       (stream *standard-output*)
                       (state-layout (least-sum-of-squares-layout (list machine)))
                       (line-layout (layout-lines machine state-layout)))
  "Generate boilerplate for a TikZ diagram of a machine.
The layout is most likely going to be wrong - the user has to tweak that by themself."
  (format stream "~&\\begin{tikzpicture}")
  (loop for x from 0 by 3
        for index across state-layout
        for letter = (capital *animals* index)
        do (format stream "~&\\node[draw, circle] (~C) at (~D, 0) {\\sffamily ~C};"
                   letter x letter))
  (format stream "~&\\draw[->] (-1, 0) to (K);")
  (loop for (from-state from-angle to-state to-angle label) in line-layout
        do (format stream "~&\\draw[->] (~C) to[~A] node[midway, ~A] {~A} (~C);"
                   (capital *animals* from-state)
                   (if (= from-state to-state)
                       (if (minusp from-angle) "loop below" "loop above")
                       (format nil "out=~D, in=~D" from-angle to-angle))
                   (if (minusp from-angle) "below" "above")
                   label
                   (capital *animals* to-state)))
  (format stream "~&\\end{tikzpicture}"))
