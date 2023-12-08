(in-package :pgwf)

(defstruct (results (:constructor %make-results))
  equivalence-sets
  (wins (error "no :POINTS") :type (simple-array (unsigned-byte 32)))
  (points (error "no :WINS") :type (simple-array (unsigned-byte 64))))

(defun make-results (equivalence-sets)
  (let ((length (length (equivalence-sets-machines equivalence-sets))))
    (%make-results
     :equivalence-sets equivalence-sets
     :wins (make-array length :element-type '(unsigned-byte 32) :initial-element 0)
     :points (make-array length :element-type '(unsigned-byte 64) :initial-element 0))))

(defun read-results (pathname results)
  (let ((wins (results-wins results))
        (points (results-points results)))
    (sqlite:with-open-database (database pathname)
      (loop with statement = (sqlite:prepare-statement database "SELECT id, wins, points FROM platypus")
            while (sqlite:step-statement statement)
            do (incf (aref wins (sqlite:statement-column-value statement 0))
                     (sqlite:statement-column-value statement 1))
               (incf (aref points (sqlite:statement-column-value statement 0))
                     (sqlite:statement-column-value statement 2))
            finally (sqlite:finalize-statement statement)))))

(defstruct result
  (machine 0 :type player)
  (count 0 :type (unsigned-byte 32))
  (wins 0 :type (unsigned-byte 32))
  (points 0 :type (unsigned-byte 64)))

(defun result> (a b)
  (or (> (result-wins a) (result-wins b))
      (and (= (result-wins a) (result-wins b))
           (> (result-points a) (result-points b)))))

(defun sort-results (results)
  (let* ((length (length (results-wins results)))
         (players (make-array length)))
    (dotimes (i length)
      (setf (aref players i)
            (make-result
             :machine (aref (equivalence-sets-machines (results-equivalence-sets results)) i)
             :count (aref (equivalence-sets-counts (results-equivalence-sets results)) i)
             :wins (aref (results-wins results) i)
             :points (aref (results-points results) i))))
    (sort players #'result>)))

(defun render-results (ranking n)
  (loop for i below n
        for r across ranking 
        do (format t "~%~%In ~:R place with ~:D wins and ~:D points, ~:D machine~:P equivalent to machine \\texttt{~7,'0X}:~%~%"
                   (1+ i)
                   (result-wins r)
                   (result-points r)
                   (result-count r)
                   (result-machine r))
           (draw-machine (result-machine r))))
