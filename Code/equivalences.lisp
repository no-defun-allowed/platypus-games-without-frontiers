(in-package :pgwf)

(deftype result-table () `(simple-array (unsigned-byte 32) (,(expt 2 28))))
(deftype ub8 () '(unsigned-byte 8))

(defun ensure-worker-pool ()
  "Create a worker pool with one worker per core, if a pool was not made before."
  (when (null lparallel:*kernel*)
    (setf lparallel:*kernel*
          (lparallel:make-kernel
           (cl-cpus:get-number-of-processors)
           ;; Give each thread its own RNG.
           :bindings '((*random-state* . (make-random-state t)))))))

;; This table is about 2GiB large, so preallocate it.
(defvar *result-table* nil)
(defun make-table ()
  (if (null *result-table*)
      (setf *result-table* (make-array (expt 2 28) :element-type '(unsigned-byte 32)))
      (fill *result-table* 0)))
(defun normalise (function)
  (make-table)
  (ensure-worker-pool)
  (lparallel:pdotimes (machine (expt 2 28) *result-table*)
    (atomic-incf-32b *result-table* (funcall function machine))))

;;; Normalisation algorithms
(declaim (inline walk-machine))
(defun walk-machine (function machine)
  "Call FUNCTION with every state and colour encountered by a depth-first traversal of MACHINE."
  (declare (player machine))
  (let ((seen (make-array 4 :initial-element nil)))
    (labels ((visit-transition (state colour)
               (funcall function state colour)
               (visit-state (next-animal (transition machine state colour))))
             (visit-state (state)
               (unless (aref seen state)
                 (setf (aref seen state) t)
                 (visit-transition state +yellow+)
                 (unless (= state +platypus+) (visit-transition state +green+)))))
      (visit-state +kangaroo+))))

(defun dead-code-elimination (machine)
  (declare (player machine))
  (let ((new-machine 0))
    (walk-machine
     (lambda (state colour)
       (setf new-machine (copy-transition new-machine machine state colour)))
     machine)
    new-machine))

(defun rename-states (machine)
  (declare (player machine))
  (let ((new-machine 0)
        ;; We can't rename Platypus or Kangaroo, so always keep them
        ;; unchanged in the map.
        (mapping (vector +platypus+ nil nil +kangaroo+))
        (next-state +wombat+))
    (flet ((remap (state)
             (or (aref mapping state)
                 (prog1 (setf (aref mapping state) next-state)
                   (incf next-state)))))
      (walk-machine
       (lambda (state colour)
         (let* ((transition (transition machine state colour))
                (new-transition (make-transition (next-colour transition)
                                                 (remap (next-animal transition))
                                                 (next-direction transition))))
           (setf new-machine (put-transition new-machine
                                             (remap state)
                                             colour
                                             new-transition))))
       machine)
      new-machine)))

(defun dfa-minimise (machine)
  (declare (player machine)
           (optimize speed))
  (labels ((encode-boolean (b)
             "Turn a Lisp boolean into a SSE-esque boolean."
             (if b #xFF #x00))
           (find-last-set (i) (1- (integer-length i)))
           (initial-distinct-table ()
             "Generate an array A s.t. (aref A X Y) denotes if states X and Y have 'obviously' different effects."
             (let ((table (make-array '(4 4) :element-type 'ub8)))
               (dotimes (x 4)
                 (dotimes (y 4)
                   (setf (aref table x y)
                         (logior (encode-boolean (/= (effects machine x) (effects machine y)))
                                 (logxor (encode-boolean (= x +platypus+))
                                         (encode-boolean (= y +platypus+)))))))
               table))
           (initial-transition-table ()
             "Generate an array A s.t. (aref A (+ (* 2 S) C)) is the state to transition to after reading the colour C when in state S."
             (let ((table (make-array 16 :element-type 'ub8 :initial-element 0)))
               (dotimes (i 7)
                 ;; 1+ skips over (platypus, green) which is undefined in the machine.
                 ;; INITIAL-DISTINCT-TABLE has Platypus be distinct from every other
                 ;; state, so the result of iteration does not depend on
                 ;; what we put in place of a transition.
                 (setf (aref table (1+ i)) (next-animal (nth-transition machine i))))
               table))
           (iterate ()
             "Repeatedly mark states with distinct transitions as distinct, until no more states change."
             ;; I treat the 16 lanes as a 4x4 array of detected distinctions.
             ;; The array is symmetric, but the names assume row-major
             ;; order.
             (let ((distinct-table (v8:row-major-aref (initial-distinct-table) 0))
                   (transition-table (v8:row-major-aref (initial-transition-table) 0))
                   ;; The states considered by each "row" of the array.
                   (rows (v8:of 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3))
                   ;; Ditto for each column.
                   (columns (v8:of 0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3))
                   (steps 0))
               (declare (fixnum steps))
               (loop
                 (flet ((next (colour)
                          (let* (;; The targets of both transitions relevant to each cell.
                                 (row-states (v8:shuffle transition-table (v8:+ colour (v8:double rows))))
                                 (column-states (v8:shuffle transition-table (v8:+ colour (v8:double columns))))
                                 ;; Compute the row-major index into the array.
                                 (indices (v8:+ (v8:double (v8:double row-states)) column-states))
                                 ;; Look up each index.
                                 (marked (v8:shuffle distinct-table indices))
                                 (new-table (v8:or marked distinct-table))
                                 ;; Check if we changed anything before we update.
                                 (changed (plusp (v8:movemask (v8:/= distinct-table new-table)))))
                            ;; Finally propagate the distinctions.
                            (setf distinct-table new-table)
                            changed)))
                   (declare (inline next))
                   (incf steps)
                   ;; Terminate when we didn't do anything.
                   (unless (or (next +green+) (next +yellow+)) (return))))
               ;; Find the highest-numbered state in each equivalence set.
               ;; Each nibble has bits set for distinct states for one state;
               ;; LOGNOT computes the not-distinct states i.e. the equivalence set.
               (let ((bits (lognot (v8:movemask distinct-table))))
                 (values (coerce (loop for n below 16 by 4
                                       collect (find-last-set (ldb (byte 4 n) bits)))
                                 '(vector ub8))
                         steps)))))
    (declare (inline encode-boolean find-last-set))
    (multiple-value-bind (mapping steps) (iterate)
      (let ((new-machine 0))
        ;; Copy in the transitions, adjusting state names using the
        ;; equivalence sets.
        (walk-machine
         (lambda (state colour)
           (let* ((transition (transition machine state colour))
                  (new-transition (put-animal transition (aref mapping (next-animal transition)))))
             (setf new-machine (put-transition new-machine
                                               (aref mapping state)
                                               colour
                                               new-transition))))
         machine)
        (values new-machine steps)))))

;;; Statistics on steps taken by DFA-MINIMISE
(defun minimisation-step-histogram ()
  (let ((results (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    (lparallel:pdotimes (machine (expt 2 28) results)
      (atomic-incf-32b results (nth-value 1 (dfa-minimise machine))))))

;;; Produce equivalence sets table
(defstruct equivalence-sets
  (machines (error "provide :MACHINES") :type (simple-array (unsigned-byte 32) 1))
  (counts (error "provide :COUNTS") :type (simple-array (unsigned-byte 32) 1)))

(defun equivalence-sets (table &key (sort nil))
  (let ((machines (make-array 1000000
                              :fill-pointer 0
                              :adjustable t)))
    (loop for machine from 0
          for count across table
          unless (zerop count)
            do (vector-push-extend (cons machine count) machines))
    ;; Sort by decreasing counts.
    (let ((sorted (if sort (sort machines #'> :key #'cdr) machines)))
      (make-equivalence-sets
       :machines (map '(vector (unsigned-byte 32)) #'car sorted)
       :counts (map '(vector (unsigned-byte 32)) #'cdr sorted)))))

(defun store-equivalence-sets (directory sets)
  (numpy-file-format:store-array
   (equivalence-sets-machines sets)
   (make-pathname :directory directory :name "machines"))
  (numpy-file-format:store-array
   (equivalence-sets-counts sets)
   (make-pathname :directory directory :name "weights")))

(defun load-equivalence-sets (directory)
  (make-equivalence-sets
   :machines (numpy-file-format:load-array
              (make-pathname :directory directory :name "machines"))
   :counts (numpy-file-format:load-array
            (make-pathname :directory directory :name "weights"))))

(defun generate-sets (directory)
  (store-equivalence-sets
   directory
   (equivalence-sets (normalise (alexandria:compose #'rename-states #'dfa-minimise)))))
