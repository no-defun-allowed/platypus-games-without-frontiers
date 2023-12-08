(in-package :platypus-games-without-frontiers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +board-size+ 21)
  (defconstant +initial-position+ (floor +board-size+ 2))
  (defconstant +move-limit+ 50))

;;; Naïve interpreter

(defun run-game (player1 player2)
  (declare (player player1 player2)
           (optimize speed))
  (let ((board +initial-board+)
        (p1-pos +initial-position+)
        (p1-animal +kangaroo+)
        (p1-score 0)
        (p2-pos +initial-position+)
        (p2-animal +kangaroo+)
        (p2-score 0)
        ;; A default for when we don't halt early.
        (turns +move-limit+))
    (declare (fixnum p1-score p2-score)
             ((mod #.+board-size+) p1-pos p2-pos)
             ((unsigned-byte #.+board-size+) board))
    (dotimes (n +move-limit+)
      (macrolet ((move (player pos animal score)
                   `(let ((read (ldb (byte 1 ,pos) board)))
                      (when (and (= read +green+) (= ,animal +platypus+))
                        (setf turns n)
                        (return (values p1-score p2-score)))
                      (let* ((tr (transition ,player ,animal read))
                             (colour (next-colour tr))
                             (animal (next-animal tr))
                             (dir (next-direction tr)))
                        (setf ,animal animal)
                        (when (/= colour read) (incf ,score))
                        (setf (ldb (byte 1 ,pos) board) colour)
                        ;; I can't decide if it's better to do this, which gets
                        ;; the division optimised away by algebraic magic,
                        ;; but the resulting multiply isn't amazingly fast either:
                        (setf ,pos (mod (+ ,pos (1- (* 2 dir))) +board-size+))
                        ;; or this, which branches.
                        #+(or)
                        (setf ,pos
                              (if (zerop dir)
                                  (if (zerop ,pos) (1- +board-size+) (1- ,pos))
                                  (if (= ,pos (1- +board-size+)) 0 (1+ ,pos))))))))
        #+log-states
        (format t "Scal ~7,'0X: move ~3D board ~21,'0B p1: ~2D ~1D ~2D p2: ~2D ~1D ~2D~%"
                player2 n board p1-pos p1-animal p1-score p2-pos p2-animal p2-score)
        (move player1 p1-pos p1-animal p1-score)
        (move player2 p2-pos p2-animal p2-score)))
    (values p1-score p2-score board turns)))

;;; SIMD interpreter

(deftype pack () '(sb-ext:simd-pack-256 (unsigned-byte 32)))
(deftype machines () '(simple-array (unsigned-byte 32) 1))

(declaim (inline mod-board refill))
(defun mod-board (x)
  (declare (pack x))
  (v32:if x ; IF checks MSB i.e. underflow i.e. -1.
          (1- +board-size+)
          (v32:if (v32:= x +board-size+) 0 x)))

(defun refill (machines done index vector)
  (declare (pack machines done)
           (machines vector))
  (let* ((done-mask (v32:movemask done))
         (n (logcount done-mask))
         (next (v32:row-major-aref vector index)))
    (values (v32:if done (v32:expand done-mask next) machines)
            (+ n index))))

(defmacro blet (bindings &body body)
  "Like LET, binding each name to its value after broadcasting.
Also binds a function RESET which takes a mask of lanes to reset as argument, unless the binding is annotated with :NO-RESET.

(BLET binding* body)
binding ::= (name value [:no-reset])"
  `(let ,(loop for (name value) in bindings
               collect `(,name (v32:broadcast ,value)))
     (flet ((reset (mask)
              (setf
               ,@(loop for (name value . configuration) in bindings
                       unless (member :no-reset configuration)
                         collect name
                         and collect `(v32:if mask ,value ,name)))))
       (declare (inline reset) (ignorable #'reset))
       ,@body)))

(defun simd-interpret (player1 opponents counts
                       &key (start 0) (end (length opponents)))
  (declare (machines opponents counts)
           (player player1)
           (optimize speed)
           ((unsigned-byte 32) start end))
  (assert (<= end (length opponents)))
  (assert (>= (length opponents) 8))
  (let ((player2 (v32:row-major-aref opponents start))
        (weight (v32:row-major-aref counts start))
        ;; We need to be able to load all 8 vector elements
        ;; from OPPONENTS at a time.
        (limit (- end 8))
        (cursor (+ start 8)))
    (blet ((player1-total-points 0 :no-reset)
           (player1-total-wins 0 :no-reset)
           (board +initial-board+)
           (finished 0)
           ;; In theory resetting this would be redundant. In practice
           ;; it really messes up performance much more than a
           ;; broadcast should have any right to.
           (vplayer1 player1 :no-reset)
           (p1-pos +initial-position+)
           (p1-animal +kangaroo+)
           (p1-score 0)
           (p2-pos +initial-position+)
           (p2-animal +kangaroo+)
           (p2-score 0)
           (turns 0))
      (loop
        (when (> cursor limit) (return))
        (let* ((finished-mask (v32:movemask finished))
               (used (logcount finished-mask))
               (new-players
                 #+naive-refill (v32:row-major-aref opponents cursor)
                 #-naive-refill (v32:expand finished-mask (v32:row-major-aref opponents cursor)))
               (new-weights
                 #+naive-refill (v32:row-major-aref counts cursor)
                 #-naive-refill (v32:expand finished-mask (v32:row-major-aref counts cursor))))
          (when #+naive-refill (= finished-mask #b11111111) ; all finished
                #-naive-refill t
            (v32:incf player1-total-points :by p1-score :if finished)
            (v32:incf player1-total-wins :by weight :if (v32:and finished (v32:> p1-score p2-score)))
            (setf player2 (v32:if finished new-players player2))
            (setf weight (v32:if finished new-weights weight))
            (incf cursor used)
            (reset finished)))
        ;; Take turns.
        (macrolet ((move (player pos animal score)
                     `(let ((read (v32:ldb 1 ,pos board)))
                        (setf finished
                              (v32:or finished
                                      (v32:and (v32:= read +green+)
                                               (v32:= ,animal +platypus+))))
                        (let* ((tr (vtransition ,player ,animal read))
                               (colour (vnext-colour tr))
                               (animal (vnext-animal tr))
                               (dir (vnext-direction tr)))
                          (setf ,animal animal)
                          ;; Don't touch the score if we finished already.
                          (v32:incf ,score
                                    :by weight
                                    :if (v32:andc1 finished (v32:/= colour read)))
                          (setf board (v32:set-bit colour ,pos board)
                                ,pos (mod-board (v32:+ ,pos (v32:- (v32:shiftl dir 1) 1))))))))
          #+log-states
          (format t "SIMD ~7,'0X: move ~3D board ~21,'0B p1: ~2D ~1D ~2D~%"
                  (sb-simd-avx2:u32.8-values player2)
                  (sb-simd-avx2:u32.8-values turns)
                  (sb-simd-avx2:u32.8-values board)
                  (sb-simd-avx2:u32.8-values p1-pos)
                  (sb-simd-avx2:u32.8-values p1-animal)
                  (sb-simd-avx2:u32.8-values p1-score))
          (move vplayer1 p1-pos p1-animal p1-score)
          (move player2 p2-pos p2-animal p2-score))
        ;; Check if we took our last turn.
        (v32:incf turns)
        (setf finished (v32:or finished (v32:= turns 50))))
      (let ((total-wins (v32:sum player1-total-wins))
            (total-points (v32:sum player1-total-points)))
        (flet ((play-serial (player2 weight)
                 (multiple-value-bind (p1-score p2-score)
                       (run-game player1 player2)
                     (incf total-points (* weight p1-score))
                     (when (> p1-score p2-score) (incf total-wins weight)))))
          ;; Process the games we didn't get to finish.
          (mapc #'play-serial
                (multiple-value-list (sb-simd-avx2:u32.8-values player2))
                (multiple-value-list (sb-simd-avx2:u32.8-values weight)))
          ;; Process the games in the rest of OPPONENTS.
          (loop for index from cursor below end
                do (play-serial (aref opponents index) (aref counts index)))
          (values total-wins total-points))))))

;;; Data collection

(defun test-one-machine-locally (machine opponents)
  (loop with wins = 0
        with points = 0
        for opponent across (equivalence-sets-machines opponents)
        for opponent-count across (equivalence-sets-counts opponents)
        do (multiple-value-bind (p1-score p2-score)
               (run-game machine opponent)
             (when (> p1-score p2-score) (incf wins opponent-count))
             (incf points (* opponent-count p1-score)))
           (multiple-value-bind (p1-score p2-score)
               (run-game opponent machine)
             (when (> p2-score p1-score) (incf wins opponent-count))
             (incf points (* opponent-count p2-score)))
        finally (return (values wins points))))

(defun test-one-machine-globally (machine machine-count opponents wins points)
  (loop for opponent across (equivalence-sets-machines opponents)
        for opponent-count across (equivalence-sets-counts opponents)
        do (multiple-value-bind (p1-score p2-score)
               (run-game machine opponent)
             (cond
               ((> p1-score p2-score)
                (atomic-incf-32b wins machine opponent-count))
               ((< p1-score p2-score)
                (atomic-incf-32b wins opponent machine-count)))
             (atomics:atomic-incf (aref points machine) (* opponent-count p1-score))
             (atomics:atomic-incf (aref points opponent) (* machine-count p2-score)))))

;;; Differential tester

(defvar *output-lock* (bt:make-lock "Output lock"))
(defun differential-testing (function &key (threads (cl-cpus:get-number-of-processors)))
  (let ((running t))
    (dotimes (n threads)
      (bt:make-thread
       (lambda ()
         (loop for n from 0
               for player1 = (random (expt 2 28))
               for norm1 = (funcall function player1)
               for player2 = (random (expt 2 28))
               for norm2 = (funcall function player2)
               unless (equalp (multiple-value-list (run-game player1 player2))
                              (multiple-value-list (run-game norm1 player2)))
                 do (bt:with-lock-held (*output-lock*)
                      (format t "~&Original p1 ~7,'0X does not behave like norm. ~7,'0X against p2 ~7,'0X"
                              player1 norm1 player2))
                    (return)
               unless (equalp (multiple-value-list (run-game player1 player2))
                              (multiple-value-list (run-game player1 norm2)))
                 do (bt:with-lock-held (*output-lock*)
                      (format t "~&Original p2 ~7,'0X does not behave like norm. ~7,'0X against p1 ~7,'0X"
                              player2 norm2 player1))
                    (return)
               when (zerop (mod n 1000000))
                 do (write-char #\.)
                    (finish-output)
               while running))))
    ;; Something to interrupt.
    (unwind-protect
         (loop (sleep 1))
      (setf running nil))))

(defun test-simd-against-scalar (&optional (n 64))
  (loop with weights = (coerce (loop repeat n collect (random 2))
                               '(vector (unsigned-byte 32)))
        for player1 = (random (expt 2 28))
        for player2s = (coerce (loop repeat n collect (random (expt 2 28)))
                               '(vector (unsigned-byte 32)))
        for (scalar-wins scalar-score) = (loop for p2 across player2s
                                               for w across weights
                                               for (p1-score p2-score) = (multiple-value-list (run-game player1 p2))
                                               sum (if (> p1-score p2-score) w 0) into wins
                                               sum (* w p1-score) into points
                                               finally (return (list wins points)))
        for (simd-wins simd-score) = (multiple-value-list (simd-interpret player1 player2s weights))
        unless (and (= scalar-score simd-score)
                    (= scalar-wins simd-wins))
          do (format t "~A vs ~A yields different results (~D, ~D scalar vs ~D, ~D SIMD)~%"
                     player1 player2s
                     scalar-wins scalar-score
                     simd-wins simd-score)
             (return)))

;;; Benchmarking
(defvar *equivalence-sets* nil)

(defun benchmark (kind threads timeout)
  (check-type kind (member :naive :simd))
  (when (null *equivalence-sets*)
    (write-line "Computing equivalence sets")
    (setf *equivalence-sets*
          (equivalence-sets (normalise (alexandria:compose #'rename-states #'dfa-minimise)))))
  (let ((sets *equivalence-sets*)
        (running t)
        (lock (bt:make-lock))
        (machines-run 0)
        (start-time (get-internal-real-time))
        (end-time (get-internal-real-time)))
    (dotimes (i threads)
      (bt:make-thread
       (lambda ()
         (loop for p1 = (alexandria:random-elt (equivalence-sets-machines sets))
               while running
               do (ecase kind
                    (:naive
                     (loop for p2 across (equivalence-sets-machines sets)
                           do (run-game p1 p2)))
                    (:simd
                     (simd-interpret p1
                                     (equivalence-sets-machines sets)
                                     (equivalence-sets-counts sets))))
                  (bt:with-lock-held (lock)
                    (incf machines-run)
                    (setf end-time (get-internal-real-time)))))))
    (sleep timeout)
    (setf running nil)
    (let* ((machines (length (equivalence-sets-machines sets)))
           (time (/ (- end-time start-time) internal-time-units-per-second))
           (throughput (/ (* machines-run machines) time))
           (full (/ (* 2 (expt machines 2)) throughput)))
      (format t "~&Took ~$ seconds to run games for ~D machines (at ~F games per second). It would take ~$ days to run a full tournament on this computer."
              time
              machines-run
              throughput
              (/ full 86400))
      (values machines-run time throughput))))
