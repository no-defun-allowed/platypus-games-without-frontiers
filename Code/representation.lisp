(in-package :pgwf)

(defmacro define-enum (table-name &rest kind-names)
  "Define each kind as successive natural numbers, and a vector s.t. (svref <table-name> <kind>) yields the name of the kind."
  `(progn
     (defvar ,table-name
       (vector ,@(loop for name in kind-names
                       collect (format nil "~@(~a~)"
                                       (remove #\+ (symbol-name name))))))
     ,@(loop for n from 0
             for name in kind-names
             collect `(defconstant ,name ,n))))

(defun capital (table index) (char (aref table index) 0))

(deftype player () '(unsigned-byte 28))
(define-enum *colours* +green+ +yellow+)
(defconstant +initial-board+ (1- (expt 2 21))) ; 21 ones
(define-enum *animals* +platypus+ +wombat+ +emu+ +kangaroo+)
(define-enum *directions* +wattle+ +ghost-gum+)
(defconstant +state-size+ 4)
(defconstant +state-shift+ (1- (integer-length +state-size+)))

(declaim (inline transition-index transition vtransition effects nth-transition))

(defun transition-index (state read) (* +state-size+ (+ (* state 2) read -1)))

(defun transition (player state read)
  (ldb (byte 4 (transition-index state read)) player))
(defun vtransition (players states read)
  (let ((index (v32:- (v32:+ (v32:shiftl states 1) read) 1)))
    (v32:ldb 4 (v32:shiftl index +state-shift+) players)))

(defun effects (player state)
  "Get bits corresponding to the effects caused by outgoing transitions from STATE."
  (logand #b10011001
          (if (= state +platypus+)
              ;; No (platypus, green) state, so produce zeroes.
              (ash (ldb (byte 4 0) player) 4)
              (ldb (byte 8 (transition-index state 0)) player))))

(defun nth-transition (player n)
  (ldb (byte 4 (* 4 n)) player))

(defmacro define-accessor (name size position)
  (let ((vector-name (alexandria:format-symbol :pgwf "V~A" name)))
    `(progn
       (declaim (inline ,name ,vector-name))
       (defun ,name (transition)
         (ldb (byte ,size ,position) transition))
       (defun ,vector-name (transitions)
         (v32:ldb ,size ,position transitions)))))

(define-accessor next-colour 1 0)
(define-accessor next-animal 2 1)
(define-accessor next-direction 1 3)
(defun put-animal (transition animal) (dpb animal (byte 2 1) transition))

(defun make-transition (colour animal direction)
  (logior (ash direction 3) (ash animal 1) colour))
(defun put-transition (player state read transition)
  (dpb transition (byte 4 (transition-index state read)) player))
(defun copy-transition (target-player source-player state read)
  (put-transition target-player
                  state read
                  (transition source-player state read)))
