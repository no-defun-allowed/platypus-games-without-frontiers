;;; Nicer names for SB-SIMD functions, and some extra algorithms.

(defpackage :pgwf-simd (:use :cl))
(defpackage :pgwf-simd8
  (:use)
  (:documentation "SSE/128-bit operations over 8-bit lanes.")
  (:export #:+ #:/= #:double #:movemask #:of #:or #:row-major-aref
           #:shuffle))
(defpackage :pgwf-simd32
  (:use)
  (:documentation "AVX/256-bit operations over 32-bit lanes.")
  (:export #:* #:+ #:- #:/= #:= #:>
           #:and #:andc1 #:broadcast #:expand #:incf #:if #:ldb #:movemask
           #:of #:or #:row-major-aref #:set-bit #:shiftl #:shiftr #:shuffle #:sum))
(in-package :pgwf-simd)

(defmacro define-alias (target-symbol source-symbol arglist)
  "Generate a macro definition which rewrites function calls to SOURCE-SYMBOL with TARGET-SYMBOL."
  `(defmacro ,target-symbol ,arglist
     ,(if (member '&rest arglist)
          `(list* ',source-symbol ,@(remove '&rest arglist))
          `(list ',source-symbol ,@arglist))))

(defmacro define-simple-aliases (target-package prefix &body specs)
  `(progn
     ,@(loop for (base target . arglist) in specs
             for base-symbol = (alexandria:format-symbol (symbol-package prefix) "~A~A" prefix base)
             for symbol = (intern (symbol-name target) target-package) 
             collect `(define-alias ,symbol ,base-symbol ,arglist))))

;;; 8-bit SSE2 operations, as used by DFA normalisation.

(define-simple-aliases :pgwf-simd8 sb-simd-sse2::u8.16
  (-row-major-aref row-major-aref array index)
  (+ + &rest vectors)
  (-or or &rest vectors)
  (-movemask movemask vector)
  (/= /= vector &rest vectors))

;; This is in the AVX package for some reason, and it's unexported.
;; (It should be in SSSE3 really.)
(define-alias pgwf-simd8:shuffle sb-simd-avx::u8.16-shuffle (vector index))
(define-alias pgwf-simd8:of sb-simd-sse2:make-u8.16 (a b c d e f g h i j k l m n o p))

(declaim (inline pgwf-simd8:double))
(defun pgwf-simd8:double (x)
  "There are no multiply/shift instructions on bytes, so we double by addition."
  (pgwf-simd8:+ x x))

;;; 32-bit AVX2 operations, as used by the SIMD Platypus interpreter.

(define-simple-aliases :pgwf-simd32 sb-simd-avx2::u32.8
  (-and and &rest vectors)
  (-andc1 andc1 mask vector)
  (-if if mask then else)
  (-movemask movemask vector)
  (-or or &rest vectors)
  (-row-major-aref row-major-aref array &rest indices)
  (-shiftl shiftl value distance)
  (-shiftr shiftr value distance)
  (/= /= vector1 vector2)
  (= = vector &rest vectors)
  (- - vector &rest vectors)
  (+ + &rest vectors)
  (> > &rest vectors))

(define-alias pgwf-simd32:of sb-simd-avx2:make-u32.8 (a b c d e f g h))
(define-alias pgwf-simd32:broadcast sb-simd-avx2:u32.8 (value))

(declaim (inline pgwf-simd32:*))
(defun pgwf-simd32:* (a b)
  (declare ((sb-ext:simd-pack-256 (unsigned-byte 32)) a b))
  ;; SB-SIMD-AVX:{S,U}32.8! aren't inlined.
  (sb-simd-avx::%u32.8!-from-p256
   (sb-simd-avx2:s32.8-mullo
    (sb-simd-avx::%s32.8!-from-p256 a)
    (sb-simd-avx::%s32.8!-from-p256 b))))

(defmacro pgwf-simd32:incf (place &key (by 1) if)
  (if if
      `(setf ,place (pgwf-simd32:if ,if (pgwf-simd32:+ ,by ,place) ,place))
      `(setf ,place (pgwf-simd32:+ ,by ,place))))

(defmacro pgwf-simd32:ldb (size position vector)
  "LDB over a vector. SIZE must be constant, POSITION and VECTOR may vary."
  (check-type size unsigned-byte)
  `(pgwf-simd32:and ,(1- (ash 1 size))
                    ,(if (eql position 0)
                         vector
                         `(pgwf-simd32:shiftr ,vector ,position))))

(declaim (inline pgwf-simd32:set-bit))
(defun pgwf-simd32:set-bit (bit n vector)
  "Replace the Nth bit in each lane of VECTOR with BIT."
  (pgwf-simd32:or (pgwf-simd32:shiftl bit n)
                  (pgwf-simd32:andc1 (pgwf-simd32:shiftl 1 n)
                                     vector)))

(declaim (inline pgwf-simd32:sum))
(defun pgwf-simd32:sum (vector)
  (multiple-value-bind (a b c d e f g h)
      (sb-simd-avx2:u32.8-values vector)
    (+ a b c d e f g h)))

;;; SB-SIMD lacks the VPERMPS instruction, so I add support for it here.
(sb-c:defknown pgwf-simd32:shuffle
    ((sb-ext:simd-pack-256 (unsigned-byte 32))
     (sb-ext:simd-pack-256 (unsigned-byte 32)))
    (sb-ext:simd-pack-256 (unsigned-byte 32))
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(in-package :sb-vm)
(define-vop (pgwf-simd32:shuffle)
  (:translate pgwf-simd32:shuffle)
  (:policy :fast-safe)
  (:args (values :scs (int-avx2-reg))
         (indices :scs (int-avx2-reg)))
  (:arg-types simd-pack-256-ub32 simd-pack-256-ub32)
  (:results (result :scs (int-avx2-reg)))
  (:result-types simd-pack-256-ub32)
  (:generator 0
    ;; The inputs are the other way around to how I'd like them, yes.
    (inst vpermps result indices values)))

(in-package :pgwf-simd)
;; n.b. the body will get replaced by the prior VOP, so this isn't infinite recursion.
(defun pgwf-simd32:shuffle (indices values) (pgwf-simd32:shuffle indices values))

;;; An expand algorithm, which uses a lookup table of shuffles.
(unless (boundp '+expand-table+)
  (defconstant +expand-table+
    ;; One extra to allow loading 16 bytes when MASK = 255
    (let ((table (make-array '(256 8)
                             :element-type '(unsigned-byte 32)
                             ;; An out-of-range number to make
                             ;; the defined values stick out.
                             :initial-element 8)))
      (dotimes (input 256)
        ;; This is a prefix sum, basically.
        (loop with count = 0
              for bit below 8
              when (logbitp bit input)
                do (setf (aref table input bit) count)
                   (incf count)))
      table)))

(declaim (inline pgwf-simd32:expand))
(defun pgwf-simd32:expand (mask values)
  "Place values in the lanes where values are true. The lanes with false values will have garbage values.
See https://aplwiki.com/wiki/Expand for a description of expansion."
  ;; This should be AREF, but there is a bug in the compiler; it'd
  ;; multiply the first index by 256 rather than 8 to compute the
  ;; equivalent 1D index.
  ;; TODO: Would it help to load 8-bit elements, then extend each
  ;; to 32-bits, to cut down on loads? A: No, that makes it slower
  ;; somehow.
  ;;
  ;; Or twiddle with PDEP? Something like
  #+(or)
  (let ((expanded (* #xFF (pdep #x0101010101010101 mask))))
    (pdep expanded #x0706050403020100))
  ;; Now there's no memory loads.
  (let* ((indices (pgwf-simd32:row-major-aref +expand-table+ (* 8 mask))))
    (pgwf-simd32:shuffle values indices)))
