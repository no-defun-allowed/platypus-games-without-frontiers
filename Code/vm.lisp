(in-package :platypus-games-without-frontiers)

;;; Other cheeky assembler things SBCL is missing.

;;; Atomic-increment of elements of (unsigned-byte 32) vectors 
(sb-c:defknown %atomic-incf-32b
    ((simple-array (unsigned-byte 32) 1)
     sb-int:index
     (unsigned-byte 32))
    (unsigned-byte 32)
    ()
  :overwrite-fndb-silently t)

(in-package :sb-vm)

;; See SBCL src/compiler/x86-64/array.lisp
(define-vop (pgwf::%atomic-incf-32b)
  (:translate pgwf::%atomic-incf-32b)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst xadd :lock
          (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
              array index 4)
          diff)
    (move result diff)))

(in-package :platypus-games-without-frontiers)

(declaim (inline atomic-incf-32b))

(defun atomic-incf-32b (array index &optional (amount 1))
  (declare ((simple-array (unsigned-byte 32) 1) array))
  (assert (< index (length array)))
  (%atomic-incf-32b array index amount))
