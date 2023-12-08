(in-package :platypus-magic)

;;; A faster implementation of (COUNT 1 #<bit-vector>), as someone did
;;; not read <https://plover.com/~mjd/misc/hbaker-archive/Bitvectors.html>

#+(and sbcl 64-bit)
(defun fast-count-1 (bitmap)
  (declare (simple-bit-vector bitmap))
  (let ((cursor (sb-sys:vector-sap bitmap))
        (words (ceiling (length bitmap) 64)))
    ;; Memory is pre-zeroed, so we'll only see more zeroes in the last word.
    ;; (Don't tell Doug)
    (loop for n below words
          sum (logcount (sb-sys:sap-ref-64 cursor (* n 8))) of-type fixnum)))

#-(and sbcl 64-bit)
(defun fast-count-1 (bitmap) (count 1 bitmap))
