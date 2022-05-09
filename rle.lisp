; Description: Assemble patterns and rules for cellular automata
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defvar *states* '("." "A" "B" "C"))

(defun write-rle-state (state) (elt *states* state))

(defun write-rle-row (row)
  (if row
      (let* ((state (car row))
             (repetitions
              (or (position-if-not #'(lambda (other) (eql state other)) row)
                  (length row))))
        (concatenate 'string
                     (if (eql repetitions 1)
                         ""
                         (format nil "~d" repetitions))
                     (write-rle-state state)
                     (write-rle-row (subseq row repetitions))))
      "$
"))

(defun write-rle (grid)
  (concatenate 'string "x = " (format nil "~d" (length (car grid))) ", y = "
               (format nil "~d" (length grid)) ", rule = WireWorld
"
               (apply #'concatenate 'string (mapcar #'write-rle-row grid)) "!
"))

(defun read-rle-line-or-more (line-or-more)
  (multiple-value-bind (repetitions next)
      (parse-integer line-or-more :junk-allowed t)
    (let ((repetitions (or repetitions 1))
          (unparsed (subseq line-or-more next)))
      (if (equal "" unparsed)
          (make-list repetitions)
          (let ((state (position (subseq unparsed 0 1) *states* :test #'equal))
                (tail (read-rle-line-or-more (subseq unparsed 1))))
            (cons
             (concatenate 'list (make-list repetitions :initial-element state)
                          (car tail))
             (cdr tail)))))))

(defun read-rle (rle)
  (let* ((varying-length
          (mapcan #'read-rle-line-or-more
                  (split-by #\$ (car (split-by #\! rle)))))
         (max-length (apply #'max (mapcar #'length varying-length))))
    (mapcar #'(lambda (line) (rpad max-length 0 line)) varying-length)))

