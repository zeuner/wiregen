; Description: Assemble patterns for the Wireworld cellular automaton
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defun empty-block (block-size)
  (make-list block-size :initial-element
             (make-list block-size :initial-element 0)))

(defun wire-horizontal (block-size offset)
  (concatenate 'list
               (make-list offset :initial-element
                          (make-list block-size :initial-element 0))
               (list (make-list block-size :initial-element 3))
               (make-list (- block-size offset 1) :initial-element
                          (make-list block-size :initial-element 0))))

(defun offset-increase-horizontal (block-size from to)
  (concatenate 'list
               (make-list from :initial-element
                          (make-list block-size :initial-element 0))
               (mapcar
                #'(lambda (offset)
                    (concatenate 'list
                                 (make-list (- offset from) :initial-element 0)
                                 '(3)
                                 (make-list (- block-size (- offset from) 1)
                                            :initial-element 0)))
                (range from (1- to)))
               (list
                (concatenate 'list (make-list (- to from) :initial-element 0)
                             (make-list (- block-size (- to from))
                                        :initial-element 3)))
               (make-list (- block-size to 1) :initial-element
                          (make-list block-size :initial-element 0))))

(defun offset-decrease-horizontal (block-size from to)
  (reverse
   (offset-increase-horizontal block-size (- block-size from 1)
    (- block-size to 1))))

(defun rpad (required padding raw)
  (concatenate 'list raw
               (make-list (- required (length raw)) :initial-element padding)))

(defun data-start-wire-horizontal (block-size offset period data)
  (let ((coded
         (mapcan
          #'(lambda (is-set)
              (if is-set
                  (list* 1 2 (make-list (- period 2) :initial-element 3))
                  (make-list period :initial-element 3)))
          data)))
    (concatenate 'list
                 (make-list offset :initial-element
                            (make-list block-size :initial-element 0))
                 (list
                  (rpad block-size 3 (subseq (reverse coded) (1- period))))
                 (make-list (- block-size offset 1) :initial-element
                            (make-list block-size :initial-element 0)))))

(defun start-wire-horizontal (block-size offset)
  (data-start-wire-horizontal block-size offset 4 '(t)))

(defun delay-wire-horizontal-multi-symmetric (block-size offset detours delay)
  (let ((vertical (1+ (/ delay detours 2))))
    (concatenate 'list
                 (make-list offset :initial-element
                            (make-list block-size :initial-element 0))
                 (list
                  (nconc
                   (apply #'concatenate 'list
                          (make-list detours :initial-element
                                     (concatenate 'list '(3)
                                                  (make-list 3 :initial-element
                                                             0))))
                   (make-list (- block-size (* 4 detours)) :initial-element
                              3)))
                 (make-list vertical :initial-element
                            (concatenate 'list
                                         (apply #'concatenate 'list
                                                (make-list (* 2 detours)
                                                           :initial-element
                                                           '(0 3)))
                                         (make-list
                                          (- block-size (* 4 detours))
                                          :initial-element 0)))
                 (list
                  (nconc
                   (apply #'concatenate 'list
                          (make-list detours :initial-element '(0 0 3 0)))
                   (make-list (- block-size (* 4 detours)) :initial-element
                              0)))
                 (make-list (- block-size vertical offset 2) :initial-element
                            (make-list block-size :initial-element 0)))))

(defun delay-wire-horizontal-fine (block-size offset delay)
  (let* ((vertical-length (1+ delay))
         (diagonal-length (+ 2 vertical-length))
         (regular-start (1+ diagonal-length)))
    (concatenate 'list
                 (make-list offset :initial-element
                            (make-list block-size :initial-element 0))
                 (list
                  (concatenate 'list '(3)
                               (make-list (1- regular-start) :initial-element
                                          0)
                               (make-list (- block-size regular-start)
                                          :initial-element 3)))
                 (mapcar
                  #'(lambda (vertical-offset)
                      (concatenate 'list
                                   (make-list vertical-offset :initial-element
                                              0)
                                   '(3)
                                   (make-list
                                    (- diagonal-length vertical-offset 1)
                                    :initial-element 0)
                                   '(3)
                                   (make-list (- block-size diagonal-length 1)
                                              :initial-element 0)))
                  (range 1 vertical-length))
                 (list
                  (concatenate 'list
                               (make-list (1- diagonal-length) :initial-element
                                          0)
                               '(3)
                               (make-list (- block-size diagonal-length)
                                          :initial-element 0)))
                 (make-list (- block-size offset diagonal-length)
                            :initial-element
                            (make-list block-size :initial-element 0)))))

(defun delay-wire-horizontal (block-size offset delay)
  (cond ((eql 0 delay) (wire-horizontal block-size offset))
        ((eql 0 (mod delay 4))
         (delay-wire-horizontal-multi-symmetric block-size offset 2 delay))
        ((evenp delay)
         (delay-wire-horizontal-multi-symmetric block-size offset 1 delay))
        (t (delay-wire-horizontal-fine block-size offset delay))))

(defun clock-horizontal (block-size offset clock)
  (let* ((remainder (mod clock 2))
         (half (/ (- clock remainder) 2))
         (undelayed
          (concatenate 'list
                       (make-list offset :initial-element
                                  (make-list block-size :initial-element 0))
                       (list
                        (concatenate 'list
                                     (make-list (- block-size half)
                                                :initial-element 0)
                                     (make-list (- half 2) :initial-element 3)
                                     '(2 1)))
                       (list
                        (concatenate 'list
                                     (make-list (- block-size half 1)
                                                :initial-element 0)
                                     '(3)
                                     (make-list (- half 1) :initial-element 0)
                                     '(1)))
                       (list
                        (concatenate 'list
                                     (make-list (- block-size half)
                                                :initial-element 0)
                                     (make-list (- half 1) :initial-element 3)
                                     '(0)))
                       (make-list (- block-size offset 3) :initial-element
                                  (make-list block-size :initial-element 0)))))
    undelayed))

(defun duplicating-wire-horizontal
       (block-size offset &optional (duplication-offset 5))
  (concatenate 'list
               (make-list offset :initial-element
                          (make-list block-size :initial-element 0))
               (list (make-list block-size :initial-element 3))
               (make-list (1- block-size) :initial-element
                          (concatenate 'list
                                       (make-list duplication-offset
                                                  :initial-element 0)
                                       '(3)
                                       (make-list
                                        (- block-size duplication-offset 1)
                                        :initial-element 0)))
               (list
                (concatenate 'list
                             (make-list duplication-offset :initial-element 0)
                             (make-list (- block-size duplication-offset)
                                        :initial-element 3)))
               (make-list (- block-size offset 1) :initial-element
                          (make-list block-size :initial-element 0))))

(defun transpose (grid)
  (when (car grid) (cons (mapcar #'car grid) (transpose (mapcar #'cdr grid)))))

(defun flatten-row (nested)
  (when (car nested)
    (cons (apply #'concatenate 'list (mapcar #'car nested))
          (flatten-row (mapcar #'cdr nested)))))

(defun flatten-grid (nested)
  (apply #'concatenate 'list (mapcar #'flatten-row nested)))

(defun max-row (&rest rows) (apply #'mapcar #'max rows))

(defun max-grid (&rest grids) (apply #'mapcar #'max-row grids))

(defun min-row (&rest rows) (apply #'mapcar #'min rows))

(defun min-grid (&rest grids) (apply #'mapcar #'min-row grids))

(defun cold-cell (state)
  (if (< 0 state)
      3
      0))

(defun cold-row (row) (mapcar #'cold-cell row))

(defun cold-grid (grid) (mapcar #'cold-row grid))

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

(defun split-by (delimiter to-split)
  (let ((found (position delimiter to-split)))
    (if found
        (cons (subseq to-split 0 found)
              (split-by delimiter (subseq to-split (1+ found))))
        (list to-split))))

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

(defun trim-north (grid)
  (when grid
    (if (not (remove 0 (car grid)))
        (trim-north (cdr grid))
        grid)))

(defun apply-west (to-north grid)
  (transpose (funcall to-north (transpose grid))))

(defun apply-south (to-north grid) (reverse (funcall to-north (reverse grid))))

(defun apply-east (to-north grid)
  (transpose (apply-south to-north (transpose grid))))

(defun trim-west (grid) (apply-west #'trim-north grid))

(defun trim-south (grid) (apply-south #'trim-north grid))

(defun trim-east (grid) (apply-east #'trim-north grid))

(defun trim-all (grid) (trim-north (trim-east (trim-south (trim-west grid)))))

(defun add-north (width state grid)
  (concatenate 'list
               (make-list width :initial-element
                          (make-list (length (car grid)) :initial-element
                                     state))
               grid))

(defun add-west (width state grid)
  (apply-west #'(lambda (grid) (add-north width state grid)) grid))

(defun add-south (width state grid)
  (apply-south #'(lambda (grid) (add-north width state grid)) grid))

(defun add-east (width state grid)
  (apply-east #'(lambda (grid) (add-north width state grid)) grid))

(defun add-around (north east south west state grid)
  (add-north north state
   (add-east east state (add-south south state (add-west west state grid)))))

(defun draw-north (drawn reversed grid)
  (concatenate 'list
               (list
                (funcall
                 (if reversed
                     #'reverse
                     #'identity)
                 (concatenate 'list drawn
                              (make-list (- (length (car grid)) (length drawn))
                                         :initial-element 0))))
               grid))

(defun draw-east (drawn reversed grid)
  (apply-east #'(lambda (grid) (draw-north drawn reversed grid)) grid))

(defun draw-south (drawn reversed grid)
  (apply-south #'(lambda (grid) (draw-north drawn reversed grid)) grid))

(defun draw-west (drawn reversed grid)
  (apply-west #'(lambda (grid) (draw-north drawn reversed grid)) grid))

(defun crop-north (width grid) (subseq grid width))

(defun crop-west (width grid)
  (apply-west #'(lambda (grid) (crop-north width grid)) grid))

(defun crop-south (width grid)
  (apply-south #'(lambda (grid) (crop-north width grid)) grid))

(defun crop-east (width grid)
  (apply-east #'(lambda (grid) (crop-north width grid)) grid))

(defun blanked (grid)
  (make-list (length grid) :initial-element
             (make-list (length (car grid)) :initial-element 0)))

(defun replace-into
       (grid replacement &optional (offset-row 0) (offset-column 0))
  (let* ((rows-grid (length grid))
         (columns-grid (length (car grid)))
         (rows-replacement (length replacement))
         (columns-replacement (length (car replacement)))
         (appended-rows (- rows-grid rows-replacement offset-row))
         (appended-columns (- columns-grid columns-replacement offset-column)))
    (max-grid
     (add-around offset-row appended-columns appended-rows offset-column 0
      replacement)
     (min-grid
      (add-around offset-row appended-columns appended-rows offset-column 3
       (blanked replacement))
      grid))))

(defvar *and-raw*
  (trim-all
   (read-rle
    (progn
     "/usr/share/golly/Patterns/WireWorld/gate-AND.mcl"
     "2$5.A5C$11.C$.A5C5.3C$7.C.C.C3.C$8.3C4.C$9.C.C.C.C$12.3C$13.C.10C!"))))

(defvar *xor-raw*
  (trim-all
   (read-rle
    (progn
     "/usr/share/golly/Patterns/WireWorld/gate-XOR.mcl"
     "$3.A$3.C$3.C$3.C$2.4C$2.C.10C$2.4C$3.C$3.C$3.C$3.C!"))))

(defvar *or-raw*
  (trim-all
   (read-rle
    (progn
     "/usr/share/golly/Patterns/WireWorld/gate-OR.mcl"
     "$5.A$5.C$5.C$5.C$5.C$4.12C$5.C$5.C$5.C$5.C$5.A!"))))

(defvar *crossing-raw*
  (trim-all
   (read-rle
    (progn
     "https://doi.org/10.25088/ComplexSystems.27.1.19"
     "$.5C5.4C$6.C.C.C$5.5C$5.C2.C$5.5C$6.C.C.C$.5C5.4C!"))))

(defun crossing-vertical (block-size left right)
  (flatten-grid
   (list
    (list
     (transpose
      (max-grid
       (concatenate 'list (wire-horizontal block-size left)
                    (empty-block block-size))
       (duplicating-wire-horizontal block-size right))))
    (list
     (replace-into
      (max-grid (transpose (duplicating-wire-horizontal block-size left))
       (transpose
        (concatenate 'list (wire-horizontal block-size right)
                     (wire-horizontal block-size right))))
      (draw-east '(0 0 3 3 3) nil
       (draw-south
        (cons 0
              (make-list (- (length (car *crossing-raw*)) 6) :initial-element
                         3))
        t
        (draw-south '(3) t
         (draw-west '(3 3 3) t
          (draw-north '(3) nil
           (draw-north '(3) nil
            (crop-east 2 (crop-west 2 *crossing-raw*))))))))
      0 (- right 1)))
    (list
     (transpose
      (max-grid (delay-wire-horizontal-multi-symmetric block-size left 4 24)
       (delay-wire-horizontal-multi-symmetric block-size right 4 16)))
     (transpose
      (max-grid (delay-wire-horizontal-multi-symmetric block-size right 1 2)
       (wire-horizontal block-size left)))))))

(defvar *block-size* 24)

(defvar *delay-test*
  (flatten-grid
   (list*
    (make-list 8 :initial-element
               (transpose (start-wire-horizontal *block-size* 1)))
    (mapcar
     #'(lambda (delay)
         (transpose (delay-wire-horizontal *block-size* 1 delay)))
     (range 0 7))
    (make-list 8 :initial-element (transpose (wire-horizontal *block-size* 1)))
    (mapcar
     #'(lambda (delay)
         (transpose (delay-wire-horizontal *block-size* 1 delay)))
     (reverse (range 0 7)))
    (make-list 4 :initial-element
               (make-list 8 :initial-element
                          (transpose (wire-horizontal *block-size* 1)))))))

(defun xor-vertical (block-size)
  (replace-into (transpose (wire-horizontal block-size 4))
   (transpose
    (draw-south '(3 3) nil
     (draw-north '(3 3) nil (crop-south 2 (crop-north 2 *xor-raw*)))))
   0 0))

(defun and-vertical (block-size)
  (replace-into (empty-block block-size)
   (transpose
    (reverse
     (draw-west '(3 0 0 0 0 0 0 0 3) t
      (draw-south nil nil
       (draw-north '(3 3 3 3 3) nil
        (draw-west '(3 3 3 3 3) t (cold-grid (crop-east 2 *and-raw*))))))))))

(defun or-vertical (block-size)
  (replace-into (transpose (wire-horizontal block-size 3))
   (transpose
    (draw-north '(3 3) nil
     (draw-south '(3 3) nil
      (crop-north 3 (crop-south 3 (crop-east 5 *or-raw*))))))))

(defun half-adder-vertical-period-17
       (block-size left right &optional (delay 0))
  (flatten-grid
   (list (list (crossing-vertical block-size left right))
         (list (and-vertical block-size) (xor-vertical block-size))
         (list (transpose (delay-wire-horizontal block-size 1 delay))
               (transpose (delay-wire-horizontal block-size 4 (+ delay 3)))))))

(defun full-adder-vertical-period-17 (block-size)
  (flatten-grid
   (list
    (mapcar #'(lambda (block) (crop-south block-size block))
            (list
             (transpose
              (max-grid (offset-decrease-horizontal (* block-size 2) 1 0)
               (offset-decrease-horizontal (* block-size 2) (+ block-size 1)
                8)))
             (apply #'concatenate 'list
                    (make-list 2 :initial-element
                               (transpose (wire-horizontal block-size 1))))))
    (list (half-adder-vertical-period-17 block-size 0 8)
          (apply #'concatenate 'list
                 (transpose (delay-wire-horizontal block-size 1 28))
                 (make-list 4 :initial-element
                            (transpose (wire-horizontal block-size 1)))))
    (mapcar #'(lambda (block) (crop-south block-size block))
            (list
             (apply #'concatenate 'list
                    (make-list 2 :initial-element
                               (transpose (wire-horizontal block-size 1))))
             (transpose
              (max-grid (offset-decrease-horizontal (* block-size 2) 4 0)
               (offset-decrease-horizontal (* block-size 2) (+ block-size 1)
                8)))))
    (list
     (apply #'concatenate 'list
            (transpose (delay-wire-horizontal block-size 1 28))
            (make-list 4 :initial-element
                       (transpose (wire-horizontal block-size 1))))
     (half-adder-vertical-period-17 block-size 0 8))
    (mapcar #'(lambda (block) (crop-south block-size block))
            (list
             (transpose
              (max-grid (offset-decrease-horizontal (* block-size 2) 1 0)
               (offset-decrease-horizontal (* block-size 2) (+ block-size 1)
                6)))
             (apply #'concatenate 'list
                    (make-list 2 :initial-element
                               (transpose (wire-horizontal block-size 4))))))
    (list (or-vertical block-size) (empty-block block-size)
          (transpose (delay-wire-horizontal block-size 4 1))))))

(format t "~d"
        (write-rle
         (flatten-grid
          (list
           (list
            (crop-east *block-size*
             (transpose
              (max-grid (clock-horizontal (* *block-size* 4) 1 (* 17 2))
               (clock-horizontal (* *block-size* 4) (+ *block-size* 1)
                (* 17 3))
               (clock-horizontal (* *block-size* 4) (+ (* *block-size* 2) 1)
                (* 17 5))))))
           (list (full-adder-vertical-period-17 *block-size*))))))

