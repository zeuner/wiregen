; Description: Assemble patterns and rules for cellular automata
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defun empty-block (block-size)
  (make-list block-size :initial-element
             (make-list block-size :initial-element 0)))

(defmacro stream-cons (head tail) `(cons ,head #'(lambda () ,tail)))

(defun stream-car (stream) (car stream))

(defun stream-cdr (stream) (funcall (cdr stream)))

(defun stream-elt (stream l)
  (if (eql 0 l)
      (stream-car stream)
      (stream-elt (stream-cdr stream) (1- l))))

(defun constant-stream (constant)
  (stream-cons constant (constant-stream constant)))

(defun cell-apply-time-stream (cell time-stream)
  (if (eql 0 cell)
      cell
      (stream-elt time-stream (1- cell))))

(defun row-apply-time-stream (row time-stream)
  (mapcar #'(lambda (cell) (cell-apply-time-stream cell time-stream)) row))

(defun grid-apply-time-stream (grid time-stream)
  (mapcar #'(lambda (row) (row-apply-time-stream row time-stream)) grid))

(defun cell-timings-flip (cell maximum)
  (if (eql 0 cell)
      cell
      (- (1+ maximum) cell)))

(defun row-timings-flip (row maximum)
  (mapcar #'(lambda (cell) (cell-timings-flip cell maximum)) row))

(defun grid-timings-flip (grid maximum)
  (mapcar #'(lambda (row) (row-timings-flip row maximum)) grid))

(defun timings-wire-horizontal (block-size offset)
  (concatenate 'list
               (make-list offset :initial-element
                          (make-list block-size :initial-element 0))
               (list (reverse (range 1 block-size)))
               (make-list (- block-size offset 1) :initial-element
                          (make-list block-size :initial-element 0))))

(defun wire-horizontal (block-size offset)
  (grid-apply-time-stream (timings-wire-horizontal block-size offset)
   (constant-stream 3)))

(defun timings-offset-increase-horizontal (block-size from to)
  (concatenate 'list
               (make-list from :initial-element
                          (make-list block-size :initial-element 0))
               (mapcar
                #'(lambda (offset)
                    (nconc (make-list (- offset from) :initial-element 0)
                           (list (- block-size (- offset from)))
                           (make-list (- block-size (- offset from) 1)
                                      :initial-element 0)))
                (range from (1- to)))
               (list
                (concatenate 'list (make-list (- to from) :initial-element 0)
                             (reverse (range 1 (- block-size (- to from))))))
               (make-list (- block-size to 1) :initial-element
                          (make-list block-size :initial-element 0))))

(defun timings-offset-decrease-horizontal (block-size from to)
  (reverse
   (timings-offset-increase-horizontal block-size (- block-size from 1)
    (- block-size to 1))))

(defun offset-increase-horizontal (block-size from to)
  (grid-apply-time-stream
   (timings-offset-increase-horizontal block-size from to) (constant-stream 3)))

(defun offset-decrease-horizontal (block-size from to)
  (grid-apply-time-stream
   (timings-offset-decrease-horizontal block-size from to) (constant-stream 3)))

(defun concat-list-stream (earlier later)
  (if earlier
      (stream-cons (car earlier) (concat-list-stream (cdr earlier) later))
      later))

(defun concat-lists-stream (earlier later)
  (if earlier
      (concat-list-stream (car earlier)
       (concat-lists-stream (cdr earlier) later))
      later))

(defun stream-skip (skipped start)
  (if (eql 0 start)
      skipped
      (stream-skip (stream-cdr skipped) (1- start))))

(defun stream-sublist (whole from to)
  (unless (eql 0 to)
    (if (eql 0 from)
        (cons (stream-car whole)
              (stream-sublist (stream-cdr whole) from (1- to)))
        (stream-sublist (stream-cdr whole) (1- from) (1- to)))))

(defun data-signal-stream (period data &optional (start-delay 0))
  (concat-lists-stream
   (cons (make-list start-delay :initial-element 3)
         (mapcar
          #'(lambda (is-set)
              (if is-set
                  (list* 1 2 (make-list (- period 2) :initial-element 3))
                  (make-list period :initial-element 3)))
          data))
   (constant-stream 3)))

(defun data-start-wire-horizontal
       (block-size offset period data &optional (start-delay 0))
  (let ((coded (data-signal-stream period data start-delay)))
    (grid-apply-time-stream (timings-wire-horizontal block-size offset) coded)))

(defun start-wire-horizontal (block-size offset)
  (data-start-wire-horizontal block-size offset 4 '(t)))

(defun timings-delay-wire-horizontal-multi-symmetric
       (block-size offset detours delay)
  (let ((vertical (1+ (/ delay detours 2))))
    (concatenate 'list
                 (make-list offset :initial-element
                            (make-list block-size :initial-element 0))
                 (list
                  (nconc
                   (mapcan
                    #'(lambda (detour)
                        (list
                         (+ (- block-size (* 4 detours))
                            (* 2 (1+ vertical) detour))
                         0 0 0))
                    (reverse (range 1 detours)))
                   (reverse (range 1 (- block-size (* 4 detours))))))
                 (mapcar
                  #'(lambda (distance)
                      (nconc
                       (mapcan
                        #'(lambda (detour)
                            (list 0
                                  (-
                                   (+ (- block-size (* 4 detours))
                                      (* 2 (1+ vertical) detour))
                                   distance)
                                  0
                                  (+ (- block-size (* 4 detours))
                                     (* 2 (1+ vertical) (1- detour))
                                     distance)))
                        (reverse (range 1 detours)))
                       (make-list (- block-size (* 4 detours)) :initial-element
                                  0)))
                  (range 1 vertical))
                 (list
                  (nconc
                   (mapcan
                    #'(lambda (detour)
                        (list 0 0
                              (+ (- block-size (* 4 detours) (1+ vertical))
                                 (* 2 (1+ vertical) detour))
                              0))
                    (reverse (range 1 detours)))
                   (make-list (- block-size (* 4 detours)) :initial-element
                              0)))
                 (make-list (- block-size vertical offset 2) :initial-element
                            (make-list block-size :initial-element 0)))))

(defun timings-delay-wire-horizontal-fine (block-size offset delay)
  (let* ((vertical-length (1+ delay))
         (diagonal-length (+ 2 vertical-length))
         (regular-start (1+ diagonal-length)))
    (concatenate 'list
                 (make-list offset :initial-element
                            (make-list block-size :initial-element 0))
                 (list
                  (concatenate 'list (list (+ block-size delay))
                               (make-list (1- regular-start) :initial-element
                                          0)
                               (reverse
                                (range 1 (- block-size regular-start)))))
                 (mapcar
                  #'(lambda (vertical-offset)
                      (nconc (make-list vertical-offset :initial-element 0)
                             (list (- (+ block-size delay) vertical-offset))
                             (make-list (- diagonal-length vertical-offset 1)
                                        :initial-element 0)
                             (list
                              (+ (- block-size regular-start) vertical-offset))
                             (make-list (- block-size diagonal-length 1)
                                        :initial-element 0)))
                  (range 1 vertical-length))
                 (list
                  (concatenate 'list
                               (make-list (1- diagonal-length) :initial-element
                                          0)
                               (list
                                (- (+ block-size delay) vertical-length 1))
                               (make-list (- block-size diagonal-length)
                                          :initial-element 0)))
                 (make-list (- block-size offset diagonal-length)
                            :initial-element
                            (make-list block-size :initial-element 0)))))

(defun timings-delay-wire-horizontal (block-size offset delay)
  (cond ((eql 0 delay) (timings-wire-horizontal block-size offset))
        ((eql 0 (mod delay 4))
         (timings-delay-wire-horizontal-multi-symmetric block-size offset 2
          delay))
        ((evenp delay)
         (timings-delay-wire-horizontal-multi-symmetric block-size offset 1
          delay))
        (t (timings-delay-wire-horizontal-fine block-size offset delay))))

(defun delay-wire-horizontal-multi-symmetric (block-size offset detours delay)
  (grid-apply-time-stream
   (timings-delay-wire-horizontal-multi-symmetric block-size offset detours
    delay)
   (constant-stream 3)))

(defun delay-wire-horizontal (block-size offset delay)
  (grid-apply-time-stream
   (timings-delay-wire-horizontal block-size offset delay) (constant-stream 3)))

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

(defun blanked (grid)
  (make-list (length grid) :initial-element
             (make-list (length (car grid)) :initial-element 0)))

(defun replace-into
       (grid replacement
        &optional (offset-row 0) (offset-column 0) (max-state 3))
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
      (add-around offset-row appended-columns appended-rows offset-column
       max-state (blanked replacement))
      grid))))

(defun timings-clock-horizontal (block-size offset clock)
  (let* ((remainder (mod clock 2))
         (half (/ (- clock remainder) 2))
         (undelayed
          (concatenate 'list
                       (make-list offset :initial-element
                                  (make-list block-size :initial-element 0))
                       (list
                        (nconc
                         (make-list (- block-size half) :initial-element 0)
                         (reverse (range 1 half))))
                       (list
                        (concatenate 'list
                                     (make-list (- block-size half 1)
                                                :initial-element 0)
                                     (list (+ half remainder 1))
                                     (make-list (- half 1) :initial-element 0)
                                     '(1)))
                       (list
                        (concatenate 'list
                                     (make-list (- block-size half)
                                                :initial-element 0)
                                     (range (+ half remainder 2) clock) '(0)))
                       (make-list (- block-size offset 3) :initial-element
                                  (make-list block-size :initial-element 0))))
         (delay
          (reverse
           (transpose
            (reverse
             (transpose
              (grid-timings-flip
               (timings-delay-wire-horizontal block-size
                (- block-size offset 1) remainder)
               (+ block-size 2 remainder))))))))
    (transpose
     (reverse
      (replace-into (reverse (transpose undelayed))
       (trim-east
        (reverse
         (transpose
          (crop-west
           (- block-size -3 (length (transpose (trim-west undelayed))))
           delay))))
       2 0 clock)))))

(defun clock-horizontal
       (block-size offset clock
        &optional (period clock) (data '(t)) (start-delay 0))
  (grid-apply-time-stream (timings-clock-horizontal block-size offset clock)
   (data-signal-stream period data start-delay)))

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

(defun full-adder-vertical-period-17 (block-size x y cin)
  (flatten-grid
   (list
    (mapcar #'(lambda (block) (crop-south block-size block))
            (list
             (transpose
              (max-grid (offset-decrease-horizontal (* block-size 2) x 0)
               (offset-decrease-horizontal (* block-size 2) (+ block-size y)
                8)))
             (apply #'concatenate 'list
                    (make-list 2 :initial-element
                               (transpose (wire-horizontal block-size cin))))))
    (list (half-adder-vertical-period-17 block-size 0 8)
          (apply #'concatenate 'list
                 (transpose (delay-wire-horizontal block-size cin 28))
                 (make-list 4 :initial-element
                            (transpose (wire-horizontal block-size cin)))))
    (mapcar #'(lambda (block) (crop-south block-size block))
            (list
             (apply #'concatenate 'list
                    (make-list 2 :initial-element
                               (transpose (wire-horizontal block-size 1))))
             (transpose
              (max-grid (offset-decrease-horizontal (* block-size 2) 4 0)
               (offset-decrease-horizontal (* block-size 2) (+ block-size cin)
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
              (max-grid
               (clock-horizontal (* *block-size* 4) 5 (1+ (* 17 8)) 17
                '(t nil t nil t nil t nil))
               (clock-horizontal (* *block-size* 4) (+ *block-size* 5)
                (1+ (* 17 8)) 17 '(t t nil nil t t nil nil))
               (clock-horizontal (* *block-size* 4) (+ (* *block-size* 2) 5)
                (1+ (* 17 8)) 17 '(t t t t nil nil nil nil))))))
           (list (full-adder-vertical-period-17 *block-size* 5 5 5))))))

