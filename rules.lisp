; Description: Assemble patterns and rules for cellular automata
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defun cartesian (&rest factors)
  (if factors
      (mapcan
       #'(lambda (tail)
           (mapcar #'(lambda (head) (cons head tail)) (car factors)))
       (apply #'cartesian (cdr factors)))
      (list factors)))

(defun mapc-cartesian (applied &rest factors)
  (if factors
      (apply #'mapc-cartesian
             #'(lambda (tuple-tail)
                 (mapc
                  #'(lambda (tuple-head)
                      (funcall applied (cons tuple-head tuple-tail)))
                  (car factors)))
             (cdr factors))
      (funcall applied factors)))

(defvar *glider* '((1 1 0) (1 0 1) (1 0 0)))

(defvar *spaceship*
  (transpose
   (trim-all
    (let ((*states* '("b" "o")))
      (read-rle
       (progn
        "/usr/share/golly/Patterns/Life/Guns/gun-p165mwss.rle"
        "$2b3o$b5o$b3ob2o$4b2o!"))))))

(defvar *moore* '((0 0) (-1 0) (-1 1) (0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1)))

(defvar *pre-spaceship-1* (transpose '((6 0 5 6) (6 5 0 6))))

(defvar *pre-spaceship-2* (transpose '((5 5))))

(defvar *spaceship-genesis* (list *pre-spaceship-1* *pre-spaceship-2*))

(defun surrounding-neighbourhoods (pattern)
  (let* ((padded
          (add-north 2 0
           (add-east 2 0 (add-south 2 0 (add-west 2 0 pattern)))))
         (rows (length padded))
         (columns (length (car padded))))
    (mapcar
     #'(lambda (offsets)
         (destructuring-bind
             (row column)
             offsets
           (crop-north row
            (crop-east (- columns column 3)
             (crop-south (- rows row 3) (crop-west column padded))))))
     (cartesian (range 0 (- rows 3)) (range 0 (- columns 3))))))

(defun fail (message)
  (format t "~d
"
          message)
  (funcall nil))

(defvar *spaceship-genesis-transitions*
  (mapcon
   #'(lambda (path)
       (when (cdr path)
         (destructuring-bind
             (now before . ignored)
             path
           (declare (ignore ignored))
           (let* ((padded
                   (add-north 2 0
                    (add-east 2 0 (add-south 2 0 (add-west 2 0 before)))))
                  (rows (length padded))
                  (columns (length (car padded)))
                  (padded-now
                   (add-north (- 2 (- (length now) (length before))) 0
                    (add-east
                     (- 2 (- (length (car now)) (length (car before)))) 0
                     now))))
             (mapcar
              #'(lambda (offsets)
                  (destructuring-bind
                      (row column)
                      offsets
                    (cons
                     (crop-north row
                      (crop-east (- columns column 3)
                       (crop-south (- rows row 3) (crop-west column padded))))
                     (elt (elt padded-now row) column))))
              (cartesian (range 0 (- rows 3)) (range 0 (- columns 3))))))))
   (cons *spaceship* *spaceship-genesis*)))

(setq *spaceship-genesis-transitions*
        (mapcar
         #'(lambda (transition)
             (cons
              (mapcar
               #'(lambda (coordinates)
                   (let ((relative (mapcar #'- '(1 1) coordinates)))
                     (elt (elt (car transition) (car relative))
                          (cadr relative))))
               *moore*)
              (cdr transition)))
         *spaceship-genesis-transitions*))

(let ((neighbourhoods (mapcar #'car *spaceship-genesis-transitions*)))
  (when (duplicates neighbourhoods :test #'equal) (fail "genesis ambiguity"))
  (when
      (remove-if
       #'(lambda (neighbourhood)
           (member-if #'(lambda (state) (<= 5 state)) neighbourhood))
       neighbourhoods)
    (fail "genesis cell state missing")))

(defvar *one-faction-spaceship-genesis-transitions*
  (subst 4 6 (subst 3 5 (subst 2 4 *spaceship-genesis-transitions*))))

(defun life-rule (neighbourhood)
  (let ((neighbours-alive (length (remove 0 (cdr neighbourhood)))))
    (if (if (eql 0 (car neighbourhood))
            (eql 3 neighbours-alive)
            (<= 2 neighbours-alive 3))
        1
        0)))

(defun two-faction-life-rule (neighbourhood)
  (let* ((unfactioned-neighbourhood
          (mapcar #'(lambda (state) (mod state 2)) neighbourhood))
         (neighbourhood-factions
          (mapcar #'- neighbourhood unfactioned-neighbourhood))
         (unfactioned-result (life-rule unfactioned-neighbourhood)))
    (cond ((eql 0 unfactioned-result) (car neighbourhood-factions))
          ((eql 0 (car unfactioned-neighbourhood))
           (if (< (count 1 neighbourhood) (count 3 neighbourhood))
               3
               1))
          (t
           (let ((competitor (- 4 (car neighbourhood))))
             (if (< (count (car neighbourhood) neighbourhood)
                    (count competitor neighbourhood))
                 competitor
                 (car neighbourhood)))))))

(defun two-faction-rule-with-glider-generator (neighbourhood)
  (let ((first-generator (position 4 neighbourhood)))
    (if first-generator
        (let ((second-generator
               (position 4 neighbourhood :start (1+ first-generator))))
          (cond (second-generator 2)
                ((member 3 neighbourhood)
                 (two-faction-life-rule (subst 3 4 neighbourhood)))
                ((member 1 neighbourhood)
                 (two-faction-life-rule (subst 2 4 neighbourhood)))
                ((member 2 neighbourhood)
                 (two-faction-life-rule (subst 3 4 neighbourhood)))
                (t
                 (let ((coordinates
                        (mapcar #'- '(1 1) (elt *moore* first-generator))))
                   (elt (elt *glider* (car coordinates))
                        (cadr coordinates))))))
        (two-faction-life-rule neighbourhood))))

(defun two-faction-rule-with-spaceship-generator (neighbourhood)
  (let ((genesis
         (assoc neighbourhood *spaceship-genesis-transitions* :test #'equal)))
    (if genesis
        (cdr genesis)
        (let ((first-generator (position 4 neighbourhood)))
          (if first-generator
              (let ((second-generator
                     (position 4 neighbourhood :start (1+ first-generator))))
                (cond (second-generator 2)
                      ((member 3 neighbourhood)
                       (two-faction-life-rule
                        (subst-if 3
                                  #'(lambda (state)
                                      (and (numberp state) (<= 4 state)))
                                  neighbourhood)))
                      ((member 1 neighbourhood)
                       (two-faction-life-rule
                        (subst-if 2
                                  #'(lambda (state)
                                      (and (numberp state) (<= 4 state)))
                                  neighbourhood)))
                      ((member 2 neighbourhood)
                       (two-faction-life-rule
                        (subst 3
                               #'(lambda (state)
                                   (and (numberp state) (<= 4 state)))
                               neighbourhood)))
                      ((member 5 neighbourhood)
                       (two-faction-life-rule
                        (subst 2
                               #'(lambda (state)
                                   (and (numberp state) (<= 4 state)))
                               neighbourhood)))
                      ((member 6 neighbourhood)
                       (two-faction-life-rule
                        (subst 2
                               #'(lambda (state)
                                   (and (numberp state) (<= 4 state)))
                               neighbourhood)))
                      (t
                       (let ((coordinates
                              (mapcar #'- '(1 1)
                                      (elt *moore* first-generator))))
                         (elt
                          (elt
                           (add-east 1 0
                            (add-west 1 0 (add-south 1 0 *pre-spaceship-2*)))
                           (car coordinates))
                          (cadr coordinates))))))
              (two-faction-life-rule neighbourhood))))))

(defun one-faction-rule-with-spaceship-generator (neighbourhood)
  (let ((genesis
         (assoc neighbourhood *one-faction-spaceship-genesis-transitions* :test
                #'equal)))
    (if genesis
        (cdr genesis)
        (let ((first-generator (position 2 neighbourhood)))
          (if first-generator
              (let ((second-generator
                     (position 2 neighbourhood :start (1+ first-generator))))
                (cond (second-generator 0)
                      ((member-if #'(lambda (state) (member state '(1 3 4)))
                                  neighbourhood)
                       (two-faction-life-rule
                        (subst-if 0
                                  #'(lambda (state)
                                      (and (numberp state) (<= 2 state)))
                                  neighbourhood)))
                      (t
                       (let ((coordinates
                              (mapcar #'- '(1 1)
                                      (elt *moore* first-generator))))
                         (elt
                          (elt
                           (add-east 1 0
                            (add-west 1 0
                             (add-south 1 0 (subst 3 5 *pre-spaceship-2*))))
                           (car coordinates))
                          (cadr coordinates))))))
              (two-faction-life-rule neighbourhood))))))

(defvar *states-rotation*
  (cartesian '(border off on)
   '(nil start-north start-east start-south start-west stop-north stop-east
     stop-south stop-west center)))

(defun cell-registers-to-state (states registers)
  (position registers states :test #'equal))

(defun row-registers-to-states (states row)
  (mapcar #'(lambda (cell) (cell-registers-to-state states cell)) row))

(defun grid-registers-to-states (states grid)
  (mapcar #'(lambda (row) (row-registers-to-states states row)) grid))

(defun cell-state-to-registers (states state) (elt states state))

(defun row-states-to-registers (states row)
  (mapcar #'(lambda (cell) (cell-state-to-registers states cell)) row))

(defun grid-states-to-registers (states grid)
  (mapcar #'(lambda (row) (row-states-to-registers states row)) grid))

(defun cell-register (n registers) (elt registers n))

(defun row-register (n row)
  (mapcar #'(lambda (cell) (cell-register n cell)) row))

(defun grid-register (n grid)
  (mapcar #'(lambda (row) (row-register n row)) grid))

(defun positions-if (condition data &optional (offset 0))
  (when data
    (let ((rest (positions-if condition (cdr data) (1+ offset))))
      (if (funcall condition (car data))
          (cons offset rest)
          rest))))

(defvar *moore-north*
  (positions-if #'(lambda (offsets) (< (car offsets) 0)) *moore*))

(defvar *moore-east*
  (positions-if #'(lambda (offsets) (< 0 (cadr offsets))) *moore*))

(defvar *moore-south*
  (positions-if #'(lambda (offsets) (< 0 (car offsets))) *moore*))

(defvar *moore-west*
  (positions-if #'(lambda (offsets) (< (cadr offsets) 0)) *moore*))

(defun rotation-rule (neighbourhood)
  (let* ((registers (row-states-to-registers *states-rotation* neighbourhood))
         (data (row-register 0 registers))
         (signals (row-register 1 registers))
         (border-north
          (every #'(lambda (position) (eq 'border (elt data position)))
                 *moore-north*))
         (border-east
          (every #'(lambda (position) (eq 'border (elt data position)))
                 *moore-east*))
         (border-south
          (every #'(lambda (position) (eq 'border (elt data position)))
                 *moore-south*))
         (border-west
          (every #'(lambda (position) (eq 'border (elt data position)))
                 *moore-west*))
         (propagate-north
          (every
           #'(lambda (position)
               (member (elt signals position) '(start-north start-east)))
           *moore-north*))
         (propagate-east
          (every
           #'(lambda (position)
               (member (elt signals position) '(start-east start-south)))
           *moore-east*))
         (propagate-south
          (every
           #'(lambda (position)
               (member (elt signals position) '(start-south start-west)))
           *moore-south*))
         (propagate-west
          (every
           #'(lambda (position)
               (member (elt signals position) '(start-west start-north)))
           *moore-west*))
         (next-signal
          (cond ((eq 'border (car data)) (car signals))
                ((and (member 'start-north signals)
                      (member 'start-east signals)
                      (member 'start-south signals)
                      (member 'start-west signals) (eq nil (car signals)))
                 'center)
                ((and (member 'start-north signals)
                      (member 'start-east signals)
                      (member 'start-south signals)
                      (member 'start-west signals) (eq 'center (car signals)))
                 nil)
                ((and (eq 'start-north (car signals)) (member 'center signals))
                 'stop-north)
                ((and (eq 'start-east (car signals)) (member 'center signals))
                 'stop-east)
                ((and (eq 'start-south (car signals)) (member 'center signals))
                 'stop-south)
                ((and (eq 'start-west (car signals)) (member 'center signals))
                 'stop-west)
                ((member (car signals)
                         '(stop-north stop-east stop-south stop-west))
                 nil)
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (member (elt signals (position '(1 0) *moore* :test #'equal))
                          '(stop-west stop-north)))
                 (elt signals (position '(1 0) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (member
                   (elt signals (position '(0 -1) *moore* :test #'equal))
                   '(stop-north stop-east)))
                 (elt signals (position '(0 -1) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (member
                   (elt signals (position '(-1 0) *moore* :test #'equal))
                   '(stop-east stop-south)))
                 (elt signals (position '(-1 0) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (member (elt signals (position '(0 1) *moore* :test #'equal))
                          '(stop-south stop-west)))
                 (elt signals (position '(0 1) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (eq (elt signals (position '(1 1) *moore* :test #'equal))
                      'stop-west))
                 (elt signals (position '(1 1) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (eq (elt signals (position '(1 -1) *moore* :test #'equal))
                      'stop-north))
                 (elt signals (position '(1 -1) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (eq (elt signals (position '(-1 -1) *moore* :test #'equal))
                      'stop-east))
                 (elt signals (position '(-1 -1) *moore* :test #'equal)))
                ((and
                  (member (car signals)
                          '(start-north start-east start-south start-west))
                  (eq (elt signals (position '(-1 1) *moore* :test #'equal))
                      'stop-south))
                 (elt signals (position '(-1 1) *moore* :test #'equal)))
                ((and (eq 'start-west (car signals))
                      (eq 'start-east
                          (elt signals
                               (position '(1 1) *moore* :test #'equal))))
                 'stop-west)
                ((and (eq 'start-north (car signals))
                      (eq 'start-south
                          (elt signals
                               (position '(1 -1) *moore* :test #'equal))))
                 'stop-north)
                ((and (eq 'start-east (car signals))
                      (eq 'start-west
                          (elt signals
                               (position '(-1 -1) *moore* :test #'equal))))
                 'stop-east)
                ((and (eq 'start-south (car signals))
                      (eq 'start-north
                          (elt signals
                               (position '(-1 1) *moore* :test #'equal))))
                 'stop-south)
                ((and border-north border-east) 'start-north)
                ((and border-east border-south) 'start-east)
                ((and border-south border-west) 'start-south)
                ((and border-west border-north) 'start-west)
                (border-north 'start-north) (border-east 'start-east)
                (border-south 'start-south) (border-west 'start-west)
                ((and (eq nil (car signals)) propagate-north propagate-east)
                 'start-north)
                ((and (eq nil (car signals)) propagate-east propagate-south)
                 'start-east)
                ((and (eq nil (car signals)) propagate-south propagate-west)
                 'start-south)
                ((and (eq nil (car signals)) propagate-west propagate-north)
                 'start-west)
                ((and (eq nil (car signals)) propagate-north) 'start-north)
                ((and (eq nil (car signals)) propagate-east) 'start-east)
                ((and (eq nil (car signals)) propagate-south) 'start-south)
                ((and (eq nil (car signals)) propagate-west) 'start-west)
                (t (car signals))))
         (next-data
          (cond
           ((eq 'start-north next-signal)
            (elt data (position '(0 -1) *moore* :test #'equal)))
           ((eq 'start-east next-signal)
            (elt data (position '(-1 0) *moore* :test #'equal)))
           ((eq 'start-south next-signal)
            (elt data (position '(0 1) *moore* :test #'equal)))
           ((eq 'start-west next-signal)
            (elt data (position '(1 0) *moore* :test #'equal)))
           (t (car data)))))
    (cell-registers-to-state *states-rotation* (list next-data next-signal))))

(defun next-state-torus (rule current)
  (let ((rows (length current)) (columns (length (car current))))
    (mapcar
     #'(lambda (row)
         (mapcar
          #'(lambda (column)
              (funcall rule
                       (mapcar
                        #'(lambda (coordinates)
                            (destructuring-bind
                                (row column)
                                coordinates
                              (elt (elt current row) column)))
                        (mapcar
                         #'(lambda (offsets)
                             (mapcar #'mod
                                     (mapcar #'+ (list rows columns)
                                             (list row column) offsets)
                                     (list rows columns)))
                         *moore*))))
          (range 0 (1- columns))))
     (range 0 (1- rows)))))

(defun after-n-steps-torus (n rule initial)
  (if (eql 0 n)
      initial
      (after-n-steps-torus (1- n) rule (next-state-torus rule initial))))

(defun delimited (delimiter raw)
  (if (cdr raw)
      (list* (car raw) delimiter (delimited delimiter (cdr raw)))
      raw))

(defun stream-write-table (output states rule)
  (format output "n_states:")
  (format output "~d" (write-to-string states))
  (format output "
neighborhood:Moore
symmetries:none
")
  (let ((format
         (if (< states 10)
             #'identity
             #'(lambda (entries) (delimited "," entries)))))
    (apply #'mapc-cartesian
           #'(lambda (neighbourhood)
               (let ((next (funcall rule neighbourhood)))
                 (unless (eql next (car neighbourhood))
                   (format output
                           (apply #'concatenate 'string
                                  (nconc
                                   (funcall format
                                            (mapcar #'write-to-string
                                                    (concatenate 'list
                                                                 neighbourhood
                                                                 (list next))))
                                   (list "
")))))))
           (make-list 9 :initial-element (range 0 (1- states))))))

(defun stream-write-rule (output)
  (format output "~d" "@RULE Rotation

@TABLE
")
  (stream-write-table output (length *states-rotation*) #'rotation-rule))

(defvar *rotation-initial-pattern-1*
  '((0 0 1 0 0 0) (0 0 1 0 0 0) (1 1 1 1 1 1) (0 0 0 0 0 0) (0 0 0 0 0 0)
    (0 0 0 0 0 0)))

(defvar *rotation-initial-1*
  (grid-registers-to-states *states-rotation*
   (concatenate 'list
                (make-list 7 :initial-element
                           (make-list 20 :initial-element '(border nil)))
                (mapcar
                 #'(lambda (row)
                     (concatenate 'list
                                  (make-list 7 :initial-element '(border nil))
                                  (mapcar
                                   #'(lambda (value)
                                       (list
                                        (if (eql 0 value)
                                            'off
                                            'on)
                                        nil))
                                   row)
                                  (concatenate 'list
                                               (make-list 7 :initial-element
                                                          '(border nil)))))
                 *rotation-initial-pattern-1*)
                (make-list 7 :initial-element
                           (make-list 20 :initial-element '(border nil))))))

(defvar *rotation-initial-pattern-2*
  '((0 0 1 0 0) (0 0 1 0 0) (1 1 1 1 1) (0 0 0 0 0) (0 0 0 0 0)))

(defvar *rotation-initial-2*
  (grid-registers-to-states *states-rotation*
   (concatenate 'list
                (make-list 7 :initial-element
                           (make-list 20 :initial-element '(border nil)))
                (mapcar
                 #'(lambda (row)
                     (concatenate 'list
                                  (make-list 7 :initial-element '(border nil))
                                  (mapcar
                                   #'(lambda (value)
                                       (list
                                        (if (eql 0 value)
                                            'off
                                            'on)
                                        nil))
                                   row)
                                  (concatenate 'list
                                               (make-list 8 :initial-element
                                                          '(border nil)))))
                 *rotation-initial-pattern-2*)
                (make-list 8 :initial-element
                           (make-list 20 :initial-element '(border nil))))))

(subst 0 'border
       (subst 0 'off
              (subst 1 'on
                     (grid-register 0
                      (grid-states-to-registers *states-rotation*
                       (after-n-steps-torus 6 #'rotation-rule
                        *rotation-initial-1*))))))

(subst 0 'border
       (subst 0 'off
              (subst 1 'on
                     (grid-register 0
                      (grid-states-to-registers *states-rotation*
                       (after-n-steps-torus 5 #'rotation-rule
                        *rotation-initial-2*))))))

