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

(defun stream-write-table (output states rule)
  (format output "n_states:")
  (format output "~d" (write-to-string states))
  (format output "
neighborhood:Moore
symmetries:none
")
  (apply #'mapc-cartesian
         #'(lambda (neighbourhood)
             (format output
                     (apply #'concatenate 'string
                            (nconc
                             (mapcar #'write-to-string
                                     (concatenate 'list neighbourhood
                                                  (list
                                                   (funcall rule
                                                            neighbourhood))))
                             (list "
")))))
         (make-list 9 :initial-element (range 0 (1- states)))))

(defun stream-write-rule (output)
  (format output "~d" "@RULE CompetitiveLife

@TABLE
")
  (stream-write-table output 5 #'two-faction-rule-with-glider-generator)
  (format output "~d" "
@COLORS
0 0 24 48
1 0 128 255
2 48 0 0
3 255 0 0
4 255 255 0
"))

(stream-write-rule *standard-output*)

