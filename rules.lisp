; Description: Assemble patterns for the Wireworld cellular automaton
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

(defvar *glider* '((1 1 0) (1 0 1) (1 0 0)))

(defvar *moore* '((0 0) (-1 0) (-1 1) (0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1)))

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

(defun two-faction-rule-with-generator (neighbourhood)
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

(defun write-table (states rule)
  (with-output-to-string (output)
    (format output "n_states:")
    (format output "~d" (write-to-string states))
    (format output "
neighborhood:Moore
symmetries:none
")
    (mapc
     #'(lambda (neighbourhood)
         (format output
                 (apply #'concatenate 'string
                        (nconc
                         (mapcar #'write-to-string
                                 (concatenate 'list neighbourhood
                                              (list
                                               (funcall rule neighbourhood))))
                         (list "
")))))
     (apply #'cartesian (make-list 9 :initial-element (range 0 (1- states)))))))

(defun write-rule ()
  (concatenate 'string "@RULE MyLife

@TABLE
"
               (write-table 5 #'two-faction-rule-with-generator) "
@COLORS
0 0 24 48
1 0 128 255
2 48 0 0
3 255 0 0
4 255 255 0
"))

(format t "~d" (write-rule))

