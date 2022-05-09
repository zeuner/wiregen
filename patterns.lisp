; Description: Assemble patterns and rules for cellular automata
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defun transpose (grid)
  (when (car grid) (cons (mapcar #'car grid) (transpose (mapcar #'cdr grid)))))

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

