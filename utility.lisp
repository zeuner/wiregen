; Description: Assemble patterns and rules for cellular automata
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defun range (from to) (when (<= from to) (cons from (range (1+ from) to))))

(defun split-by (delimiter to-split)
  (let ((found (position delimiter to-split)))
    (if found
        (cons (subseq to-split 0 found)
              (split-by delimiter (subseq to-split (1+ found))))
        (list to-split))))

(defun rpad (required padding raw)
  (concatenate 'list raw
               (make-list (- required (length raw)) :initial-element padding)))

(defun duplicates (checked &key (test #'eql))
  (when checked
    (if (member (car checked) (cdr checked) :test test)
        (cons (car checked) (duplicates (cdr checked) :test test))
        (duplicates (cdr checked) :test test))))

