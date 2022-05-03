; Description: Assemble patterns for the Wireworld cellular automaton
; Author: Isidor Zeuner
; compsys@quidecco.de
; For license see: https://mit-license.org
; -------------------------------------------------------------------

(defun range (from to) (when (<= from to) (cons from (range (1+ from) to))))

