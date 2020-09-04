;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Veneer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Veneer (/ i entity offset layer color point)
	(progn
		(if (>= (getvar "osmode") 16384)
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode")))))
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" 16384))))
		)
		(environment variable_list)
		(setq i 0)
		(while (< i 1)
			(setvar "errno" 7)
			(while (= (getvar "errno") 7)
				(setvar "errno" 0)
				(setq entity (entsel "\nSelect object to offset or <ENTER> to exit: "))
				(if (= (getvar "errno") 7)
					(prompt "\nNothing selected. ")
				)
			)
			(if entity
				(progn
					(setq point (getpoint "\nPick a point on the side to offset: "))
					(if point
						(progn
							(command "offset" (/ 3.0 128.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "")
							(command "offset" (/ 3.0 32.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer" "")
						)
						(progn)
					)
				)
				(progn (setq i 1))
			)
		)
		(restore variable_list)
	)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Veneer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:3PlyVeneer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:3PlyVeneer (/ i entity offset1 offset2 offset3 layer color point)
	(progn
		(if (>= (getvar "osmode") 16384)
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode")))))
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" 16384))))
		)
		(environment variable_list)
		(setq i 0)
		(while (< i 1)
			(setvar "errno" 7)
			(while (= (getvar "errno") 7)
				(setvar "errno" 0)
				(setq entity (entsel "\nSelect object to offset or <ENTER> to exit: "))
				(if (= (getvar "errno") 7)
					(prompt "\nNothing selected. ")
				)
			)
			(if entity
				(progn
					(setq point (getpoint "\nPick a point on the side to offset: "))
					(if point
						(progn
							(command "offset" (* (/ (/ 1.0 16.0) 3.0) 3.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "")
							(command "offset" (* (/ (/ 1.0 16.0) 3.0) 2.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "")
							(command "offset" (* (/ (/ 1.0 16.0) 3.0) 1.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "")
							(command "offset" (/ 3.0 32.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer" "")
						)
						(progn)
					)
				)
				(setq i (+ i 1))
			)
		)
		(restore variable_list)
	)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:3PlyVeneer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Melamine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Melamine	(/ i entity offset1 offset2 layer color point)
	(progn
		(if (>= (getvar "osmode") 16384)
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode")))))
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" 16384))))
		)
		(environment variable_list)
		(setq i 0)
		(while (< i 1)
			(setvar "errno" 7)
			(while (= (getvar "errno") 7)
				(setvar "errno" 0)
				(setq entity (entsel "\nSelect object to offset or <ENTER> to exit: "))
				(if (= (getvar "errno") 7)
					(prompt "\nNothing selected. ")
				)
			)
			(if entity
				(progn
					(setq point (getpoint "\nPick a point on the side to offset: "))
					(if point
						(progn
							(command "offset" (/ 3.0 128.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "ltype" "Fetzer -- Melamine" "")
							(command "offset" (/ 3.0 32.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer" "ltype" "Fetzer -- Melamine" "")
						)
						(progn)
					)
				)
				(setq i (+ i 1))
			)
		)
		(restore variable_list)
	)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Melamine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Backer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Backer (/ i entity offset1 offset2 layer color point)
	(progn
		(if (>= (getvar "osmode") 16384)
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode")))))
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" 16384))))
		)
		(environment variable_list)
		(setq i 0)
		(while (< i 1)
			(setvar "errno" 7)
			(while (= (getvar "errno") 7)
				(setvar "errno" 0)
				(setq entity (entsel "\nSelect object to offset or <ENTER> to exit: "))
				(if (= (getvar "errno") 7)
					(prompt "\nNothing selected. ")
				)
			)
			(if entity
				(progn
					(setq point (getpoint "\nPick a point on the side to offset: "))
					(if point
						(progn
							(command "offset" (/ 3.0 128.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "ltype" "Fetzer -- Backer" "")
							(command "offset" (/ 3.0 32.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer" "ltype" "Fetzer -- Backer" "")
						)
						(progn)
					)
				)
				(setq i (+ i 1))
			)
		)
		(restore variable_list)
	)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Backer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:PlasticLaminate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:PlasticLaminate	(/ i entity offset1 offset2 layer color point)
	(progn
		(if (>= (getvar "osmode") 16384)
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode")))))
			(progn (setq variable_list (list (list "cmdecho" 0 "osmode" 16384))))
		)
		(environment variable_list)
		(setq i 0)
		(while (< i 1)
			(setvar "errno" 7)
			(while (= (getvar "errno") 7)
				(setvar "errno" 0)
				(setq entity (entsel "\nSelect object to offset or <ENTER> to exit: "))
				(if (= (getvar "errno") 7)
					(prompt "\nNothing selected. ")
				)
			)
			(if entity
				(progn
					(setq point (getpoint "\nPick a point on the side to offset: "))
					(if point
						(progn
							(command "offset" (* (/ (/ 3.0 128.0) 2.0) 2.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "ltype" "Continuous" "")
							(command "offset" (* (/ (/ 3.0 128.0) 2.0) 1.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer -- FS" "ltype" "Fetzer -- PlasticLaminate" "color" "35" "lweight" "0.60" "")
							(command "offset" (* (/ (/ 3.0 32.0) 2.0) 2.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer" "ltype" "Continuous" "")
							(command "offset" (* (/ (/ 3.0 32.0) 2.0) 1.0) entity point "")
							(command "chprop" "last" "" "layer" "Veneer" "ltype" "Fetzer -- PlasticLaminate" "color" "31" "lweight" "0.20" "")
						)
						(progn)
					)
				)
				(setq i (+ i 1))
			)
		)
		(restore variable_list)
	)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:PlasticLaminate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;