;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Part_Dimension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:part_dimension (/ maxextents minextents ss box)
	(if (>= (getvar "osmode") 16384)
		(progn (setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode")))))
		(progn (setq variable_list (list (list "cmdecho" 0 "osmode" 16384))))
	)
	(environment variable_list)
	(if (ssget "_I");implied-selection
		(progn
			(setq ss (ssget))
		)
		(progn
			(princ "\nSelect objects to dimension: ")
			(setq ss (ssget))
		)
	)
	(command "ucs" "world")
	(if ss
		(progn
			(setq box (get_bounding_box ss))
			(setq minextents (car box))
			(setq maxextents (cadr box))
			(if (and minextents maxextents)
				(progn
					(command "dimstyle" "restore" "Fetzer")
					(command "cannoscale" "1\" = 1'-0\"")
					(command "dimlinear" maxextents (list (car minextents) (cadr maxextents) 0.0) (polar maxextents (/ PI 2) 6.0))
					(command "dimlinear" minextents (list (car minextents) (cadr maxextents) 0.0) (polar minextents PI 6.0))
				)
			)
		)
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Part_Dimension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Program_Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:program_number (/ point e xdata elist elistnew pn programNumber process)
	(progn (setq variable_list (list (list "cmdecho" 0 ))))
	(environment variable_list)
	(regapp "FETZER")
	(initget 128)
	(setq pn (getpoint "\nSpecify program number: "))
	(if (and (= pn nil) programNumber)
		(progn
			(setq pn programNumber)
		)
	)
	(if pn
		(progn
			(setq process T)
			(while process
				(initget 128)
				(setq point (getpoint (strcat "\nSelect insertion point for program " pn " (or ESC/ENTER when done): ")))
				(if point
					(progn
						(setq point (polar point (* (/ 3.0 2.0) pi) 6.0))
						(command "cannoscale" "1\" = 1'-0\"")
						(command "TEXTSIZE" ".125")
						(command "text" "justify" "mc" point "" "" pn)
						(setq e (entlast))
						(setq elist (entget e))
						(setq xdata (list (list -3 (list "FETZER" (cons 1000 "PROGRAM NUMBER")))))
						(setq elistnew (append elist xdata))
						(entmod elistnew)
						(command "chprop" e "" "layer" "Note" "")
						(setq pn (itoa (+ (atoi pn) 1)))
					)
					(progn
						(setq process nil)
					)
				)
			)
		)
		(progn
			(princ "\nInvalid program number!")
		)
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Program_Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Part_DXF_Mastercam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:part_dxf_mastercam (/ ss mode elev)
	(progn (setq variable_list (list (list "cmdecho" 0 ))))
	(environment variable_list)
	(princ "\nSelect objects to rotate -90 deg about the bottom right and dxf: ")
	(setq ss (ssget))
	(setq mode 1)
	(setq elev "0,0,0")
	(export_dxf mode ss elev)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Part_DXF_Mastercam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Part_DXF_Bus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:part_dxf_bus (/ ss mode elev)
	(progn (setq variable_list (list (list "cmdecho" 0 ))))
	(environment variable_list)
	(princ "\nSelect objects to rotate 90 deg about the top right and dxf: ")
	(setq ss (ssget))
	(setq mode 2)
	(setq elev "0,0,0")
	(export_dxf mode ss elev)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Part_DXF_Bus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Part_DXF_Mastercam_Metric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun c:part_dxf_mastercam_metric (/ ss mode elev thick)
;	(princ "\nSelect objects to rotate 90 deg about the bottom right, scale 25.4, and dxf: ")
;	(setq ss (ssget))
;	(setq mode 3)
;	(setq thick (getstring "\nEnter part thickness: "))
;	(setq elev (strcat "0,0," thick))
;	(export_dxf mode ss elev)
;	(princ)
;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Part_DXF_Mastercam_Metric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Part_DXF_Mastercam_Alt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:part_dxf_mastercam_alt (/ ss mode elev)
	(progn (setq variable_list (list (list "cmdecho" 0 ))))
	(environment variable_list)
	(princ "\nSelect objects to rotate 90 deg about the bottom right, scale 25.4, and dxf: ")
	(setq ss (ssget))
	(setq mode 4)
	(setq elev "0,0,0")
	(export_dxf mode ss elev)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Part_DXF_Mastercam_Alt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Part_DXF_Mastercam_Morb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun c:part_dxf_mastercam_morb (/ ss mode elev)
;	(princ "\nSelect objects to rotate -90 deg about the bottom left and dxf: ")
;	(setq ss (ssget))
;	(setq mode 5)
;	(setq thick (getstring "\nEnter part thickness: "))
;	(setq elev (strcat "0,0," thick))
;	(export_dxf mode ss elev)
;	(princ)
;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Part_DXF_Mastercam_Morb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Export_DXF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun export_dxf (mode ss elev / origin box minextents maxextents base_point rotation ex_datalist programNumber e_name e_datalist e_datatype msg dwgPrefix camDirectory fileName scale)
	(setq I 0)
	(while (< I (sslength ss))
		(setq e_name (ssname ss I))
		(setq e_datalist (entget e_name '("FETZER")))
		(setq e_datatype (cdr (assoc 0 e_datalist)))
		(if (= e_datatype "TEXT")
			(progn
				(setq ex_datalist (assoc -3 e_datalist))
				(if ex_datalist
					(progn
						(setq programNumber (cdr (nth 1 (car (cdr (assoc -3 e_datalist))))))
						(if programNumber
							(progn
								(setq programNumber (cdr (assoc 1 e_datalist)))
							)
						)
					)
				)
			)
			(progn)
		)
		(setq I (+ I 1))
	)
	(if programNumber
		(progn
			(setq msg (strcat "\nEnter program number <" programNumber ">: "))
		)
		(progn
			(setq msg (strcat "\nEnter program number: "))
		)
	)
	(initget 128)
	(setq pn (getpoint msg))
	(if (and (= pn nil) programNumber)
		(progn
			(setq pn programNumber)
		)
	)
	(if pn
		(progn
			(setq dwgPrefix (getvar "dwgprefix"))
			(setq camDirectory dwgPrefix)
			(setq fileName (strcat camDirectory pn ".dxf"))
			(setq origin (list 0.0 0.0 0.0))
			(setq rotation 0.0)
			(setq scale 1)
			(if (= mode 1) ;;c:part_dxf_mastercam
				(progn
					(setq rotation -90.0)
					(setq scale 1.0)
				)
			)
			(if (= mode 2) ;;c:part_dxf_bus
				(progn
					(setq rotation 90.0)
					(setq scale 1.0)
				)
			)
;			(if (= mode 3) ;;c:part_dxf_mastercam_metric
;				(progn
;					(setq rotation -90.0)
;					(setq scale 25.4)
;				)
;			)
			(if (= mode 4) ;;c:part_dxf_mastercam_alt
				(progn
					(setq rotation 0.0)
					(setq scale 1.0)
				)
			)
;			(if (= mode 5) ;;c:part_dxf_mastercam_morb
;				(progn
;					(setq rotation -90.0)
;					(setq scale 1.0)
;				)
;			)
			(command "copy" ss "" origin origin)
			(command "rotate" ss "" origin rotation)
			(command "scale" ss "" "0,0,0" scale)
			(command "move" ss "" "0,0,0" elev)
			(setq box (get_bounding_box ss))
			(setq minextents (car box))
			(setq maxextents (cadr box))
			(if (= mode 1) ;;c:part_dxf_mastercam
				(progn
					(setq base_point (list (car minextents) (cadr minextents) 0.0))
				)
			)
			(if (= mode 2) ;;c:part_dxf_bus
				(progn
					(setq base_point (list (car minextents) (cadr maxextents) 0.0))
				)
			)
;			(if (= mode 3) ;;c:part_dxf_mastercam_metric
;				(progn
;					(setq base_point (list (car minextents) (cadr minextents) 0.0))
;				)
;			)
			(if (= mode 4) ;;c:part_dxf_mastercam_alt
				(progn
					(setq base_point (list (car maxextents) (cadr minextents) 0.0))
				)
			)
;			(if (= mode 5) ;;c:part_dxf_mastercam_morb
;				(progn
;					(setq base_point (list (car minextents) (cadr maxextents) 0.0))
;				)
;			)
			(command "move" ss "" base_point origin)
			;(setvar "cmdecho" 1)
			(command ".dxfout" filename "Objects" ss "" "Version" "2000" "")
			;(setvar "cmdecho" 0)
			(command "erase" ss "")
			(princ (strcat "\nProgram number " pn " has been exported to: " filename))
		)
	)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Export_DXF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Get_Bounding_Box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_bounding_box (ss / I e_name e_datalist e_datatype e_layer e_minextents e_maxextents minextents maxextents)
	(setq maxextents nil)
	(setq minextents nil)
	(setq I 0)
	(while (< I (sslength ss))
		(setq e_name (ssname ss I))
		(setq e_datalist (entget e_name))
		(setq e_datatype (cdr (assoc 0 e_datalist)))
		(setq e_layer (cdr (assoc 8 e_datalist)))
		(if (and
				(or (= e_datatype "ARC")
					(= e_datatype "CIRCLE")
					(= e_datatype "ELLIPSE")
					(= e_datatype "INSERT")
					(= e_datatype "LINE")
					(= e_datatype "LWPOLYLINE")
					(= e_datatype "POINT")
					(= e_datatype "POLYLINE")
					(= e_datatype "SPLINE")
				)
				(= (strcase e_layer) "CNC")
			)
			(progn
				(setq e (vlax-ename->vla-object e_name))
				(vla-getboundingbox e 'e_minextents 'e_maxextents)
				(setq e_minextents (vlax-safearray->list e_minextents))
				(setq e_maxextents (vlax-safearray->list e_maxextents))
				(if (and minextents maxextents)
					(progn
						(if (< (car e_minextents) (car minextents))
							(setq minextents (list (car e_minextents) (cadr minextents) 0.0))
						)
						(if (< (cadr e_minextents) (cadr minextents))
							(setq minextents (list (car minextents) (cadr e_minextents) 0.0))
						)
						(if (> (car e_maxextents) (car maxextents))
							(setq maxextents (list (car e_maxextents) (cadr maxextents) 0.0))
						)
						(if (> (cadr e_maxextents) (cadr maxextents))
							(setq maxextents (list (car maxextents) (cadr e_maxextents) 0.0))
						)
					)
					(progn
						(setq minextents (list (car e_minextents) (cadr e_minextents) 0.0))
						(setq maxextents (list (car e_maxextents) (cadr e_maxextents) 0.0))
					)
				)
			)
		)
		(setq I (+ I 1))
	)
	(setq result (list (list (car minextents) (cadr minextents) 0.0) (list (car maxextents) (cadr maxextents) 0.0)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Get_Bounding_Box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;