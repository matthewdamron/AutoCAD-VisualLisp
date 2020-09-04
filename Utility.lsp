;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin FixZ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:fixz (/ ed mysel total count cur_ent curtype as1 as2 ptx1 ptx2 pty1 pty2 ptz1 ptz2 oldpt newpt )
	(setq mysel (ssget))
	(setq total (sslength mysel))
	(setq count 0)
	(while (< count total)
		(setq cur_ent (ssname mysel count))
		(setq ed (entget cur_ent))
		(setq curtype (cdr (assoc '0 ed)))
		(if (= curtype "LINE")
			(progn
				(setq as1 (assoc '10 ed))
				(setq as2 (assoc '11 ed))
				(setq ptx1 (cadr as1))
				(setq ptx2 (cadr as2))
				(setq pty1 (caddr as1))
				(setq pty2 (caddr as2))
				(setq ptz1 (cadddr as1))
				(setq ptz2 (cadddr as2))
				(setq oldpt (list '10 ptx1 pty1 ptz1))
				(setq newpt (list '10 ptx1 pty1 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
				(setq oldpt (list '11 ptx2 pty2 ptz2))
				(setq newpt (list '11 ptx2 pty2 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
			)
		)
		(if (or (= curtype "MTEXT") (= curtype "TEXT"))
			(progn
				(setq as1 (assoc '10 ed))
				(setq ptx1 (cadr as1))
				(setq pty1 (caddr as1))
				(setq ptz1 (cadddr as1))
				(setq oldpt (list '10 ptx1 pty1 ptz1))
				(setq newpt (list '10 ptx1 pty1 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
			)
		)
		(if (or (= curtype "ARC") (= curtype "CIRCLE") (= curtype "ELLIPSE"))
			(progn
				(setq as1 (assoc '10 ed))
				(setq ptx1 (cadr as1))
				(setq pty1 (caddr as1))
				(setq ptz1 (cadddr as1))
				(setq oldpt (list '10 ptx1 pty1 ptz1))
				(setq newpt (list '10 ptx1 pty1 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
			)
		)
		(if (= curtype "DIMENSION")
			(progn
				(setq as1 (assoc '13 ed))
				(setq as2 (assoc '14 ed))
				(setq ptx1 (cadr as1))
				(setq ptx2 (cadr as2))
				(setq pty1 (caddr as1))
				(setq pty2 (caddr as2))
				(setq ptz1 (cadddr as1))
				(setq ptz2 (cadddr as2))
				(setq oldpt (list '13 ptx1 pty1 ptz1))
				(setq newpt (list '13 ptx1 pty1 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
				(setq oldpt (list '14 ptx2 pty2 ptz2))
				(setq newpt (list '14 ptx2 pty2 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
			)
		)
		(if (= curtype "POLYLINE")
			(progn
				(setq as1 (assoc '10 ed))
				(setq ptx1 (cadr as1))
				(setq pty1 (caddr as1))
				(setq ptz1 (cadddr as1))
				(setq oldpt (list '10 ptx1 pty1 ptz1))
				(setq newpt (list '10 ptx1 pty1 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
			)
		)
		(if (or (= curtype "LEADER") (= curtype "SPLINE"))
			(progn
				(setq as1 (assoc '10 ed))
				(setq ptx1 (cadr as1))
				(setq pty1 (caddr as1))
				(setq ptz1 (cadddr as1))
				(setq oldpt (list '10 ptx1 pty1 ptz1))
				(setq newpt (list '10 ptx1 pty1 '0.0))
				(setq ed (subst newpt oldpt ed))
				(entmod ed)
			)
		)
		(setq count (1+ count))
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End FixZ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin SENDFRONT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:SendFront (/ ss)
	(setvar "cmdecho" 0)
	(setq ss nil)
	(princ "\nSelect objects: \n")
	(setq ss (ssget))
	(if ss
		(progn (command "DRAWORDER" ss "" "FRONT"))
	)
	(setvar "cmdecho" 1)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;end SendFront
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin  SendBack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:SendBack (/ ss)
	(setvar "cmdecho" 0)
	(setq ss nil)
	(princ "\nSelect objects: \n")
	(setq ss (ssget))
	(if ss
		(progn (command "DRAWORDER" ss "" "BACK"))
	)
	(setvar "cmdecho" 1)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;end SendBack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin ATTEDIT_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ATTEDIT_FETZER	(/ e ename elist etype handled)
	(setvar "cmdecho" 0)
	(setq handled nil)
	(setq e (entsel "\n\nSelect an object (block with attributes, multileader, text) to edit: \n"))
	(if e
		(progn
			(setq ename (car e))
			(setq elist (entget ename))
			(setq etype (cdr (assoc 0 elist)))
			(if (or (= etype "MTEXT") (= etype "TEXT") (= etype "MULTILEADER"))
				(progn
					(command "_textedit" ename "")
					(setq handled T)
				)
			)
			(if (= etype "INSERT")
				(progn
					(setq blockname (cdr (assoc 2 elist)))
					(if (and (= (strcase blockname) "DWG_SHEETFIELDS_42X30") (= (strcase (IsLayoutProduction (getvar "CTAB"))) "TRUE") (= (strcase (IsDatabaseAvailable (getvar "CTAB"))) "TRUE") (= (strcase (IsDrawingFetzerXRecordSet)) "TRUE"))
						(progn
							(command "ProductionTitleBlockEditor" ename)
							(setq handled T)
						)
						(progn
							(command "ddatte" ename)
							(setq handled T)
						)
					)
				)
			)
		)
	)
	(if (= handled nil)
		(princ "\nUnknown object - funciton cancelled.\n")
	)
	(setvar "cmdecho" 1)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;end ATTEDIT_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Offset75
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Offset75 (/ Message OldError cmdecho I e_name pt_1)
	(setvar "cmdecho" 0)
	(command "offset" "0.75")
	(setvar "cmdecho" 1)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Offset75
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Breaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Breaks	(/ Message OldError cmdecho I e_name pt_1)
	(setq cmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(setq OldError *error*)
;;;;; Begin error handling
	(defun *error* (ErrorMessage /)
		(if	(/= ErrorMessage "Function cancelled")
			(Alert (strcat "Error: " ErrorMessage))
;(princ (strcat "\nError: " Message))
		)
		(setq *error* OldError)
		(setvar "cmdecho" cmdecho)
		(princ)
  )
;;;;; End error handling
;;;;; Begin variable definition for error handling
;;;;; End variable definition for error handling
;;;;; Begin setting variables
;;;;; End setting variables
	(setq I 0)
	(while (< I 1)
		(setvar "errno" 7)
		(while (= (getvar "errno") 7)
			(setvar "errno" 0)
			(setq e_name (car (entsel "\nSelect object to break: ")))
			(if (= (getvar "errno") 7)
				(prompt "\nNothing selected.")
			)
		)
		(if	e_name
			(progn
				(redraw e_name 3)
				(setq pt_1 (getpoint "\nSelect break point: "))
				(if pt_1
					(progn
						(setq pt_1 (list (car pt_1) (cadr pt_1)))
						(command "break" e_name pt_1 pt_1)
					)
				)
				(redraw e_name 4)
			)
			(progn
				(setq I 1)
			)
		)
		(setq e_name nil)
		(setq pt_1 nil)
	)
;;;;; Begin if there was not an error, restore variables
	(setvar "cmdecho" cmdecho)
;;;;; End if there was not an error, restore variables
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Breaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin AlignCrosshairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:AlignCrosshairs (/ angl angl2 e count ename edata etype centerpoint radius startangle endangle startpt endpt matrix startx starty endx endy origin selpt ename2 edata2 ss display Message)
	(setq cmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(command "undo" "begin")
	(setq OldError *error*)
;;;;; Begin error handling
	(defun *error* (ErrorMessage /)
		(if	(/= ErrorMessage "Function cancelled")
			(Alert (strcat "Error: " ErrorMessage))
;(princ (strcat "\nError: " Message))
		)
		(setq *error* OldError)
		(command "snap" "rotate" "" 0)
		(command "undo" "end")
		(command "u")
		(setvar "cmdecho" cmdecho)
		(princ)
	)
;;;;; End error handling
;;;;; Begin variable definition for error handling
	(setq osmode (getvar "osmode"))
	(setq snapmode (getvar "snapmode"))
	(setq aunits (getvar "aunits"))
;;;;; End variable definition for error handling
;;;;; Begin setting variables
;;;;; End setting variables
	(setq angl nil)
	(setq angl2 nil)
	(princ (strcat "\nThe current crosshair angle is ( " (angtos (getvar "snapang") aunits (getvar "AUPREC")) " ) "))
	(initget 128)
	(setq angl (getangle "\nInput the new crosshair angle, or <Enter> to select an object to align with: "))
	(if (= angl nil)
		(progn
			(setq angl "")
		)
		(progn
			(setq angl (angtos angl 0 16))
		)
	)
	(princ "\n ")
	(if (= angl "")
		(progn
			(setvar "errno" 7)
			(while (= (getvar "errno") 7)
				(setvar "errno" 0)
				(setq e (nentsel "\nSelect an object to align crosshairs with: "))
				(if (= (getvar "errno") 7)
					(progn
						(prompt "\nNothing selected.")
					)
					(progn
						(setq count 0)
						(setq ename (car e))
						(setq edata (entget ename))
						(setq etype (cdr (assoc 0 edata)))
						(if	(= etype "ARC")
							(progn
								(if (> (length e) 2)
									(progn
										(setq centerpoint (cdr (assoc 10 edata)))
										(setq radius (cdr (assoc 40 edata)))
										(setq startangle (cdr (assoc 50 edata)))
										(setq endangle (cdr (assoc 51 edata)))
										(setq startpt (polar centerpoint startangle radius))
										(setq startpt (list 10 (car startpt) (cadr startpt) 0.0))
										(setq endpt (polar centerpoint endangle radius))
										(setq endpt (list 11 (car endpt) (cadr endpt) 0.0))
										(setq matrix (caddr e))
										(setq startx (+ (* (car (nth 0 matrix)) (cadr startpt)) (* (car (nth 1 matrix)) (caddr startpt)) (* (car (nth 2 matrix)) (cadddr startpt)) (car (nth 3 matrix))))
										(setq starty (+ (* (cadr (nth 0 matrix)) (cadr startpt)) (* (cadr (nth 1 matrix)) (caddr startpt)) (* (cadr (nth 2 matrix)) (cadddr startpt)) (cadr (nth 3 matrix))))
										(setq endx (+ (* (car (nth 0 matrix)) (cadr endpt)) (* (car (nth 1 matrix)) (caddr endpt)) (* (car (nth 2 matrix)) (cadddr endpt)) (car (nth 3 matrix))))
										(setq endy (+ (* (cadr (nth 0 matrix)) (cadr endpt)) (* (cadr (nth 1 matrix)) (caddr endpt)) (* (cadr (nth 2 matrix)) (cadddr endpt)) (cadr (nth 3 matrix))))
										(setq startpt (list startx starty 0.0))
										(setq endpt (list endx endy 0.0))
										(setq angl (angtos (angle startpt endpt) 0 16))
										(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
									)
									(progn
										(setq centerpoint (cdr (assoc 10 edata)))
										(setq radius (cdr (assoc 40 edata)))
										(setq startangle (cdr (assoc 50 edata)))
										(setq endangle (cdr (assoc 51 edata)))
										(setq startpt (polar centerpoint startangle radius))
										(setq endpt (polar centerpoint endangle radius))
										(setq angl (angtos (angle startpt endpt) 0 16))
										(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
									)
								)
							)
							(progn
								(setq count (+ count 1))
							)
						)
						(if	(= etype "ELLIPSE")
							(progn
								(if (> (length e) 2)
									(progn
										(setq startpt (assoc 10 edata))
										(setq endpt	(list 11 (+ (cadr startpt) (car (cdr (assoc 11 edata)))) (+ (caddr startpt) (cadr (cdr (assoc 11 edata)))) 0.0))
										(setq matrix (caddr e))
										(setq startx (+ (* (car (nth 0 matrix)) (cadr startpt)) (* (car (nth 1 matrix)) (caddr startpt)) (* (car (nth 2 matrix)) (cadddr startpt)) (car (nth 3 matrix))))
										(setq starty (+ (* (cadr (nth 0 matrix)) (cadr startpt)) (* (cadr (nth 1 matrix)) (caddr startpt)) (* (cadr (nth 2 matrix)) (cadddr startpt)) (cadr (nth 3 matrix))))
										(setq endx (+ (* (car (nth 0 matrix)) (cadr endpt)) (* (car (nth 1 matrix)) (caddr endpt)) (* (car (nth 2 matrix)) (cadddr endpt)) (car (nth 3 matrix))))
										(setq endy (+ (* (cadr (nth 0 matrix)) (cadr endpt)) (* (cadr (nth 1 matrix)) (caddr endpt)) (* (cadr (nth 2 matrix)) (cadddr endpt)) (cadr (nth 3 matrix))))
										(setq startpt (list startx starty 0.0))
										(setq endpt (list endx endy 0.0))
										(setq angl (angtos (angle startpt endpt) 0 16))
										(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
									)
									(progn
										(setq startpt (cdr (assoc 10 edata)))
										(setq endpt	(list (+ (car startpt) (car (cdr (assoc 11 edata)))) (+ (cadr startpt) (cadr (cdr (assoc 11 edata)))) 0.0))
										(setq angl (angtos (angle startpt endpt) 0 16))
										(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
									)
								)
							)
							(progn
								(setq count (+ count 1))
							)
						)
						(if	(= etype "LINE")
							(progn
								(if (> (length e) 2)
									(progn
										(setq startpt (assoc 10 edata))
										(setq endpt (assoc 11 edata))
										(setq matrix (caddr e))
										(setq startx (+ (* (car (nth 0 matrix)) (cadr startpt)) (* (car (nth 1 matrix)) (caddr startpt)) (* (car (nth 2 matrix)) (cadddr startpt)) (car (nth 3 matrix))))
										(setq starty (+ (* (cadr (nth 0 matrix)) (cadr startpt)) (* (cadr (nth 1 matrix)) (caddr startpt)) (* (cadr (nth 2 matrix)) (cadddr startpt)) (cadr (nth 3 matrix))))
										(setq endx (+ (* (car (nth 0 matrix)) (cadr endpt)) (* (car (nth 1 matrix)) (caddr endpt)) (* (car (nth 2 matrix)) (cadddr endpt)) (car (nth 3 matrix))))
										(setq endy (+ (* (cadr (nth 0 matrix)) (cadr endpt)) (* (cadr (nth 1 matrix)) (caddr endpt)) (* (cadr (nth 2 matrix)) (cadddr endpt)) (cadr (nth 3 matrix))))
										(setq startpt (list startx starty 0.0))
										(setq endpt (list endx endy 0.0))
										(setq angl (angtos (angle startpt endpt) 0 16))
										(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
									)
									(progn
										(setq startpt (cdr (assoc 10 edata)))
										(setq endpt (cdr (assoc 11 edata)))
										(setq angl (angtos (angle startpt endpt) 0 16))
										(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
									)
								)
							)
							(progn
								(setq count (+ count 1))
							)
						)
						(if	(= etype "LWPOLYLINE")
							(progn
								(if (> (length e) 2)
									(progn
										(setq count (+ count 1))
									)
									(progn
										(command "-layer" "new" "AlignCrosshairs" "color" "7" "AlignCrosshairs" "")
										(setq origin (list 0.0 0.0 0.0))
										(setq selpt (cadr e))
										(command "copy" ename "" origin origin "")
										(setq ename2 (entlast))
										(command "chprop" ename2 "" "layer" "aligncrosshairs" "")
										(command "explode" ename2 "")
										(setq e (nentselp selpt))
										(setq ename (car e))
										(setq edata (entget ename))
										(setq etype (cdr (assoc 0 edata)))
										(if	(= etype "ARC")
											(progn
												(if (> (length e) 2)
													(progn
														(setq centerpoint (cdr (assoc 10 edata)))
														(setq radius (cdr (assoc 40 edata)))
														(setq startangle (cdr (assoc 50 edata)))
														(setq endangle (cdr (assoc 51 edata)))
														(setq startpt (polar centerpoint startangle radius))
														(setq startpt (list	10 (car startpt) (cadr startpt) 0.0))
														(setq endpt (polar centerpoint endangle radius))
														(setq endpt	(list 11 (car endpt) (cadr endpt) 0.0))
														(setq matrix (caddr e))
														(setq startx (+ (* (car (nth 0 matrix)) (cadr startpt)) (* (car (nth 1 matrix)) (caddr startpt)) (* (car (nth 2 matrix)) (cadddr startpt)) (car (nth 3 matrix))))
														(setq starty (+ (* (cadr (nth 0 matrix)) (cadr startpt)) (* (cadr (nth 1 matrix)) (caddr startpt)) (* (cadr (nth 2 matrix)) (cadddr startpt)) (cadr (nth 3 matrix))))
														(setq endx (+ (* (car (nth 0 matrix)) (cadr endpt)) (* (car (nth 1 matrix)) (caddr endpt)) (* (car (nth 2 matrix)) (cadddr endpt)) (car (nth 3 matrix))))
														(setq endy (+ (* (cadr (nth 0 matrix)) (cadr endpt)) (* (cadr (nth 1 matrix)) (caddr endpt)) (* (cadr (nth 2 matrix)) (cadddr endpt)) (cadr (nth 3 matrix))))
														(setq startpt (list startx starty 0.0))
														(setq endpt (list endx endy 0.0))
														(setq angl (angtos (angle startpt endpt) 0 16))
														(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
													)
													(progn
														(setq centerpoint (cdr (assoc 10 edata)))
														(setq radius (cdr (assoc 40 edata)))
														(setq startangle (cdr (assoc 50 edata)))
														(setq endangle (cdr (assoc 51 edata)))
														(setq startpt (polar centerpoint startangle radius))
														(setq endpt (polar centerpoint endangle radius))
														(setq angl (angtos (angle startpt endpt) 0 16))
														(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
													)
												)
											)
										)
										(if	(= etype "ELLIPSE")
											(progn
												(if (> (length e) 2)
													(progn
														(setq startpt (assoc 10 edata))
														(setq endpt (list 11 (+ (cadr startpt) (car (cdr (assoc 11 edata)))) (+ (caddr startpt) (cadr (cdr (assoc 11 edata)))) 0.0))
														(setq matrix (caddr e))
														(setq startx (+ (* (car (nth 0 matrix)) (cadr startpt)) (* (car (nth 1 matrix)) (caddr startpt)) (* (car (nth 2 matrix)) (cadddr startpt)) (car (nth 3 matrix))))
														(setq starty (+ (* (cadr (nth 0 matrix)) (cadr startpt)) (* (cadr (nth 1 matrix)) (caddr startpt)) (* (cadr (nth 2 matrix)) (cadddr startpt)) (cadr (nth 3 matrix))))
														(setq endx (+ (* (car (nth 0 matrix)) (cadr endpt)) (* (car (nth 1 matrix)) (caddr endpt)) (* (car (nth 2 matrix)) (cadddr endpt)) (car (nth 3 matrix))))
														(setq endy (+ (* (cadr (nth 0 matrix)) (cadr endpt)) (* (cadr (nth 1 matrix)) (caddr endpt)) (* (cadr (nth 2 matrix)) (cadddr endpt)) (cadr (nth 3 matrix))))
														(setq startpt (list startx starty 0.0))
														(setq endpt (list endx endy 0.0))
														(setq angl (angtos (angle startpt endpt) 0 16))
														(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
													)
													(progn
														(setq startpt (cdr (assoc 10 edata)))
														(setq endpt (list (+ (car startpt) (car (cdr (assoc 11 edata)))) (+ (cadr startpt) (cadr (cdr (assoc 11 edata)))) 0.0))
														(setq angl (angtos (angle startpt endpt) 0 16))
														(setq angl2 (angtos (angle startpt endpt) aunits (getvar "AUPREC")))
													)
												)
											)
										)
										(if	(= etype "LINE")
											(progn
												(if (> (length e) 2)
													(progn
														(setq startpt (assoc 10 edata))
														(setq endpt (assoc 11 edata))
														(setq matrix (caddr e))
														(setq startx (+ (* (car (nth 0 matrix)) (cadr startpt)) (* (car (nth 1 matrix)) (caddr startpt)) (* (car (nth 2 matrix)) (cadddr startpt)) (car (nth 3 matrix))))
														(setq starty (+ (* (cadr (nth 0 matrix)) (cadr startpt)) (* (cadr (nth 1 matrix)) (caddr startpt)) (* (cadr (nth 2 matrix)) (cadddr startpt)) (cadr (nth 3 matrix))))
														(setq endx (+ (* (car (nth 0 matrix)) (cadr endpt)) (* (car (nth 1 matrix)) (caddr endpt)) (* (car (nth 2 matrix)) (cadddr endpt)) (car (nth 3 matrix))))
														(setq endy (+ (* (cadr (nth 0 matrix)) (cadr endpt)) (* (cadr (nth 1 matrix)) (caddr endpt)) (* (cadr (nth 2 matrix)) (cadddr endpt)) (cadr (nth 3 matrix))))
														(setq startpt (list startx starty 0.0))
														(setq endpt (list endx endy 0.0))
														(setq angl (angtos (angle startpt endpt) 0 16))
														(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
													)
													(progn
														(setq startpt (cdr (assoc 10 edata)))
														(setq endpt (cdr (assoc 11 edata)))
														(setq angl (angtos (angle startpt endpt) 0 16))
														(setq angl2	(angtos	(angle startpt endpt) aunits (getvar "AUPREC")))
													)
												)
											)
										)
										(setq ss (ssget "X" '((8 . "AlignCrosshairs"))))
										(command "erase" ss "")
										(command "-purge" "layer" "AlignCrosshairs" "n" "")
									)
								)
							)
							(progn
								(setq count (+ count 1))
							)
						)
						(if	(or (= etype "TEXT") (= etype "MTEXT"))
							(progn
								(if (> (length e) 2)
									(progn
										(setq ename2 (car (nth 3 e)))
										(setq edata2 (entget ename2))
										(setq angl (angtos (+ (cdr (assoc 50 edata2)) (cdr (assoc 50 edata))) 0 16))
										(setq angl2	(angtos	(+ (cdr (assoc 50 edata2)) (cdr (assoc 50 edata))) aunits (getvar "AUPREC")))
									)
									(progn
										(setq angl (angtos (cdr (assoc 50 edata)) 0 16))
										(setq angl2	(angtos	(cdr (assoc 50 edata)) aunits (getvar "AUPREC")))
									)
								)
							)
							(progn
								(setq count (+ count 1))
							)
						)
						(if	(< count 5)
							(progn)
							(progn
								(setvar "errno" 7)
								(prompt "\nCannot align crosshairs to selected object.")
							)
						)
					)
				)
			)
		)
	)
	(if (= (vl-string-search "." angl) nil)
		(progn
			(setq angl (strcat angl ".000000000000000000000000"))
		)
		(progn
			(setq angl (strcat angl "000000000000000000000000"))
		)
	)
	(setq angl (substr angl 1 21))
	(if (or (= angl "0.0000000000000000000") (= angl "90.000000000000000000") (= angl "180.00000000000000000") (= angl "270.00000000000000000") (= angl "360.00000000000000000"))
		(progn
			(setq angl "0")
		)
	)
	(if (= nil angl2)
		(progn
			(if (= (type angl) 'STR)
				(progn
					(setq	angl2 (angtos (/ (* (atof angl) pi) 180.0) aunits (getvar "AUPREC")))
				)
			)
		)
	)
	(setq Display (vla-get-display (vla-get-preferences (vla-get-application (vla-get-ActiveDocument (vlax-get-Acad-Object))))))
	(if (= (getvar "tilemode") 1)
		(progn
			(if (= angl "0")
				(progn
					(vlax-put-property display 'modelcrosshaircolor 16777215)
				)
				(progn
					(vlax-put-property display 'modelcrosshaircolor 255)
				)
			)
		)
		(progn
			(if (= angl "0")
				(progn
					(vlax-put-property display 'layoutcrosshaircolor 16777215)
				)
				(progn
					(vlax-put-property display 'layoutcrosshaircolor 255)
				)
			)
		)
	)
	(princ (strcat "\nAligning crosshair to an angle of: " angl2))
	(command "snap" "rotate" "" angl)
;;;;; Begin if there was not an error, restore variables
	(setvar "snapmode" snapmode)
	(setvar "aunits" aunits)
	(command "undo" "end")
	(setvar "cmdecho" cmdecho)
;;;;; End if there was not an error, restore variables
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End AlignCrosshairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin MText_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:MText_Fetzer (/	     Layers	   Layer
		       Layer_On	     Layer_Lock	   Layer_Color
		       Layer_Linetype		   Layer_Freeze
		       Layer_Lineweight		   Layer_Plot
		       cmdecho	     clayer	   osmode
		       pt0	     pt1	   height
		       entitydata    mtext	   Message
		      )
  (if (or (tblsearch "block" "DWG_DrawingSetup_2012")
	  (tblsearch "block" "DWG_DrawingSetup_2012_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2015")
	  (tblsearch "block" "DWG_DrawingSetup_2015_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2019")
	  (tblsearch "block" "DWG_DrawingSetup")
      )
    (progn
      (setq cmdecho (getvar "cmdecho"))
      (command "undo" "begin")
      (setvar "cmdecho" 0)
      (setq OldError *error*)
;;;;; Begin error handling
      (defun *error* (ErrorMessage)
	(if (/= ErrorMessage "Function cancelled")
	  (Alert (strcat "Error: " ErrorMessage))
;(princ (strcat "\nError: " Message))
	)
	(setvar "clayer" clayer)
	(setvar "osmode" osmode)
	(setq Layers (vla-get-layers
		       (vla-get-ActiveDocument (vlax-get-Acad-Object))
		     )
	)
	(setq Layer (vla-item Layers "Note"))
	(vlax-put-property Layer 'LayerOn Layer_On)
	(vlax-put-property Layer 'Lock Layer_Lock)
	(vlax-put-property Layer 'Linetype Layer_Linetype)
	(vlax-put-property Layer 'Lineweight Layer_Lineweight)
	(vlax-put-property Layer 'Plottable Layer_Plot)
	(vlax-put-property Layer 'TrueColor Layer_Color)
	(if (/= (strcase clayer T) (strcase "Note" T))
	  (progn
	    (vlax-put-property Layer 'Freeze Layer_Freeze)
	  )
	  (progn
	  )
	)
	(setq *error* OldError)
	(command "undo" "end")
	(command "u")
	(setvar "cmdecho" cmdecho)
	(princ)
      )
;;;;; End error handling
;;;;; Begin variable definition for error handling
      (setq clayer (getvar "clayer"))
      (setq osmode (getvar "osmode"))
      (setq Layers (vla-get-layers
		     (vla-get-ActiveDocument (vlax-get-Acad-Object))
		   )
      )
      (setq Layer (vla-item Layers "Note"))
      (setq Layer_On (vlax-get-property Layer 'LayerOn))
      (setq Layer_Lock (vlax-get-property Layer 'Lock))
      (setq Layer_Color (vlax-get-property Layer 'TrueColor))
      (setq Layer_Linetype (vlax-get-property Layer 'Linetype))
      (setq Layer_Freeze (vlax-get-property Layer 'Freeze))
      (setq Layer_Lineweight (vlax-get-property Layer 'Lineweight))
      (setq Layer_Plot (vlax-get-property Layer 'Plottable))
;;;;; End variable definition for error handling
;;;;; Begin setting variables
      (setvar "clayer" "note")
      (if (>= (getvar "osmode") 16384)
	(progn
	  (setvar "osmode" (getvar "osmode"))
	)
	(progn
	  (setvar "osmode" (+ (getvar "osmode") 16384))
	)
      )
;;;;; End setting variables
      (setq pt0 (getpoint "\nPick first corner: "))
      (setq pt1 (getcorner pt0 "\nPick opposite corner: "))
      (if (= (getvar "tilemode") 1)
	(progn
	  (setq height (rtos (* (getvar "cannoscalevalue") 0.125) 2 16))
	)
	(progn
	  (setq height "0.125")
	)
      )
      (if (> (car pt0) (car pt1))
	(progn
	  (if (> (cadr pt0) (cadr pt1))
	    (progn
	      (command "mtext" pt0 "justify" "tr" "height" height pt1
		       "." "")
	    )
	    (progn
	      (command "mtext" pt0 "justify" "br" "height" height pt1
		       "." "")
	    )
	  )
	)
	(progn
	  (if (> (cadr pt0) (cadr pt1))
	    (progn
	      (command "mtext" pt0 "justify" "tl" "height" height pt1
		       "." "")
	    )
	    (progn
	      (command "mtext" pt0 "justify" "bl" "height" height pt1
		       "." "")
	    )
	  )
	)
      )
      (setq mtext (entlast))
      (setq entitydata (entget mtext))
      (setq entitydata
	     (subst (cons 1 "") (assoc 1 entitydata) entitydata)
      )
      (entmod entitydata)
      (command "TEXTEDIT" mtext "")
      (if (= (cdr (assoc 1 (entget mtext))) "")
	(progn
	  (command "erase" mtext "")
	)
	(progn
	)
      )
;;;;; Begin if there was not an error, restore variables
      (setvar "clayer" clayer)
      (setvar "osmode" osmode)
      (setq Layers (vla-get-layers
		     (vla-get-ActiveDocument (vlax-get-Acad-Object))
		   )
      )
      (setq Layer (vla-item Layers "Note"))
      (vlax-put-property Layer 'LayerOn Layer_On)
      (vlax-put-property Layer 'Lock Layer_Lock)
      (vlax-put-property Layer 'Linetype Layer_Linetype)
      (vlax-put-property Layer 'Lineweight Layer_Lineweight)
      (vlax-put-property Layer 'Plottable Layer_Plot)
      (vlax-put-property Layer 'TrueColor Layer_Color)
      (if (/= (strcase clayer T) (strcase "Note" T))
	(progn
	  (vlax-put-property Layer 'Freeze Layer_Freeze)
	)
	(progn
	)
      )
      (command "undo" "end")
      (setvar "cmdecho" cmdecho)
;;;;; End if there was not an error, restore variables
      (gc)
    )
    (progn
      (Alert
	"This command can only be called from a Fetzer certified drawing."
      )
    )
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End MText_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin MLeader_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:MLeader_Fetzer	(/	       CMDECHO	     OldError
			 CLAYER	       OSMODE	     CMLEADERSTYLE
			 Layers	       Layer	     Layer_On
			 Layer_Lock    Layer_Linetype
			 Layer_Lineweight	     Layer_Plot
			 Layer_Color   Layer_Freeze  mtext
			 pt0	       pt1	     height
			 entitydata    mleader
			)
  (if (or (tblsearch "block" "DWG_DrawingSetup_2012")
	  (tblsearch "block" "DWG_DrawingSetup_2012_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2015")
	  (tblsearch "block" "DWG_DrawingSetup_2015_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2019")
	  (tblsearch "block" "DWG_DrawingSetup")
      )
    (progn
      (setq CMDECHO (getvar "cmdecho"))
;(command "undo" "begin")
      (setvar "cmdecho" 0)
      (setq OldError *error*)
;Begin error handling
      (defun *error* (ErrorMessage /)
	(if (/= ErrorMessage "Function cancelled")
	  (progn
	    (Alert (strcat "Error: " ErrorMessage))
	  )
	  (progn
	  )
	)
	(setvar "CLAYER" CLAYER)
	(setvar "OSMODE" OSMODE)
	(setvar "CMLEADERSTYLE" CMLEADERSTYLE)
	(setq Layers (vla-get-layers
		       (vla-get-ActiveDocument (vlax-get-Acad-Object))
		     )
	)
	(setq Layer (vla-item Layers "Note"))
	(vlax-put-property Layer 'LayerOn Layer_On)
	(vlax-put-property Layer 'Lock Layer_Lock)
	(vlax-put-property Layer 'Linetype Layer_Linetype)
	(vlax-put-property Layer 'Lineweight Layer_Lineweight)
	(vlax-put-property Layer 'Plottable Layer_Plot)
	(vlax-put-property Layer 'TrueColor Layer_Color)
	(if (/= (strcase clayer T) (strcase "Note" T))
	  (progn
	    (vlax-put-property Layer 'Freeze Layer_Freeze)
	  )
	  (progn
	  )
	)
	(setq *error* OldError)
;(command "undo" "end")
;(command "u")
	(setvar "CMDECHO" CMDECHO)
	(princ)
      )
;End error handling
;Begin variable definition for error handling
      (setq CLAYER (getvar "CLAYER"))
      (setq OSMODE (getvar "OSMODE"))
      (setq CMLEADERSTYLE (getvar "CMLEADERSTYLE"))
      (setq Layers (vla-get-layers
		     (vla-get-ActiveDocument (vlax-get-Acad-Object))
		   )
      )
      (setq Layer (vla-item Layers "Note"))
      (setq Layer_On (vlax-get-property Layer 'LayerOn))
      (setq Layer_Lock (vlax-get-property Layer 'Lock))
      (setq Layer_Color (vlax-get-property Layer 'TrueColor))
      (setq Layer_Linetype (vlax-get-property Layer 'Linetype))
      (setq Layer_Freeze (vlax-get-property Layer 'Freeze))
      (setq Layer_Lineweight (vlax-get-property Layer 'Lineweight))
      (setq Layer_Plot (vlax-get-property Layer 'Plottable))
;End variable definition for error handling
;Begin setting variables
      (command "-layer"	"on" "note" "thaw" "note" "unlock" "note" "")
      (setvar "CLAYER" "Note")
      (setvar "CMLEADERSTYLE" "Fetzer -- Note")
      (if (>= (getvar "osmode") 16384)
	(progn
	  (setvar "osmode" (getvar "osmode"))
	)
	(progn
	  (setvar "osmode" (+ (getvar "osmode") 16384))
	)
      )
;End setting variables
      (setq pt0 (getpoint "\nPick first corner: "))
      (setq pt1 (getcorner pt0 "\nPick opposite corner: "))
      (if (= (getvar "tilemode") 1)
	(progn
	  (setq height (rtos (* (getvar "cannoscalevalue") 0.125) 2 16))
	)
	(progn
	  (setq height "0.125")
	)
      )
      (if (> (car pt0) (car pt1))
	(progn
	  (if (> (cadr pt0) (cadr pt1))
	    (progn
	      (command "mtext" pt0 "justify" "tr" "height" height pt1
		       "." "")
	    )
	    (progn
	      (command "mtext" pt0 "justify" "br" "height" height pt1
		       "." "")
	    )
	  )
	)
	(progn
	  (if (> (cadr pt0) (cadr pt1))
	    (progn
	      (command "mtext" pt0 "justify" "tl" "height" height pt1
		       "." "")
	    )
	    (progn
	      (command "mtext" pt0 "justify" "bl" "height" height pt1
		       "." "")
	    )
	  )
	)
      )
      (setq mtext (entlast))
      (command "chprop" mtext "" "layer" "note" "")
      (setq entitydata (entget mtext))
      (setq entitydata
	     (subst (cons 1 "") (assoc 1 entitydata) entitydata)
      )
      (entmod entitydata)
      (command "TEXTEDIT" mtext "")
      (if (= (cdr (assoc 1 (entget mtext))) "")
	(progn
	  (command "erase" mtext "")
	)
	(progn
	  (setq text (cdr (assoc 1 (entget mtext))))
	  (command "mleader" "l" "c" pt0 pt1 text pause)
	  (setq mleader (entlast))
	  (if (= (cdr (assoc 0 (entget mleader))) "MULTILEADER")
	    (progn
	      (command "chprop" mleader "" "layer" "note" "")
	      (command "erase" mtext "")
	    )
	  )
	)
      )
;Begin if there was not an error, restore variables
      (setvar "CLAYER" CLAYER)
      (setvar "OSMODE" OSMODE)
      (setvar "CMLEADERSTYLE" CMLEADERSTYLE)
      (setq Layers (vla-get-layers
		     (vla-get-ActiveDocument (vlax-get-Acad-Object))
		   )
      )
      (setq Layer (vla-item Layers "Note"))
      (vlax-put-property Layer 'LayerOn Layer_On)
      (vlax-put-property Layer 'Lock Layer_Lock)
      (vlax-put-property Layer 'Linetype Layer_Linetype)
      (vlax-put-property Layer 'Lineweight Layer_Lineweight)
      (vlax-put-property Layer 'Plottable Layer_Plot)
      (vlax-put-property Layer 'TrueColor Layer_Color)
      (if (/= (strcase clayer T) (strcase "Note" T))
	(progn
	  (vlax-put-property Layer 'Freeze Layer_Freeze)
	)
	(progn
	)
      )
;(command "undo" "end")
      (setvar "cmdecho" CMDECHO)
;End if there was not an error, restore variables
    )
    (progn
      (Alert
	"This command can only be called from a Fetzer certified drawing."
      )
    )
  )
  (command "mleaderedit" mleader)
  (princ)
  (gc)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End MLeader_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin OffsetUsingLastOffsetDist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:OffsetUsingLastOffsetDist (/ offsetdist)
  (setq offsetdist (getvar "offsetdist"))
  (command "offset" offsetdist)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End OffsetUsingLastOffsetDist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Offset34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Offset34 (/ offsetdist)
  (command "offset" "0.75")
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Offset34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Offset50
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Offset34 (/ offsetdist)
  (command "offset" "0.50")
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Offset50
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Offset25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Offset25 (/ offsetdist)
  (command "offset" "0.25")
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Offset25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin ReversePolyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ReversePolyline
			   (/	   olderr cmde	 blip	ltsc   cclr
			    snap   pwid	  pgenen1	nam    ent
			    p	   obj	  ltp	 clr	lts    wid
			    flgs   first  final	 next	spl    cur
			    vert   a	  clos	 zoomit	clyr   lyr
			   )
  (setq olderr *error*)
  (defun
	    *error*
		   (ErrorMessage /)
    (setvar "cmdecho" cmde)
    (setvar "blipmode" blip)
    (setvar "osmode" snap)
    (setvar "celtscale" ltsc)
    (setvar "cecolor" cclr)
    (setvar "plinewid" pwid)
    (setvar "plinegen" pgen)
    (setq *error* olderr)
    (princ)
  )
;;;;; end of *error* function
  (setq	cmde (getvar "cmdecho")
	blip (getvar "blipmode")
	ltsc (getvar "celtscale")
	cclr (getvar "cecolor")
	snap (getvar "osmode")
	pwid (getvar "plinewid")
	clyr (getvar "clayer")
	pgen (getvar "plinegen")
  )
  (setvar "cmdecho" 0)
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (setvar "plinewid" 0)
  (setvar "plinegen" 1)
  (command "_.undo" "_be")
  (while (null (setq en1 (entsel "\nPick an object to reverse: ")))
  )
  (setq	nam (car en1)
	ent (entget nam)
	p   (cadr en1)
	obj (cdr (assoc 0 ent))
  )
  (cond	((= obj "CIRCLE")
	 (setq ctr (cdr (assoc 10 ent))
	       dia (* 2.0 (cdr (assoc 40 ent)))
	       a   (angle p ctr)
	 )
	 (command "_.break"
		  p
		  (polar p (/ pi 4) 0.001)
		  "_.pedit"
		  p
		  "_y"
		  "_c"
		  "_x"
	 )
	 (carc)
	)
	((= obj "ARC")
	 (command "_.break"
		  p
		  "@"
		  "_.pedit"
		  p
		  "_y"
		  "_j"
		  nam
		  (entlast)
		  ""
		  "_x"
	 )
	 (carc)
	)
	(T nil)
  )
  (setq	ltp  (cdr (assoc 6 ent))
	lyr  (cdr (assoc 8 ent))
	clr  (cdr (assoc 62 ent))
	lts  (cdr (assoc 48 ent))
	wid  (cdr (assoc 40 ent))
	flgs (cdr (assoc 70 ent))
  )
  (if (not ltp)
    (setq ltp "bylayer")
  )
  (cond	((= obj "LINE")
	 (setq first (assoc 10 ent)
	       final (assoc 11 ent)
	       ent   (subst (cons 10 (cdr final)) first ent)
	       ent   (subst (cons 11 (cdr first)) final ent)
	 )
	 (entmod ent)
	)
	((= obj "LWPOLYLINE")
	 (setq final (cdr (assoc 10 (setq ent (reverse ent))))
	       next  (cdr (assoc 10 (cdr (member (assoc 10 ent) ent))))
	 )
	 (prev)
	)
	((= obj "POLYLINE")
	 (setq spl  (= (logand flgs 4) 4)
	       cur  (= (logand flgs 2) 2)
	       vert (entnext nam)
	 )
	 (if cur
	   (command "_.pedit" p "_s" "")
	 )
	 (while	(= (cdr (assoc 0 (entget (setq vert (entnext vert)))))
		   "VERTEX"
		)
	   (setq next  final
		 final (cdr (assoc 10 (entget vert)))
	   )
	 )
	 (prev)
	)
	(T (alert "Not a Reversible object."))
  )
  (command "_.undo" "_e")
  (setvar "cmdecho" cmde)
  (setvar "blipmode" blip)
  (setvar "osmode" snap)
  (setvar "celtscale" ltsc)
  (setvar "cecolor" cclr)
  (setvar "plinewid" pwid)
  (setvar "plinegen" pgen)
  (setvar "clayer" clyr)
  (setq *error* olderr)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun carc
	      ()
  (setq	ent (entget (entlast))
	nam (cdr (assoc -1 ent))
	obj (cdr (assoc 0 ent))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prev
	      ()
  (setq	a    (angle next final)
	clos (= (logand flgs 1) 1)
  )
  (if clos
    (command "_.pedit" nam "_o" "")
  )
  (setq zoomit (null (ssget "_c" final final)))
  (if zoomit
    (command "_.zoom" "_c" final "")
  )
  (if clr
    (command "_.color" clr)
  )
  (if lts
    (setvar "celtscale" lts)
  )
  (setvar "clayer" lyr)
  (command
    "_.pline"
    (polar final a 0.0001)
    final
    ""
    "_.chprop"
    (entlast)
    ""
    "_lt"
    ltp
    ""
    "_.pedit"
    (entlast)
    "_j"
    nam
    ""
    ""
    "_.break"
    final
    (polar final a 0.001)
  )
  (if cur
    (command "_.pedit" (entlast) "_f" "")
  )
  (if spl
    (command "_.pedit" (entlast) "_s" "")
  )
  (if clos
    (command "_.pedit" (entlast) "_c" "")
  )
  (if wid
    (command "_.pedit" (entlast) "_w" wid "")
  )
  (if zoomit
    (command "_.zoom" "_p")
  )
)
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End ReversePolyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin CRay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:CRay (/ i pt)
  (setvar "cmdecho" 0)
  (setq i 0)
  (while (< i 1)
    (setq
      pt (getpoint "\nSpecify ray start point or [ESQ] to quit: \n")
    )
    (command "ray" pt pause "")
    (command "chprop"	  "last"       ""	    "layer"
	     "XPlot"	  "color"      "91"	    "ltype"
	     "Fetzer -- Phantom"       ""
	    )
  )
  (setvar "cmdecho" 1)
  (gc)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End CRay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:DrawerBox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DrawerBox (/ pt0 pt1 pt2 pt3 pt4 pt5 width height)
  (if (or (tblsearch "block" "DWG_DrawingSetup_2012")
	  (tblsearch "block" "DWG_DrawingSetup_2012_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2015")
	  (tblsearch "block" "DWG_DrawingSetup_2015_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2019")
	  (tblsearch "block" "DWG_DrawingSetup")
      )
    (progn
      (if (>= (getvar "osmode") 16384)
	(progn
	  (setq	variable_list
		 (list (list "cmdecho"
			     0
			     "osmode"
			     (getvar "osmode")
			     "clayer"
			     "part"
			     "attdia"
			     0
		       )
		       T
		 )
	  )
	)
	(progn
	  (setq	variable_list
		 (list (list "cmdecho"
			     0
			     "osmode"
			     (+ (getvar "osmode") 16384)
			     "clayer"
			     "part"
			     "attdia"
			     0
		       )
		       T
		 )
	  )
	)
      )
      (environment variable_list)
      (setq pt0 (getpoint "\nPick lower right corner. "))
      (setq width (getreal "\nEnter drawer box width/length: "))
      (while (< width 1.25)
	(setq width
	       (getreal
		 "\nThe wdith/length entered is to narrow. \nEnter drawer box width/length: "
	       )
	)
      )
      (setq height (getreal "\nEnter drawer box height: "))
      (while (< height 1.0)
	(setq height
	       (getreal
		 "\nThe height entered is to short. \nEnter drawer box height: "
	       )
	)
      )
      (setq pt1 (polar pt0 (* pi 0.5) height))
      (setq pt2 (polar pt1 pi 0.5))
      (setq pt3 (polar pt2 (* pi 1.5) (- height 0.75)))
      (setq pt4 (polar pt3 (* pi 1.5) 0.25))
      (setq pt5 (polar pt4 (* pi 1.5) 0.5))
      (command "line" pt0 pt1 pt2 pt3 "")
      (command "line" pt4 pt5 pt0 "")
      (command "-layer" "set" "part -- d" "")
      (setq pt0 pt2)
      (setq pt1 (polar pt0 pi (- width 1.0)))
      (command "line" pt0 pt1 "")
      (setq pt0 (polar pt1 (* pi 1.5) height))
      (setq pt1 (polar pt0 0.0 (- width 1.0)))
      (command "line" pt0 pt1 "")
      (command "-layer" "set" "part" "")
      (setq pt0 (polar pt1 0.0 0.25))
      (setq pt0 (polar pt0 (* pi 0.5) 0.5))
      (setq pt1 (polar pt0 (* pi 0.5) 0.25))
      (setq pt2 (polar pt1 pi (- width 0.5)))
      (setq pt3 (polar pt2 (* pi 1.5) 0.25))
      (command "line" pt0 pt1 pt2 pt3 pt0 "")
      (setq pt0 (polar pt0 (* pi 1.5) 0.5))
      (setq pt0 (polar pt0 pi (- width 0.25)))
      (setq pt1 (polar pt0 (* pi 0.5) height))
      (setq pt2 (polar pt1 0.0 0.5))
      (setq pt3 (polar pt2 (* pi 1.5) (- height 0.75)))
      (setq pt4 (polar pt3 (* pi 1.5) 0.25))
      (setq pt5 (polar pt4 (* pi 1.5) 0.5))
      (command "line" pt0 pt1 pt2 pt3 "")
      (command "line" pt4 pt5 pt0 "")
      (gc)
      (restore variable_list)
    )
    (progn
      (Alert
	"This command can only be called from a Fetzer certified drawing."
      )
    )
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:DrawerBox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Frame (/ maxextents minextents ss i e_name e_datalist e_datatype e e_minextents e_maxextents dimension measurement r associative dimclrt)
	(if (not (tblsearch "layer" "cnc"))
		(command "_.layer" "_thaw" "cnc" "_make" "cnc" "_color" 80 "" "")
	)
	(if (>= (getvar "osmode") 16384)
		(progn
			(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "cnc")))
		)
		(progn
			(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "cnc")))
		)
	)
	(environment variable_list)
	(command "-layer" "on" "dimension" "")
	(setq maxextents (list -1E50 -1E50 0.0))
	(setq minextents (list 1E50 1E50 0.0))
	(setq ss nil)
	(if (ssget "_I");implied-selection
		(progn
			(setq ss (ssget))
		)
		(progn
			(princ "\nSelect objects to frame: \n")
			(setq ss (ssget))
		)
	)
	(if ss
		(progn
			(setq I 0)
			(while (< I (sslength ss))
				(setq e_name (ssname ss I))
				(setq e_datalist (entget e_name))
				(setq e_datatype (cdr (assoc 0 e_datalist)))
				(if	(or (= e_datatype "ARC")
						(= e_datatype "CIRCLE")
						(= e_datatype "ELLIPSE")
						(= e_datatype "INSERT")
						(= e_datatype "LINE")
						(= e_datatype "LWPOLYLINE")
						(= e_datatype "POINT")
						(= e_datatype "POLYLINE")
						(= e_datatype "SPLINE")
					)
					(progn
						(setq e (vlax-ename->vla-object e_name))
						(vla-getboundingbox e 'e_minextents 'e_maxextents)
						(setq e_minextents (vlax-safearray->list e_minextents))
						(setq e_maxextents (vlax-safearray->list e_maxextents))
						(if (< (car e_minextents) (car minextents))
							(setq minextents (list (car e_minextents) (cadr minextents) 0.0))
						)
						(if (< (cadr e_minextents) (cadr minextents))
							(setq	minextents (list (car minextents) (cadr e_minextents) 0.0))
						)
						(if (> (car e_maxextents) (car maxextents))
							(setq	maxextents (list (car e_maxextents) (cadr maxextents) 0.0))
						)
						(if (> (cadr e_maxextents) (cadr maxextents))
							(setq	maxextents (list (car maxextents) (cadr e_maxextents) 0.0))
						)
					)
				)
				(setq I (+ I 1))
			)
			(if (and minextents maxextents)
				(progn
					(command "zoom" "window" maxextents minextents)
					(command "zoom" "0.5x")
					(setq minextents (polar minextents (/ (* 5 PI) 4) (sqrt (+ (expt 0.125 2) (expt 0.125 2)))))
					(setq maxextents (polar maxextents (/ PI 4) (sqrt (+ (expt 0.125 2) (expt 0.125 2)))))
					(command "rectangle" minextents maxextents)
					(setvar "osmode" 1)
					(command "dimlinear" maxextents (list (car minextents) (cadr maxextents) 0.0) (polar maxextents (/ PI 2) 6.0))
					(setq e_name (entlast))
					(setq e_datalist (entget e_name))
					(setq dimension (vlax-ename->vla-object e_name))
					(vlax-put-property dimension 'Color acByLayer)
					(vlax-put-property dimension 'Layer "DIMENSION")
					(vlax-put-property dimension 'Linetype "BYLAYER")
					(vlax-put-property dimension 'Lineweight acLnWtByLayer)
					(setq measurement (atof (rtos (cdr (assoc 42 e_datalist)) 2 6)))
					;(setq r (rem measurement 0.015625))
					;(if (> r 1.0E-8)
					;	(progn
					;		(command "dim" "update" e_name "" "exit")
					;		(setq dimclrt (getvar "dimclrt"))
					;		(command "dimrnd" "1/64")
					;		(command "dimclrt" "253")
					;		(command "dim" "up" e_name "" "exit")
					;		(setvar "dimclrt" dimclrt)
					;		(if (>= measurement 144.0)
					;			(command "dimoverride" "dimalt" "on" "dimaltd" "6" "" e_name "")
					;		)
					;	)
					;	(progn
					;		(command "dim" "update" e_name "" "exit")
					;		(if (>= measurement 144.0)
					;			(command "dimoverride" "dimalt" "on" "dimaltd" "6" "" e_name "")
					;		)
					;	)
					;)
					(setq associative (cdr (assoc 102 e_datalist)))
					(if (= associative nil)
						(command "chprop" e_name "" "color" "160" "")
					)
					(command "dimlinear" minextents (list (car minextents) (cadr maxextents) 0.0) (polar minextents PI 6.0))
					(setq e_name (entlast))
					(setq e_datalist (entget e_name))
					(setq dimension (vlax-ename->vla-object e_name))
					(vlax-put-property dimension 'Color acByLayer)
					(vlax-put-property dimension 'Layer "DIMENSION")
					(vlax-put-property dimension 'Linetype "BYLAYER")
					(vlax-put-property dimension 'Lineweight acLnWtByLayer)
					(setq measurement (atof (rtos (cdr (assoc 42 e_datalist)) 2 6)))
					;(setq r (rem measurement 0.015625))
					;(if (> r 1.0E-8)
					;	(progn
					;		(command "dim" "update" e_name "" "exit")
					;		(setq dimclrt (getvar "dimclrt"))
					;		(command "dimrnd" "1/64")
					;		(command "dimclrt" "253")
					;		(command "dim" "up" e_name "" "exit")
					;		(setvar "dimclrt" dimclrt)
					;		(if (>= measurement 144.0)
					;			(command "dimoverride" "dimalt" "on" "dimaltd" "6" "" e_name "")
					;		)
					;	)
					;	(progn
					;		(command "dim" "update" e_name "" "exit")
					;		(if (>= measurement 144.0)
					;			(command "dimoverride" "dimalt" "on" "dimaltd" "6" "" e_name "")
					;		)
					;	)
					;)
					(setq associative (cdr (assoc 102 e_datalist)))
					(if (= associative nil)
						(command "chprop" e_name "" "color" "160" "")
					)
				)
			)
		)
	)
	(setq ss nil)
	(command "dimstyle" "restore" "Fetzer")
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Holes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Holes (/ holediameter holespacing continue osmode pt00 pt01 pt02 direction01 direction02 ceiling doorcolumnx frontcolumnx backcolumnx columny sret shelf shelfcount shelfspace proposedy counter01 counter02 counter03 y y+1 startpoint endpoint)
  (if (or (tblsearch "block" "DWG_DrawingSetup_2012")
	  (tblsearch "block" "DWG_DrawingSetup_2012_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2015")
	  (tblsearch "block" "DWG_DrawingSetup_2015_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2019")
	  (tblsearch "block" "DWG_DrawingSetup")
	  )
	(progn
	  (if (>= (getvar "osmode") 16384)
		(progn
		(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "Part -- D" "attdia" 0) nil))
		)
		(progn
		(setq variable_list (list (list "cmdecho" 0 "osmode" (+ (getvar "osmode") 16384) "clayer" "Part -- D" "attdia" 0) nil))
		)
      )
      (environment variable_list)
      (setq holediameter (/ 5.0 25.4))
      (setq holespacing (/ 32.0 25.4))
      (setq continue T)
      (while (= continue T)
	(setq osmode (getvar "osmode"))
	(setq pt00 nil)
	(setq pt01 nil)
	(setq pt02 nil)
	(setq direction01 nil)
	(setq direction02 nil)
	(setq ceiling nil)
	(setq doorcolumnx nil)
	(setq frontcolumnx nil)
	(setq backcolumnx nil)
	(setq columny nil)
	(princ
	  "\nThink about shelf alignment with adjacent fixtures."
	)
	(initget "Shelf Door eXit")
	(setq sret (getkword "\n[Shelf/Door/eXit] <eXit>: "))
	(if (or (= sret "eXit") (= sret nil))
	  (progn
	    (setq continue nil)
	  )
	  (progn
	  )
	)
	(if (= continue T)
	  (progn
	    (if	(= (strcase sret T) "shelf")
	      (progn
		(setvar "osmode" 1)
		(princ
		  "\nProvide inside fixture clear opening for adjustable shelf holes."
		)
		(setq pt01
		       (getpoint
			 "\nPick the clear inside TOP and FRONT corner of the FIXTURE:"
		       )
		)
		(setq pt02
		       (getcorner
			 pt01
			 "\nPick the clear inside BOTTOM and BACK corner of the FIXTURE:"
		       )
		)
		(if (> (cadr pt02) (cadr pt01))
		  (progn
		    (setq pt00 pt02)
		    (setq pt02 pt01)
		    (setq pt01 pt00)
		  )
		  (progn
		  )
		)
		(if (> (car pt01) (car pt02))
		  (progn
		    (setq direction01 pi)
		    (setq direction02 0.0)
		  )
		  (progn
		    (setq direction01 0.0)
		    (setq direction02 pi)
		  )
		)
		(command "zoom"
			 "window"
			 (list (- (min (car pt01) (car pt02)) 3.0)
			       (+ (max (cadr pt01) (cadr pt02)) 3.0)
			       0.0
			 )
			 (list (+ (max (car pt01) (car pt02)) 3.0)
			       (- (min (cadr pt01) (cadr pt02)) 3.0)
			       0.0
			 )
		)
		(setvar "osmode" osmode)
		(setq ceiling (- (cadr pt01) 4.28125))
		(setq frontcolumnx (car (polar pt01 direction01 2.0)))
		(setq backcolumnx (car (polar pt02 direction02 1.5)))
		(setq columny (+ (cadr pt02) 4.28125))
		(while (< columny ceiling)
		  (command "circle"
			   (list backcolumnx columny 0.0)
			   "diameter"
			   holediameter
		  )
		  (command "circle"
			   (list frontcolumnx columny 0.0)
			   "diameter"
			   holediameter
		  )
		  (setq columny (+ columny holespacing))
		)
		(initget "Yes No")
		(setq shelf (getkword "\nDraw shelf? [Yes/No]: <No> "))
		(if (or (= shelf "No") (= shelf nil))
		  (progn
		    (setq shelf "No")
		  )
		  (progn
		  )
		)
		(if (= shelf "Yes")
		  (progn
		    (initget)
		    (setq shelfcount
			   (getdist
			     "\nSpecify the number of shelves: <1> "
			   )
		    )
		    (if	(= shelfcount nil)
		      (progn
			(setq shelfcount 1)
		      )
		      (progn
			(if (= (type shelfcount) 'REAL)
			  (progn
			    (setq shelfcount
				   (atoi
				     (rtos (fix shelfcount) 2 4)
				   )
			    )
			  )
			  (progn
			  )
			)
		      )
		    )
		    (if	(> shelfcount 0)
		      (progn
			(setq shelfspace
			       (/
				 (-
				   (- (max (cadr pt01) (cadr pt02))
				      (min (cadr pt01) (cadr pt02))
				   )
				   (* shelfcount 0.75)
				 )
				 (+ shelfcount 1)
			       )
			)
			(setq counter01 0)
			(while (< counter01 shelfcount)
			  (setq counter02 0)
			  (setq counter03 0)
			  (while (< counter02 1)
			    (setq proposedy (+ (+ (cadr pt02)
						  (* shelfspace
						     (+ counter01 1)
						  )
					       )
					       (* counter01 0.75)
					    )
			    )
			    (setq y (+ (+ (cadr pt02) 4.28125)
				       (* counter03 holespacing)
				    )
			    )
			    (setq
			      y+1 (+ (+ (cadr pt02) 4.28125)
				     (* (+ counter03 1) holespacing)
				  )
			    )
			    (if	(< (abs (- proposedy y))
				   (abs (- proposedy y+1))
				)
			      (progn
				(setq counter02 (+ counter02 1))
				(setq startpoint
				       (list
					 (car pt02)
					 (+ y
					    (/ holediameter 2.0)
					 )
					 0.0
				       )
				)
				(setq
				  endpoint (polar startpoint
						  direction02
						  (- (max (car pt01)
							  (car pt02)
						     )
						     (min (car pt01)
							  (car pt02)
						     )
						     0.25
						  )
					   )
				)
				(command "line" startpoint endpoint "")
				(command "chprop"
					 (entlast)
					 ""
					 "layer"
					 "part"
					 ""
				)
				(setq startpoint endpoint)
				(setq endpoint (polar startpoint
						      (/ pi 2.0)
						      0.75
					       )
				)
				(command "line" startpoint endpoint "")
				(command "chprop"
					 (entlast)
					 ""
					 "layer"
					 "part"
					 ""
				)
				(setq startpoint endpoint)
				(setq
				  endpoint (polar startpoint
						  direction01
						  (- (max (car pt01)
							  (car pt02)
						     )
						     (min (car pt01)
							  (car pt02)
						     )
						     0.25
						  )
					   )
				)
				(command "line" startpoint endpoint "")
				(command "chprop"
					 (entlast)
					 ""
					 "layer"
					 "part"
					 ""
				)
			      )
			      (progn
			      )
			    )
			    (setq counter03 (+ counter03 1))
			  )
			  (setq counter01 (+ counter01 1))
			)
		      )
		      (progn
		      )
		    )
		  )
		  (progn
		  )
		)
	      )
	      (progn
	      )
	    )
	    (if	(= (strcase sret T) "door")
	      (progn
		(setvar "osmode" 1)
		(princ "\nProvide the door bounds.")
		(setq pt01
		       (getpoint
			 "\nPick the TOP and FRONT corner of the DOOR:"
		       )
		)
		(setq pt02
		       (getcorner
			 pt01
			 "\nPick the BOTTOM and BACK corner of the DOOR:"
		       )
		)
		(if (> (cadr pt02) (cadr pt01))
		  (progn
		    (setq pt00 pt02)
		    (setq pt02 pt01)
		    (setq pt01 pt00)
		  )
		  (progn
		  )
		)
		(if (> (car pt01) (car pt02))
		  (progn
		    (setq direction01 pi)
		    (setq direction02 0.0)
		  )
		  (progn
		    (setq direction01 0.0)
		    (setq direction02 pi)
		  )
		)
		(command "zoom"
			 "window"
			 (list (- (min (car pt01) (car pt02)) 3.0)
			       (+ (max (cadr pt01) (cadr pt02)) 3.0)
			       0.0
			 )
			 (list (+ (max (car pt01) (car pt02)) 3.0)
			       (- (min (cadr pt01) (cadr pt02)) 3.0)
			       0.0
			 )
		)
		(setvar "osmode" osmode)
		(initget "Yes No")
		(setq inset
		       (getkword "\nIs the door inset? [Yes/No]: <No> "
		       )
		)
		(if (or (= inset "No") (= inset nil))
		  (progn
		    (setq inset "No")
		  )
		  (progn
		  )
		)
		(if (= inset "Yes")
		  (progn
		    (setq hingecolumnx
			   (car	(polar pt02
				       direction01
				       (/ 38.5 25.4)
				)
			   )
		    )
		    (setq hinge "hin_95-120InsetElevation.dwg")
		  )
		  (progn
		    (setq hingecolumnx
			   (car	(polar pt02
				       direction01
				       (+ (/ 37.0 25.4) 0.125)
				)
			   )
		    )
		    (setq hinge "hin_95-120FullOverlayElevation.dwg")
		  )
		)
		(setq long (abs	(- (max (cadr pt01) (cadr pt02))
				   (min (cadr pt01) (cadr pt02))
				)
			   )
		)
		(setq thick (abs (- (max (car pt01) (car pt02))
				    (min (car pt01) (car pt02))
				 )
			    )
		)
		(setq hingecount 1)
		(while (< hingecount 2)
		  (princ (strcat "\nDoor dimensions: "
				 (rtos long 2 4)
				 "\" long, and "
				 (rtos thick 2 4)
				 "\" thick."
			 )
		  )
		  (initget)
		  (setq	hingecount
			 (getdist
			   "\nSpecify the number of hinges: <2> "
			 )
		  )
		  (if (= hingecount nil)
		    (progn
		      (setq hingecount 2)
		    )
		    (progn
		      (if (= (type hingecount) 'REAL)
			(progn
			  (setq	hingecount
				 (atoi (rtos (fix hingecount) 2 4)
				 )
			  )
			)
			(progn
			)
		      )
		      (if (< hingecount 2)
			(progn
			  (princ
			    "Total number of hinges must be equal to, or greater than, 2."
			  )
			)
			(progn
			)
		      )
		    )
		  )
		)
;(setq hingecolumny (+ (cadr pt02) (/ 80.0 25.4)))
		(setq hingecolumny (+ (cadr pt02) (/ 80.0 25.4)))
		(setq counter01 0)
		(while (< counter01 hingecount)
;(setq hingecolumny (+ (+ (cadr pt02) (/ 80.0 25.4)) (* counter01 (/ (- long (/ 160.0 25.4)) (- hingecount 1)))))
		  (setq	hingecolumny
			 (+ (+ (cadr pt02) (/ 80.0 25.4))
			    (* counter01
			       (/ (- long (/ 160.0 25.4)) (- hingecount 1))
			    )
			 )
		  )
		  (command "circle"
			   (list hingecolumnx
				 (- hingecolumny (/ 16.0 25.4))
				 0.0
			   )
			   "diameter"
			   holediameter
		  )
		  (command "circle"
			   (list hingecolumnx
				 (+ hingecolumny (/ 16.0 25.4))
				 0.0
			   )
			   "diameter"
			   holediameter
		  )
		  (if (= direction02 0.0)
		    (progn
		      (command "-insert"
			       hinge
			       (list hingecolumnx
				     (- hingecolumny (/ 16.0 25.4))
				     0.0
			       )
			       "1.0"
			       "1.0"
			       "0.0"
		      )
		      (command "chprop"
			       (entlast)
			       ""
			       "layer"
			       "hardware"
			       ""
		      )
		    )
		    (progn
		      (command "-insert"
			       hinge
			       (list hingecolumnx
				     (+ hingecolumny (/ 16.0 25.4))
				     0.0
			       )
			       "1.0"
			       "1.0"
			       "180.0"
		      )
		      (command "chprop"
			       (entlast)
			       ""
			       "layer"
			       "hardware"
			       ""
		      )
		    )
		  )
		  (setq counter01 (+ counter01 1))
		)
	      )
	      (progn
	      )
	    )
	  )
	  (progn
	  )
	)
      )
      (gc)
      (restore variable_list)
    )
    (progn
      (Alert
	"This command can only be called from a Fetzer certified drawing."
      )
    )
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Holes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:Blocking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Blocking (/ pt1 pt3 pt2 pt4 osmode)
  (setvar "cmdecho" 0)
  (setq osmode (getvar "osmode"))
  (setq pt1 (getpoint "\nPick the first block corner: "))
  (setq pt3 (getcorner "\nPick the opposite block corner: " pt1))
  (setq pt2 (list (car pt1) (cadr pt3)))
  (setq pt4 (list (car pt3) (cadr pt1)))
  (setvar "osmode" 16563)
  (if (or (tblsearch "block" "DWG_DrawingSetup_2012")
	  (tblsearch "block" "DWG_DrawingSetup_2012_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2015")
	  (tblsearch "block" "DWG_DrawingSetup_2015_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2019")
	  (tblsearch "block" "DWG_DrawingSetup")
      )
    (progn
      (command "pline" pt1 pt2 pt3 pt4 "C")
      (command "chprop" "last" "" "layer" "Part" "")
      (command "pline" pt1 pt3 "")
      (command "chprop" "last" "" "layer" "Hatch" "")
      (command "pline" pt2 pt4 "")
      (command "chprop" "last" "" "layer" "Hatch" "")
    )
    (progn
      (command "pline" pt1 pt2 pt3 pt4 "C")
      (command "pline" pt1 pt3 "")
      (command "pline" pt2 pt4 "")
    )
  )
  (setvar "osmode" osmode)
  (setvar "cmdecho" 1)
  (gc)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:Blocking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:rl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:rl (/ pt1 pt2 d ptext ptextfeet pt)
  (setvar "cmdecho" 0)
  (setq pt1 (getpoint "\nPick the First Point "))
  (terpri)
  (setq pt2 (getpoint "	Next Point " pt1))
  (terpri)
  (setq d (distance pt1 pt2))
  (prompt "Running Distance: ")
  (prompt (rtos d 2 4))
  (while (setq pt1 (getpoint "\n	Next Point: " pt2))
    (terpri)
    (setq d (+ (distance pt1 pt2) d))
    (prompt "Running Distance: ")
    (prompt (rtos d 2 4))
    (setq pt2 pt1)
  )
  (prompt "Total Distance: ")
  (prompt (rtos d 2 4))
  (setq ptext (strcat "RUNNING LENGTH IN INCHES = " (rtos d 2 4) "\""))
  (setq ptextfeet (strcat "RUNNING LENGTH IN FEET = " (rtos d 4 4)))
  (setq pt (getpoint "\nPick text location: "))
  (command "text" pt "1.5" "" ptext)
  (command "text" "" ptextfeet)
  (setq d nil)
  (gc)
  (setvar "cmdecho" 1)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:rl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:rll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:rll (/ pt1 pt2 d ptext ptextfeet pt)
  (setvar "cmdecho" 0)
  (setq pt1 (getpoint "\nPick the First Point "))
  (terpri)
  (setq pt2 (getpoint "	Next Point " pt1))
  (terpri)
  (setq d (distance pt1 pt2))
;(setq d (/ d (getvar "userr1")))
  (setq d (/ d 1))
  (prompt "Running Distance: ")
  (prompt (rtos d 2 4))
  (while (setq pt1 (getpoint "\n	First Point: "))
    (terpri)
    (setq pt2 (getpoint "	Next Point: " pt1))
    (terpri)
;(setq d (+ (/ (distance pt1 pt2) (getvar "userr1")) d))
    (setq d (+ (/ (distance pt1 pt2) 1) d))
    (prompt "Running Distance: ")
    (prompt (rtos d 2 4))
  )
  (prompt "Total Distance: ")
  (prompt (rtos d 2 4))
  (setq ptext (strcat "RUNNING LENGTH IN INCHES = " (rtos d 2 4) "\""))
  (setq ptextfeet (strcat "RUNNING LENGTH IN FEET = " (rtos d 4 4)))
  (setq pt (getpoint "\nPick text location: "))
  (command "text" pt "1.5" "" ptext)
  (command "text" "" ptextfeet)
  (setvar "cmdecho" 1)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:rll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:os
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:os (/ oldsnap offi pt1 pt2 off1 num count)
  (if (or (tblsearch "block" "DWG_DrawingSetup_2012")
	  (tblsearch "block" "DWG_DrawingSetup_2012_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2015")
	  (tblsearch "block" "DWG_DrawingSetup_2015_FRS")
	  (tblsearch "block" "DWG_DrawingSetup_2019")
	  (tblsearch "block" "DWG_DrawingSetup")
      )
    (progn
      (setvar "cmdecho" 0)
      (setq oldsnap (getvar "osmode"))
      (command "osnap" "none")
      (setq offi (getreal "\nSpecify initial offset distance:"))
      (setq pt1 (entsel "\nSpecify object to offset:"))
      (setq pt2 (getpoint "\nSpecify point on side to offset:"))
      (setq num (getint "\nSpecify number of offsets:"))
      (setq off1 offi)
      (setq count 1)
      (while (<= count num)
	(command "offset" off1 pt1 pt2 "")
	(command "chprop" "l" "" "la" "Linetype -- Dotted" "")
	(setq count (1+ count))
	(setq off1 (+ off1 (* offi count)))
      )
      (setvar "osmode" oldsnap)
      (setvar "cmdecho" 1)
    )
    (progn
      (Alert
	"This command can only be called from a Fetzer certified drawing."
      )
    )
  )
  (gc)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:os
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;