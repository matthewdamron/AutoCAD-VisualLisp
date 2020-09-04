;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:EndGrain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:EndGrain (/ boundary checkpoint myflag	origin clayer boundarypt entitydatalist entitytype myBoundary MinimumPoint MaximumPoint	InsertionPoint Wide Thick groupname	na redraw_it p1 e1 lst n layers mylayername color plot linetype layers layerexists layername EndGrainGroup ss ssl e et saw Wide Thick PointList na)
	(progn
		(acet-error-init
			(list
				(list
					"cmdecho" 0
					"highlight" 0
					"regenmode" 1
					"osmode" 0
					"ucsicon" 0
					"offsetdist" 0
					"attreq" 0
					"plinewid" 0
					"plinetype" 1
					"gridmode" 0
					"celtype" "CONTINUOUS"
					"ucsfollow" 0
					"limcheck" 0
				)
				T '(if redraw_it (redraw na 4))
			)
		)
		(setq boundary nil)
		(setq checkpoint nil)
		(setq myflag nil)
		(setq origin nil)
		(setq clayer nil)
		(setq boundarypt nil)
		(setq entitydatalist nil)
		(setq entitytype nil)
		(setq myBoundary nil)
		(setq MinimumPoint nil)
		(setq MaximumPoint nil)
		(setq InsertionPoint nil)
		(setq Wide nil)
		(setq Thick nil)
		(setq groupname nil)
		(setq na nil)
		(setq redraw_it nil)
		(setq p1 nil)
		(setq e1 nil)
		(setq lst nil)
		(setq n nil)
		(setq layers (vla-get-layers (vla-get-ActiveDocument (vlax-get-Acad-Object))))
		(setq mylayername "Hatch")
		(setq color "red")
		(setq plot "plot")
		(setq linetype "continuous")
		(setq layers (vla-get-layers (vla-get-ActiveDocument (vlax-get-Acad-Object))))
		(setq layerexists nil)
		(vlax-for layer layers (setq layername (vla-get-Name layer))
			(if (= (strcase layername T) (strcase mylayername T))
				(progn (setq layerexists T))
			)
		)
		(if	(= layerexists T)
			(progn)
			(progn
				(command
					"-layer" "new" mylayername
					"on" mylayername
					"thaw" mylayername
					"unlock" mylayername
					"color"	color mylayername
					"ltype" linetype mylayername
					"lweight" "default" mylayername
					"plot" plot	mylayername
					""
				)
			)
		)
		(setq mylayername "Hatch -- FS")
		(setq color "14")
		(setq plot "plot")
		(setq linetype "continuous")
		(setq layers (vla-get-layers (vla-get-ActiveDocument (vlax-get-Acad-Object))))
		(setq layerexists nil)
		(vlax-for layer layers (setq layername (vla-get-Name layer))
			(if (= (strcase layername T) (strcase mylayername T))
				(progn (setq layerexists T))
			)
		)
		(if	(= layerexists T)
			(progn)
			(progn
				(command
					"-layer" "new" mylayername
					"on" mylayername
					"thaw" mylayername
					"unlock" mylayername
					"color" color mylayername
					"ltype" linetype mylayername
					"lweight" "default" mylayername
					"plot" plot mylayername
					""
				)
			)
		)
		(setq clayer (getvar "clayer"))
		(command "-layer" "new" "boundary" "new" "grain" "color" "7" "boundary,grain" "set" "boundary" "")
		(setq origin (list 0.0 0.0 0.0))
		(setq myflag nil)
		(while (not myflag)
			(command "point" origin)
			(setq boundary nil)
			(setq checkpoint nil)
			(setq checkpoint (entlast))
			(initget 128 "Rectangle polylinE Options")
			(setq boundarypt (getpoint "\nPick a point within the area to be grained [Rectangle polylinE Options]:"))
			(cond
				((equal boundarypt "Options")
					(setq CurrentValue (vl-registry-read "HKEY_CURRENT_USER\\Software\\Fetzer\\VisualLISP" "EndGrainGroup"))
					(if (= CurrentValue nil)
						(setq CurrentValue "Yes")
					)
					(setq EndGrainGroup (list 0.0 0.0 0.0))
					(while (= 'LIST (type EndGrainGroup))
						(initget 128 "Yes No")
						(setq EndGrainGroup (getpoint (strcat "\nDo you want to group end grain objects [Yes No] <" CurrentValue ">:")))
						(if (= EndGrainGroup nil)
							(setq EndGrainGroup CurrentValue)
						)
						(if (/= EndGrainGroup "Yes")
							(if (/= EndGrainGroup "No")
								(setq EndGrainGroup (list 0.0 0.0 0.0))
								(vl-registry-write "HKEY_CURRENT_USER\\Software\\Fetzer\\VisualLISP" "EndGrainGroup" EndGrainGroup)
							)
							(vl-registry-write "HKEY_CURRENT_USER\\Software\\Fetzer\\VisualLISP" "EndGrainGroup" EndGrainGroup)
						)
					)
				)
				((equal boundarypt "Rectangle")
					(princ "\n \nSpecify first corner point: ")
					(command "rectangle" pause pause)
					(setq boundary (entlast))
					(setq entitydatalist (entget boundary))
					(setq entitytype (cdr (assoc 0 entitydatalist)))
					(if (/= entitytype "LWPOLYLINE")
						(progn
							(prompt "\n*** A valid boundary was not found. ***\n")
							(command "erase" checkpoint "")
							(setq myflag nil)
						)
						(progn
							(command "erase" checkpoint "")
							(setq myflag T)
						)
					)
				)
				((equal boundarypt "polylinE")
					(setq ss (ssget))
					(setq ssl (sslength ss))
					(setq e (ssname ss 0))
					(setq e (entget e))
					(setq et (cdr (assoc 0 e)))
					(if (or (/= et "LWPOLYLINE") (> ssl 1))
						(progn
							(princ "\n*** One closed polyline is allowed in your selection. ***\n")
							(command "erase" checkpoint "")
							(setq myflag nil)
						)
						(progn
							(command "copy" ss "" origin origin)
							(setq boundary (entlast))
							(setq entitydatalist (entget boundary))
							(setq entitytype (cdr (assoc 0 entitydatalist)))
							(command "chprop" boundary "" "layer" "boundary" "")
							(command "erase" checkpoint "")
							(setq myflag T)
						)
					)
				)
				((equal (type boundarypt) 'LIST)
					(command "-boundary" boundarypt "")
					(setq boundary (entlast))
					(setq entitydatalist (entget boundary))
					(setq entitytype (cdr (assoc 0 entitydatalist)))
					(if (/= entitytype "LWPOLYLINE")
						(progn
							(prompt "\n*** A valid boundary was not found. ***\n")
							(command "erase" checkpoint "")
							(setq myflag nil)
						)
						(progn
							(command "chprop" boundary "" "layer" "boundary" "")
							(command "erase" checkpoint "")
							(setq myflag T)
						)
					)
				)
				(T (prompt "\n*** Invalid.  Expects a point or a keyword. ***\n"))
			)
		)
		(initget 128 "Plain Quarter")
		(setq saw (getpoint "\nPlain or Quarter sawn <Plain>:"))
		(if	(or (= saw nil) (equal (type saw) 'LIST))
			(setq saw "PLAIN")
		)
		(setq saw (strcase saw))
		(setq myBoundary (vlax-ename->vla-object boundary))
		(vla-GetBoundingBox myBoundary 'MinimumPoint 'MaximumPoint)
		(setq PointList (mapcar 'vlax-safearray->list (list MinimumPoint MaximumPoint)))
		(setq MinimumPoint (car PointList))
		(setq MaximumPoint (cadr PointList))
		(setq InsertionPoint (list (car MinimumPoint) (cadr MinimumPoint) 0.0))
		(setq Wide (- (car MaximumPoint) (car MinimumPoint)))
		(setq Thick (- (cadr MaximumPoint) (cadr MinimumPoint)))
		(if	(and (<= Wide Thick) (= saw "PLAIN"))
			(progn
				(setq InsertionPoint (polar InsertionPoint (* pi 0.5) (/ Thick 2.0)))
				(setq InsertionPoint (polar InsertionPoint (* pi 1.0) 2.5))
			)
		)
		(if	(and (> Wide Thick) (= saw "PLAIN"))
			(progn
				(setq InsertionPoint (polar InsertionPoint (* pi 0.5) (/ Thick 1.0)))
				(setq InsertionPoint (polar InsertionPoint (* pi 2.0) (/ Wide 2.0)))
				(setq InsertionPoint (polar InsertionPoint (* pi 0.5) 2.5))
			)
		)
		(if	(and (< Wide Thick) (= saw "QUARTER"))
			(progn
				(setq InsertionPoint (polar InsertionPoint (* pi 0.5) (/ Thick 1.0)))
				(setq InsertionPoint (polar InsertionPoint (* pi 2.0) (/ Wide 2.0)))
				(setq InsertionPoint (polar InsertionPoint (* pi 0.5) 2.5))
			)
		)
		(if	(and (>= Wide Thick) (= saw "QUARTER"))
			(progn
				(setq InsertionPoint (polar InsertionPoint (* pi 0.5) (/ Thick 2.0)))
				(setq InsertionPoint (polar InsertionPoint (* pi 1.0) 2.5))
			)
		)
		(command "-layer" "set" "grain" "")
		(command "-insert" "hat_endgrain" InsertionPoint "" "" "")
		(command "explode" "last" "")
		(command "chprop" "previous" "" "layer" "grain" "")
		(command "-layer" "off" "*" "y" "")
		(command "-layer" "on" "boundary,grain" "")
		(setq na (ssget "X" (list (cons 8 "boundary"))))
		(setq na (ssname na 0))
		(if	na
			(progn
				(redraw na 3)
				(setq redraw_it T)
				(setq p1 InsertionPoint)
				(redraw na 4)
				(setq redraw_it nil)
				(if	p1
					(etrim na p1)
				)
			)
		)
		(setq ss (ssget "X" '((0 . "CIRCLE") (8 . "grain"))))
		(command "erase" ss "")
		(setq ss (ssget "X" '((0 . "ARC") (8 . "grain"))))
		(command "chprop" ss "" "layer" "hatch" "")
		(if	(or (= (vl-registry-read "HKEY_CURRENT_USER\\Software\\Fetzer\\VisualLISP" "EndGrainGroup") nil)
				(= (vl-registry-read "HKEY_CURRENT_USER\\Software\\Fetzer\\VisualLISP" "EndGrainGroup") "Yes")
			)
			(progn
				(setq groupname (vl-string-subst "-" "." (rtos (getvar "DATE"))))
				(command "-group" "create" groupname "" ss "")
			)
		)
		(command "-layer" "on" "*" "")
		(setq ss nil)
		(setq ss (ssget "X" (list (cons 8 "boundary"))))
		(command "erase" ss "")
		(setq ss (ssget "X" (list (cons 8 "grain"))))
		(command "erase" ss "")
		(setvar "clayer" clayer)
		(command "-purge" "layer" "boundary,grain" "no")
		(setq ss nil)
		(acet-error-restore)
	)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:EndGrain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Etrim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun etrim (na a /)
	(setq e1 nil)
	(setq vpna nil)
	(setq lst nil)
	(setq x nil)
	(setq y nil)
	(setq z nil)
	(setq zlst nil)
	(setq flag nil)
	(setq flag2 nil)
	(setq flag3 nil)
	(setq la nil)
	(setq b nil)
	(setq n nil)
	(setq d nil)
	(setq j nil)
	(setq k nil)
	(setq m nil)
	(setq ss nil)
	(setq lst2 nil)
	(setq na2 nil)
	(setq na3 nil)
	(setq na4 nil)
	(setq vplocked nil)
	(setq e1 (entget na))
	(if	(or (setq flag (equal (acet-dxf 0 e1) "POLYLINE"))
			(setq flag (equal (acet-dxf 0 e1) "LWPOLYLINE"))
			(equal (acet-dxf 0 e1) "LINE")
			(equal (acet-dxf 0 e1) "CIRCLE")
			(equal (acet-dxf 0 e1) "ARC")
			(equal (acet-dxf 0 e1) "ELLIPSE")
			(equal (acet-dxf 0 e1) "TEXT")
			(equal (acet-dxf 0 e1) "ATTDEF")
			(equal (acet-dxf 0 e1) "MTEXT")
			(equal (acet-dxf 0 e1) "SPLINE")
		)
		(progn
			(if	(and flag
					(equal 8 (logand 8 (acet-dxf 70 e1)))
				)
				(setq flag nil)
			)
			(setq a (trans a 1 0) vpna (acet-currentviewport-ename))
			(acet-ucs-cmd (list "_View"))
			(setq	lst		(acet-geom-object-point-list na nil)
					lst		(acet-geom-list-extents lst)
					x		(- (car (cadr lst)) (car (car lst)))
					y		(- (cadr (cadr lst)) (cadr (car lst)))
					x		(* 0.075 x)
					y		(* 0.075 y)
					z		(list x y)
					x		(list (+ (car (cadr lst)) (car z)) (+ (cadr (cadr lst)) (cadr z)))
					y		(list (- (car (car lst)) (car z)) (- (cadr (car lst)) (cadr z)))
					zlst	(zoom_2_object (list x y))
			) ;find extents of selected cutting edge object
			(if	vpna
				(setq vplocked (acet-viewport-lock-set vpna nil)) ;unlock curent viewport if needed
			)
			(command "_.zoom" "_w" (car zlst) (cadr zlst))
			(entupd na) ;update the ent. so it's curves display smoothly
			(setq lst (acet-geom-object-point-list na (/ (acet-geom-pixel-unit) 2.0)))
			(if	(or (not flag)
					(not (acet-geom-self-intersect lst nil))
				)
				(progn ;then the object is valid and not a self intersecting polyline.
					(if	(and flag
							(equal (car lst) (last lst) 0.0001)
						)
						(setq flag3 T) ;then the polyline could potentialy need a second offset
					)
					(if	(setq la (acet-layer-locked (getvar "clayer")))
						(command "_.layer" "_unl" (getvar "clayer") "")
					)
					(command "_.pline")
					(setq b nil)
					(setq n 0) ;setq
					(repeat (length lst)
						(setq d (nth n lst))
						(if (not (equal d b 0.0001))
							(progn
								(command d)
								(setq lst2 (append lst2 (list d))) ;setq
								(setq b d)
							)
						)
						(setq n (+ n 1))
					)
					(command "")
					(setq na2 (entlast) ss (ssadd) ss (ssadd na2 ss) lst nil)
					(acet-ss-visible ss 1)
					(setq lst2 (get_fence_points na2 a lst2 flag3 flag)) ;setq
					(if	la
						(command "_.layer" "_lock" (getvar "clayer") "")
					)
					(acet-ucs-cmd (list "_p"))
					(setvar "highlight" 0)
					(if	(setq ss (ssget "_f" (last lst2)))
						(command "_.move" ss "" "0,0,0" "0,0,0") ;Move the ents to force a display update of the ents to avoid viewres problems
					)
					(if	flag
						(progn
							(if	(setq la (acet-layer-locked (acet-dxf 8 e1)))
								(command "_.layer" "_unl" (acet-dxf 8 e1) "")
							)
							(acet-ucs-set-z (acet-dxf 210 e1))
							(command "_.copy" na "" "0,0,0" "0,0,0")
							(acet-ss-visible (ssadd na (ssadd)) 1) ;make it invisible for a while.
							(setq na3 na na (entlast))
							(command "_.pedit" na "_w" "0.0" "_x")
							(acet-ucs-cmd (list "_p"))
							(if	la
								(command "_.layer" "_lock" (acet-dxf 8 e1) "")
							)
						)
					)
					(command "_.trim" na "")
					(setq m (- (length lst2) 1))
					(setq k 0)
					(repeat (length lst2)
						(setq lst (nth k lst2))
						(setq a (trans (car lst) 0 1))
						(setq n 1)
						(repeat	(- (length lst) 1) ;repeat each fence list
							(setq b (trans (nth n lst) 0 1))
							(if	(equal a b 0.0001)
								(setq flag2 T)
								(setq flag2 nil)
							)
							(setq na4 nil) ;setq
							(setq j 0) ;setq
							(while (not flag2) ;repeat each segment of the fence until no new ents are created.
								(setq na4 (entlast)) ;setq
								(command "_F" a b "")
								(if (and (equal na4 (entlast))
										(or (not (equal k m))
											(> j 0)
										)
									)
									(setq flag2 T)
								)
								(setq j (+ j 1)) ;setq
							)
							(setq a b)
							(setq n (+ n 1))
						)
						(setq k (+ k 1))
					)
					(command "")
					(if	flag
						(progn
							(if	(setq la (acet-layer-locked (acet-dxf 8 e1)))
								(command "_.layer" "_unl" (acet-dxf 8 e1) "")
							)
							(entdel na) ;get rid of the copy
							(acet-ss-visible (ssadd na3 (ssadd)) 0) ;bring back the original
							(if	la
								(command "_.layer" "_lock" (acet-dxf 8 e1) "")
							)
						)
					)
				)
				(progn
					(acet-ucs-cmd (list "_p"))
					(princ "\nSelf intersecting edges are not acceptable.")
				)
			)
			(command "_.zoom" "_p")
			(if	vplocked
				(acet-viewport-lock-set vpna T) ;then re-lock the viewport
			)
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Etrim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Another_Offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun another_offset (pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4 / na ss lst da1 da2)
	(setq da1 (abs (- a2 a1)))
	(setq da2 (- (* b (max pl2 pl1)) (/ (* b (abs (- pl2 pl1))) 2.0)))
	(if	(> (abs (- da2 da1))
			(* 0.01 (max a1 a2))
		)
		(progn
			(acet-pline-make (list lst2))
			(setq na (entlast) na2 (entlast) ss (ssadd) ss (ssadd na ss))
			(acet-ss-visible ss 1)
			(command "_.offset" b na2 a "")
			(if	(and (not (equal na (entlast)))
					(setq lst3 (acet-geom-vertex-list (entlast)))
					(setq lst3 (intersect_check lst2 lst3 lst4))
				)
				(progn
					(acet-ss-visible (ssadd (entlast) (ssadd)) 1)
					(command "_.area" "_ob" (entlast))
					(setq pl2 (getvar "perimeter") a2 (getvar "area"))
					(setq lst (list (acet-geom-vertex-list (list (entlast) 0))))
					(entdel (entlast))
				)
				(if (not (equal na (entlast)))
					(entdel (entlast))
				)
			)
			(entdel na2)
		)
	)
	lst
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Another_Offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Get_Fence_Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_fence_points (na2 a lst2 flag plflag / a1 a2 pl1 pl2 b c d n lst lst3 lst4 na)
	(if	flag
		(progn
			(setq lst2 (cdr lst2)) ;setq
			(repeat (fix (/ (length lst2) 2))
				(setq lst2 (append (cdr lst2) (list (car lst2))))
			)
			(setq lst2 (append lst2 (list (car lst2)))) ;setq
			(command "_.area" "_ob" na2)
			(setq pl1 (getvar "perimeter") a1 (getvar "area"))
		)
	)
	(setq
		a		(trans a 0 1)
		b		(* (getvar "viewsize") 0.05) ;initial offset distance
		n		3.0 ;number of offsets
		d		(/ b (- n 1)) ;delta offset
		c		(acet-geom-pixel-unit)
		lst4	(acet-geom-view-points)
	)
	(while (> b c)
		(setq na (entlast))
		(command "_.offset" b na2 a "")
		(if (and (not (equal na (entlast)))
				(setq lst3 (acet-geom-vertex-list (entlast)))
				(or (not plflag)
					(setq lst3 (intersect_check lst2 lst3 lst4))
				)
			)
			(progn
				(setq lst3 (acet-geom-m-trans lst3 1 0))
				(acet-ss-visible (ssadd (entlast) (ssadd)) 1)
				(if flag
					(progn
						(command "_.area" "_ob" (entlast))
						(setq pl2 (getvar "perimeter") a2 (getvar "area"))
					)
				)
				(setq lst (append lst (list lst3))) ;setq
				(entdel (entlast)) ;delete the ent after getting it's vertex info
				(if flag
					(setq lst (append lst (another_offset pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4)))
				)
			)
			(if	(not (equal na (entlast)))
				(entdel (entlast))
			)
		)
		(setq b (- b d))
	)
	(setq na (entlast))
	(command "_.offset" c na2 a "")
	(if	(and (not (equal na (entlast)))
			(setq lst3 (acet-geom-vertex-list (entlast)))
			(or (not plflag)
				(setq lst3 (intersect_check lst2 lst3 lst4))
			)
		)
		(progn
			(setq lst3 (acet-geom-m-trans lst3 1 0))
			(acet-ss-visible (ssadd (entlast) (ssadd)) 1)
			(if	flag
				(progn
					(command "_.area" "_ob" (entlast))
					(setq pl2 (getvar "perimeter") a2 (getvar "area"))
				)
			)
			(setq lst (append lst (list lst3)))
			(entdel (entlast)) ;then offset was a success so delete the ent after getting it's info
			(if	flag
				(setq lst (append lst (another_offset pl1 pl2 a1 a2 c na2 lst2 a lst3 lst4)))
			)
		)
		(if (not (equal na (entlast)))
			(entdel (entlast))
		)
	)
	(entdel na2)
	lst
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Get_Fence_Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Intersect_Check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intersect_check (lst lst2 lst3 / x x2 y y2 lst4 flag len len2 a aa b bb c d n j) ;returns a list of points on screen if the first two lists do not contain segments that intersect each other.
	(setq	len		(length lst)
			len2	(length lst2)
			x		(car (car lst3))
			x2		(car (cadr lst3))
			y		(cadr (car lst3))
			y2		(cadr (cadr lst3))
	)
	(setq n 0)
	(while	(and	(not flag)
				(< (+ n 1) len2)
			)
			(setq	aa		(nth n lst2)
					bb		(nth (+ n 1) lst2)
					a		(bns_truncate_2_view aa bb x y x2 y2)
					b		(bns_truncate_2_view bb aa x y x2 y2)
					lst4	(append lst4 (list a))
			)
			(if (or	(not (equal a aa))
					(not (equal b bb))
				)
				(setq lst4 (append lst4 (list b)))
			)
			(setq j 0) ;setq
			(while	(and (not flag)
						(< (+ j 1) len)
					)
					(setq	c		(nth j lst)
							d		(nth (+ j 1) lst)
							flag	(inters a b c d)
					)
					(setq j (+ j 1)) ;setq
			)
			(setq n (+ n 1)) ;setq
	)
	(if	(not (equal b (last lst4)))
		(setq lst4 (append lst4 (list b))) ;setq
	)
	(if	(not flag)
		(setq flag lst4)
		(setq flag nil)
	)
	flag
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Intersect_Check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Zoom_2_Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zoom_2_object (lst / p1 p2 p3 p4 p5 p6 mp dx dy dx2 dy2 r1 r2 na e1 x w h dv1 dv2 x)
	(setq	lst	(acet-geom-m-trans lst 1 2) ;p1 and p2 are the viewpnts
			p1	(acet-geom-m-trans (acet-geom-view-points) 1 2)
			p2	(cadr p1)
			p1	(car p1)
			p1	(list (car p1) (cadr p1))
			p2	(list (car p2) (cadr p2))
	)
	(if	lst
		(progn
			(setq	p5	(acet-geom-list-extents lst) ;p5 and p6 are the geometry points
					p6	(cadr p5)
					p5	(car p5)
					p5	(list (car p5) (cadr p5))
					p6	(list (car p6) (cadr p6))
					mp	(acet-geom-midpoint p5 p6)
					;prepare to resize the geometry rectang to
					dx  (- (car p2) (car p1)) ;have the same dy/dx ratio as p1 p2.
					dy  (- (cadr p2) (cadr p1))
					dx2 (- (car p6) (car p5))
					dy2 (- (cadr p6) (cadr p5))
			)
			(if	(equal dx 0.0)
				(setq dx 0.000001)
			) ;just in case div by zero
			(if	(equal dx2 0.0)
				(setq dx2 0.000001)
			)
			(setq r1 (/ dy dx) r2 (/ dy2 dx2))
			(if	(< r2 r1)
				(setq dy2 (* r1 dx2)) ;then scale dy2 up
				(progn
					(if	(equal r1 0.0)
						(setq r1 0.000001)
					) ;just in case div by zero
					(setq dx2 (* dy2 (/ 1.0 r1))) ;else scale dx2 up
				)
			)
			(setq	p5	(list	(- (car mp) (/ dx2 1.98)) ;1.98 is used instead of 2.0 to expand
								(- (cadr mp) (/ dy2 1.98)) ;the rectangle slightly
						)
					p6	(list	(+ (car mp) (/ dx2 1.98))
								(+ (cadr mp) (/ dy2 1.98))
						)
			)
		)
	)
	(if	(and lst
			(equal 0 (getvar "tilemode"))
			(not (equal 1 (getvar "cvport")))
			(setq na (acet-currentviewport-ename))
		)
		(progn
			(setq	e1	(entget na)
					x	(cdr (assoc 10 e1))
					w	(cdr (assoc 40 e1))
					h	(cdr (assoc 41 e1))
					p3	(list (- (car x) (/ w 2.0)) (- (cadr x) (/ h 2.0)))
					p4	(list (+ (car x) (/ w 2.0)) (+ (cadr x) (/ h 2.0)))
					p3	(trans p3 3 2) ;p3 and p4 are the viewport points
					p4	(trans p4 3 2)
					dv1	(acet-geom-delta-vector p1 p3)
					dv2	(acet-geom-delta-vector p2 p4)
					x	(distance p1 p2)
			)
			(if	(equal 0 x)
				(setq x 0.000001)
			) ;just in case
			(setq	x	(/ (distance p5 p6) x)
					dv1	(acet-geom-vector-scale dv1 x)
					dv2	(acet-geom-vector-scale dv2 x)
					p5	(acet-geom-vector-add p5 dv1)
					p6	(acet-geom-vector-add p6 dv2)
			)
		)
	)
	(setq	p1	(list (car p1) (cadr p1) 0.0)
			p2	(list (car p2) (cadr p2) 0.0)
			p5	(list (car p5) (cadr p5) 0.0)
			p6	(list (car p6) (cadr p6) 0.0)
	)
	(if	lst
		(setq lst (list	(trans p5 2 1) (trans p6 2 1)))
		(setq lst nil)
	)
	lst
)
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Zoom_2_Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;