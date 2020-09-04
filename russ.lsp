;;; LISP CONTAINS 
	;;; COPY MULTIPLY SLEEPERS (POS)
	;;; ARC WITH GIVEN LENGTH AND RADIUS (AQ)
	;;; DRAWS SLATS WITH RADIUS AT END (SLOT)
	;;; BREAK A LINE,ARC THEN MOVE TO HIDDEN LAYER (BKH)
	;;; CHANGE CASE OF TEXT TO UPPER OR LOWER CASE (CASE)
	;;; ARC TO CIRCLE (ATC)
	;;; POLAR ARRAY (ACC)
	;;; ARRAY (AAR)
	;;; POLAR ARRAY WITH EXTRA OPTIONS (DAP)
	;;; LAYER FILTER DELETE (LFD)
	;;; CHAMFER WITH DISTANCE (CHH)
	;;; LINE FROM END TO PER (SS)
	;;; LENGTH WITH DISTANCE (LNN)
	;;; MATCH ATTRIBUTE LAYER (MAT)
	;;; ENTEND LINE TO POINT (CHG)
	;;; DRAWER BOX @ 5/8 (DB5)
	;;; SPLINE TO P-LINE (S2P)

;Non-Standard Lisp Functions

(defun rtd (R) (/ (* R 180.0) pi))  ; Radians to Degrees

(defun dtr (D) (/ (* D pi) 180.0)) ; Degrees to Radians

(defun hyp (a b) (sqrt (+ (expt a 2.0)(expt b 2.0)))) ; figures hypotenuse of triangle when other 2 legs are given

(defun deg (G) (strcat G "%%d")) ; Add degree symbol to string
  
(defun tan (A) (/ (sin A) (cos A))) ; Tangent of variable (A) in Radians

(defun sqr (S) (* S S)) ; Squares the variable (S)

(defun feet (dec) (rtos dec 4 4))  ; converts decimal inches to feet and inches

(defun fixx (realnum) ; Rounds the variable (realnum) to the next highest whole integer (opposite of the "fix" function)
(if (> realnum (+ (fix realnum) 0)) 
    (setq realnum (1+ (fix realnum))) 
    (setq realnum  (fix realnum)) 
) 
realnum 
) 



(defun getarc () 
(setq en (car (entsel "\nSelect Arc for Radius/Arc Length/Chord Length: ")))
(setq enlist (entget en))
(setq RAD (cdr (assoc 40 enlist)))
(setq ANG1 (cdr (assoc 50 enlist)))
(setq ANG2 (cdr (assoc 51 enlist)))
(setq ANG (- ANG2 ANG1))
(if
(< ANG 0)
(setq ANG (- (* 2 pi) (abs ANG)))
)
(setq ARCLENGTH (* RAD ANG))
(setq CHORDLENGTH (* 2 (* RAD (sin (/ ANG 2)))))
)

;(setq variable (ssget)) - allows multiple picks in lisp

;(setq OS (getvar "osmode"))...(setvar "osmode" OS) - gets current O-snap settings @ front of lisp, then re-establishes them at the end

;(setq DS (getvar "dimscale"))...(setvar "dimscale" DS) - gets current dimscale settings @ front of lisp, then re-establishes them at the end

(setq ANGDIR 0)

(defun ARC ()
(* RAD (/ ANG 57.2958))
)

(defun ANG ()
(* (/ ARC RAD) 57.2958)
)

(defun RAD ()
(/ ARC (/ ANG 57.2958))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;Copies object @ equal space in a given distance (multiple-post-insertions.lsp	(c) 2006 Derek Beal)


(defun c:POS ()
(setq P1 (getpoint "\nPick starting point for posts: "))
(setq P2 (getpoint "\nPick ending point for posts: "))
(setq DIST (abs (distance P1 P2)))
(setq ANG (angle P1 P2))
(setq ANG (/ (* ANG 180) pi))

(setq MAX2 (getstring "\nEnter maximum distance between posts: "))
(setq MAX2 (distof MAX2))

(setq DIST2 (/ DIST MAX2))
(setq DIST2 (fixx DIST2))
(setq DIST3 (/ DIST DIST2))


(setq POST (ssget))

(setq P3 (polar P1 (dtr ANG) DIST3))

(repeat (- DIST2 1)
(command "copy" POST "" P1 P3)
(setq P3 (polar P3 (dtr ANG) DIST3))

(command "-osnap" "end,mid,int,qua")

))


(princ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;Arc with given length and radius

(defun c:AQ ()
(setq LENG (getdist "\nEnter length of arc: ") 
RAD (getdist "\nEnter radius of arc: ") 
CEN (getpoint "\nPick center point of arc: ") 
ANG (/ LENG RAD) 
ANG1 (/ (- PI ANG) 2.0) 
ANG2 (/ (+ PI ANG) 2.0) 
ARC (entmake (list 
(cons 0 "ARC") 
(cons 10 CEN) 
(cons 40 RAD) 
(cons 50 ANG1) 
(cons 51 ANG2))) 
))
 
(princ) 


:::::::::::::::::::::::::::::::::::::::::::::::::

		; DRAWS SLATS WITH RADIUS AT END
	
(defun c:slot () 
(setq osm (getvar "osmode")) 
(setq lu (getvar "lunits"))
 (setvar "lunits" 2)
 (setq wldv (getvar "worldview"))
 (setvar "worldview" 0)
 (setq eecc (getstring "\nEnd to end or center to center? (E/C) <E>: "))
 (setq ang (getangle "\nAt what angle?/<No angle>: "))
 (if (> ang 0)(setq ang (atof (angtos ang 0 2))))
 (command "ucs" "save" "ucswxy3" "y")
 (setq wdth (getdist "\nSlot width?: "))
 (setq lnth (getdist "\nSlot length?: "))
 (setq cntr (getpoint "\nSlot center location?: "))
 (setq x (-(/ lnth 2.00000000)(/ wdth 2.00000000)))
 (if (= eecc "c")(setq x (/ lnth 2.00000000)))
 (if (= eecc "C")(setq x (/ lnth 2.00000000)))
 (setq y (/ wdth 2.00000000))
 (setq stx (rtos x))
 (setq sty (rtos y))
 (setq ul (strcat "-" stx "," sty "," "0"))
 (setq ur (strcat stx "," sty "," "0"))
 (setq ll (strcat "-" stx "," "-" sty "," "0"))
 (setq lr (strcat stx "," "-" sty "," "0"))
 (setq acl (strcat "-" stx "," "0"))
 (setq acr (strcat stx "," "0"))
 (command "ucs" "origin" cntr)
 (setq ui (getvar "ucsicon"))
 (setvar "ucsicon" 0)
 (if (> ang 0)(command "ucs" "z" ang))
 (setvar "osmode" 0) 
 (command "line"  ul ur "")
 (command "line"  ll lr "")
 (command "arc" ul "c" acl "a" 180)
 (command "arc" lr "c" acr "a" 180)
 (setvar "osmode" osm)
 (setvar "lunits" lu)
 (setvar "worldview" wldv)
 (command "ucs" "restore" "ucswxy3")
 (setvar "ucsicon" ui)
 (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		; BREAK A LINE,ARC THEN MOVE TO HIDDEN LAYER (BKH)
	

(defun c:BKH (/ oclayer olsnap bobj bobj2 bobytyp pt1 pt2 pt3 bobjcen rdist)
 
  ;;
  (setq oclayer (getvar "clayer"))
  (setq olsnap (getvar "osmode"))
  (setvar "osmode" 1536)
  (setq bobj (entsel "\n Select Line, Polyline, Circle or Arc to Break: "))
  (setq bobj2 (assoc 0 (entget (car bobj))))
  (setq bobjtyp (cdr bobj2))
  (if ( or (= bobjtyp "LINE")(= bobjtyp "LWPOLYLINE") (= bobjtyp "POLYLINE"))
    (progn
      (setvar "osmode" 33)
      (setq pt2 (getpoint "\nSelect First Intersection. "))
      (setq pt3 (getpoint "\nSelect Second Intersection. "))
      (command "break" bobj "_f" pt2 pt3)
      (setvar "clayer" "linetype -- Hidden")
      (setvar "osmode" 0)
      (command "line" pt2 pt3 "")
      (setvar "osmode" olsnap)
      (setvar "clayer" oclayer)
    )					;progn
    (if	(or (= bobjtyp "CIRCLE") (= bobjtyp "ARC"))
      (progn
	(princ "\nsee here")
	(setvar "osmode" 33)
	(setq pt2 (getpoint "\nSelect First Intersection. "))
	(setq pt3 (getpoint "\nSelect Second Intersection. "))
	(setq bobjcen (assoc 10 (entget (car bobj))))
	(setq pt1 (cdr bobjcen))
	(setq rdist (distance pt1 pt2))
	(command "break" bobj "_f" pt2 pt3)
	(setvar "clayer" "hidden")
	;(setvar "osmode" 0)
	(setvar "osmode" olsnap)
	(command "arc" "C" pt1 pt2 pt3)
	(setvar "osmode" olsnap)
	(setvar "clayer" oclayer)
      )					;progn
      (princ "\n   Arc, Circle, Polyline or Line not selected")
    )					;if
  )					;if
  (princ)
)					;defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		; CHANGE CASE OF TEXT TO UPPER OR LOWER CASE (CASE)
	
(defun c:CASE	()
  (prompt "Convert case to [Upper or Lower]:")
  (setq casestr (getstring))
  (if (or (or (= casestr "u") (= casestr "U")) (= casestr ""))
    (setq casemode nil)
					;else
    (if	(or (= casestr "l") (= casestr "L"))
      (setq casemode 1)
    )
  )
  (prompt "\nPick lines of text: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if (/= ss nil)
    (progn
      (setq ssposition 0)		;Setup index to point at first entity in selection set
      (while (< ssposition (sslength ss))
					;For each entity in the selection set, do....
	(setq entityname (ssname ss ssposition))
					;Get next entities name from the selection set
	(setq entity1 (entget entityname)) ;Get entity list from name
	(setq ssposition (+ ssposition 1)) ;Advance index

	(setq string1 (cdr (assoc 1 entity1)))
	(setq string1 (strcase string1 casemode))
					;Convert text to upper/lower case
	(setq string1 (fixmtext string1))
	(setq
	  entity1 (subst (cons 1 string1) (assoc 1 entity1) entity1)
	)				;Substitue new text element with new
	(entmod entity1)		;Update entity
      )					;end while
    )					;end progn
					;else
    (princ "No text entities were selected")
  )					;end if
  (princ)				;Exit quietly

)					;end defun


(defun fixmtext	(string1)
  (while (/= (vl-string-search "\\p" string1) nil)
    (setq string1 (vl-string-subst "\\P" "\\p" string1))
  )
  (while (/= (vl-string-search "\\l" string1) nil)
    (setq string1 (vl-string-subst "\\L" "\\l" string1))
  )
  (while (/= (vl-string-search "\\F" string1) nil)
    (setq string1 (vl-string-subst "\\f" "\\F" string1))
  )
  (while (/= (vl-string-search "\\h" string1) nil)
    (setq string1 (vl-string-subst "\\H" "\\h" string1))
  )
  (while (/= (vl-string-search "\\s" string1) nil)
    (setq string1 (vl-string-subst "\\s" "\\S" string1))
  )
  (while (/= (vl-string-search "\\q" string1) nil)
    (setq string1 (vl-string-subst "\\q" "\\Q" string1))
  )
  (while (/= (vl-string-search "\\t" string1) nil)
    (setq string1 (vl-string-subst "\\t" "\\T" string1))
  )
  (while (/= (vl-string-search "\\w" string1) nil)
    (setq string1 (vl-string-subst "\\w" "\\W" string1))
  )
  (while (/= (vl-string-search "\\a" string1) nil)
    (setq string1 (vl-string-subst "\\a" "\\A" string1))
  )
  (while (/= (vl-string-search "\\o" string1) nil)
    (setq string1 (vl-string-subst "\\o" "\\O" string1))
  )

  string1
)					;end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		; ARC TO CIRCLE (ATC)	

(defun c:atc (/ a ar ce co ra la lt typ)
 (while (not (setq a (entsel "\nSelect ARC: "))))
 (setq ar (entget (car a)))
 (if (= (cdr (assoc 0 ar)) "ARC")
  (progn
   (setq ce (cdr (assoc 10 ar)) ra (cdr (assoc 40 ar))
         la (cdr (assoc 8 ar)))
   (if (cdr (assoc 62 ar))
     (setq co (cdr (assoc 62 ar)))
     (setq co 256)
   )
   (if (cdr (assoc 6 ar))
     (setq lt (cdr (assoc 6 ar)))
     (setq lt "BYLAYER")
   )
   (setq typ (cons 0 "CIRCLE") ce (cons 10 ce) ra (cons 40 ra) 
         la (cons 8 la) co (cons 62 co) lt (cons 6 lt)
   )
   (entdel (car a))
   (entmake (list typ ce ra la co lt))
   (prompt "\nARC turned to CIRCLE ")
  )
  (prompt "\nNot ARC ")
 )
 (redraw)
 (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;Copies object like polar array

(defun c:ACC ()
(setq ANGDIR 0)
(setq EN (ssget))
(setq BP (getpoint "\nPick basepoint for rotation:"))
(setq REF (getpoint BP "\nPick reference point for rotation:"))
(repeat 99
(setq REF2 (getpoint BP "\nPick reference point for new angle of rotation:"))
(command "copy" EN "" BP BP)
(command "rotate" EN "" BP "R" BP REF REF2)
(setq REF REF2)
))

(princ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					;Angle array routine, much easier than autocad array.

(defun C:AAR (/ ss ang bp cnt dist d old)
  (setq old (getvar "osmode"))
  (setvar "osmode" 0)
  (cond
    ((setq ss (ssget))
     (initget 1)
     (setq bp (getpoint "\nBase point: "))
     (initget 1)
     (setq ang
	    (getangle bp "\nArray direction: ")
     )
     (initget 7)
     (setq dist
	    (getdist "\nDistance between objects: ")
     )
     (initget 7)
     (setq cnt (getint "\nNumber of objects: "))
     (setq d 0.0)
     (setvar "cmdecho" 0)
     (command "_.undo" "_g" "_.copy" ss "" "_m" bp)
     (repeat (1- cnt)
       (command
	 (polar bp ang (setq d (+ d dist)))
       )
     )
     (command "" "_.undo" "_e")
    )
  )
  (princ)
  (setvar "osmode" old)
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		; POLAR ARRAY WITH EXTRA OPTIONS (DAP)

(defun c:dap ( / ss1 ang1 ang2 ang3 cent num a1 a2 reply 2pi rot 
                 old_cmdecho old_osmode old_error old_angbase old_angdir)  
  (command "undo" "begin")

  (prologue)
  (get_objects)
  (setq base nil) ;leave base an option for reply-loop
  (get_number)  
  (get_cent)
  (get_begin)
  (get_end)
  (set_angles)
  (draw_array)

  (setq done nil)
  (while (not done)
     (get_reply)
     )

  (prompt (strcat "\n" (itoa num) " sets in final array."))
  (epilogue)

  (command "undo" "end")
  (princ)
);defun

(princ)

;;==========================================
(defun prologue (/)
  (setq old_osmode (getvar "osmode"))
  (setvar "osmode" 0)

  (setq old_cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (setq old_angbase (getvar "angbase"))
  (setvar "angbase" 0)

  (setq old_angdir (getvar "angdir"))
  (setvar "angdir" 0)

  (setq old_error *error*)
  (setq *error* dap_error)

  (setq 2pi (* 2 pi))
  (setq rot "y")

);defun

;;==========================================
(defun epilogue (/)
  (setvar "osmode" old_osmode)
  (setvar "cmdecho" old_cmdecho)
  (setvar "angbase" old_angbase)
  (setvar "angdir" old_angdir)
  (setq *error* old_error)
)

;;==========================================
(defun get_objects (/)
  (prompt "Select object(s) to array: ")
  (setq ss1 (ssget))
);defun

;;==========================================
(defun get_number (/)
  (setq num 0)
  (while (< num 2)
     (setq num (getint "\nInitial number of sets in array (min=2): "))
     );while
);defun

;;==========================================
(defun get_cent (/)
  (setq cent nil)
  (while (not cent) 
         (setq cent (getpoint "\nSelect center point of polar array: "))
         )
);defun

;;==========================================
(defun get_base (/)
    (setq base (getpoint "\nSelect rotation base point of object(s)/<Enter> for default: "))
);defun

;;==========================================
(defun get_begin (/)
  (setq ang1 nil )
  (while (not ang1)
         (initget 32)
         (setq ang1 (angle cent (getpoint cent "\nStart of array angle: ")))
         )
);defun

;;==========================================
(defun get_end (/)
  (setq ang2 nil)
  (setvar "angbase" ang1)
  (while (not ang2)
         (initget 32)   
         (setq ang2 (getangle cent "\n  End of array angle (indicate point or enter angle): "))
         )
  (setvar "angbase" 0)
  );defun

;;==========================================
(defun set_angles (/)
(if (<= ang2 pi)
    (setq a1 ang2)
    (setq a1 (- (- 2pi ang2)))
    )
    (if (= ang2 0) (setq a1 2pi))  ;;so full circles will work

  (setq a2 a1) ;; actually draw with a2, remember a1 if needed for inversion
);defun

;;==========================================
(defun invert_angles (/)
   (if (/= a2 2pi)  ;leave full circles alone
     (if (= a1 ang2)
         (setq a1 (- (- 2pi ang2)))
         (setq a1 ang2)
         )
      )
  (setq a2 a1)
);defun

;;==========================================
(defun draw_array (/)
  (if (= a2 2pi)
      (setq ang3 "")
      (if (< a2 0)
        (setq ang3 (strcat "-" (angtos (abs a2) 2 8))) ;;maximize accuracy
        (setq ang3 (angtos a2 2 8))
        )
      )
  (if (not base)
    (command "array" ss1 "" "p" cent num ang3 rot)
    (command "array" ss1 "" "p" "b" base cent num ang3 rot) 
    )
  (prompt (strcat "\n" (itoa num) " sets in array."))
);defun

;;==========================================
(defun dap_error (msg)
  (if 
    (or 
      (/= msg "Function cancelled")
      (= msg "quit / exit abort") 
      )
    (princ)
    (princ (strcat "\nError: " msg))
    )
  (command) ;; to terminate AutoCAD's active ARRAY command
  (setvar "angbase" old_angbase)
  (setvar "angdir" old_angdir)
  (setvar "cmdecho" old_cmd)
  (setvar "osmode" old_osmode)
  (setq *error* old_error)
  (command "undo" "end")
  (princ)
);defun
  

;;==========================================
(defun get_reply (/)
    (initget  "Objects BAse Center BEgin End Invert Rotate Add Subtract Number Help eXit")
    (setq reply 
       (getkword "\nObjects/BAse/Center/BEgin/End/Invert/Rotate/Add/Subtract/Number/Help/eXit: "))

    (cond

        (
        (= reply "Objects")
        (progn
           (command "u")
           (get_objects)
           (draw_array)
           )
        )

        (
        (= reply "BAse")
        (progn
           (command "u")
           (get_base)
           (draw_array)
           )
        )

         (
        (= reply "Center")
        (progn
           (command "u")
           (get_cent)
           (set_angles)
           (draw_array)
           )
        )

        (
        (= reply "BEgin")
        (progn
           (command "u")
           (get_begin)
           (set_angles)
           (draw_array)
           )
        )

        (
        (= reply "End")
        (progn
           (command "u")
           (get_end)
           (set_angles)
           (draw_array)
           )
        )

        (
        (= reply "Add")
        (progn
           (command "u")
           (setq num (1+ num))
           (draw_array)
           )
         )

         (
         (= reply "Subtract")
         (progn
           (command "u")
           (setq num (1- num))
           (if (< num 2)
               (progn 
                 (prompt "\nToo few sets! Number reset to 2 (min).")
                 (setq num 2)
                 )
                )
            (draw_array)
           );progn
         )

        (
        (= reply "Number")
        (progn
           (command "u")
           (get_number)
           (draw_array)
           )
         )

          (
          (= reply "Invert")
          (progn
            (command "u")
            (invert_angles)
            (draw_array)
            )
          )

          (
          (= reply "Rotate")
          (progn
            (command "u")
            (if (= rot "y")
                (setq rot "n")
                (setq rot "y")
                )
            (draw_array)
            )
          )

        (
        (= reply "Help")
(alert
"Explanation of options:
  O  to change Objects to be arrayed
  BA to change rotation point for rotated objects
  C  to change Center point of array
  BE to change Beginning point of array angle
  E  to change Ending point of array angle
  I  to Invert the angle
  R  to Rotate/unrotate the copies
  A  to Add one copy to the array
  S  to Subtract one copy from the array
  N  to enter Number of copies directly
  H  to redisplay this Help information
  X  to eXit the routine.")
          )

          (
          (= reply "eXit")
          (setq done T)
          )

          (
          (= reply nil)
          (setq done T)
          )
       );cond                                       
);defun

(princ)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:s1 () (command "view" "s" "view 1"))
(defun c:s2 () (command "view" "s" "view 2"))
(defun c:s3 () (command "view" "s" "view 3"))
(defun c:s4 () (command "view" "s" "view 4"))
(defun c:s5 () (command "view" "s" "view 5"))
(defun c:s6 () (command "view" "s" "view 6"))
(defun c:s7 () (command "view" "s" "view 7"))
(defun c:s8 () (command "view" "s" "view 8"))
(defun c:s9 () (command "view" "s" "view 9"))

(defun c:v1 () (command "view" "r" "view 1"))
(defun c:v2 () (command "view" "r" "view 2"))
(defun c:v3 () (command "view" "r" "view 3"))
(defun c:v4 () (command "view" "r" "view 4"))
(defun c:v5 () (command "view" "r" "view 5"))
(defun c:v6 () (command "view" "r" "view 6"))
(defun c:v7 () (command "view" "r" "view 7"))
(defun c:v8 () (command "view" "r" "view 8"))
(defun c:v9 () (command "view" "r" "view 9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun C:LayerFiltersDelete ()
  (vl-Load-Com)
  (vl-Catch-All-Apply
    '(lambda ()
      (vla-Remove 
        (vla-GetExtensionDictionary
          (vla-Get-Layers 
            (vla-Get-ActiveDocument
              (vlax-Get-Acad-Object)
            )
          )
        )
      "ACAD_LAYERFILTERS")
    )
  )
  (princ "\nAll layer filters have been deleted.")
  (princ)
)
(defun C:LFD () (C:LayerFiltersDelete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:chh (/ DIS)
   (setvar "cmdecho" 0)
  (setq d (* (getreal "\nEnter Chamfer Distance: ")))
  (command "chamfer" "d" d "" "chamfer")
  (setvar "cmdecho" 1)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:ss ()
  (setvar "cmdecho" 0)
  (prompt "Draw line from end to per.  Pick end: ")
  (command "line" "end" pause "per" pause "")
  (setvar "cmdecho" 1)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:lnn ()
  (setq aa (getstring T " Enter Lenghten amount:"))
  (command "lengthen" "de" aa)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;CADALYST 12/05  Tip2076: mat.lsp   Attribute Entity Object Edit   (c) 2005 Craig Klein 


				;Match Attribute Layers
				;
				;This routine allows you to change an attributes layer
				;based on a pick of a new layer.
				;
(defun c:mat
     ( / pt sset os la1 la2 la3 layer lstr)
  	(setq os (getvar "osmode"))
	(defun *error* (s)
	  (setq *error* nil)
	  (setvar "osmode" os)
	  (princ "\n \n \nROUTINE CANCELED!!")
	)
	(setq la3 (nentsel "\nPick New Layer: "))           ;Nentsel selects Attrib vs block
        (setq la2 (car la3))
        (setq la1 (entget la2))
        (setq layer (cdr(assoc 8 la1)))
        (setq lstr (strcat "\nSelect Attribute to Apply New Layer <" layer ">:"))
        (setvar "cmdecho" 0)
	(setvar "osmode" 8)				; Use Node Osnap
        (setq pt (getpoint lstr))  	; As long as we get a
        (while pt                                    	; point value!
          (if  (setq sset (ssget pt))                	; Check for an entity!
             (progn
                (if (= nil (assoc 66 (entget (ssname sset 0))))
                  (progn                             	; If it does not have
                                                     	; Attributes worry!

                    (prompt "\nNot An Attributed Block")
                    (setq sset nil)
                  )
                )
             )
             (progn                                  	; If you dont find
               (prompt "\nNo Entity Found")          	; an entity at the
               (setq sset nil)                       	; given location
             )                                       	; worry!
          )
         (if (and pt sset)
          (progn                                     	; If all is well,
          (command ".attedit" "" "" "" "" pt "" )    	; Start the ATTEDIT
          (if layer (command "l" layer))             	; that has a value.
          (command "")                               	; Terminate the
          )                                          	; command.
         )
         (setq pt (getpoint "\nSelect Attribute: ")) 	; Get a new point!
        )
        (princ)                                         ; Cleanup nil
)
;
(princ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;Extends line to a point

(defun c:chg (/	     oldech oldhil lent	  lentlist	slope  px1
	      py1    px2    py2	   pp	  px3	 py3	px4    py4
	      dp1    dp2
	     )
  (setq oldech (getvar "cmdecho"))
  (setq oldhil (getvar "highlight"))
  (setvar "cmdecho" 0)
  (setvar "highlight" 1)
  (print)
  (princ " ChgLn LINE Extender ! ")
  (print)
  (setq lentlst (entsel "Pick the LINE +> "))
  (setq lent (entget (car lentlst)))
  (setq px1 (cadr (assoc 10 lent)))
  (setq py1 (caddr (assoc 10 lent)))
  (setq px2 (cadr (assoc 11 lent)))
  (setq py2 (caddr (assoc 11 lent)))
  (print)
  (princ "  The line end points are: ")
  (print)
  (princ "<")
  (princ px1)
  (princ ",")
  (princ py1)
  (princ "> - <")
  (princ px2)
  (princ ",")
  (princ py2)
  (princ ">")
  (print)
  (setq pp (getpoint "Pick the Extend to point +> "))
  (setq px3 (car pp))
  (setq py3 (cadr pp))
  (print)
  (princ " Extend to Point Chosen at <")
  (princ px3)
  (princ ",")
  (princ py3)
  (princ ">")
  (print)
					; Calculate new line endpoint <px4,py4> with following conditions
  (if (= px1 px2)
    (progn
      (setq px4 px1)
      (setq py4 py3)
    )
    (progn
					; Compute the slope of the line = (y dif)/(x dif)
      (setq slope (/ (- py2 py1) (- px2 px1)))
      (if (= slope 0)
	(progn
	  (setq px4 px3)
	  (setq py4 py1)
	)
	(progn
					; Compute <px4> component for extended line endpoint
					; ((px3/slope)+py3+(slope*px1) -py1)/((1/slope)+slope)
	  (setq	px4 (/ (+ (/ px3 slope)
			  (+ py3 (- (* slope px1) py1))
		       )
		       (+ (/ 1 slope) slope)
		    )
	  )
					; Compute <py4> component for extended line endpoint
					; (py1+(slope*(px4-px1)))
	  (setq py4 (+ py1 (* slope (- px4 px1))))
	)
      )
    )
  )
  (print)
  (princ "New line end point = <")
  (princ px4)
  (princ ",")
  (princ py4)
  (princ ">")
  (print)
					;  Compute the distance (dp1) from <px1,py1> to <px3,py3>
					;  Distance Formula =  dp1 = sqrt( (px3-px1)^2 + (py3-py1)^2 )
  (setq	dp1 (sqrt (+ (* (- px3 px1) (- px3 px1))
		     (* (- py3 py1) (- py3 py1))
		  )
	    )
  )
					;  Compute the distance (dp2) from <px2,py2> to <px3,py3>
  (setq	dp2 (sqrt (+ (* (- px3 px2) (- px3 px2))
		     (* (- py3 py2) (- py3 py2))
		  )
	    )
  )
  (if (<= dp1 dp2)
    (progn
      (setq lent (subst	(list '10 px4 py4 '0)
			(assoc 10 lent)
			lent
		 )
      )
      (entmod lent)
    )
    (progn
      (setq lent (subst	(list '11 px4 py4 '0)
			(assoc 11 lent)
			lent
		 )
      )
      (entmod lent)
    )
  )
  (setvar "CMDECHO" oldech)
  (setvar "HIGHLIGHT" oldhil)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:db5 (/)
  (acet-error-init
    (list
      (list "cmdecho" 0 "osmode" 0 "clayer" "HARDWARE")
      nil
    )
  )
  (setq scalar 1)
  (princ
    "\nThis function will draw a drawer box using the scale factor. \n"
  )
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
  (setq pt1 (polar pt0 (* pi 0.5) (* scalar height)))
  (setq pt2 (polar pt1 pi (* scalar 0.625)))
  (setq pt3 (polar pt2 (* pi 1.5) (* scalar (- height 0.75))))
  (setq pt4 (polar pt3 (* pi 1.5) (* scalar 0.25)))
  (setq pt5 (polar pt4 (* pi 1.5) (* scalar 0.5)))
  ;draw right end	
  (command "line" pt0 pt1 pt2 pt3 "")
  (command "line" pt4 pt5 pt0 "")
  (command "-layer" "set" "HARDWARE" "")
  (setq pt0 pt2)
  (setq pt1 (polar pt0 pi (* scalar (- width 1.25))))
  ; draw top line
  (command "line" pt0 pt1 "")
  (setq pt0 (polar pt1 (* pi 1.5) (* scalar height)))
  (setq pt1 (polar pt0 0.0 (* scalar (- width 1.25))))
  ; draw bottom line
  (command "line" pt0 pt1 "")
  (command "-layer" "set" "HARDWARE" "")
  (setq pt0 (polar pt1 0.0 (* scalar 0.25)))
  (setq pt0 (polar pt0 (* pi 0.5) (* scalar 0.5)))
  (setq pt1 (polar pt0 (* pi 0.5) (* scalar 0.25)))
  (setq pt2 (polar pt1 pi (* scalar (- width 0.75))))
  (setq pt3 (polar pt2 (* pi 1.5) (* scalar 0.25)))
  ; draw 1/4" bottom
  (command "line" pt0 pt1 pt2 pt3 pt0 "")
  (setq pt0 (polar pt0 (* pi 1.5) (* scalar 0.5)))
  (setq pt0 (polar pt0 pi (* scalar (- width 0.375))))
  (setq pt1 (polar pt0 (* pi 0.5) (* scalar height)))
  (setq pt2 (polar pt1 0.0 (* scalar 0.625)))
  (setq pt3 (polar pt2 (* pi 1.5) (* scalar (- height 0.75))))
  (setq pt4 (polar pt3 (* pi 1.5) (* scalar 0.25)))
  (setq pt5 (polar pt4 (* pi 1.5) (* scalar 0.5)))
  ; draw left end
  (command "line" pt0 pt1 pt2 pt3 "")
  (command "line" pt4 pt5 pt0 "")
  (acet-error-restore)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun spline-to-pline (/ i)
  (vl-load-com)
  (setq	*thisdrawing* (vla-get-activedocument
			(vlax-get-acad-object)
		      ) ;_ end of vla-get-activedocument
	*modelspace*  (vla-get-ModelSpace *thisdrawing*)
  ) ;_ end of setq
  (setq spline-list (get-spline))
  (setq i (- 1))
  (if spline-list
    (progn
      (setq msg "\nNumber of segments <100>: ")
      (initget 6)
      (setq num (getint msg))
      (if (or (= num 100) (= num nil))
	(setq num 100)
      ) ;_ end of if
      (repeat (length spline-list)
	(setq splobj (nth (setq i (1+ i)) spline-list))
	(convert-spline splobj num)
      ) ;_ end of repeat
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of spline-to-pline

(defun get-spline (/ spl-list obj spline no-ent i)
  (setq	spl-list nil
	obj	 nil
	spline	 "AcDbSpline"
	selsets	 (vla-get-selectionsets *thisdrawing*)
	ss1	 (vlax-make-variant "ss1")
  ) ;_ end of setq
  (if (= (vla-get-count selsets) 0)
    (setq ssobj (vla-add selsets ss1))
  ) ;_ end of if
  (vla-clear ssobj)
  (setq no-ent 1)
  (while no-ent
    (prompt "\nSelect splines: ")
    (vla-Selectonscreen ssobj)
    (if	(> (vla-get-count ssobj) 0)
      (progn
	(setq no-ent nil)
	(setq i (- 1))
	(repeat	(vla-get-count ssobj)
	  (setq
	    obj	(vla-item ssobj
			  (vlax-make-variant (setq i (1+ i)))
		) ;_ end of vla-item
	  ) ;_ end of setq
	  (cond
	    ((= (vlax-get-property obj "ObjectName") spline)
	     (setq spl-list
		    (append spl-list (list obj))
	     ) ;_ end of setq
	    )
	  ) ;_ end-of cond
	) ;_ end of repeat
      ) ;_ end of progn
      (prompt "\nNo entities selected, try again.")
    ) ;_ end of if
    (if	(and (= nil no-ent) (= nil spl-list))
      (progn
	(setq no-ent 1)
	(prompt "\nNo splines selected.")
	(quit)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of while  
  (vla-delete (vla-item selsets 0))
  spl-list
) ;_ end of get-spline

(defun convert-spline (splobj n / i)
  (setq	point-list   nil
	2Dpoint-list nil
	z-list	     nil
	spl-lyr	     (vlax-get-property splobj 'Layer)
	startSpline  (vlax-curve-getStartParam splobj)
	endSpline    (vlax-curve-getEndParam splobj)
	i	     (- 1)
  ) ;_ end of setq
  (repeat (+ n 1)
    (setq i (1+ i))
    (setq p (vlax-curve-getPointAtParam
	      splobj
	      (* i
		 (/ (- endspline startspline) n)
	      ) ;_ end of *
	    ) ;_ end of vlax-curve-getPointAtParam
    ) ;_ end of setq
    (setq 2Dp	       (list (car p) (cadr p))
	  2Dpoint-list (append 2Dpoint-list 2Dp)
	  point-list   (append point-list p)
	  z	       (caddr p)
	  z-list       (append z-list (list z))
    ) ;_ end of setq
  ) ;_ end of repeat
  (setq summ (apply '+ z-list))
  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble ; element type
	   (cons 0
		 (- (length point-list) 1)
	   ) ; array dimension
	 ) ;_ end of vlax-make-safearray
  ) ;_ end of setq
  (setq vert-array (vlax-safearray-fill arraySpace point-list))
  (vlax-make-variant vert-array)
  (if (and (= :vlax-true (vlax-get-property splobj 'IsPLanar))
	   (= summ 0.0)
      ) ;_ end of and
    (setq plobj	(add-polyline
		  2Dpoint-list
		  vla-AddLightweightPolyline
		) ;_ end of add-polyline
    ) ;_ end of setq
    (setq plobj	(add-polyline
		  point-list
		  vla-Add3DPoly
		) ;_ end of add-polyline
    ) ;_ end of setq
  ) ;_ end of if
  (vlax-put-property plobj 'Layer spl-lyr)
  (vla-delete splobj)
  (vlax-release-object splobj)
) ;_ end of convert-spline

(defun add-polyline (pt-list poly-func)
  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble
	   (cons 0
		 (- (length pt-list) 1)
	   ) ; array dimension
	 ) ;_ end of vlax-make-safearray
  ) ;_ end of setq
  (setq	vertex-array
	 (vlax-safearray-fill arraySpace pt-list)
  ) ;_ end of setq
  (vlax-make-variant vertex-array)
  (setq	plobj (poly-func
		*modelspace*
		vertex-array
	      ) ;_ end of poly-func
  ) ;_ end of setq
) ;_ end of add-polyline

(defun c:s2p ()
  (spline-to-pline)
  (princ)
) ;_ end of c:s2p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;dIMCIONTINUES
(defun c:RDIMCONTINUE ()
 (command "clayer" "dimension")
 (command "DIMCONTINUE")
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN C:as (/ x p v pval selset att1 att2 att3)
  (setvar "cmdecho" 0)
  (setq selset (ssget))
  (setq x (entsel "\nSelect object whose property is to be matched: "))
  (setq att1 (cdr (assoc 6 (entget (car x)))))
  (if (= (type att1) 'nil)
    (setq att1 "bylayer")
  )
  (setq att2 (cdr (assoc 8 (entget (car x)))))
  (setq att3 (cdr (assoc 62 (entget (car x)))))
  (if (= (type att3) 'nil)
    (setq att3 "bylayer")
  )
  (command "chprop" selset "" "lt" att1 "la" att2 "c" att3 "")
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:swap (/ cmd ss1 ss1bp ss2 ss2bp)
  (setq cmd (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (prompt " Select 1st items to swap: ")
  (setq ss1 (ssget))
  (initget 1)
  (setq	ss1bp
	 (getpoint "\nPick 1st base point:")
  )
  (terpri)
  (prompt "\nSelect 2nd items to swap places with 1st: ")
  (setq ss2 (ssget))
  (initget 1)
  (setq ss2bp (getpoint "\nPick 2nd base point:"))
  (command ".move" ss1	   ""	   ss1bp   ss2bp   ".move" ss2
	   ""	   ss2bp   ss1bp   ".move" ss1	   ""	   ss1bp
	   ss1bp
	  )
  (setvar "cmdecho" cmd)
  (princ)
)					; end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:1 ()
 (command "ZOOM" "ALL")
  (princ)
)
