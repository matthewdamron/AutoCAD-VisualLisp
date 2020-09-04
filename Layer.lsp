;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Layer Director
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
layerdirector:data
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
;;  Command Pattern  |  Layer Name    |       Description       |    Colour    |   Linetype   |    Lineweight    |       Plot       |    Plot Style    ;;
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
;;     [string]      |   [string]     |         [string]        | 0 < int <256 |   [string]   | -3 = Default     |  1 = Will Plot   |     [string]     ;;
;;                   |                |     Use "" for none     |              |              |  0 <= int <= 211 |  0 = Won't Plot  |  Use nil for CTB ;;
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
	'(
		("[DM]TEXT,TEXT"	"NOTE"			"Fetzer -- Annotation"			150			"Continuous"		30		1		nil)
		("DIM*"				"DIMENSION"		"Fetzer -- Dimension"			142			"Continuous"		05		1		nil)
	)
layerdirector:forcelayprops nil
layerdirector:sysvars
	'(
		(cecolor				"bylayer")
		(celtype				"bylayer")
		(celweight				-1)
		(cetransparency			-1)
	)
layerdirector:xreflayer nil
layerdirector:printcommand nil
)
;;  Commands:  [ LDON / LDOFF ]                                         ;;
(defun c:ldon  nil (LM:layerdirector  t ))
(defun c:ldoff nil (LM:layerdirector nil))
;;----------------------------------------------------------------------;;

(if layerdirector:sysvars
	(setq layerdirector:sysvars
		(apply 'mapcar
			(cons 'list
				(vl-remove-if-not '(lambda ( x ) (getvar (car x)))
					layerdirector:sysvars
				)
			)
		)
	)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector ( on )
	(foreach grp (vlr-reactors :vlr-command-reactor :vlr-lisp-reactor)
		(foreach obj (cdr grp)
			(if (= "LM:layerdirector" (vlr-data obj))
				(vlr-remove obj)
			)
		)
	)
	(or
		(and on
			(vlr-command-reactor "LM:layerdirector"
				'(
					(:vlr-commandwillstart . LM:layerdirector:set)
					(:vlr-commandended     . LM:layerdirector:reset)
					(:vlr-commandcancelled . LM:layerdirector:reset)
					(:vlr-commandfailed    . LM:layerdirector:reset)
				)
			)
			(vlr-lisp-reactor "LM:layerdirector"
				'(
					(:vlr-lispwillstart . LM:layerdirector:set)
					(:vlr-lispended     . LM:layerdirector:reset)
					(:vlr-lispcancelled . LM:layerdirector:reset)
				)
			)
			(princ "\nLayer Director enabled.")
		)
		(princ "\nLayer Director disabled.")
	)
	(princ)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:lispcommand ( str )
	(if (wcmatch str "(C:*)") (substr str 4 (- (strlen str) 4)) str)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:set ( obj arg / lst tmp )
	(if (and (setq arg (car arg))
			 (setq arg (LM:layerdirector:lispcommand (strcase arg)))
			 (not (wcmatch arg "U,UNDO,NUDGE,3DORBITTRANSPARENT,SETVAR"))
		)
		(progn
			(if (and (setq lst (cdar (vl-member-if '(lambda ( x ) (wcmatch arg (strcase (car x)))) layerdirector:data)))
					 (setq tmp (LM:layerdirector:createlayer lst))
					 (zerop (logand 1 (cdr (assoc 70 tmp))))
				)
				(progn
					(setq layerdirector:oldlayer (getvar 'clayer)
						layerdirector:oldvars  (mapcar 'getvar (car layerdirector:sysvars))
					)
					(if layerdirector:sysvars
						(apply 'mapcar (cons 'setvar layerdirector:sysvars))
					)
					(setvar 'clayer (car lst))
				)
			)
			(if (and (= 'list (type layerdirector:xreflayer)) (wcmatch arg "XATTACH,CLASSICXREF"))
				(setq layerdirector:entlast (LM:layerdirector:entlast))
			)
			(if layerdirector:printcommand (print arg))
		)
	)
	(princ)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:reset ( obj arg / ent tmp )
	(if (or (null (car arg)) (not (wcmatch (strcase (car arg)) "U,UNDO,NUDGE,3DORBITTRANSPARENT,SETVAR")))
		(progn
			(if
				(and
					(= 'str (type layerdirector:oldlayer))
					(setq tmp (tblsearch "layer" layerdirector:oldlayer))
					(zerop (logand 1 (cdr (assoc 70 tmp))))
				)
				(setvar 'clayer layerdirector:oldlayer)
			)
			(mapcar 'setvar (car layerdirector:sysvars) layerdirector:oldvars)
			(if
				(and
					(car arg)
					(= 'list (type layerdirector:xreflayer))
					(wcmatch (strcase (car arg)) "XATTACH,CLASSICXREF")
				)
				(if (= 'ename (type (setq ent layerdirector:entlast)))
					(while (setq ent (entnext ent)) (LM:layerdirector:xreflayer ent layerdirector:xreflayer))
					(LM:layerdirector:xreflayer (entlast) layerdirector:xreflayer)
				)
			)
			(setq layerdirector:entlast  nil
				layerdirector:oldvars  nil
				layerdirector:oldlayer nil
			)
		)
	)
	(princ)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:xreflayer ( ent lst / enx lay obj xrf )
	(if
		(and
			(setq enx (entget ent))
			(= "INSERT" (cdr (assoc 0 enx)))
			(setq xrf   (cdr (assoc 2 enx))
				lay   (strcat (car lst) xrf (cadr lst))
			)
			(= 4 (logand 4 (cdr (assoc 70 (tblsearch "block" xrf)))))
			(LM:layerdirector:createlayer (cons lay (cddr lst)))
			(setq obj (vlax-ename->vla-object ent))
			(vlax-write-enabled-p obj)
		)
		(vla-put-layer obj lay)
	)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:entlast ( / ent tmp )
	(setq ent (entlast))
	(while (setq tmp (entnext ent)) (setq ent tmp))
	ent
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:createlayer ( lst / def )
	(if (or layerdirector:forcelayprops (not (setq def (tblsearch "layer" (car lst)))))
		(apply
			'(lambda ( lay des col ltp lwt plt pst / dic )
				(	(lambda ( def / ent )
						(if (setq ent (tblobjname "layer" (car lst)))
							(entmod (cons (cons -1 ent) def))
							(entmake def)
						)
					)
					(vl-list*
						'(000 . "LAYER")
						'(100 . "AcDbSymbolTableRecord")
						'(100 . "AcDbLayerTableRecord")
						'(070 . 0)
						(cons 002 lay)
						(cons 062 (if (< 0 col 256) col 7))
						(cons 006 (if (LM:layerdirector:loadlinetype ltp) ltp "Continuous"))
						(cons 370 (if (or (= -3 lwt) (<= 0 lwt 211)) lwt -3))
						(cons 290 plt)
						(append
							(if (and (= 'str (type pst))
									(zerop (getvar 'pstylemode))
									(setq dic (dictsearch (namedobjdict) "acad_plotstylename"))
									(setq dic (dictsearch (cdr (assoc -1 dic)) pst))
								)
								(list (cons 390 (cdr (assoc -1 dic))))
							)
							(if (and des (/= "" des))
								(progn (regapp "AcAecLayerStandard")
									(list
										(list -3
											(list
												"AcAecLayerStandard"
												'(1000 . "")
												(cons 1000 des)
											)
										)
									)
								)
							)
						)
					)
				)
			)
			lst
		)
		def
	)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:loadlinetype ( ltp )
	(eval
		(list 'defun 'LM:layerdirector:loadlinetype '( ltp )
			(list 'cond
				'(	(tblsearch "ltype" ltp) ltp)
				(list
					(list 'vl-some
						(list 'quote
							(list 'lambda '( lin )
								(list 'vl-catch-all-apply ''vla-load
									(list 'list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) 'ltp 'lin)
								)
								'(tblsearch "ltype" ltp)
							)
						)
						(list 'quote
							(vl-remove-if
								'(lambda ( x )
									(member (strcase x t)
										(if (zerop (getvar 'measurement))
											'("acadiso.lin"  "iso.lin") ;; Known metric   .lin files
											'("acad.lin" "default.lin") ;; Known imperial .lin files
										)
									)
								)
								(apply 'append
									(mapcar
										'(lambda ( dir ) (vl-directory-files dir "*.lin" 1))
										(vl-remove "" (LM:layerdirector:str->lst (getenv "ACAD") ";"))
									)
								)
							)
						)
					)
					'ltp
				)
			)
		)
	)
	(LM:layerdirector:loadlinetype ltp)
)
;;----------------------------------------------------------------------;;

(defun LM:layerdirector:str->lst ( str del / pos )
	(if (setq pos (vl-string-search del str))
		(cons (substr str 1 pos) (LM:layerdirector:str->lst (substr str (+ pos 1 (strlen del))) del))
		(list str)
	)
)
;;----------------------------------------------------------------------;;

(	(lambda ( ) (vl-load-com)
		(if (= 'list (type s::startup))
			(if (not (member '(LM:layerdirector t) s::startup))
				(setq s::startup (append s::startup '((LM:layerdirector t))))
			)
			(defun-q s::startup nil (LM:layerdirector t))
		)
		(princ)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Layer Director
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Current Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:SetLayer_0_Current (/ layer)
	(setq layer "0")
	(change_layer layer)
)
;
(defun c:SetLayer_Dimension_Current (/ layer)
	(setq layer "Dimension")
	(change_layer layer)
)
;
(defun c:SetLayer_Hardware_Current (/ layer)
	(setq layer "Hardware")
	(change_layer layer)
)
;
(defun c:SetLayer_Hatch_Current (/ layer)
	(setq layer "Hatch")
	(change_layer layer)
)
;
(defun c:SetLayer_HatchFS_Current (/ layer)
	(setq layer "Hatch -- FS")
	(change_layer layer)
)
;
(defun c:SetLayer_LinetypeCenter_Current (/ layer)
	(setq layer "Linetype -- Center")
	(change_layer layer)
)
;
(defun c:SetLayer_LinetypeDotted_Current (/ layer)
	(setq layer "Linetype -- Dotted")
	(change_layer layer)
)
;
(defun c:SetLayer_LinetypeHidden_Current (/ layer)
	(setq layer "Linetype -- Hidden")
	(change_layer layer)
)
;
(defun c:SetLayer_LinetypePhantom_Current (/ layer)
	(setq layer "Linetype -- Phantom")
	(change_layer layer)
)
;
(defun c:SetLayer_Machining_Current (/ layer)
	(setq layer "Machining")
	(change_layer layer)
)
;
(defun c:SetLayer_Note_Current (/ layer)
	(setq layer "Note")
	(change_layer layer)
)
;
(defun c:SetLayer_Part_Current (/ layer)
	(setq layer "Part")
	(change_layer layer)
)
;
(defun c:SetLayer_PartD_Current (/ layer)
	(setq layer "Part -- D")
	(change_layer layer)
)
;
(defun c:SetLayer_Revision_Current (/ layer)
	(setq layer "Revision")
	(change_layer layer)
)
;
(defun c:SetLayer_Sheet_Current (/ layer)
	(setq layer "Sheet")
	(change_layer layer)
)
;
(defun c:SetLayer_SheetField_Current (/ layer)
	(setq layer "Sheet -- Field")
	(change_layer layer)
)
;
(defun c:SetLayer_SheetXPlot_Current (/ layer)
	(setq layer "Sheet -- XPlot")
	(change_layer layer)
)
;
(defun c:SetLayer_Structure_Current (/ layer)
	(setq layer "Structure")
	(change_layer layer)
)
;
(defun c:SetLayer_StructureD_Current (/ layer)
	(setq layer "Structure -- D")
	(change_layer layer)
)
;
(defun c:SetLayer_StructureGridline_Current (/ layer)
	(setq layer "Structure -- Gridline")
	(change_layer layer)
)
;
(defun c:SetLayer_StructureHatch_Current (/ layer)
	(setq layer "Structure -- Hatch")
	(change_layer layer)
)
;
(defun c:SetLayer_Symbol_Current (/ layer)
	(setq layer "Symbol")
	(change_layer layer)
)
;
(defun c:SetLayer_Table_Current (/ layer)
	(setq layer "Table")
	(change_layer layer)
)
;
(defun c:SetLayer_Viewport_Current (/ layer)
	(setq layer "Viewport")
	(change_layer layer)
)
;
(defun c:SetLayer_XPlot_Current (/ layer)
	(setq layer "XPlot")
	(change_layer layer)
)
;
(defun c:SetLayer_XRef_Current (/ layer)
	(setq layer "XRef")
	(change_layer layer)
)
;
(defun change_layer (layer/)
	(progn
		(setvar "clayer" layer)
		(princ (strcat "\nLayer has been changed to " layer "."))
	)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Current Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin LineTypeDisplay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:LineTypeDisplay (/)
	(setvar "cmdecho" 0)
	(progn
		(if (= (getvar "USERI1") 1)
			(progn
				(command "-layer" "Ltype" "Continuous" "Linetype -- Dotted" "")
				(setvar "USERI1" 0)
				(princ "\nThickdot is turned OFF")
			)
			(progn
				(command "-layer" "Ltype" "Fetzer -- Dotted" "Linetype -- Dotted" "")
				(setvar "USERI1" 1)
				(princ "\nThickdot is turned ON")
			)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin LineTypeDisplay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin ChangeLayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ChangeLayer (/ OldError ErrorMessage cmdecho ss e layer)
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
;;;;; End variable definition for error handling
;;;;; Begin setting variables
;;;;; End setting variables
	(princ "Select objects for layer change: \n")
	(setq ss (ssget))
	(if ss
		(progn
			(setq e (car (entsel "Pick a source object on the desired layer [Enter to use current layer]: \n")))
			(if e
				(progn
					(setq e (entget e))
					(setq layer (cdr (assoc 8 e)))
					(command "change" ss "" "prop" "layer" layer "")
				)
				(progn
					(setq layer (getvar "clayer"))
					(command "change" ss "" "properties" "layer" layer "")
				)
			)
		)
	)
;;;;; Begin if there was not an error, restore variables
	(command "undo" "end")
	(setvar "cmdecho" cmdecho)
;;;;; End if there was not an error, restore variables
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End ChangeLayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin MatchProp_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:MatchProp_Fetzer (/ selset x att1 att2 att3)
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
	(setvar "cmdecho" 1)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;end MatchProp_Fetzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;