;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_PartNumber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:PartNumber (/ part_number1 str part_number2)
	(if (>= (getvar "osmode") 16384)
		(progn
			(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Part")))
		)
		(progn
			(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Part")))
		)
	)
	(environment variable_list)
	(setq part_number1 1)
	(while (< 0 1)
		(prompt "\nSpecify insertion point for the part number symbol or...\n[ESC] to end function: \n")
		(command "-insert" "SYM_Part" pause "" "" "" "")
		(setq SYM_Part (entlast))
		(command "attedit" "yes" "" "" "" "last" "value" "replace" (strcat "" (itoa part_number1)) "")
		(setq insertionpoint (getvar "lastpoint"))
		(setq part_number2 (list 0.0 0.0 0.0))
		(while (= 'LIST (type part_number2))
			(initget 128)
			(setq part_number2 (getpoint (strcat "\nSpecify the part number <" (itoa part_number1) ">:")))
			(if (= part_number2 nil)
				(progn
					(setq part_number1 part_number1)
				)
				(progn
					(if (/= 'LIST (type part_number2))
						(setq part_number1 (atoi part_number2))
					)
				)
			)
		)
		(setq LeaderPoint "Fetzer")
		(while (= 'str (type LeaderPoint))
			(initget 128)
			(setq LeaderPoint (getpoint "\nSpecify the leader point, or <enter> for none:"))
			(if (= 'LIST (type LeaderPoint))
				(progn
					(command "erase" SYM_Part "")
					(command "mleader" insertionpoint (strcat "" (itoa part_number1)) LeaderPoint)
				)
				(progn
					(command "erase" SYM_Part "")
					(command "-insert" "SYM_Part" insertionpoint "" "" "" (strcat "" (itoa part_number1)))
				)
			)
		)
		(setq part_number1 (+ part_number1 1))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_PartNumber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_AssemblyNumber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:AssemblyNumber (/ assembly_number1 insertionpoint str assembly_number2)
	(if (>= (getvar "osmode") 16384)
		(progn
			(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Assembly")))
		)
		(progn
			(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Assembly")))
		)
	)
	(environment variable_list)
	(setq assembly_number1 1)
	(while (< 0 1)
		(prompt "\nSpecify insertion point for the part number symbol or...\n[ESC] to end function: \n")
		(command "-insert" "SYM_Assembly" pause "" "" "" "")
		(setq SYM_Assembly (entlast))
		(command "attedit" "yes" "" "" "" "last" "value" "replace" (strcat "A" (itoa assembly_number1)) "")
		(setq insertionpoint (getvar "lastpoint"))
		(setq assembly_number2 (list 0.0 0.0 0.0))
		(while (= 'LIST (type assembly_number2))
			(initget 128)
			(setq assembly_number2 (getpoint (strcat "\nSpecify the assembly number (you can specify the assembly number with or without the A) <A" (itoa assembly_number1) ">:")))
			(if (= assembly_number2 nil)
				(progn
					(setq assembly_number1 assembly_number1)
				)
				(progn
					(if (/= 'LIST (type assembly_number2))
						(setq assembly_number1 (atoi (vl-string-subst "" "A" (strcase assembly_number2))))
					)
				)
			)
		)
		(setq LeaderPoint "Fetzer")
		(while (= 'str (type LeaderPoint))
			(initget 128)
			(setq LeaderPoint (getpoint "\nSpecify the leader point, or <enter> for none:"))
			(if (= 'LIST (type LeaderPoint))
				(progn
					(command "erase" SYM_Assembly "")
					(command "mleader" insertionpoint (strcat "A" (itoa assembly_number1)) LeaderPoint)
				)
				(progn
					(command "erase" SYM_Assembly "")
					(command "-insert" "SYM_Assembly" insertionpoint "" "" "" (strcat "A" (itoa assembly_number1)))
				)
			)
		)
		(setq assembly_number1 (+ assembly_number1 1))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_AssemblyNumber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_ViewTitle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ViewTitle (/ ssl entinf phght mhght entity viewportdata xpscale vpscale vscale)
	(if (= (getvar "tilemode") 0)
		(progn
			(if (>= (getvar "osmode") 16384)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol")))
				)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol")))
				)
			)
			(environment variable_list)
			(setq ssl (ssget "x" '((0 . "VIEWPORT") (68 . 1))))
			(if ssl
				(progn
					(setq viewportdata (entsel "\nSelect a viewport for title."))
					(if viewportdata
						(progn
							(setq entinf (entget (car viewportdata)))
							(setq phght (cdr (assoc 41 entinf)))
							(setq mhght (cdr (assoc 45 entinf)))
							(setq xpscale (/ 1.00000 (/ phght mhght)))
							(setq vscale (rtos (* 12.00000 (/ 1 xpscale)) 5 6))
							(setq vpscale (strcat vscale "\"" "=1'-0\""))
							(if (= vpscale "12\"=1'-0\"")
								(setq vpscale "FULL")
							)
						)
						(progn (setq vpscale "1\"=1'-0\""))
					)
					(command "-insert" "sym_ViewTitle" (getpoint "\nPick insertion point: ") "" "" "" "" "" "" vpscale "")
					(setq entity (entlast))
					(command "ddatte" entity)
				)
				(progn (Alert "This command can only be called when a viewport is present."))
			)
			(restore variable_list)
		)
		(progn (Alert "This command can only be called from paper space."))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_ViewTitle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_DetailCallout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DetailCallout (/ DetailLetter1 InsertionPoint SYM_DetailCallout AttributeList Attribute DETAIL REFERENCE InsertionPoint LeaderPoint)
	(if (= (getvar "tilemode") 0)
		(progn
			(if (>= (getvar "osmode") 16384)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Detail")))
				)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Detail")))
				)
			)
			(environment variable_list)
			(command "-layer" "on" "symbol" "")
			(if (= (getvar "userr4") 0)
				(progn (setvar "userr4" 65))
				(progn)
			)
			(setq DetailLetter1 (chr (fix (getvar "userr4"))))
			(setq i 0)
			(while (< i 1)
				(prompt "\nSpecify insertion point for the diamond symbol or...\n[ESC] to end function: \n")
				(command "-insert" "SYM_DetailCallout" pause "" "" "" "" "")
				(setq InsertionPoint (getvar "LASTPOINT"))
				(setq SYM_DetailCallout (entlast))
				(command "ddatte" SYM_DetailCallout)
				(setq Attribute (entnext SYM_DetailCallout))
				(while (= "ATTRIB" (cdr (assoc 0 (setq AttributeList (entget Attribute)))))
					(if (= (cdr (assoc 2 AttributeList)) "DETAIL")
						(progn (setq DETAIL (cdr (assoc 1 AttributeList))))
					)
					(if (= (cdr (assoc 2 AttributeList)) "REFERENCE")
						(progn (setq REFERENCE (cdr (assoc 1 AttributeList))))
					)
					(setq Attribute (entnext Attribute))
				)
				(setq LeaderPoint "Fetzer")
				(while (= 'STR (type LeaderPoint))
					(initget 128)
					(setq LeaderPoint (getpoint "\nSpecify the leader point, or <enter> for none:"))
					(if (= 'LIST (type LeaderPoint))
						(progn
							(command "erase" SYM_DetailCallout "")
							(command "mleader" InsertionPoint DETAIL REFERENCE LeaderPoint)
						)
						(progn
							(command "erase" SYM_DetailCallout "")
							(command "-insert" "SYM_DetailCallout" InsertionPoint "" "" "" DETAIL REFERENCE)
						)
					)
				)
			)
		)
		(progn (Alert "This command can only be called from paper space."))
    )
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_DetailCallout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_Diamond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Diamond (/ i InsertionPoint SYM_Diamond Attribute AttributeList TAG NUMBER LeaderPoint)
	(if (= (getvar "tilemode") 0)
		(progn
			(if (>= (getvar "osmode") 16384)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Diamond" "attreq" 1)))
				)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Diamond" "attreq" 1)))
				)
			)
			(environment variable_list)
			(command "-layer" "on" "symbol" "")
			(setq i 0)
			(while (< i 1)
				(setvar "attreq" 0)
				(prompt "\nSpecify insertion point for the diamond symbol or...\n[ESC] to end function: \n")
				(command "-insert" "SYM_Diamond" pause "" "" "")
				(setq InsertionPoint (getvar "LASTPOINT"))
				(setq SYM_Diamond (entlast))
				(command "ddatte" SYM_Diamond)
				(setq Attribute (entnext SYM_Diamond))
				(while (= "ATTRIB" (cdr (assoc 0 (setq AttributeList (entget Attribute)))))
					(if (= (cdr (assoc 2 AttributeList)) "TAG")
						(progn (setq TAG (cdr (assoc 1 AttributeList))))
					)
					(if (= (cdr (assoc 2 AttributeList)) "NUMBER")
						(progn (setq NUMBER (cdr (assoc 1 AttributeList))))
					)
					(setq Attribute (entnext Attribute))
				)
				(setq LeaderPoint "Fetzer")
				(while (= 'STR (type LeaderPoint))
					(initget 128)
					(setq LeaderPoint (getpoint "\nSpecify the leader point, or <enter> for none:"))
					(if (= 'LIST (type LeaderPoint))
						(progn
							(setvar "attreq" 1)
							(command "erase" SYM_Diamond "")
							(command "mleader" InsertionPoint TAG NUMBER LeaderPoint)
						)
						(progn
							(setvar "attreq" 1)
							(command "erase" SYM_Diamond "")
							(command "-insert" "SYM_Diamond" InsertionPoint "" "" "" TAG NUMBER)
						)
					)
				)
			)
		)
		(progn (Alert "This command can only be called from paper space."))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_Diamond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_Molding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Molding (/ i InsertionPoint SYM_Molding MoldingNumber AttributeList LeaderPoint)
	(if (= (getvar "tilemode") 0)
		(progn
			(if (>= (getvar "osmode") 16384)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Molding")))
				)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Molding")))
				)
			)
			(environment variable_list)
			(prompt "\nSpecify insertion point for the molding symbol or...\n[ESC] to end function: \n")
			(command "-insert" "SYM_Molding" pause "" "" "" "")
			(setq SYM_Molding (entlast))
			(setq MoldingNumber "XX-X")
			(command "attedit" "yes" "" "" "" "last" "value" "replace" MoldingNumber "")
			(setq insertionpoint (getvar "lastpoint"))
			(setq MoldingNumber (list 0.0 0.0 0.0))
			(while (= 'LIST (type MoldingNumber))
				(initget 128)
				(setq MoldingNumber (getpoint (strcat "\nSpecify the molding number <XX-X>:")))
				(if	(= MoldingNumber nil)
					(progn (setq MoldingNumber "XX-X"))
					(progn
						(if (/= 'LIST (type MoldingNumber))
							(setq MoldingNumber MoldingNumber)
						)
					)
				)
			)
			(command "attedit" "yes" "" "" "" "last" "value" "replace" MoldingNumber "")
			(setq LeaderPoint "Fetzer")
			(while (= 'STR (type LeaderPoint))
				(initget 128)
				(setq LeaderPoint (getpoint "\nSpecify the leader point, or <enter> for none:"))
				(if	(= 'LIST (type LeaderPoint))
					(progn
						(command "erase" SYM_Molding "")
						(command "mleader" insertionpoint MoldingNumber LeaderPoint)
					)
					(progn
						(command "erase" SYM_Molding "")
						(command "-insert" "SYM_Molding" insertionpoint "" "" "" MoldingNumber)
					)
				)
			)
		)
		(progn (Alert "This command can only be called from paper space."))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_Molding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin sym_Revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Revision (/ origin i sret arclength dimscale arcstyle incangle lastpoint startpoint nextpoint readtype readvalue pt1 pt2 insertionpoint dist endpoint zpt1 zpt2 date pt3 pt4 pt5 pt6 cp1 cp2 ss1 ss2 ss3 ss4 ss5 ss6 entityname entitydatalist revisionnumber scalar movepoint newrevision currentlayout layouts layoutscount layoutname)
	(if (= (getvar "tilemode") 0)
		(progn
			(if (>= (getvar "osmode") 16384)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Revision")))
				)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- Revision")))
				)
			)
			(environment variable_list)
			(command "-layer" "on" "revision" "")
			(setq origin (list 0.0 0.0 0.0))
			(setq i 0)
			(while (< i 1)
				(initget "Cloud Revision Title eXit")
				(setq sret (getkword "\n[Cloud/Revision/Title/eXit] <eXit>: "))
				(if	(or (= sret "eXit") (= sret nil))
					(progn (setq i 1))
				)
				(if	(= sret "Cloud")
					(progn
						(setq arclength 0.375)
						(setq dimscale 1.0)
						(setq arcstyle "calligraphy")
						(setq incangle 110)
						(setq lastpoint (getpoint "\nSpecify start point for revision cloud: "))
						(if lastpoint
							(progn
								(setq startpoint lastpoint)
								(prompt "\nGuide crosshairs along cloud path...")
								(command "_.pline" lastpoint "_a" "_a" incangle lastpoint "_a" incangle)
							)
						)
						(while lastpoint
							(setq nextpoint (GrRead 1))
							(setq readtype (car nextpoint))
							(setq readvalue (cadr nextpoint))
							(cond	((or (= 5 readtype) (= 3 readtype))
										(setq nextpoint (cadr nextpoint))
										(if (or (> (distance lastpoint nextpoint) arclength) (= readtype 3))
											(progn (Command nextpoint "_a" incangle) (Setq lastpoint nextpoint))
											(progn
												(command nextpoint "_u")
												(if (= arcstyle "calligraphy")
													(command "_w" "0" (* arclength 0.10))
												)
												(command "_a" incangle)
											)
										)
										(if (> (Distance lastpoint nextpoint) (Distance startpoint nextpoint))
											(progn
												(command startpoint "_cl")
												;(command "chprop" "last" "" "color" "10" "")
												(setq lastpoint nil)
												(prompt "\nCloud finished.")
											)
										)
									)
								((and (= 11 readtype) (= 0 readvalue)) (command lastpoint "_u" "") (setq lastpoint nil))
								((and (= 2 readtype) (or (= 13 readvalue) (= 32 readvalue))) (command lastpoint "_u" "") (setq lastpoint nil))
								(T (prompt "\nMove the pointer to draw the cloud"))
							)
						)
					)
				)
				(if	(= sret "Revision")
					(progn
						(prompt "\nSpecify insertion point for the revision symbol or...\n[ESC] to end function: \n")
						(command "-insert" "SYM_Revision" pause "" "" "" "")
						(setq SYM_Revision (entlast))
						(setq RevisionNumber1 1)
						(command "attedit" "yes" "" "" "" "last" "value" "replace" RevisionNumber1 "")
						(setq insertionpoint (getvar "lastpoint"))
						(setq RevisionNumber2 (list 0.0 0.0 0.0))
						(while (= 'LIST (type RevisionNumber2))
							(initget 128)
							(setq RevisionNumber2 (getpoint (strcat "\nSpecify the revision number <" (itoa RevisionNumber1) ">:")))
							(if (= RevisionNumber2 nil)
								(progn (setq RevisionNumber1 RevisionNumber1))
								(progn
									(if (/= 'LIST (type RevisionNumber2))
										(setq RevisionNumber1 RevisionNumber2)
									)
								)
							)
						)
						(command "attedit" "yes" "" "" "" "last" "value" "replace" RevisionNumber1 "")
						(setq LeaderPoint "Fetzer")
						(while (= 'STR (type LeaderPoint))
							(initget 128)
							(setq LeaderPoint (getpoint "\nSpecify the leader point, or <enter> for none:"))
							(if (= 'LIST (type LeaderPoint))
								(progn
									(command "erase" SYM_Revision "")
									(command "mleader" insertionpoint RevisionNumber1 LeaderPoint)
								)
								(progn
									(command "erase" SYM_Revision "")
									(command "-insert" "SYM_Revision" pause "" "" "" RevisionNumber1)
								)							
							)
						)
					)
				)
				(if	(= sret "Title")
					(progn
						(setq date (menucmd "M=$(edtime,$(getvar,date),M/D/YYYY)"))
						(prompt "\nSpecify insertion point for the revision title symbol or...\n[ESC] to end function: \n")
						(command "-insert" "SYM_RevisionTitle" pause "" "" "" "" date "")
						(command "ddatte" "last")
					)
				) ; end if title
			)
			(setq ss1 nil)
			(setq ss2 nil)
			(setq ss3 nil)
			(setq ss4 nil)
			(setq ss5 nil)
			(setq ss6 nil)
		)
		(progn (Alert "This command can only be called from paper space."))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End sym_Revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin c:WorkOrderDrawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:WorkOrderDrawing (/ osmode clayer attdia Layers Layer cmleaderstyle Layer_On Layer_Lock Layer_Color Layer_Linetype Layer_Freeze Layer_Lineweight Layer_Plot i sret Area pt1 pt2 AreaName AreaDataList AreaType Symbol SymbolName SymbolDataList SymbolType InsertionPoint AttributeList Attribute WorkOrder LayerName LeaderPoint)
	(if (= (getvar "tilemode") 0)
		(progn
			(if (>= (getvar "osmode") 16384)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" (getvar "osmode") "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- WorkOrder")))
				)
				(progn
					(setq variable_list (list (list "cmdecho" 0 "osmode" 16384 "clayer" "symbol" "CMLEADERSTYLE" "Fetzer -- WorkOrder")))
				)
			)
			(environment variable_list)
			(initget 128)
			(setq sret (getpoint "\nWork Order <eXit>: "))
			(if (= sret nil)
				(progn (setq i 1))
				(progn
					(if (or  (= (strcase sret) "X") (= (strcase sret) "EXIT") (= sret ""))
						(progn (setq i 1))
						(progn
							(setq WorkOrder sret)
							(if (/= WorkOrder 0)
								(progn
									(setq Area nil)
									(initget "Rectangle Circle Ellipse convertPolyline eXit")
									(setq sret (getkword "\nWork Order Area: [Rectangle/Circle/Ellipse/convert existing Polyline/eXit] <eXit>: "))
									(if (or (= sret "eXit") (= sret nil))
										(progn (setq i 1))
									)
									(if (= sret "Rectangle")
										(progn
											(setq pt1 (getpoint "\nSpecify rectangle start point:"))
											(if (/= pt1 nil)
												(progn
													(setq pt2 (getcorner pt1 "\nSpecify rectangle end point:"))
													(if (/= pt2 nil)
														(progn
															(command "_.rectangle" pt1 pt2)
															(setq Area (entlast))
														)
													)
												)
											)
										)
									) ; end if rectangle
									(if (= sret "Circle")
										(progn
											(initget 128)
											(setq pt1 (getpoint "\nSpecify center point for circle or [3P/2P/Ttr (tan tan radius)]:"))
											(if (/= pt1 nil)
												(progn
													(if (= 'LIST (type pt1))
														(progn
															(command "_.circle" pt1 pause)
															(setq Area (entlast))
														)
													)
													(if (= 'STR (type pt1))
														(progn
															(if (or (= (strcase pt1) "3P") (= (strcase pt1) "3"))
																(progn
																	(command "_.circle" "3p" pause pause pause)
																	(setq Area (entlast))
																)
															)
															(if (or (= (strcase pt1) "2P") (= (strcase pt1) "2"))
																(progn
																	(command "_.circle" "2p" pause pause)
																	(setq Area (entlast))
																)
															)
															(if (or (= (strcase pt1) "T") (= (strcase pt1) "TTR"))
																(progn
																	(command "_.circle" "ttr" pause pause pause)
																	(setq Area (entlast))
																)
															)
														)
													)
												)
											)
										)
									) ; end if circle
									(if (= sret "Ellipse")
										(progn
											(initget 128)
											(setq pt1 (getpoint "\nSpecify axis endpoint of ellipse or [Center]:"))
											(if (/= pt1 nil)
												(progn
													(if (= 'LIST (type pt1))
														(progn
															(command "_.ellipse" pt1 pause pause)
															(setq Area (entlast))
														)
													)
													(if (= 'STR (type pt1))
														(progn
															(if (or (= (strcase pt1) "C") (= (strcase pt1) "CENTER"))
																(progn
																	(command "_.ellipse" "c" pause pause pause)
																	(setq Area (entlast))
																)
															)
														)
													)
												)
											)
										)
									) ; end if ellipse
									(if (= sret "convertPolyline")
										(progn
											(setq j 0)
											(while (< j 1)
												(setvar "errno" 7)
												(while (= (getvar "errno") 7)
													(setvar "errno" 0)
													(setq AreaName (car (entsel "\nSelect polyline: ")))
													(if (= (getvar "errno") 7)
														(prompt "\nNothing selected.")
													)
												)
												(if (/= AreaName nil)
													(progn
														(setq AreaDataList (entget AreaName '("ACAD")))
														(setq AreaType (cdr (assoc 0 AreaDataList)))
														(if (/= AreaType "LWPOLYLINE")
															(progn
																(prompt "\n*** Object selected is not a polyline. ***\n")
															)
															(progn
																(setq j 1)
																(setq Area AreaName)
															)
														)
													)
													(progn (setq j 1))
												)
											)
										)
									) ; end if convertPolyline
									(prompt "\nSpecify insertion point for the work order symbol: \n")
									(command "-insert" "SYM_WorkOrder" pause "" "" "" WorkOrder)
									(setq InsertionPoint (getvar "LASTPOINT"))
									(setq Symbol (entlast))
									(setq LeaderPoint "Fetzer")
									(while (= 'STR (type LeaderPoint))
										(initget 128)
										(setq LeaderPoint (getpoint "\nSpecify the leader point:"))
										(if (= 'LIST (type LeaderPoint))
											(progn
												(command "erase" Symbol "")
												(command "mleader" InsertionPoint WorkOrder LeaderPoint)
												(setq Symbol (entlast))
											)
											(progn (setq LeaderPoint "Fetzer"))
										)
									)
									(if (and (/= Area nil) (/= Symbol nil))
										(progn
											(setq LayerName (strcase (strcat "WO -- " WorkOrder)))
											(setq Layers (vla-get-layers (vla-get-ActiveDocument (vlax-get-Acad-Object))))
											(setq LayerExists nil)
											(vlax-for Layer Layers
												(setq Name (strcase (vla-get-name Layer)))
												(if (= LayerName Name)
													(setq LayerExists T)
												)
											)
											(if (= LayerExists nil)
												(command "-layer" "new" LayerName "color" "131" LayerName "Ltype" "Fetzer -- Work Order Extents" LayerName "LWeight" "0.6" LayerName "PStyle" "Solid" LayerName "Description" "Fetzer -- Work Order" LayerName "rEconcile" LayerName "")
											)
											(command "_.chprop" Symbol "" "_layer" LayerName "ltype" "continuous" "lweight" "0.3" "")
											(command "_.chprop" Area "" "_layer" LayerName "")
										)
									)
								)
							)
						)
					)
				)
			)
		)
		(progn (Alert "This command can only be called from paper space."))
	)
	(restore variable_list)
	(gc)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End c:WorkOrderDrawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;