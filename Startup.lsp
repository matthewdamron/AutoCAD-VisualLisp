;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                              Begin StartUp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setvar "cmdecho" 0)
(vl-load-com)

;Location of Local Support Path
(setq supportloc "C:\\CAD Support\\AutoCAD")

;Location of AutoCAD Install
(setq acadloc (vl-registry-read (strcat "HKEY_LOCAL_MACHINE\\" (vlax-product-key)) "ACADLOCATION"))

;Set up Preferences Files
(setq *files* (vla-get-files (vla-get-preferences (vlax-get-acad-object))))

;Load dotNet and Lisp Files
(command "netload" (strcat supportloc "\\Users\\Fetzer\\FetzerAutoCAD.dll"))
(load (strcat supportloc "\\Visual Lisp\\CNC.fas"))
(load (strcat supportloc "\\Visual Lisp\\Dimension.fas"))
(load (strcat supportloc "\\Visual Lisp\\EndGrain.fas"))
(load (strcat supportloc "\\Visual Lisp\\FSDetail.fas"))
(load (strcat supportloc "\\Visual Lisp\\HatchMaterial.fas"))
(load (strcat supportloc "\\Visual Lisp\\Layer.fas"))
(load (strcat supportloc "\\Visual Lisp\\Symbol.fas"))
(load (strcat supportloc "\\Visual Lisp\\Utility.fas"))
(load (strcat supportloc "\\Visual Lisp\\EventHandler.fas"))

;Create User Folders on Local Machine
(vl-mkdir (strcat supportloc "\\Users\\" (getvar "loginname")))
(vl-mkdir (strcat supportloc "\\Users\\" (getvar "loginname") "\\Tool Palettes"))

;Support File Search Path
(setq sfsp	(strcat
				supportloc "\\Users\\" (getvar "loginname") ";"
				supportloc "\\Users\\Fetzer;"
				supportloc "\\Blocks;"
				supportloc "\\Icons;"
				supportloc "\\Slides;"
				supportloc "\\Visual LISP;"
				supportloc "\\Fonts\\SHK Fonts;"
				(getvar "ROAMABLEROOTPREFIX") "support;"
				acadloc "\\support;"
				acadloc "\\support\\en-US;"
				acadloc "\\fonts;"
				acadloc "\\help;"
				acadloc "\\express;"
				acadloc "\\color"
			)
)
(vla-put-SupportPath *files* sfsp)

;Trusted Locations
(setvar "TrustedPaths"	(strcat
							supportloc "\\dotNet;"
							supportloc "\\Visual LISP"
						)
)

;Device Driver File Search Path
(vla-put-DriversPath *files* (strcat supportloc "\\drv"))

;Custom Icon Location
(vla-put-CustomIconPath *files* (strcat supportloc "\\Icons"))

;Default Internet Location
(vla-put-DefaultInternetURL *files* (strcat "sandbox/home.php"))

;Custom Dictionary File
(setvar "DCTCUST" (strcat supportloc "\\Users\\Fetzer\\Fetzer.cus"))

;Font Mapping File
(setvar "FONTMAP" (strcat supportloc "\\Users\\Fetzer\\Fetzer.fmp"))

;Printer Configuration Search Path
(vla-put-PrinterConfigPath *files* (strcat supportloc "\\Plotters"))

;Printer Description File Search Path
(vla-put-PrinterDescPath *files* (strcat supportloc "\\Plotters\\PMP Files"))

;Plot Style Table Search Path
(vla-put-PrinterStyleSheetPath *files* (strcat supportloc "\\Plotters\\Plot Styles"))

;Drawing Template File Location
(vla-put-TemplateDwgPath *files* (strcat supportloc "\\Templates"))

;Default Template File Name for QNEW
(vla-put-QNewTemplateFile *files* (strcat supportloc "\\Templates\\Fetzer.dwt"))

;Tool Palettes File Location
(setvar "*_ToolPalettePath" (strcat
								supportloc "\\Users\\" (getvar "loginname") "\\Tool Palettes;"
								supportloc "\\Users\\Fetzer\\Tool Palettes"
								
							)
)

;Replace Fetzer Drawing Setup
(command "INSERT" (strcat supportloc "\\Blocks\\DWG_DrawingSetup.dwg") "0,0,0" "" "" "")
(command "ERASE" "LAST" "")
(command "REPLACEBLOCK")

;Set Variables and Options
(setenv "Background" "0")
(setenv "Layout background" "0")
(setenv "XhairPickboxEtc" "16777215")
(setenv "LayoutXhairPickboxEtc" "16777215")
(setvar "SECURELOAD" 0)
(setvar "STARTMODE" 0)
(setvar "NAVBARDISPLAY" 0)
(setvar "SAVETIME" 10)
(setvar "ISAVEBAK" 0)
;LTGAPSELECTION
(command "HPLINETYPE" "ON")
(setvar "SELECTIONEFFECT" 0)
(setvar "PRESELECTIONEFFECT" 0)
(setvar "BACKGROUNDPLOT" 2)
(setvar "PLINEWID" 0)
(setvar "AUNITS" 0)
(setvar "AUPREC" 8)
(setvar "COORDS" 0)
(setvar "INSUNITS" 1)
(setvar "LUNITS" 2)
(setvar "LUPREC" 8)
(setvar "LWUNITS" 1)
(setvar "LWDISPLAY" 0)
(setvar "LTSCALE" 1)
(setvar "PICKAUTO" 1)
(setvar "PICKDRAG" 0)
(setvar "FIELDDISPLAY" 0)
(setvar "TEXTSIZE" 0.125)
(setvar "ATTDIA" 0)
(setvar "ATTREQ" 1)
;(setvar "TEXTALLCAPS" ON)
(command "RIBBONCLOSE")
(setvar "MENUBAR" 1)
(command "viewres" "yes" "20000")
(command "wipeout" "frames" "off")
(command "ucsicon" "off")
(command "_-PLOTSTAMP" "_LOG" "_NO" "PLOT.LOG" "")
(setvar "modemacro" "-{  Fetzer  }-  $(edtime, $(getvar,date), MON-D-YYYY)  --  OSMODE = $(getvar,osmode)")
(setvar "cmdecho" 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                End StartUp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;