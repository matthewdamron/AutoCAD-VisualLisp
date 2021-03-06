;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Event Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun environment (variable_list /)
	(if (= (car (cdr variable_list)) T)
		(progn (command "undo" "begin"))
	)
	(setq old_error *error*)
	(setq *error* error)
	(setq n 0)
	(setq variable_environment (list (nth n (car variable_list))))
	(setq i (length (car variable_list)))
	(while (< n i)
		(if	(= n 0)
			(progn
				(setq variable (list (getvar (nth n (car variable_list)))))
				(setq variable_environment (append variable_environment variable))
			)
			(progn
				(setq variable_environment (append variable_environment (list (nth n (car variable_list)))))
				(setq variable (list (getvar (nth n (car variable_list)))))
				(setq variable_environment (append variable_environment variable))
			)
		)
		(setvar (nth n (car variable_list)) (nth (+ n 1) (car variable_list)))
		(setq n (+ n 2))
	)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun restore (variable_list /)
	(if (= (car (cdr variable_list)) T)
		(progn (command "undo" "end"))
	)
	(setq n 0)
	(setq i (length (car variable_list)))
	(while (< n i)
		(setvar (nth n (car variable_list)) (nth (+ n 1) variable_environment))
		(setq n (+ n 2))
	)
	(setq *error* old_error)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Error (ErrorMessage /)
	(if (/= ErrorMessage "Function cancelled")
		(princ (strcat "\nError: " ErrorMessage))
	)
	(if (= (car (cdr variable_list)) T)
		(progn
			(command-s "undo" "end")
			(command-s "u")
		)
	)
	(setq n 0)
	(setq i (length (car variable_list)))
	(while (< n i)
		(setvar	(nth n (car variable_list)) (nth (+ n 1) variable_environment))
		(setq n (+ n 2))
	)
	(setq *error* old_error)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Event Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;