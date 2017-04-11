;usings:
(require 'subr-x)



;clojure environment:
(defvar lein-project-directory "~")
(defvar lein-executer 
   (cond 
		((equal system-type "windows-nt") "lein.bat " )
		((equal system-type "gnu/linux") "lein " )
		(t "lein ")
	)
)
;(defvar lein-project-name "hello-world")
;(defvar lein-template "default")



;clojure tools:
(defun update-lein-directory () (setq lein-project-directory (read-directory-name "Lein project directory: ")) (print lein-project-directory))
(defun update-lein-project-name () (setq lein-project-name (read-string-not-whitespace "Lein project name")))
(defun update-lein-template () (setq lein-template (read-string-not-whitespace "Lein template name (default - for library; app - for application)" "default" "lein template name")))



;clojure commands:
(defun lein-help ()
  (interactive)
  (shell-command (concat lein-executer "help " (read-string "Input Lein params for help or RET: "))))
  
(defun lein-execute-command ()
	(interactive)
	(let ((default-directory lein-project-directory))
		(shell-command 
			(concat lein-executer (read-string "Input Lein params: "))
		)
	)
)

(defun set-lein-project-directory ()
	(interactive)
	(let ((default-directory lein-project-directory))
		(update-lein-directory)
	)
)

(defun lein-new-project ()
	(interactive)
	(let ((default-directory lein-project-directory))
		(update-lein-directory)
		(read-string (concat "Directory '" lein-project-directory "'. Press RET"))
		(if (file-exists-p lein-project-directory) nil (make-directory lein-project-directory))
		(update-lein-template)
		(update-lein-project-name)
		(let ((default-directory lein-project-directory))
			;(if 
				;(eq 0
					(shell-command 
						(concat lein-executer "new " lein-template " " lein-project-name )
					)
				;)
				(setq lein-project-directory (path-concat lein-project-directory lein-project-name))
				;nil
			;)
		)
	)
)

(defun lein-full-compile-project ()
	(interactive)
	(let ((default-directory lein-project-directory))
		(shell-command 
			(concat lein-executer " clean && " lein-executer " deps && " lein-executer " compile && " lein-executer " uberjar")
		)
	)
)

(defun lein-run-project ()
	(interactive)
	(let ((default-directory lein-project-directory))
		(shell-command 
			(concat lein-executer " run")
		)
	)
)

(defun lein-interactive-repl-project ()
	(interactive)
	(let ((default-directory lein-project-directory))
		(shell-command 
			(concat lein-executer " repl")
		)
	)
)



;clojure menu:
(make-main-menu clojure 'tools)
(make-menu clojure lein-interactive-repl-project "M-[ i")
(make-menu clojure lein-run-project "M-[ r")
(make-menu clojure lein-full-compile-project "M-[ f")
(make-menu clojure lein-new-project "M-[ n")
(make-menu clojure set-lein-project-directory "M-[ d")
(make-menu clojure lein-execute-command "M-[ l")
(make-menu clojure lein-help "M-[ h")
;(make-menu clojure shell-command "M-[ s")