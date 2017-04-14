					;usings:
(require 'subr-x)


					;clojure environment:
(defvar lein-project-directory "~")

(defvar lein-executer 
  (cond 
   ((equal system-type "windows-nt") "lein.bat " )
   ((equal system-type "gnu/linux") "lein " )
   (t "lein ")))

(defvar lein-project-file "project.clj")
(defvar lein-project-name nil)
(defvar lein-project-version nil)


					;clojure tools:
(defun read-lein-directory ()
  (setq lein-project-directory (read-directory-name "Lein project directory: "))
  (message "Lein project directory is %s" lein-project-directory))

(defun read-lein-project-name ()
  (setq lein-project-name (read-string-not-whitespace "Lein project name")))

(defun load-lein-project-configuration ()
  (let ((project_text (get-string-from-file (lein-project-file-path))))
    (let ((match_result (string-match "[ \t\n]*([ \t\n]*defproject[ \t\n]+\\([-._0-9a-zA-Z]+\\)[ \t\n]*\"\\([-._0-9a-zA-Z]+\\)\"" project_text)))
      (if match_result
	  (progn
	    (setq lein-project-name (match-string 1 project_text))
	    (setq lein-project-version (match-string 2 project_text)))
	(error "Can't load project configuration")))))

(defun read-lein-template ()
  (setq lein-template (read-string-not-whitespace "Lein template name (default - for library; app - for application)" "default" "lein template name")))

(defun lein-project-file-path ()
  (path-concat lein-project-directory lein-project-file))

(defun check-opened-project ()
  (unless (file-exists-p (lein-project-file-path)) (error (concat "Not found project file at '" lein-project-directory "'"))))


					;clojure commands:
(defun lein-help ()
  (interactive)
  (shell-command (concat lein-executer "help " (read-string "Input Lein params for help or RET: "))))

(defun lein-execute-command ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (shell-command (concat lein-executer (read-string "Input Lein params: ")))))

(defun lein-set-project-directory ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (read-lein-directory)
    (check-opened-project)
    (load-lein-project-configuration)))

(defun lein-new-project ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (read-lein-directory)
    (read-string (concat "Directory '" lein-project-directory "'. Press RET for project creation in this subdirectory"))
    (read-lein-project-name)
    (read-lein-template)
    (if (file-exists-p lein-project-directory) nil (make-directory lein-project-directory))
    (let ((default-directory lein-project-directory))
      (shell-command 
       (concat lein-executer "new " lein-template " " lein-project-name ))
      (setq lein-project-directory (path-concat lein-project-directory lein-project-name))
      (load-lein-project-configuration))))

(defun lein-full-compile-project ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (check-opened-project)
    (load-lein-project-configuration)
    (shell-command 
     (concat lein-executer " clean && " lein-executer " deps && " lein-executer " compile && " lein-executer " uberjar"))))

(defun lein-run-project ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (check-opened-project)
    (shell-command 
     (concat lein-executer " run"))))

;(defun lein-execute-project ()
;  (interactive)
;  (let ((default-directory lein-project-directory))
;    (check-opened-project)
;    (shell-command 
;     (concat lein-executer " run"))))

(defun lein-project-edit ()
  (interactive)
  (check-opened-project)
  (find-file (lein-project-file-path)))

(defun lein-view-project-directory ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (dired (list "Current Lein project directory" lein-project-directory))))

(defun lein-interactive-repl-project ()
  (interactive)
  (error "unimplemented")
  (let ((default-directory lein-project-directory))
    (check-opened-project)
    (shell-command 
     (concat lein-executer " repl"))))


					;clojure menu:
(make-main-menu clojure 'tools)
(make-menu clojure lein-interactive-repl-project "M-[ i")
(make-menu clojure lein-run-project "M-[ r")
(make-menu clojure lein-full-compile-project "M-[ f")
(make-menu clojure lein-project-edit "M-[ p")
(make-menu clojure lein-view-project-directory "M-[ v")
(make-menu clojure lein-new-project "M-[ n")
(make-menu clojure lein-set-project-directory "M-[ d")
(make-menu clojure lein-execute-command "M-[ l")
(make-menu clojure lein-help "M-[ h")
