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


					;clojure tools:
(defun read-lein-directory ()
  (setq lein-project-directory (read-directory-name "Lein project directory: "))
  (message "Lein project directory is %s" lein-project-directory))

(defun read-lein-project-name ()
  (setq lein-project-name (read-string-not-whitespace "Lein project name")))

(defun load-lein-project-name ()
  nil) ;unimplemented

(defun read-lein-template ()
  (setq lein-template (read-string-not-whitespace "Lein template name (default - for library; app - for application)" "default" "lein template name")))


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
    (load-lein-project-name)))

(defun lein-new-project ()
  (interactive)
  (let ((default-directory lein-project-directory)) (update-lein-directory)
       (read-string (concat "Directory '" lein-project-directory "'. Press RET"))
       (if (file-exists-p lein-project-directory) nil (make-directory lein-project-directory))
       (read-lein-template)
       (read-lein-project-name)
       (let ((default-directory lein-project-directory))
	 (shell-command 
	  (concat lein-executer "new " lein-template " " lein-project-name ))
	 (setq lein-project-directory (path-concat lein-project-directory lein-project-name)))))

(defun lein-full-compile-project ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (shell-command 
     (concat lein-executer " clean && " lein-executer " deps && " lein-executer " compile && " lein-executer " uberjar"))))

(defun lein-run-project ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (shell-command 
     (concat lein-executer " run"))))

(defun lein-project-edit ()
  (interactive)
  (let ((file_path (path-concat lein-project-directory lein-project-file)))
    (if (file-exists-p file_path)
	(find-file file_path)
      (error "Not opened project"))))

(defun lein-view-project-directory ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (error "dired unimplemented")))

(defun lein-interactive-repl-project ()
  (interactive)
  (error "unimplemented")
  (let ((default-directory lein-project-directory))
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
