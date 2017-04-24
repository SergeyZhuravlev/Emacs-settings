					;usings:
(require 'subr-x)


					;clojure environment:
(defvar lein-project-directory "~")
(defvar lein-executer "lein ")
(defvar java-executer "java ")
(defvar lein-project-file "project.clj")
(defvar lein-project-name nil)
(defvar lein-project-version nil)
(defvar lein-target-directory-name "target/uberjar")
(defvar async-executer-suffix "&")
(defvar new-env-executer 
  (cond 
   ((string-equal system-type "windows-nt") "start " )
   ((string-equal system-type "gnu/linux") "xterm -e " )
   (t "")))


					;clojure tools:
(defun read-lein-directory ()
  (setq lein-project-directory (read-directory-name "Lein project directory: "))
  (message "Lein project directory is %s" lein-project-directory))

(defun read-lein-project-name ()
  (setq lein-project-name (read-string-not-whitespace "Lein project name")))

(defun load-lein-project-configuration ()
  (let ((project_text (get-string-from-file (lein-project-file-path))))
    (let ((match_result (string-match "[ \t\n]*([ \t\n]*defproject[ \t\n]+\\([-._0-9a-zA-Z]+\\)[ \t\n]*\"\\([- ._0-9a-zA-Z]+\\)\"" project_text)))
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

(defun lein-run-project-generic (prefix suffix)
  (let ((default-directory lein-project-directory))
    (check-opened-project)
    (shell-command 
     (concat prefix lein-executer " run " suffix))))

(defun java-execute-project-generic (prefix suffix)
  (let ((default-directory (path-concat lein-project-directory lein-target-directory-name)))
    (check-opened-project)
    (shell-command 
     (concat prefix java-executer " -jar " lein-project-name "-" lein-project-version "-standalone.jar " suffix))))

(defun lein-async-run-project ()
  (interactive)
  (lein-run-project-generic "" async-executer-suffix))

(defun java-async-execute-project ()
  (interactive)
  (java-execute-project-generic "" async-executer-suffix))

(defun lein-new-environment-run-project ()
  (interactive)
  (lein-run-project-generic new-env-executer async-executer-suffix))

(defun java-new-environment-execute-project ()
  (interactive)
  (java-execute-project-generic new-env-executer async-executer-suffix))

(defun lein-project-edit ()
  (interactive)
  (check-opened-project)
  (find-file (lein-project-file-path)))

(defun lein-view-project-directory ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (dired (list "Current Lein project directory" lein-project-directory))))

(autoload 'cider-connected-p "cider-client")
(autoload 'cider-restart "cider-interaction")

(defun lein-full-compile-project ()
  (interactive)
  (let ((default-directory lein-project-directory))
    (check-opened-project)
    (load-lein-project-configuration)
    (shell-command 
     (concat lein-executer " clean && " lein-executer " deps && " lein-executer " compile && " lein-executer " uberjar"))
    (if (cider-connected-p)(cider-restart t))))

(defun lein-interactive-repl-current-file ()
  (interactive)
  (cider-jack-in))

					;settings:
(unless (package-installed-p 'cider)
  (package-install 'cider))

					;clojure menu:
(make-main-menu lein-clojure 'tools)
(make-menu lein-clojure lein-interactive-repl-current-file "M-[ i")
(make-menu lein-clojure java-new-environment-execute-project "M-[ e")
(make-menu lein-clojure lein-new-environment-run-project "M-[ r")
(make-menu lein-clojure java-async-execute-project "M-[ a e")
(make-menu lein-clojure lein-async-run-project "M-[ a r")
(make-menu lein-clojure lein-full-compile-project "M-[ f")
(make-menu lein-clojure lein-project-edit "M-[ p")
(make-menu lein-clojure lein-view-project-directory "M-[ v")
(make-menu lein-clojure lein-new-project "M-[ n")
(make-menu lein-clojure lein-set-project-directory "M-[ d")
(make-menu lein-clojure lein-execute-command "M-[ l")
(make-menu lein-clojure lein-help "M-[ h")
