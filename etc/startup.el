					;usings:
(require 'cl)


					;common:
(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)


					;symbols:
(defun replace-char (string replaced-char char-replacer) 
  (concat 
   (mapcar 
    (lambda (x) 
      (if (= x replaced-char) char-replacer x))
    string)))

(defun upper-first (s)
  (if (> (length s) 0)
      (concat (upcase (substring s 0 1)) (substring s 1))
    nil))

(defun symbol-to-text (symbol) 
  (upper-first 
   (replace-char 
    (symbol-name symbol) 
    ?- 
    ? )))

(defmacro upper-first-symbol (symbol)
  `(make-symbol (upper-first (symbol-name ,symbol))))


					;emacs-menus:
(defmacro symbol-to-menu (symbol)
  `(list* (symbol-to-text ,symbol) ,symbol))

(defmacro make-main-menu (main_menu after_menu)  ;(make-main-menu main-Menu-Name 'afterThisMenu) 
  `(define-key-after global-map [menu-bar ,main_menu] (cons ,(symbol-to-text main_menu) (make-sparse-keymap)) ,after_menu))

(defmacro make-menu (main_menu command key_binding)  ;(make-menu mainMenuName command-for-execute-menu-item "M-[ h") 
  `(progn 
     (define-key global-map [menu-bar ,main_menu ,(upper-first-symbol command)] ',(symbol-to-menu command))
     (global-set-key (kbd ,key_binding) ',command)))


					;interactive:
(defun read-string-not-whitespace (prompt &optional default_input parameter_name)
  (let ((default_input (if default_input default_input ""))
	(readed (string-trim (read-string (concat prompt ": ") default_input)))
	(parameter_name (if parameter_name parameter_name (downcase prompt))))
    (if 
	(equal "" readed)
	(error (concat "Error: Invalid " parameter_name ". Quit")) 
      readed)))


					;paths
(defun path-concat (path new-part)
  (if (string-match ".*/$" path)
      (concat path new-part)
    (concat path "/" new-part)))


					;files
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

					;settings
(require 'package)
(add-to-list 'package-archives '("melpa-STABLE" . "http://stable.melpa.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
