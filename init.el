;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.3")
  (error "This requires Emacs 25.3 or above!"))

(when (version< emacs-version "27.1")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

;; Load `custom-file'.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; If it doesn't exist, copy from the template, then load it.
(let ((custom-template-file (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
	   (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))
(if (file-exists-p custom-file)
    (load custom-file)
  (message "custom file is not exist!"))

;; Load `custom-post.el'.
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((file (expand-file-name "custom-post.el" user-emacs-directory)))
	      (when (file-exists-p file)
		(load file)))))

(require 'subr-x)

(defun ewx-get-shell-variable (name &optional shell)
  "Return the value associated with the NAME in the default shell environments
or in the SHELL environments if it exists."
  (let* ((shell (or shell ewx-shell-executable))
	 (shell-exec (executable-find shell))
	 (name (cond
		((stringp name) name)
		((symbolp name) (symbol-name name))
		(t (error "Unknown name %S" name))))
	 (printf (or (executable-find "printf") "printf"))
	 (printf-command (concat printf " '%s'" (format "${%s}" name)))
	 (shell-args (cond
		      ((string-match-p "t?csh$" shell)
		       `("-d" "-c" ,(concat "sh -c" printf-command)))
		      ((string-match-p "fish" shell)
		       `("-l" "-c" ,(concat "sh -c" printf-command)))
		      (t
		       `("-l" "-c" ,printf-command)))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process shell-exec nil t nil shell-args)))
	(unless (zerop exit-code)
	  (error "Non-zero exit code when execute `%s' with '%S" shell shell-args)))
      (buffer-string))))

(defun ewx-set-shell-variable (name value)
  "Set the value of environment variable NAME to VALUE.
If NAME is 'PATH', it will also set corresponding variables
such as `exec-path', `eshell-path-env' and so on."
  (setenv name value)
  (when (and (string-equal "PATH" name)
	     (not (string-empty-p value)))
    (setq eshell-path-env value
	  exec-path (append (parse-colon-path value) (list exec-directory)))))

(defun ewx-copy-shell-variables (shell &rest vars)
  "Set the environemnt VARS from the given shell.
It will return the pairs that are set into the environment variables."
  (mapcar (lambda (name)
	    (let ((value (condition-case err
			     (ewx-get-shell-variable name shell)
			   (message "get %s variable error: %s. skip it" name (error-message-string err)))))
	      (if value
		  (progn (ewx-set-shell-variable name value)
			 (cons name value))
		(cons name nil))))
	  vars))


(when (display-graphic-p)
  (when (or sys/macp sys/linuxp)
    (ewx-copy-shell-variables ewx-shell-executable "PATH")))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
	("http" . "127.0.0.1:1235")
	("https" . "127.0.0.1:1235")))

(setq straight-base-dir user-emacs-directory
      straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-enable-package-integration nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      use-package-always-ensure nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

;; (use-package benchmark-init
;;   :ensure t
;;   :hook (after-init . benchmark-init/deactivate))
;; https://github.com/dholm/benchmark-init-el/issues/15
;; (cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
;;   (use-package benchmark-init
;;     :config
;;     (require 'benchmark-init-modes)                                     ; explicitly required
;;     (add-hook 'after-init-hook #'benchmark-init/deactivate)))

(use-package which-key
  :config
  (which-key-mode t))

(require 'simple)
(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)

(use-package diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require '+font)
(require '+buffer)
(require '+window)
(require '+projectile)
(require '+sidebar)
(require '+themes)
(require '+editor)
(require '+company)
(require '+flycheck)
(require '+lsp)
(require '+go)
(require '+rust)
(require '+markdown)
(require '+org)
(require '+plantuml)
(require '+git)
(require '+protobuf)

(provide 'init)
;;; Init.el ends here
