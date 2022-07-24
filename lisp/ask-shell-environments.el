;;; ask-shell-environments.el --- Copy shell environments -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defgroup ask-shell nil
  "Define the group for coping shell environments."
  :group 'convenience)

(defcustom ask-shell-executable (getenv "SHELL")
  "Where the environment variable get from."
  :type 'string
  :group 'ask-shell)

(defun ask-get-shell-variable (name &optional shell)
  "Return the value associated with the NAME in the default shell environments
or in the SHELL environments if it exists."
  (let* ((shell (or shell ask-shell-executable))
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

(defun ask-set-shell-variable (name value)
  "Set the value of environment variable NAME to VALUE.
If NAME is 'PATH', it will also set corresponding variables
such as `exec-path', `eshell-path-env' and so on."
  (setenv name value)
  (when (and (string-equal "PATH" name)
	     (not (string-empty-p value)))
    (setq eshell-path-env value
	  exec-path (append (parse-colon-path value) (list exec-directory)))))

(defun ask-copy-shell-variables (shell &rest vars)
  "Set the environemnt VARS from the given shell.
It will return the pairs that are set into the environment variables."
  (mapcar (lambda (name)
	    (let ((value (condition-case err
			     (ask-get-shell-variable name shell)
			   (message "get %s variable error: %s. skip it" name (error-message-string err)))))
	      (if value
		  (progn (ask-set-shell-variable name value)
			 (cons name value))
		(cons name nil))))
	  vars))

(provide 'ask-shell-environments)
;;; ask-shell-environments.el ends here
