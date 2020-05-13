;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.3")
  (error "This requires Emacs 25.3 or above!"))

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

(power-emacs-copy-shell-variables "zsh" "PATH")
(setq power-emacs-build-stable nil)

(defun power-package-install (pkg &optional try)
  "Install the package named PKG .When try is non-nil, it will return t if is is
successful to install the PKG. Otherwise raise an error."
  (let* ((pkg-plist (pcase pkg
		     (`(,a . ,b) `(,a ,@b))
		     (`(,a . nil) `(,a))
		     ((pred symbolp) `(,pkg))))
	 (pkg (car pkg-plist))
	 (plist (cdr pkg-plist)))
    (message "(%S . %S)" pkg plist)
    (if try
	(apply #'power-emacs-try pkg plist)
      (apply #'power-emacs-install pkg plist))))

(defmacro @-> (packages &rest body)
  "Install the PACKAGES and eval the BODY."
  (declare (indent 1) (debug t))
  `(progn
     ,@(mapcar #'(lambda (pkg)
		   `(power-package-install ,pkg))
	       packages)
     ,@body))

(macroexpand-1 '(@-> ('xx)))

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

;; (require '+editor)

;; (package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '+base)
(require '+editor)
(require '+magit)
(require '+org)
(require '+programmer)
(require '+go)
(require '+common-lisp)
(require '+dsl)

(provide 'init)
;;; Init.el ends here
