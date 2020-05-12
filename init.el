;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "27.0")
  (error "This requires Emacs 27.0 or above!"))

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

(defmacro power-require (packages &rest body)
  `(progn ,@(mapcar #'(lambda (pkg)
		     (cond
		      ((listp pkg) `(power-emacs-install (quote ,(car pkg)) ,@(cdr pkg)))
		      ((symbolp pkg) `(power-emacs-install (quote ,pkg)))
		      (t (error "Unknown package type %S" pkg))))
		 packages)
	  ,@body))

(defmacro @keys (keymap &rest key-bindings)
  `(progn ,@(mapcar #'(lambda (b) `(define-key ,keymap ,(car b) ,(cdr b))) key-bindings)))

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

;; (require '+editor)

;; (package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '+base)
(require '+magit)
(require '+org)
(require '+programmer)
(require '+go)
(require '+common-lisp)
(require '+dsl)

(provide 'init)
;;; Init.el ends here
