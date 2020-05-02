;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 or above!"))

(defun cgfork-add-load-path (&rest _)
  "Add 'conf' and 'site-conf' to `load-path'."
  (add-to-list 'load-path (expand-file-name "site-conf" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "conf" user-emacs-directory)))

;; (advice-add #'package-initialize :after #'cgfork/add-load-path)

(cgfork-add-load-path)

(package-initialize)

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

;; Set Key Modifiers.
(with-no-warnings
  (cond
   ((eq system-type 'windows-nt)
    (setq w32-lwindow-modifier 'super ; Left windows key
	  w32-apps-modifier 'hyper) ; Menu key
    (w32-register-hot-key [s-t]))
   ((eq system-type 'darwin)
    (setq mac-option-modifier 'super ; option
	  mac-command-modifier 'meta ; command
	  mac-control-modifier 'control ; control
	  ns-function-modifier 'hyper)))) ; fn


(power-emacs-copy-shell-variables "zsh" "PATH")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '+base)
(require '+magit)
(require '+org)
(require '+programmer)
(require '+go)
;; (provide 'init)

;;; Init.el ends here
