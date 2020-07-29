;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.3")
  (error "This requires Emacs 25.3 or above!"))
;; (add-to-list 'load-path (expand-file-name "org-mode/lisp" user-emacs-directory))

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

(when (not (require 'power-emacs nil t))
  (add-to-list 'load-path (expand-file-name "power-emacs" user-emacs-directory)))

(require 'power-emacs)
(power-emacs-copy-shell-variables "zsh" "PATH")
(setq power-emacs-build-stable nil)

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '+base)
(require '+themes)
(require '+editor)
(require '+magit)
(require '+company)
(require '+flycheck)
(require '+org)
(require '+lsp)
(require '+go)
(require '+common-lisp)
(require '+dsl)
(require '+clojure)

;; (add-to-list 'load-path (expand-file-name "site-conf" user-emacs-directory))
;; (require 'ob-plantuml)

(provide 'init)
;;; Init.el ends here
