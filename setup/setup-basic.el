;;; package --- summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'setup-const)
  )

;; disable files backup
(setq make-backup-files nil)

;; disable tool bar
(tool-bar-mode -1)

;; Key Modifiers in windows
(when sys/win32p
  (setq w32-lwindow-modifier 'super) ; left win
  (setq w32-apps-modifier 'hyper) ; Menu key
  (w32-register-hot-key [s-t]))

;; set environment for linux
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))
  
;; Start server
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (add-hook 'after-init-hook #'server-mode))
  )

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  ;; lazy load recentf
  :hook (find-file . (lambda () (unless recentf-mode
				  (recentf-mode)
				  (recentf-track-opened-file))))
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude ".elfeed")
  (add-to-list 'recentf-exclude ".idea")
  (add-to-list 'recentf-exclude ".DS_Store")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "cache")
  (add-to-list 'recentf-exclude "persp-confs")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "url")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

;; Which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Try
(use-package try
  :ensure t)

;; Mutliple cursors
(use-package multiple-cursors
  :ensure t
  :bind (
	 ("M-3" . mc/mark-next-like-this)
	 ("M-4" . mc/mark-previous-like-this)
	 :map ctl-x-map
	 ("\C-m" . mc/mark-all-dwim)
	 ("<return>" . mule-keymap)
	 )
  )

(provide 'setup-basic)
;;; setup-basic.el ends here
