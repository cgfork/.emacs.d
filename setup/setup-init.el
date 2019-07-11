;;; package --- summary
;;; Commentary:
;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

;; disable files backup
(setq make-backup-files nil)

;; disable tool bar
(tool-bar-mode -1)

;; disable scroll mode
(set-scroll-bar-mode nil)

;; display tim
(display-time-mode t)

(column-number-mode t)

;; Key Modifiers in windows
(when sys/win32p
  (setq w32-lwindow-modifier 'super) ; left win
  (setq w32-apps-modifier 'hyper) ; Menu key
  (w32-register-hot-key [s-t]))

;; set environment for linux
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(when sys/mac-x-p
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  )

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

(use-package command-log-mode
  :ensure t
  )

(use-package highlight-parentheses
  :ensure t
  :init
  (highlight-parentheses-mode t))

;; (use-package doom-modeline
;;       :ensure t
;;       :hook (after-init . doom-modeline-mode))


(provide 'setup-init)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; setup-init.el ends here
