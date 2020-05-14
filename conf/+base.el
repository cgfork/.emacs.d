;;; +base.el --- Initialize GUI and base configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; early-init
(when (not (version< emacs-version "27.0"))
  ;; Disable startup screen
  (setq inhibit-startup-screen t)

  ;; Disable tool bar
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Disable scroll bar
  (when (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

  (when (fboundp 'set-language-environment)
    (set-language-environment 'utf-8))

  (when (fboundp 'display-time-mode)
    (display-time-mode t))

  (when (fboundp 'save-place-mode)
    (save-place-mode t))

  (when (fboundp 'electric-pair-mode)
    (electric-pair-mode t))

  (setq-default
   column-number-mode t
   auto-save-default nil
   make-backup-files nil
   cursor-type 'bar
   ns-pop-up-frames nil
   tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

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
  )

(require 'server)
(with-eval-after-load 'server
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-mode)))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(define-key global-map (kbd "C-x C-b") 'ibuffer)

(add-hook 'after-init-hook 'winner-mode)

(power-emacs-install 'diminish)

(when (power-emacs-try 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  ;; TODO: file upstream as a PR
  (with-eval-after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

;; Set recentf
(add-hook 'after-init-hook #'recentf-mode)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 200
	recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude))

(add-hook 'after-init-hook #'savehist-mode)
(with-eval-after-load 'savehist
   (setq enable-recursive-minibuffers t 
         history-length 1000
         savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
         savehist-autosave-interval 300))

;; Which-key is a minor mode for Emacs that
;; displays the key bindings following the
;; currently entered incomplete command in
;; a popup.
 ;; Github: https://github.com/justbur/emacs-which-key
(when (power-emacs-try 'which-key)
  (which-key-mode t))

;; Dired
(setq-default dired-dwim-target t)

(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-directory-program gls)))

(when (power-emacs-try 'diredfl)
  (with-eval-after-load 'dired
    (diredfl-global-mode)
    (require 'dired-x)))

(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(when (power-emacs-try 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

;; ivy
(power-emacs-try 'ivy)
(diminish 'ivy-mode)
(add-hook 'after-init-hook 'ivy-mode)
(define-key global-map (kbd "C-x b") 'ivy-switch-buffer)
(define-key global-map (kbd "C-c C-r") 'ivy-resume)
(define-key global-map (kbd "C-c C .") 'ivy-switch-view)
(with-eval-after-load 'ivy
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
	ivy-count-format "%d/%d"
        ivy-display-style 'fancy
        ivy-format-function 'ivy-format-function-line
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) 
				(t . ivy--regex-plus))
        ivy-initial-inputs-alist nil)    
  (define-key global-map (kbd "C-c v p") 'ivy-push-view))
(define-key global-map (kbd "C-c v o") 'ivy-pop-view)

(power-emacs-try 'swiper)
(define-key global-map (kbd "C-s") 'swiper-isearch)
(define-key global-map (kbd "C-r") 'swiper-isearch-backward)
(power-emacs-try 'counsel)
(diminish 'counsel-mode)
(add-hook 'after-init-hook 'counsel-mode)
(define-key global-map (kbd "M-x") 'counsel-M-x)
(define-key global-map (kbd "C-x C-f") 'counsel-find-file)
(with-eval-after-load 'ibuffer
  (defun my-ibuffer-find-file ()
    (interactive)
    (let ((default-directory (let ((buf (ibuffer-current-buffer)))
			       (if (buffer-live-p buf)
				   (with-current-buffer buf default-directory)
				 default-directory))))
      (counsel-find-file default-directory)))
  (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

(setq-default grep-highlight-matches t
	      grep-scroll-output t)

(when (eq system-type 'darwin)
  (setq-default locate-command "mdfind"))

(power-emacs-install 'wgrep)
(with-eval-after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (and (executable-find "ag")
	   (power-emacs-try 'ag))
  (power-emacs-install 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "C-c C-p") 'ag-project))

(when (and (executable-find "rg")
	   (power-emacs-try 'rg))
  (power-emacs-install 'deadgrep)
  (global-set-key (kbd "C-c C-p") 'rg-project))

(when (power-emacs-try 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)
  (setq-default projectile-mode-line-prefix " Proj")
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(power-emacs-install 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(with-eval-after-load 'neotree
  (setq neo-window-width 30
	neo-smart-open t
	neo-autorefresh nil
	neo-window-fixed-size nil))
    
(provide '+base)
;;; +base.el ends here
