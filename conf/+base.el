;;; +base.el --- Initialize GUI and base configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'server)
(with-eval-after-load 'server
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-mode)))

(setq inhibit-startup-screen t)

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(defalias 'list-buffers 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'ibuffer)

(power-emacs-install 'diminish)

;; Setup paredit for lisp programming.
;; If you want to open paredit mode, you should add the hook
;; to `enable-paredit-mode'.
(power-emacs-install 'paredit)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
 
;; A macro for openning paredit.
(defmacro cgfork-open-paredit (mode-hook)
  "Add the `enable-paredit-mode' to the specific MODE-HOOK."
  `(add-hook ,mode-hook 'enable-paredit-mode))

(with-eval-after-load 'elisp-mode
  (cgfork-open-paredit 'emacs-lisp-mode-hook))

;; Highlight parentheses.
(power-emacs-install 'highlight-parentheses)
(global-highlight-parentheses-mode 1)

(when (power-emacs-try 'goto-line-preview)
  (global-set-key [remap goto-line] 'goto-line-preview)

  (when (fboundp 'display-line-numbers-mode)
    (defun cgfork-with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'cgfork-with-display-line-numbers)))

(add-hook 'after-init-hook 'show-paren-mode)

(add-hook 'after-init-hook 'winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;
(power-emacs-install 'color-theme-sanityinc-solarized)
(power-emacs-install 'color-theme-sanityinc-tomorrow)

;; (setq custom-safe-themes t)
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (add-hook 'after-init-hook 'power-emacs-apply-themes)

(defun cgfork-light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (power-emacs-apply-themes))

(defun cgfork-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (power-emacs-apply-themes))


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

(when (power-emacs-try 'multiple-cursors)
   (define-key global-map (kbd "C->") 'mc/mark-next-like-this)
   (define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
   (define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)
   (define-key global-map (kbd "C-c C-m") 'mc/mark-all-dwim))

(when (power-emacs-try 'avy)
  (avy-setup-default)
  (define-key global-map (kbd "C-:") 'avy-goto-char-2))

(when (power-emacs-try 'ivy)
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

  (when (power-emacs-try 'swiper)
    (define-key global-map (kbd "C-s") 'swiper-isearch)
    (define-key global-map (kbd "C-r") 'swiper-isearch-backward))

  (when (power-emacs-try 'counsel)
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
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))
    
(power-emacs-install 'yasnippet)
(power-emacs-install 'yasnippet-snippets)

(diminish 'yas-minor-mode)
(yas-global-mode 1)

;; Or you can use `yas-minor-mode' on per-buffer basic
;; (yas-reload-all)
;;
;; (cgfork/after-load 'yasnippet
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))

;; Setup autoinsert.
(autoload 'yas-expand-snippet "yasnippet")
(defun autoinsert-yas-expand (&rest _)
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(auto-insert-mode 1) ;; trigger to load the autoinsert package.
(with-eval-after-load 'autoinsert
  (setq auto-insert-query nil
	auto-insert-directory (locate-user-emacs-file "templates"))
  (define-auto-insert "\\.org?$" [ "default-org.org" autoinsert-yas-expand ])
  (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
  (define-auto-insert "\\.sh?$" "default-sh.sh"))

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
