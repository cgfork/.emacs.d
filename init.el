;;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 or above!"))

(defun add-load-path (&rest _)
  "Add 'conf' and 'site-conf' to `load-path'."
  (add-to-list 'load-path (expand-file-name "site-conf" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "conf" user-emacs-directory)))

(defun add-subdirs-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
	  (expand-file-name "site-conf" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'add-load-path)
(advice-add #'package-initialize :after #'add-subdirs-load-path)

(add-load-path)

;; Define consts
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

;; Define the group
(defgroup cgfork nil
  "Define the group for my emacs config."
  :group 'convenience)

;; Define the customize variables
(defcustom cgfork-package-archives 'tuna
  "Set package archives from which to fetch."
  :type '(choice
	  (const :tag "Melpa" melpa)
	  (const :tag "Melpa Mirror" melpa-mirror)
	  (const :tag "Emacs-China" emacs-china)
	  (const :tag "Netease" netease)
	  (const :tag "Tuna" tuna))
  :group 'cgfork)

(defcustom cgfork-org-home (expand-file-name "~/note")
  "Set the org home path."
  :type 'string
  :group 'cgfork)

;; Load `custom-file'.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; If it doesn't exist, copy from the template, then load it.
(let ((custom-template-file (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
	   (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))
(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'.
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((file (expand-file-name "custom-post.el" user-emacs-directory)))
	      (if (file-exists-p file)
acka		  (load file)))))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read "Choose package archives: "
				  '(melpa melpa-mirror emacs-china netease tuna)))))
  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            (archives
             (error "Unknown archives: '%s'" archives)))))
  (message "Set package archives to '%s'." archives))

;; Set package archives.
(set-package-archives cgfork-package-archives)

;; Set list-buffers to ibuffer
(defalias 'list-buffers 'ibuffer)

(require 'package)

;; Initialize packages.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Set `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Eval before `use-package'.
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Keep keyring updated.
(use-package gnu-elpa-keyring-update)

;; Auto update packages.
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

;; A modern Packages Menu.
(use-package paradox
  :init
  (setq paradox-execute-asynchronously t
	paradox-github-token t
	paradox-display-star-count nil)
  (defun my-paradox-enable (&rest _)
    "Enable paradox, replacing the `list-packages'."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                (let ((buf (get-buffer-create "*Paradox Report*"))
                      (inhibit-read-only t))
                  (with-current-buffer buf
                    (page-break-lines-mode 1))))
              t)))

;; Setup environment for linux.
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Set Key Modifiers.
(with-no-warnings
  (cond
   (sys/win32p
    (setq w32-lwindow-modifier 'super ; Left windows key
	  w32-apps-modifier 'hyper) ; Menu key
    (w32-register-hot-key [s-t]))
   (sys/mac-x-p
    (setq mac-option-modifier 'super ; option
	  mac-command-modifier 'meta ; command
	  mac-control-modifier 'control ; control
	  ns-function-modifier 'hyper)))) ; fn

;;;;;;;;;;;;;;;;;;;;;; Basic Setup ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start server.
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Set history.
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Set recentf
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 200
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
                "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

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
  :bind (("M-3" . mc/mark-next-like-this)
	 ("M-4" . mc/mark-previous-like-this)
	 :map ctl-x-map
	 ("\C-m" . mc/mark-all-dwim)
	 ("<return>" . mule-keymap)))

;; Record the command
(use-package command-log-mode
  :ensure t)

;; highlight the brackets
(use-package highlight-parentheses
  :ensure t
  :init
  (highlight-parentheses-mode t))

; Setup find-file and M-x
(use-package counsel
  :ensure t)

;; Setup navigation
(use-package avy
  :ensure t
  :config
  (avy-setup-default))

;; Setup ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; Setup search
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;; Setup ace-window
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))


(provide 'init)
;;; init.el ends here
