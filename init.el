;; -*- lexical-binding: t -*-XS
;;; package --- summary
;;; Commentary:
;;;
;;; Code:
;; Check the Emacs version
(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Setup startup message
(setq inhibit-startup-message t)

;; Setup load-path
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

;; Setup the group
(defgroup cgfork nil
  "cgfork group for emacs customizations."
  :group 'convenience)

;; Setup `package`
;; Define the customize variable
(defcustom cgfork-package-archives 'netease
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna))
  :group 'cgfork)

;; Define the set function
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

;; Setup `use-package`
(set-package-archives cgfork-package-archives)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set default font
(set-frame-font "-*-Source Code Pro for Powerline-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")

;; Load setup
(require 'setup-const)
(require 'setup-basic)
(require 'setup-buffer)
(require 'setup-dired)
(require 'setup-company)
(require 'setup-projectile)
(require 'setup-go)
(require 'setup-markdown)
(require 'setup-org)
(require 'setup-treemacs)
(require 'setup-git)
(require 'setup-clisp)

;; A temporary codes for fixing some bugs of the libs
;; fix the problems with the "go tool vet unsupported"
;; (let ((govet (flycheck-checker-get 'go-vet 'command)))
;;  (when (equal (cadr govet) "tool")
;;    (setf (cdr govet) (cddr govet))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgfork-package-archives (quote netease))
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" default)))
 '(electric-pair-mode t)
 '(ns-pop-up-frames nil)
 '(global-display-line-numbers-mode t)
 '(org-adapt-indentation nil)
 '(package-selected-packages
   (quote
    (go-rename go-dlv golint highlight-parentheses slime smex neotree command-log-mode zenburn-theme treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs go-guru go-eldoc company-go multiple-cursors ob-go org-preview-html go-mode counsel-projectile projectile diredfl all-the-icons-dired pcre2el dired+ yasnippet-snippets company counsel ace-window exec-path-from-shell try use-package)))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(provide 'init)
;;; init.el ends here
