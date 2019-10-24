;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china, netease, tencent or tuna
;; (setq centaur-theme 'classic)                  ; Color theme: default, classic, dark, light, day or night
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-chinese-calendar nil)            ; Use Chinese calendar or not: t or nil
;; (setq centaur-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (catch 'loop
    (dolist (font '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                    "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"))
      (when (member font (font-family-list))
        (set-face-attribute 'default nil :font font :height (cond
                                                             (sys/mac-x-p 130)
                                                             (sys/win32p 110)
                                                             (t 100)))
        (throw 'loop t))))

  ;; Specify font for all unicode characters
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (throw 'loop t))))

  ;; Specify font for Chinese characters
  (catch 'loop
    (dolist (font '("WenQuanYi Micro Hei" "Microsoft Yahei"))
      (when (member font (font-family-list))
        (set-fontset-font t '(#x4e00 . #x9fff) font)
        (throw 'loop t)))))

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgfork-package-archives (quote netease))
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" default)))
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(ns-pop-up-frames nil)
 '(org-adapt-indentation nil)
 '(org-agenda-custom-commands
   (quote
    (("w" . "任务安排")
     ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
     ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
     ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
     ("b" "NOTE" tags-todo "NOTE")
     ("p" . "项目安排")
     ("pw" "迭代任务" tags "CATEGORY=\"WORK\"")
     ("pf" "未来要做的任务" tags-todo "CATEGORY=\"WORK\"")
     ("P" "Programming Language"
      ((tags "JAVA|GO|CLJ|CLJS|JS|SHELL")
       (tags-todo "JAVA|GO|CLJ|CLJS|JS|SHELL")))
     ("S" "Skills Review"
      ((tags "CATEGORY=\"LEARN\"")
       (tags-todo "CATEGORY=\"LEARN\"")))
     ("W" "Weekly Review"
      ((stuck "")
       (tags-todo "PROJECT"))))))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+olp+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
%?
%i
Added:%U" :time-prompt t)
     ("T" "Todo with Clipboard" entry
      (file+olp+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
%c
Added:%U" :time-prompt t)
     ("S" "Todo with Scheduled" entry
      (file+olp+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
SCHEDULED: %^t
%?
%i
Added:%U" :time-prompt t)
     ("D" "Todo with Deadline" entry
      (file+olp+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
%?
%i
Added:%U" :time-prompt t)
     ("P" "TODO with Properties" entry
      (file+olp+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
:PROPERTIES:
:CATEGORY: %^{Category}
:END:
%?
 %i
Added:%U" :time-prompt t)
     ("j" "Journal" entry
      (file+olp cgfork-journal-file "Journal")
      "* %U - %^{Heading}
 %?")
     ("l" "Log Time" entry
      (file+olp cgfork-journal-file "Log Time")
      "* %U - %^{Activity}	 :TIME:")
     ("s" "Code Snippets" entry
      (file+olp cgfork-journal-file "Code Snippets")
      "* %U - %^{Heading}%^g
%?
")
     ("c" "Contacts" table-line
      (file+olp cgfork-journal-file "Contacts")
      "| %U | %^{Name} | %^{Phone}| %^{E-mail} |"))))
 '(org-export-headline-levels 6)
 '(org-plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
 '(plantuml-default-exec-mode (quote jar))
 '(plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
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
 )

;;; custom.el ends here
