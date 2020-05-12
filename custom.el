;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:
;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (catch 'loop
    (dolist (font '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                    "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"))
      (when (member font (font-family-list))
        (set-face-attribute 'default nil :font font :height (cond
                                                             ((and (display-graphic-p) (eq system-type 'darwin))  130)
                                                             ((eq system-type 'windows-nt) 110)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-bright))
 '(custom-safe-themes t)
 '(org-adapt-indentation nil)
 '(org-agenda-custom-commands
   '(("w" . "任务安排")
     ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
     ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
     ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
     ("p" . "项目安排")
     ("pw" "迭代任务" tags "CATEGORY=\"WORKLIST\"")
     ("pf" "未来要做的任务" tags-todo "CATEGORY=\"WORKLIST\"")
     ("t" . "个人任务")
     ("tw" "任务清单" tags "CATEGORY=\"TASK\"")
     ("tf" "未来要做的任务" tags-todo "CATEGORY=\"TASK\"")
     ("P" "编程"
      ((tags "java|go|clj|racket|js|shell|c++")
       (tags-todo "java|go|clj|racket|js|shell|c++")))
     ("R" "提醒事项"
      ((tags "CATEGORY=\"LEARN\"")
       (tags-todo "CATEGORY=\"LEARN\"")))
     ("W" "每周计划"
      ((stuck "")
       (tags-todo "CATEGORY=\"PLAN\"")))))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+olp+datetree "/Users/yiwen/Prophet/getting-things-done.org")
      "* TODO [#B] %^{Description} %^g
%?
%i
Added:%U" :time-prompt t)
     ("T" "Todo with Clipboard" entry
      (file+olp+datetree "/Users/yiwen/Prophet/getting-things-done.org")
      "* TODO [#B] %^{Description} %^g
%c
Added:%U" :time-prompt t)
     ("S" "Todo with Scheduled" entry
      (file+olp+datetree "/Users/yiwen/Prophet/getting-things-done.org")
      "* TODO [#B] %^{Description} %^g
SCHEDULED: %^t
%?
%i
Added:%U" :time-prompt t)
     ("D" "Todo with Deadline" entry
      (file+olp+datetree "/Users/yiwen/Prophet/getting-things-done.org")
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
%?
%i
Added:%U" :time-prompt t)
     ("P" "TODO with Properties" entry
      (file+olp+datetree "/Users/yiwen/Prophet/getting-things-done.org")
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
:PROPERTIES:
:CATEGORY: %^{Category}
:END:
%?
 %i
Added:%U" :time-prompt t)
     ("j" "Journal" entry
      (file+olp "/Users/yiwen/Prophet/journal.org" "Journal")
      "* %U - %^{Heading}
 %?")
     ("l" "Log Time" entry
      (file+olp "/Users/yiwen/Prophet/journal.org" "Log Time")
      "* %U - %^{Activity}	 :TIME:")
     ("s" "Code Snippets" entry
      (file+olp "/Users/yiwen/Prophet/journal.org" "Code Snippets")
      "* %U - %^{Heading}%^g
%?
")
     ("c" "Contacts" table-line
      (file+olp "/Users/yiwen/Prophet/journal.org" "Contacts")
      "| %U | %^{Name} | %^{Phone}| %^{E-mail} |")))
 '(org-export-headline-levels 6)
 '(org-plantuml-jar-path nil)
 '(package-selected-packages
   '(transient dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized goto-line-preview all-the-icons memoize company-box flycheck-popup-tip popup slime macrostep protobuf-mode gotest go-mode htmlize ob-go org-tree-slide toc-org org-rich-yank bnf-mode grab-mac-link flycheck-posframe posframe ibuffer-projectile projectile company-lsp lsp-ui lsp-mode lv markdown-mode ht f dash-functional flycheck pkg-info epl company-prescient prescient neotree company magit magit-popup git-commit with-editor ghub treepy async deadgrep spinner dash rg s yasnippet-snippets wgrep yasnippet counsel swiper ivy avy diff-hl diredfl which-key multiple-cursors epm highlight-parentheses paredit diminish power-emacs)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
