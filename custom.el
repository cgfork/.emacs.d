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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(org-adapt-indentation nil)
 '(org-agenda-custom-commands
   (quote
    (("w" . "任务安排")
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
       (tags-todo "CATEGORY=\"PLAN\""))))))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+olp+datetree cgfork/tasks-file)
      "* TODO [#B] %^{Description} %^g
%?
%i
Added:%U" :time-prompt t)
     ("T" "Todo with Clipboard" entry
      (file+olp+datetree cgfork/tasks-file)
      "* TODO [#B] %^{Description} %^g
%c
Added:%U" :time-prompt t)
     ("S" "Todo with Scheduled" entry
      (file+olp+datetree cgfork/tasks-file)
      "* TODO [#B] %^{Description} %^g
SCHEDULED: %^t
%?
%i
Added:%U" :time-prompt t)
     ("D" "Todo with Deadline" entry
      (file+olp+datetree cgfork/tasks-file)
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
%?
%i
Added:%U" :time-prompt t)
     ("P" "TODO with Properties" entry
      (file+olp+datetree cgfork/tasks-file)
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
:PROPERTIES:
:CATEGORY: %^{Category}
:END:
%?
 %i
Added:%U" :time-prompt t)
     ("j" "Journal" entry
      (file+olp cgfork/journal-file "Journal")
      "* %U - %^{Heading}
 %?")
     ("l" "Log Time" entry
      (file+olp cgfork/journal-file "Log Time")
      "* %U - %^{Activity}	 :TIME:")
     ("s" "Code Snippets" entry
      (file+olp cgfork/journal-file "Code Snippets")
      "* %U - %^{Heading}%^g
%?
")
     ("c" "Contacts" table-line
      (file+olp cgfork/journal-file "Contacts")
      "| %U | %^{Name} | %^{Phone}| %^{E-mail} |"))))
 '(org-export-headline-levels 6)
 '(org-plantuml-jar-path cgfork/plantuml-jar)
 '(package-selected-packages
   (quote
    (company-prescient htmlize org-tree-slide toc-org org-preview-html org-rich-yank bnf-mode ob-go yaml-mode neotree go-mode lsp-ui company-lsp lsp-mode flycheck markdown-mode plantuml-mode magit wgrep company ibuffer-projectile projectile clj-refactor flycheck-clojure cider elein cljsbuild-mode clojure-mode counsel swiper ivy yasnippet-snippets yasnippet avy multiple-cursors diff-hl diredfl which-key try dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized goto-line-preview highlight-parentheses paredit auto-package-update gnu-elpa-keyring-update diminish seq quelpa grab-mac-link racket-mode deadgrep rg wgrep-ag ag all-the-icons exec-path-from-shell go-rename go-impl go-fill-struct go-dlv flycheck-golangci-lint))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
