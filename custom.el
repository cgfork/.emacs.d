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
 '(column-number-mode t)
 '(custom-enabled-themes '(doom-xcode))
 '(custom-safe-themes
   '("8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "8f0a782ba26728fa692d35e82367235ec607d0c836e06bc39eb750ecc8e08258" "3828c997d4c95b9a995bfee4eebc4c4262042de88fe8cbf70572e23ce99d22c0" "c7302f7def35329e8c871e32cbf281e97eddd5af2f904032e90a1aa7436d04e1" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "94c58c823c52282f8881b56743b90c87b431903822eb8a2369446ee7785d1f4b" "4a201d19d8f7864e930fbb67e5c2029b558d26a658be1313b19b8958fe451b55" "6f895d86fb25fac5dd4fcce3aec0fe1d88cf3b3677db18a9607cf7a3ef474f02" "8f54cfa3f010d83d782fbcdc3a34cdc9dfe23c8515d87ba22d410c033160ad7e" "b9e406b52f60a61c969f203958f406fed50b5db5ac16c127b86bbddd9d8444f7" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "38143778a2b0b81fb7c7d0e286e5b0e27cd6b2ba1c3b0aa4efbc33e6ac2ed482" default))
 '(display-time-mode t)
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
      ((tags "CATEGORY=\"REMINDER\"")
       (tags-todo "CATEGORY=\"REMINDER\"")))
     ("W" "每周工作"
      ((stuck "")
       (tags-todo "CATEGORY=\"WORKLIST\"")))))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+olp+datetree "/Users/yiwen/Oympt/agenda.org" "GTDs")
      "* TODO [#B] %^{Description} %^g
%?
%i
Added:%U
" :time-prompt t)
     ("T" "Todo with Clipboard" entry
      (file+olp+datetree "/Users/yiwen/Oympt/agenda.org" "GTDs")
      "* TODO [#B] %^{Description} %^g
%c
Added:%U
" :time-prompt t)
     ("S" "Todo with Scheduled" entry
      (file+olp+datetree "/Users/yiwen/Oympt/agenda.org" "GTDs")
      "* TODO [#B] %^{Description} %^g
SCHEDULED: %^t
%?
%i
Added:%U
" :time-prompt t)
     ("d" "Todo with Deadline" entry
      (file+olp+datetree "/Users/yiwen/Oympt/agenda.org" "GTDs")
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
%?
%i
Added:%U
" :time-prompt t)
     ("D" "Todo with Scheduled + Deadline" entry
      (file+olp+datetree "/Users/yiwen/Oympt/agenda.org" "GTDs")
      "* TODO [#B] %^{Description} %^g
SCHEDULED: %^t
DEADLINE: %^t
%?
%i
Added:%U
" :time-prompt t)
     ("P" "TODO with Properties" entry
      (file+olp+datetree "/Users/yiwen/Oympt/agenda.org" "GTDs")
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
:PROPERTIES:
:CATEGORY: %^{Category}
:END:
%?
 %i
Added:%U
" :time-prompt t)
     ("s" "Code Snippets" table-line
      (file+olp "/Users/yiwen/Oympt/log.org" "Code Snippets")
      "| %U | %^{LANG} | %^{Description} | %^{Links} |")
     ("c" "Contacts" table-line
      (file+olp "/Users/yiwen/Oympt/log.org" "Contacts")
      "| %U | %^{Name} | %^{Phone}| %^{E-mail} |")
     ("l" "Log Records" entry
      (file+olp "/Users/yiwen/Oympt/log.org" "Log Records")
      "* %U %^{Message} %^g
%?
")))
 '(org-export-headline-levels 6)
 '(org-plantuml-jar-path "/Users/yiwen/.emacs.d/plantuml.jar")
 '(package-selected-packages '(ansi package-build shut-up epl git commander f s))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
