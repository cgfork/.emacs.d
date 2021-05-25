;;; +org.el --- org mode configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (ob-go))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(require 'org)

(defcustom yw-org-agenda-file (expand-file-name "~/Oympt/agenda.org")
  "Define my org agenda file."
  :group 'yiwen
  :type 'string)

(defcustom yw-org-code-snappets-file (expand-file-name "~/Oympt/code-snappets.org.")
  "Define my org code snappets file."
  :group 'yiwen
  :type 'string)

(defcustom yw-org-contacts-file (expand-file-name "~/Oympt/contacts.org")
  "Define my contacts file."
  :group 'yiwen
  :type 'string)

(setq org-agenda-files (list yw-org-agenda-file)
      org-todo-keywords '((sequence "TODO(t)" "DODING(i)" "HANGHUP(h)" "|" "DONE(d)" "CANCEL(c)"))
      org-log-done 'time
      org-startup-indented nil
      org-ellipsis "   "
      org-pretty-entities t
      org-src-fontify-natively t
      org-edit-src-content-indentation 0)

(yw-space-key-define
  "o" '(nil :wk "org")
  "o a" 'org-agenda
  "o c" 'org-capture)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (go . t)))

(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(custom-set-variables
  '(org-adapt-indentation nil)
  '(org-export-headline-levels 6)
  `(org-plantuml-jar-path ,(expand-file-name "plantuml.jar" user-emacs-directory))
  `(org-capture-templates
    (quote (("t" "Todo" entry (file+olp+datetree ,yw-org-agenda-file)
	     "* TODO [#B] %^{Description} %^g\n%?\n%i\nAdded:%U" :time-prompt t)
	    ("T" "Todo with Clipboard" entry (file+olp+datetree ,yw-org-agenda-file)
	     "* TODO [#B] %^{Description} %^g\n%c\nAdded:%U" :time-prompt t)
	    ("S" "Todo with Scheduled" entry (file+olp+datetree ,yw-org-agenda-file)
	      "* TODO [#B] %^{Description} %^g\nSCHEDULED: %^t\n%?\n%i\nAdded:%U" :time-prompt t)
	     ("D" "Todo with Deadline" entry (file+olp+datetree ,yw-org-agenda-file)
	      "* TODO [#B] %^{Description} %^g\nDEADLINE: %^t\n%?\n%i\nAdded:%U" :time-prompt t)
	     ("P" "TODO with Properties" entry (file+olp+datetree ,yw-org-agenda-file)
	      "* TODO [#B] %^{Description} %^g\nDEADLINE: %^t\n:PROPERTIES:\n:CATEGORY: %^{Category}\n:END:\n%?\n %i\nAdded:%U" :time-prompt t)
	     ("s" "Code Snippets" entry (file+olp ,yw-org-code-snappets-file "Code Snippets")
	      "* %U - %^{Heading} %^g\n%?\n")
	     ("c" "Contacts" table-line (file+olp ,yw-org-contacts-file "Contacts")
	      "| %U | %^{Name} | %^{Phone}| %^{E-mail} |"))))
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
      ("P" "编程" ((tags "java|go|clj|racket|js|shell|c++")
		   (tags-todo "java|go|clj|racket|js|shell|c++")))
      ("R" "提醒事项" ((tags "CATEGORY=\"REMINDER\"")
		       (tags-todo "CATEGORY=\"REMINDER\"")))
      ("W" "每周工作"
       ((stuck "") ;; review stuck projects as designated by org-stuck-projects
	(tags-todo "CATEGORY=\"WORKLIST\"") ;; review all projects (assuming you use todo keywords to designate projects)
	)))))

(defun ox-org-clean-space (text backend _)
  "Clean the space between chinese when export to html.
Replace the TEXT when the BACKEND is html."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
	  (s text))
      ;; 删除换行产生的空格，当使用中文的时候
      (setq s
	    (replace-regexp-in-string
	     (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
	     "\\1\\2" s))
      ;; 删除粗体之前的空格
      (setq s
	    (replace-regexp-in-string
	     (format "\\(%s\\) +\\(<\\)" regexp)
	     "\\1\\2" s))
      (setq s
	    (replace-regexp-in-string
	     (format "\\(>\\) +\\(%s\\)" regexp)
	     "\\1\\2" s))
      s)))
;; (add-to-list 'org-export-filter-paragraph-functions 'ox-org-clean-space)


(provide '+org)
;;; +org.el ends here
