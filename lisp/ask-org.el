;;; ask-org.el --- Org -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Installation
(unless ask-start-emacs-without-packages
  (use-package ox-gfm)
  (use-package htmlize))

;; Define
(defgroup ask-org nil
  "Org directories."
  :group 'convenience)

(defcustom ask-org-tasks-file "~/org/notes.org"
  "The file to store TODOs."
  :type 'string
  :group 'ask-org)

(defcustom ask-org-journal-file "~/org/journal.org"
  "The file to store Journal Logs."
  :type 'string
  :group 'ask-org)

(defun ask-ox-org-clean-space (text backend _)
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

(defun ask-org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (dolist (font '("Consolas-with-Yahei"))
      (when (member font (font-family-list))
	(set-face-attribute (car face) nil :font font :weight 'regular :height (cdr face))))))

;; Configuration
(with-eval-after-load 'org
  ;;enabel <s TAB 
  (require 'org-tempo)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGHUP(h)" "|" "DONE(d)" "CANCEL(c)"))
	org-log-done 'time
	org-indent-mode nil	
	org-startup-indented nil
	org-ellipsis " ▾"
	org-pretty-entities t
	org-src-fontify-natively t
	org-edit-src-content-indentation 0
	org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
  ;; Latex
  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted") ("" "xeCJK"))
	org-latex-minted-options '(("breaklines")))

  (ask-org-font-setup)

  (with-eval-after-load 'org-capture
    (setq org-capture-templates
	  '(("t" "Tasks" entry (file+headline ask-org-tasks-file "Tasks")
	     "* TODO [#B] %^{Title} %^g\n DEADLINE: %^t\n :PROPERTIES:\n :CATEGORY: %^{Category}\n :END:\n\n %?\n Added: %U"
	     :empty-lines 1
	     :time-prompt t)
	    ("j" "Journal" entry (file+headline ask-org-journal-file "Journal")
	     "* %U %^{Title} %^g\n %?"
	     :empty-lines 1))))

  (with-eval-after-load 'org-agenda
    (setq org-agenda-files (delete-dups (list ask-org-tasks-file ask-org-journal-file))
	  org-agenda-custom-commands
	  '(("w" . "任务安排")
	    ("ww" "所有任务" tags-todo "*")
	    ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
	    ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
	    ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
	    ("v" . "周视图")
	    ("vv" "所有事项"
	     ((agenda "" ((org-agenda-span 7)))))
	    ("vr" "提醒事项"
	     ((agenda "" ((org-agenda-span 7)))
	      (tags "CATEGORY=\"Reminder\"")	     
	    ("vw" "每周工作"
	     ((agenda "" ((org-agenda-span 7)))
	      (stuck "") ;; review stuck projects as designated by org-stuck-projects
	      (tags "CATEGORY=\"Work\"") ;; review all projects (assuming you use todo keywords to designate projects)
	      )))))))
  (when (featurep 'plantuml-mode)
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (org-babel-do-load-languages 'org-babel-load-languages
				 (append org-babel-load-languages
					 '((plantuml . t))))))

(provide 'ask-org)
;;; ask-org.el ends here
