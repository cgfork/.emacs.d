;;; +org.el --- org mode configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (ob-go) (htmlize))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

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

(use-package org
  :ensure nil
  :straight (:type built-in)
  :config
  (add-to-list 'org-export-backends 'md)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGHUP(h)" "|" "DONE(d)" "CANCEL(c)"))
	org-log-done 'time
	org-startup-indented nil
	org-ellipsis "   "
	org-pretty-entities t
	org-src-fontify-natively t
	org-edit-src-content-indentation 0
	org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
  
  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted") ("" "xeCJK"))
	org-latex-minted-options '(("breaklines")))

  (use-package ob-go)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (go . t)
     (dot . t)))

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'org-src-lang-modes '("rust" . rustic)))

(use-package htmlize)

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

(provide '+org)
;;; +org.el ends here
