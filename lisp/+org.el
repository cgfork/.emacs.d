;;; +org.el --- org mode configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (ob-go) (htmlize))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(defun ewx-ox-org-clean-space (text backend _)
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

(defun ewx-org-font-setup ()
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

(defun ewx-org-mode-setup ()
  (org-indent-mode nil)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode 0))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
(set-face-attribute 'line-number nil :inherit 'fixed-pitch)
(set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

(use-package org
  :ensure nil
  :straight (:type built-in)
  :hook (org-mode . ewx-org-mode-setup)
  :config
  (add-to-list 'org-export-backends 'md)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGHUP(h)" "|" "DONE(d)" "CANCEL(c)"))
	org-log-done 'time
	org-startup-indented nil
	org-ellipsis " ▾"
	org-pretty-entities t
	org-src-fontify-natively t
	org-edit-src-content-indentation 0
	org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
  
  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted") ("" "xeCJK"))
	org-latex-minted-options '(("breaklines")))

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (use-package htmlize)
  (use-package ob-go)

  (use-package ox-gfm)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (go . t)
     (dot . t)))

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'org-src-lang-modes '("rust" . rustic))
  (ewx-org-font-setup))

;; (defun ewx-org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . ewx-org-mode-visual-fill))


;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

(provide '+org)
;;; +org.el ends here
