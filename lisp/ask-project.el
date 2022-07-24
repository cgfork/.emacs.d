;;; ask-project.el --- Project functions -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'files)
(require 'project)
(eval-when-compile (require 'cl-lib))

;; Installation
(unless ask-start-emacs-without-packages
  (use-package rg))

(defun ask-project-try-local (dir)
  "`project-try-vc' only resolves the `.git' or `.svn'. This function provides
a way to resolve the project root if DIR is not VC project."
  (catch 'ret
    (let ((root-tags '((".askproject"))))
      (dolist (current-level root-tags)
	(dolist (f current-level)
	  (when-let ((root (locate-dominating-file dir f)))
	    (throw 'ret (list 'local 'ask root))))))))

(cl-defmethod project-root ((project (head local)))
  (nth 2 project))
 
(defun ask-make-ask-project-file ()
  (interactive)
  (let* ((root-dir (read-directory-name "Root: "))
	 (f (expand-file-name ".askproject" root-dir)))
    (message "Create %s" f)
    (make-empty-file f)))

(defun ask-project-info ()
  "Print the project info."
  (interactive)
  (let* ((pc (project-current t))
	 (tag (car pc))
	 (tag-name (cadr pc))
	 (root (caddr pc)))
    (message "[%s/%s] %s" tag tag-name root)))

(defun ask-project-file-name ()
  "Get the current file name.

Return relative path to the project root if it is in a project."
  (let* ((pc (project-current))       
	 (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn root)
      (file-relative-name bfn root))
     (bfn bfn)
     (t (buffer-name)))))



;; Configuration
(setq project-find-functions '(project-try-vc ask-project-try-local))

(when (eq system-type 'darwin)
  (setq-default locate-command "mdfind"))

(unless ask-start-emacs-without-packages
  (with-eval-after-load 'rg
    (when (executable-find "rg")
      (global-set-key (kbd "C-c C-p") 'rg-project))))

(provide 'ask-project)
;;; ask-project.el ends here
