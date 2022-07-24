;;; ask-go.el --- Go -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Installation
(unless ask-start-emacs-without-packages
  (use-package go-mode)
  (use-package gotest)
  (use-package ob-go))

;; Define
(defvar ask-go-pkgs--repos `("github.com/uudashr/gopkgs/v2/cmd/gopkgs"
			     "github.com/ramya-rao-a/go-outline"
			     "github.com/cweill/gotests/gotests"
			     "github.com/fatih/gomodifytags"
			     "github.com/josharian/impl"
			     "github.com/haya14busa/goplay/cmd/goplay"
			     "github.com/go-delve/delve/cmd/dlv"
			     "golang.org/x/tools/gopls"))

;; Use `lexical-let' or `backquote' if you disabled lexical-binding.
;; (set-process-sentinel
;;  (start-process name buffer "echo" msg)
;;  `(lambda (proc _)
;;    (message ,msg)))
;; You can learn more detail about `lexical-binding' and `dynamic-binding'.
(defun ask-go-pkgs--install (pkg &optional version)
  "Install the PKG with specified VERSION if provided."
  (unless (executable-find "go")
    (user-error "Unable to find `go' executable in `exec-path'!"))
  (let* ((proc-name "go-pkgs")
	 (proc-buffer "*Go PKGs*")
	 (pkg-version (if (or (null version)
			      (string-empty-p version))
			  "latest"
			version))
	 (whole-pkg (format "%s@%s" pkg pkg-version)))
    (message "Install %s." whole-pkg)
    (set-process-sentinel
     (start-process proc-name proc-buffer "go" "install" whole-pkg)
     (lambda (proc _)
       (let ((status (process-exit-status proc)))
	 (if (= 0 status)
	     (message "Installed %s." whole-pkg)
	   (message "Failed to install %s." whole-pkg)))))))

(defun ask-go-pkgs-install ()
  "Install the package provided by minubuffer"
  (interactive)
  (let ((pkg (completing-read "Go Package: " ask-go-pkgs--repos nil t)))
    (ask-go-pkgs--install pkg)))

;; Configuration
(unless ask-start-emacs-without-packages
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode-hook 'eglot-ensure)
    (add-hook 'go-mode-hook (lambda ()
			      (setq tab-width 4
				    standard-indent 2
				    indent-tabs-mode nil)))
    (when (executable-find "goimports")
      (setq gofmt-command "goimports"))
    (add-hook 'before-save-hook #'gofmt-before-save)
    (with-eval-after-load 'gotest
      (define-key go-mode-map (kbd "C-c C-t") 'go-test-current-test))
    (with-eval-after-load 'org
      (org-babel-do-load-languages 'org-babel-load-languages
				   (append org-babel-load-languages
					   '((go . t)))))))

(provide 'ask-go)
;;; ask-go.el ends here
