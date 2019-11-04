;;; package --- Initialize Golang -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar cgfork/go--tools '("golang.org/x/tools/cmd/imports"
			   "golang.org/x/tools/cmd/gorename"
			   "github.com/go-delve/delve/cmd/dlv"
			   "github.com/josharian/impl"
			   "github.com/cweill/gotest..."
			   "github.com/fatih/gomodifytags"
			   "github.com/davidrjenni/reftools/cmd/fillstruct"
			   "github.com/golangci/golangci-lint/cmd/golangci-lint")
  "All necessary go tools.")

(defvar cgfork/go--tools-no-update `("golang.org/x/tools/gopls")
  "All necessary go tools without updateing the dependencies.")

;; Use `lexical-let' or `backquote' if you disabled lexical-binding.
;; (set-process-sentinel
;;  (start-process name buffer "echo" msg)
;;  `(lambda (proc _)
;;    (message ,msg)))
;; You can learn more detail about `lexical-binding' and `dynamic-binding'.
(defun cgfork/go-install-pkg (pkg)
  "Install the PKG, optionally NO-UPDATE."
  (interactive "sGo Package:")
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))
  (let* ((proc-name "go-tools")
	 (proc-buffer "*Go Tools*"))
    (message "Installing %s." pkg)
    (set-process-sentinel
     (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
     (lambda (proc _) 
       (let ((status (process-exit-status proc)))
	 (if (= 0 status)
	     (message "Installed %s." pkg)
	   (message "Failed to install %s: %d." pkg status)))))))

(defun cgfork/go-install-pkg-no-update (pkg)
  "Install the PKG, optionally NO-UPDATE."
  (interactive "sGo Package:")
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))
  (let* ((proc-name "go-tools")
	 (proc-buffer "*Go Tools*"))
    (message "Installing %s." pkg)
    (set-process-sentinel
     (start-process proc-name proc-buffer "go" "get" "-v" pkg)
     (lambda (proc _) 
       (let ((status (process-exit-status proc)))
	 (if (= 0 status)
	     (message "Installed %s." pkg)
	   (message "Failed to install %s: %d." pkg status)))))))

(defun cgfork/go-update-tools ()
  "Install or update go tools."
  (interactive)
  (dolist (pkg cgfork/go--tools-no-update)
    (cgfork/go-install-pkg-no-update pkg))
  (dolist (pkg cgfork/go--tools)
    (cgfork/go-install-pkg pkg)))

(cgfork/install 'go-mode)
(cgfork/after-load 'go-mode
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4
				  standard-indent 2
				  indent-tabs-mode nil)))
  (add-hook 'go-mode-hook #'lsp)
  (cgfork/after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (define-key go-mode-map [remap xref-find-definitions] 'godef-jump)
  (define-key go-mode-map (kbd "C-c R") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "<f1>") 'godoc-at-point)
  (unless (executable-find "gopls")
    (cgfork/go-update-tools))
  (cgfork/install 'go-dlv)
  (cgfork/install 'go-fill-struct)
  (cgfork/install 'go-impl)
  (cgfork/install 'go-rename)
  (when (executable-find "golangci-lint")
    (cgfork/install 'flycheck-golangci-lint)
    (cgfork/after-load 'flycheck
      (cgfork/after-load 'flycheck-golangci-lint
	(add-hook 'go-mode-hook (lambda ()
				  (add-to-list 'flycheck-disabled-checkers 'go-gofmt)
				  (add-to-list 'flycheck-disabled-checkers 'go-golint)
				  (add-to-list 'flycheck-disabled-checkers 'go-govet)
				  (add-to-list 'flycheck-disabled-checkers 'go-gobuild)
				  (add-to-list 'flycheck-disabled-checkers 'go-gotest)
				  (add-to-list 'flycheck-disabled-checkers 'go-errcheck)
				  (flycheck-golangci-lint-setup)))))))

(provide '+go)
;;; +go.el ends here
