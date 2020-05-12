;;; package --- Initialize Golang -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar cgfork-go--tools '("golang.org/x/tools/cmd/goimports"
			   "golang.org/x/tools/cmd/gorename"
			   "github.com/go-delve/delve/cmd/dlv"
			   "github.com/josharian/impl"
			   "github.com/cweill/gotests/..."
			   "github.com/fatih/gomodifytags"
			   "github.com/davidrjenni/reftools/cmd/fillstruct"
			   "github.com/golangci/golangci-lint/cmd/golangci-lint")
  "All necessary go tools.")

(defvar cgfork-go--tools-no-update `("golang.org/x/tools/gopls")
  "All necessary go tools without updateing the dependencies.")

;; Use `lexical-let' or `backquote' if you disabled lexical-binding.
;; (set-process-sentinel
;;  (start-process name buffer "echo" msg)
;;  `(lambda (proc _)
;;    (message ,msg)))
;; You can learn more detail about `lexical-binding' and `dynamic-binding'.
(defun cgfork-go-install-pkg (pkg)
  "Install the PKG."
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

(defun cgfork-go-install-pkg-no-update (pkg)
  "Install the PKG."
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

(defun cgfork-go-update-tools ()
  "Install or update go tools."
  (interactive)
  (dolist (pkg cgfork-go--tools-no-update)
    (cgfork-go-install-pkg-no-update pkg))
  (dolist (pkg cgfork-go--tools)
    (cgfork-go-install-pkg pkg)))

(power-emacs-install 'go-mode)
(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4
				  standard-indent 2
				  indent-tabs-mode nil)))
  (add-hook 'go-mode-hook #'lsp)
  ;; (add-hook 'go-mode-hook #'yas-minor-mode)
  (power-emacs-copy-shell-variables "zsh" "GOPATH" "GO111MODULE" "GOPROXY")
  ;; (power-emacs-set-shell-variable "GO111MODULE" "on")
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (unless (executable-find "gopls")
    (cgfork-go-update-tools))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (define-key go-mode-map (kbd "C-c i r") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "C-c i a") 'go-import-add)
  (define-key go-mode-map (kbd "<f1>") 'godoc-at-point)

  (power-emacs-install 'gotest)
  (define-key go-mode-map (kbd "C-c t t") 'go-test-current-test)
  (define-key go-mode-map (kbd "C-c t f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t p") 'go-test-current-project)
  (with-eval-after-load 'gotest (setq go-test-verbose t)))

(provide '+go)
;;; +go.el ends here
