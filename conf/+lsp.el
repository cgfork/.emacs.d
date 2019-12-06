;;; package --- Initialize LSP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(cgfork/install 'lsp-mode)

(diminish 'lsp-mode)
(cgfork/after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (setq lsp-auto-guess-root t
	lsp-prefer-flymake nil
	flymake-fringe-indicator-position 'right-fringe)
  (cgfork/after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	  (append '("compile_commands.json"
		    ".ccls")
		  projectile-project-root-files-top-down-recurring))))

(when (cgfork/try-install 'company-lsp)
  (setq company-lsp-cache-candidates 'auto))

(when (cgfork/try-install 'lsp-ui)
  ;; `C-g'to close doc
  ;; (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (cgfork/after-load 'lsp-mode
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(provide '+lsp)
;;; +lsp.el ends here
