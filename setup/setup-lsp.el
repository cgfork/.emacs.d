;;; package --- summary
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook (go-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'setup-lsp)
;;; setup-lsp.el ends here
