;;; package --- summary
;;; Commentary:
;;; Code:

;; Start server.
(use-package server
  :ensure nil
					;:hook (after-init . server-mode)
  :config
  (unless (server-running-p) (add-hook 'after-init-hook #'server-mode))
  )

(provide '+server)
;;; +server.el ends here
