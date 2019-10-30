;;; package --- summary
;;; Commentary:
;;; Code:

;; Start server.
;; (require 'server)
(with-eval-after-load 'server
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-mode)))

(provide '+server)
;;; +server.el ends here
