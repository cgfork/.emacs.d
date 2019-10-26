;;; package --- summary
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(provide '+projectile)
;;; +projectile.el ends here
