;;; package --- summary
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (cgfork/after-load 'projectile
    (setq projectile-completion-system 'ivy)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (cgfork/try-install 'ibuffer-projectile))

(provide '+projectile)
;;; +projectile.el ends here
