;;; package --- summary
;;; Commentary:
;;; Code:

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex)
  )

(provide 'setup-smex)
;;; setup-smex.el ends here
