;;; package --- summary
;;; Commentary:
;;; Code:

(cgfork/install 'flycheck)
(with-eval-after-load 'flycheck
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(provide '+flycheck)
;;; +flycheck.el ends here
