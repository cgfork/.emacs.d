;;; package --- summary
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'setup-const)
  (require 'setup-basic))

(use-package slime
  :ensure t
  :init
  :config
  (slime-setup)
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  )

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (go-mode . enable-paredit-mode)
	 (eval-expression-minibuffer-setup . enable-paredit-mode))
  :config
  )

(provide 'setup-clisp)
;;; setup-clisp.el ends here
