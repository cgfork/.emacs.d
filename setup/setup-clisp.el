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
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  )

(provide 'setup-clisp)
;;; setup-clisp.el ends here
