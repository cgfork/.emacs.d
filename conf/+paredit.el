;;; package --- summary
;;; Commentary:
;;; Code:

;; highlight the brackets
(use-package highlight-parentheses
  :ensure t
  :init
  (highlight-parentheses-mode t))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (clojure-mode . enable-paredit-mode)
	 (eval-expression-minibuffer-setup . enable-paredit-mode))
  :config)

(provide '+paredit)
;;; +paredit.el ends here
