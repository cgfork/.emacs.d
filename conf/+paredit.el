;;; package --- summary
;;; Commentary:
;;; Code:

;; highlight the brackets
(when (cgfork/try-install 'highlight-parentheses)
  (with-eval-after-load 'highlight-parentheses
    (highlight-parentheses-mode t)))

(cgfork/install 'paredit)
(with-eval-after-load 'paredit
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode))

(provide '+paredit)
;;; +paredit.el ends here
