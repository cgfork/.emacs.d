;;; package --- summary
;;; Commentary:
;;; Code:

(use-package cider
  :ensure t
  :config
  (use-package clj-refactor
    :ensure t
    :config
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(clj-refactor-mode 1)
		(yas-minor-mode 1)
		(cljr-add-keybindings-with-prefix "C-c C-m")))))

(provide '+cider)
;;; +cider.el ends here
