;;; package --- summary
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets
    :ensure t)

  (defun autoinsert-yas-expand (&rest _)
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (use-package autoinsert
    :ensure t
    :init
    (setq auto-insert-query nil
	  auto-insert-directory (locate-user-emacs-file "templates"))
    (add-hook 'find-file-hook 'auto-insert)
    (auto-insert-mode 1)
    :config
    (define-auto-insert "\\.org?$" [ "default-org.org" autoinert-yas-expand ])
    (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
    (define-auto-insert "\\.sh?$" "default-sh.sh")))

(provide '+yasnippet)
;;; +yasnippet.el ends here
