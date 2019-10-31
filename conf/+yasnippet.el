;;; package --- summary
;;; Commentary:
;;; Code:

(cgfork/install 'yasnippet)
(cgfork/install 'yasnippet-snippets)
(cgfork/install 'autoinsert)

(diminish 'yas-minor-mode)
(yas-minor-mode 1)

(setq auto-insert-query nil
      auto-insert-directory (locate-user-emacs-file "templates"))
(add-hook 'find-file-hook 'auto-insert)
(with-eval-after-load 'yasnippet
  (defun autoinsert-yas-expand (&rest _)
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (with-eval-after-load 'autoinsert
    (define-auto-insert "\\.org?$" [ "default-org.org" autoinsert-yas-expand ])
    (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
    (define-auto-insert "\\.sh?$" "default-sh.sh")))

(provide '+yasnippet)
;;; +yasnippet.el ends here
