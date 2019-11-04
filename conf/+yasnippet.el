;;; package --- summary
;;; Commentary:
;;; Code:

(cgfork/install 'yasnippet)
(cgfork/install 'yasnippet-snippets)

(diminish 'yas-minor-mode)
(yas-global-mode 1)

;; Or you can use `yas-minor-mode' on per-buffer basic
;; (yas-reload-all)
;;
;; (cgfork/after-load 'yasnippet
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))

;; Setup autoinsert.
(autoload 'yas-expand-snippet "yasnippet")
(defun autoinsert-yas-expand (&rest _)
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(auto-insert-mode 1) ;; trigger to load the autoinsert package.
(cgfork/after-load 'autoinsert
  (setq auto-insert-query nil
	auto-insert-directory (locate-user-emacs-file "templates"))
  (define-auto-insert "\\.org?$" [ "default-org.org" autoinsert-yas-expand ])
  (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
  (define-auto-insert "\\.sh?$" "default-sh.sh"))

(provide '+yasnippet)
;;; +yasnippet.el ends here
