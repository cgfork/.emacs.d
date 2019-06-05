;;; package --- summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'setup-const)
  (require 'setup-basic))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config (use-package yasnippet-snippets))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t)
  )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "\C-n") #'company-select-next)
  (define-key company-active-map (kbd "\C-p") #'company-select-previous)
  )

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :ensure t
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (define-auto-insert "\\.org?$" [ "default-org.org" autoinsert-yas-expand ])
  (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
  (define-auto-insert "\\.sh?$" "default-sh.sh")
  )

(provide 'setup-company)
;;; setup-company.el ends here
