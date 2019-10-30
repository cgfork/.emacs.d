;;; package --- summary
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'company)
  (diminish 'company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (define-key global-map (kbd "M-/") 'company-complete)
    (define-key global-map (kbd "<backtab>") 'company-yasnippet)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (setq company-idle-delay 0
	  company-minimum-prefix-length 3)
    (cgfork/install 'company-prescient)
    (company-prescient-mode 1)))

(provide '+company)
;;; +company.el ends here
