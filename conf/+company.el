;;; package --- summary
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-/" . company-complete)
	 ("<backtab>" . company-yasnippet)
	 :map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 3)
    ;; Better sorting and filtering
  (use-package company-prescient
    :ensure t
    :init (company-prescient-mode 1)))

(provide '+company)
;;; +company.el ends here
