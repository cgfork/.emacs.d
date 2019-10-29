;;; package --- summary
;;; Commentary:
;;; Code:

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
	      ("\C-c R" . go-remove-unused-imports)
	      ("<f1>" . godoc-at-point)
	 )
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :hook (
         (before-save . gofmt-before-save)
         )
  :config
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda ()
			    (setq-default)
			    (setq tab-width 4)
			    (setq standard-indent 2)
			    (setq indent-tabs-mode nil)
			    ))

  (use-package golint
    :ensure t
    )
  (use-package go-dlv
    :ensure t
    )
  (use-package go-rename
    :ensure t
    )
  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook (lambda ()
			      (add-to-list (make-local-variable 'company-backends)
					   '(company-go company-yasnippet company-files company-capf)
					   )))
    )
  (use-package go-eldoc
    :ensure t
    :hook (go-mode . go-eldoc-setup)
    )
  (use-package go-guru
    :ensure t
    :hook (go-mode . go-guru-hl-identifier-mode)
    )
  )

(provide '+go)
;;; +go.el ends here
