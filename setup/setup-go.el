;;; package --- summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'setup-const)
  (require 'setup-basic))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

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
  (setq compilation-window-height 14)
  (add-hook 'compilation-mode-hook (lambda ()
				     (when (not (get-buffer-window "*compilation*"))
				       (save-selected-window
					 (save-excursion
					   (let* ((w (split-window-vertically))
						  (h (window-height w)))
					     (select-window w)
					     (switch-to-buffer "*compilation*")
					     (shrink-window (- h compilation-window-height))))))
				     ))
  (setq compilation-exit-message-function (lambda (status code msg)
					    (when (and (eq status 'exit) (zerop code))
					      (bury-buffer "*compilation*")
					      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
					    (cons msg code)))
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

(provide 'setup-go)
;;; setup-go.el ends here
