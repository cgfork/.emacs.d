;;; package --- summary
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'clojure-mode)
  (cgfork/install 'cljsbuild-mode)
  (cgfork/install 'elein)

  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
    (add-hook 'clojure-mode-hook 'subword-mode)))

(when (cgfork/try-install 'cider)
  (setq nrepl-popup-stacktraces nil)

  (with-eval-after-load 'cider
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode))

  (cgfork/install 'flycheck-clojure)
  (with-eval-after-load 'clojure-mode
    (with-eval-after-load 'cider
      (with-eval-after-load 'flycheck
	(flycheck-clojure-setup))))

  (cgfork/install 'clj-refactor)
  (with-eval-after-load 'clj-refactor
    (add-hook 'clojure-mode-hook (lambda ()
				    (clj-refactor-mode 1)
				    (yas-minor-mode 1)
				    (cljr-add-keybindings-with-prefix "C-c C-m")))))

(provide '+cider)
;;; +cider.el ends here
