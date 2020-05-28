;;; +clojure.el --- Clojure Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(power-emacs-install 'clojure-mode)
(power-emacs-install 'cljsbuild-mode)
(power-emacs-install 'elein)
(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'subword-mode)
  (cgfork-open-paredit 'clojure-mode-hook))

(power-emacs-install 'cider)
(setq nrepl-popup-stacktraces nil)
(with-eval-after-load 'cider
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (cgfork-open-paredit 'cider-repl-mode-hook))

(power-emacs-install 'flycheck-clojure)
(with-eval-after-load 'clojure-mode
  (with-eval-after-load 'cider
    (with-eval-after-load 'flycheck
      (flycheck-clojure-setup))))

(power-emacs-install 'clj-refactor)
(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook (lambda ()
				 (clj-refactor-mode 1)
				 (yas-minor-mode 1)
				 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(provide '+clojure)
;;; +clojure.el ends here
