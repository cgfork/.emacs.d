;;; package --- summary
;;; Commentary:
;;; Code:

;; Avy is a package for jumping to visible text
;; using a char-based decision tree.
;; Github: https://github.com/abo-abo/avy
;; Useful Commands:
;;   (avy-goto-char)
;;   (avy-goto-char-2)
;;   (avy-goto-char-timer)
;;   (avy-goto-line)
;;   (avy-goto-word-0)
;;   (avy-goto-word-1)
(use-package avy
  :ensure t
  :config
  (avy-setup-default))

(provide '+avy)
;;; +avy.el ends here
