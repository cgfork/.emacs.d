;;; package --- Initialize Avy -*- lexical-binding: t -*-
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
(cgfork/install 'avy)
(with-eval-after-load 'avy
  (avy-setup-default))

(provide '+avy)
;;; +avy.el ends here
