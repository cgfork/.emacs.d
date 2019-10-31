;;; package --- summary
;;; Commentary:
;;; Code:

;; Which-key is a minor mode for Emacs that
;; displays the key bindings following the
;; currently entered incomplete command in
;; a popup.
 ;; Github: https://github.com/justbur/emacs-which-key
(when (cgfork/try-install 'which-key)
  (with-eval-after-load 'which-key
    (which-key-mode)))

(provide '+which-key)
;;; +which-key.el ends here
