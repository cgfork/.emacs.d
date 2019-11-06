;;; package --- Initialize Plantuml -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'plantuml-mode)
  (with-eval-after-load 'plantuml-mode
    (plantuml-set-exec-mode "jar")))
(provide '+plantuml)
;;; +plantuml.el ends here
