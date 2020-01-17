;;; package --- Initialize Plantuml -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'plantuml-mode)
  (setq plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
  (cgfork/after-load 'plantuml-mode
    (plantuml-set-exec-mode "jar")))
(provide '+plantuml)
;;; +plantuml.el ends here
