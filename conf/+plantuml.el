;;; package --- Initialize Plantuml -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar cgfork/plantuml-jar
  (expand-file-name "plantuml.jar" cgfork/exec-path)
  "The absolute path of plantuml.jar")

(when (cgfork/try-install 'plantuml-mode)
  (setq plantuml-jar-path cgfork/plantuml-jar)
  (cgfork/after-load 'plantuml-mode
    (plantuml-set-exec-mode "jar")))
(provide '+plantuml)
;;; +plantuml.el ends here
