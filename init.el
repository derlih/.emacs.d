(package-initialize)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
;; Use doom themes for now
;; (require 'dark-mint-theme)
(require 'scratch_my)
(require 'package_my)
(require 'hooks_my)
(require 'keybindings_my)
