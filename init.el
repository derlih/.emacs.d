(package-initialize)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(require 'dark-mint-theme)
(require 'scratch_my)
(require 'package_my)
(require 'hooks_my)
(require 'keybindings_my)

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (powershell rust-mode flycheck-pos-tip flycheck-color-mode-line flycheck yaml-mode winnow web-mode use-package typescript-mode protobuf-mode powerline magit lsp-ui imenu-anywhere groovy-mode go-mode flx-ido expand-region exec-path-from-shell dtrt-indent dockerfile-mode dimmer company-quickhelp company-lsp cmake-mode clang-format auto-compile ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
