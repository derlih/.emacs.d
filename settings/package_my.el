;; Package manager:
;; Initialise package and add Melpa repository

(require 'package)

;; for gnu repository
(setq package-check-signature nil)

;; add repos
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(package-refresh-contents)
(package-install 'company)
(package-install 'company-quickhelp)
(package-install 'magit)
(package-install 'lsp-mode)
(package-install 'company-lsp)
(package-install 'go-mode)
(package-install 'flx-ido)
(package-install 'imenu-anywhere)

;; flx-ido
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'package_my)
