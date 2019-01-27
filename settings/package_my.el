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
(package-install 'magit)

(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-z") 'magit-status)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'package_my)
