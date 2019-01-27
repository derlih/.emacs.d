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
(package-refresh-contents)

(setq my_packages
      '(
        company
        company-quickhelp
        magit
        lsp-mode
        company-lsp
        go-mode
        flx-ido
        imenu-anywhere
        dockerfile-mode
        exec-path-from-shell
        dtrt-indent
        groovy-mode
        yaml-mode
        ag
        winnow
        ))

(cl-loop for pkg in my_packages do
         (message "Ensure %s installed" pkg)
         (unless (package-installed-p pkg)
             (package-install pkg)))

;; flx-ido
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; dockerfile-mode
(add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))

;; groovy mode
(add-to-list 'auto-mode-alist '("Jenkinsfile.*\\'" . groovy-mode))

;; YAML mode
(add-to-list 'auto-mode-alist '(".*\.yml\\'" . yaml-mode))

;; ag
(setq ag-highlight-search t)

(provide 'package_my)
