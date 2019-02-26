;; Package manager:
;; Initialise package and add Melpa repository

(require 'package)

;;; Code:

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
        lsp-ui
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
        dimmer
        powerline
        web-mode
        cmake-mode
        typescript-mode
        clang-format
        protobuf-mode
        ))

(cl-loop for pkg in my_packages do
         (message "Ensure %s installed" pkg)
         (unless (package-installed-p pkg)
             (package-install pkg)))

;; powerline
(require 'powerline)
(powerline-default-theme)

;; flx-ido
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t
      ido-use-faces nil
      ido-ignore-extensions t)
(push ".exe" completion-ignored-extensions)

;; company
(require 'company-quickhelp)
(setq company-quickhelp-delay 0.2
      company-dabbrev-downcase nil
      company-show-numbers t
      company-tooltip-idle-delay 0
      company-idle-delay 0.1
      company-lsp-async t)

;; exec-path-from-shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

;; dimmer
(dimmer-mode)
(with-eval-after-load 'dimmer
    (setq dimmer-fraction 0.4))

;; lsp
(require 'lsp-ui)
(setq lsp-ui-sideline-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-doc-enable nil
      lsp-enable-on-type-formatting nil
      lsp-clients-go-gocode-completion-enabled nil)
(setq lsp-clients-go-server-args
      '(
        "-enhance-signature-help"
        "-format-style=goimports"
))

;; dockerfile-mode
(add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))

;; groovy mode
(add-to-list 'auto-mode-alist '("Jenkinsfile.*\\'" . groovy-mode))

;; YAML mode
(add-to-list 'auto-mode-alist '(".*\.yml\\'" . yaml-mode))

;; ag
(require 'ag)
(setq ag-highlight-search t)

;; Web dev
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))


(provide 'package_my)
;;; package_my.el ends here
