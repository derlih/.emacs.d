(require 'package)

;; package repos
(customize-set-variable 'package-archives
                        '(
                          ("marmalade" . "https://marmalade-repo.org/packages/")
                          ("Org-mode"  . "https://orgmode.org/elpa/")
                          ("melpa"     . "https://melpa.org/packages/")
                          ))

;; package initialization
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))

;; use-package
(when (not (package-installed-p 'use-package))
    (package-install 'use-package))
(require 'use-package)
(customize-set-variable 'use-package-always-ensure t)
(customize-set-variable 'use-package-always-defer t)
(customize-set-variable 'use-package-verbose nil) ;; useful for debug

;; Recompile the package if .el is newer then .elc
(customize-set-variable 'load-prefer-newer t)
(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

;; Builtin packages
(use-package shell
    :bind
    ("<f12>" . 'shell)
    :hook
    (shell-mode . compilation-shell-minor-mode))

;; Packages
(use-package magit
    :bind
    ("C-x C-z" . 'magit-status))

(use-package expand-region
    :bind
    ("C-q" . 'er/expand-region))

(use-package flx-ido
    :config
    (flx-ido-mode)
    (push ".exe" completion-ignored-extensions)
    :custom
    (ido-enable-flex-matching t)
    (ido-ignore-extensions t))

(use-package imenu-anywhere
    :bind
    ("C-." . #'ido-imenu-anywhere))

(use-package exec-path-from-shell
    :if
    (memq window-system '(mac ns x))
    :config
    (exec-path-from-shell-initialize))

(use-package company
    :diminish company-mode
    :custom
    (company-dabbrev-downcase nil)
    (company-show-numbers t)
    (company-tooltip-idle-delay 0)
    (company-idle-delay 0.1))
(use-package company-quickhelp
    :custom
    (company-quickhelp-delay 0.2)
    :hook
    (company-mode . company-quickhelp-local-mode))

(use-package flycheck
    :config
    (use-package flycheck-pos-tip
        :config
        (flycheck-pos-tip-mode)))

(use-package lsp-mode
    :custom
    (lsp-prefer-flymake :none)
    (lsp-enable-on-type-formatting nil)
    (lsp-clients-go-gocode-completion-enabled nil)
    (lsp-clients-go-server-args '(
                                  "-enhance-signature-help"
                                  "-format-style=goimports"))
    :config
    (use-package lsp-ui
        :custom
        (lsp-ui-sideline-enable nil)
        (lsp-ui-doc-enable nil)
        (lsp-ui-peek-enable nil)
        (lsp-ui-flycheck-enable t))
    (use-package company-lsp
        :custom
        (company-lsp-async t)))

(use-package ag
    :custom
    (ag-highlight-search t))

(use-package projectile
    :config
    (projectile-mode +1)
    :bind-keymap
    ("C-c p" . projectile-command-map))

(use-package dtrt-indent)
(use-package clang-format)

;; Modes
(use-package dockerfile-mode
    :mode "Dockerfile.*\\'")

(use-package groovy-mode
    :mode "Jenkinsfile.*\\'")

(use-package yaml-mode
    :mode ".*\.yml\\'")

(use-package web-mode
    :mode
    (("\\.tsx\\'" . web-mode)
     ("\\.js[x]\\'" . web-mode)))

(use-package typescript-mode
    :mode "\\.ts\\'")

(use-package go-mode)
(use-package cmake-mode)
(use-package protobuf-mode)

;; UI enchancements
(use-package powerline
    :init
    (powerline-default-theme))

(use-package dimmer
    :config
    (dimmer-mode)
    :custom
    (dimmer-fraction 0.4))

(provide 'package_my)
