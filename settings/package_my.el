(require 'package)

;; package repos
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

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
    (ido-ignore-extensions t)
    (ido-auto-merge-work-directories-length -1))

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
    :after company
    :custom
    (company-quickhelp-delay 0.2)
    :hook
    (company-mode . company-quickhelp-local-mode))

(use-package flycheck
    :config
    (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-pos-tip
    :after (company flycheck)
    :config
    (flycheck-pos-tip-mode))

(use-package flycheck-pycheckers
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint json-jsonlist))))

(use-package lsp-mode
    :custom
    ;; (lsp-print-io t)
    ;; (lsp-trace t)
    (lsp-prefer-flymake nil)
    (lsp-enable-on-type-formatting nil)
    (lsp-clients-go-gocode-completion-enabled nil)
    (lsp-clients-go-server-args '(
                                  "-enhance-signature-help"
                                  "-format-style=goimports"))

    (lsp-pyls-plugins-pyflakes-enabled nil)
    (lsp-pyls-plugins-pydocstyle-enabled nil)
    (lsp-pyls-plugins-pycodestyle-enabled nil)
    (lsp-pyls-plugins-pylint-enabled nil)
    (lsp-pyls-plugins-yapf-enabled nil)
    (lsp-enable-snippet nil))
;; (use-package lsp-ui
;;     :after lsp-mode
;;     :custom
;;     (lsp-ui-sideline-enable nil)
;;     (lsp-ui-doc-enable nil)
;;     (lsp-ui-peek-enable nil)
;;     (lsp-ui-flycheck-enable nil))
(use-package company-lsp
    :after lsp-mode
    :custom
    (company-lsp-async t))

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
    :mode
    ((".*\.yml\\'" . yaml-mode)
     (".*\.sls\\'" . yaml-mode))
    :bind
    ("C-c C-c" . 'comment-or-uncomment-region))

(use-package web-mode
    :mode
    (("\\.tsx\\'" . web-mode)
     ("\\.js\\'" . web-mode)
     ("\\.jsx\\'" . web-mode))
    :custom
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2)
    (web-mode-enable-auto-pairing nil)
    :config
    (add-hook 'web-mode-hook
              (lambda ()
                  (if (equal web-mode-content-type "javascript")
                          (web-mode-set-content-type "jsx"))))
    (setq-default web-mode-comment-formats
                  '(("javascript" . "//")
                    ("jsx" . "//"))))

(use-package prettier-js)

(use-package add-node-modules-path
    :config
    (add-hook 'flycheck-mode-hook 'add-node-modules-path)
    (add-hook 'web-mode-hook  (lambda()
                                  (add-node-modules-path)
                                  (prettier-js-mode))))

(use-package typescript-mode
    :mode "\\.ts\\'")

(use-package cmake-mode
    :mode "CMakeLists.txt"
    :custom
    (cmake-tab-width 4))

(use-package go-mode)
(use-package protobuf-mode)
(use-package rust-mode)
(use-package lua-mode)
(use-package powershell)
(use-package restclient)

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
