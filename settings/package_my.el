(require 'package)

;; package repos
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
(when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
(add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
(add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)

(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

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

;; Optional dependencies for use-package
(use-package delight)

;; Builtin packages
(use-package shell
    :bind
    ("<f12>" . 'shell)
    :hook
    (shell-mode . compilation-shell-minor-mode))

(use-package autorevert
    :delight auto-revert-mode)

(use-package flyspell
    :if (or (executable-find "aspell") (executable-find "ispell"))
    :delight)

;; Packages
(use-package auto-package-update
    :custom
    (auto-package-update-delete-old-versions t)
    :config
    (auto-package-update-maybe))

(use-package magit
    :bind
    ("C-x C-z" . 'magit-status))

(use-package forge
    :after magit)

(use-package magit-todos
    :after magit)

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
    :delight
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
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js-mode))

(use-package flycheck-pos-tip
    :after (company flycheck))

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
    :delight
    :config
    (projectile-mode +1)
    :custom
    (projectile-indexing-method 'alien)
    :bind-keymap
    ("C-c p" . projectile-command-map))

(use-package dtrt-indent
    :after web-mode
    :delight
    :config
    (add-to-list 'dtrt-indent-hook-mapping-list '(web-mode-prog-mode javascript web-mode-code-indent-offset) t)
    (add-to-list 'dtrt-indent-hook-mapping-list '(web-mode-prog-mode css web-mode-css-indent-offset) t)
    (add-to-list 'dtrt-indent-hook-mapping-list '(web-mode-prog-mode sgml web-mode-markup-indent-offset) t))

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
    (web-mode-enable-auto-pairing nil)
    :config
    (add-hook 'web-mode-hook
              (lambda ()
                  (if (equal web-mode-content-type "javascript")
                          (web-mode-set-content-type "jsx"))))
    (setq-default web-mode-comment-formats
                  '(("javascript" . "//")
                    ("jsx" . "//"))))

(use-package prettier-js
    :hook ((web-mode . prettier-js-mode)
           (js-mode . prettier-js-mode)))

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
(use-package nginx-mode)
(use-package qml-mode)

(use-package helpful
  :if (>= emacs-major-version 25)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("H-h" . helpful-at-point)))

;; UI enchancements
(use-package doom-themes
    :custom
    (doom-themes-enable-bold t)
    (doom-themes-enable-italic t)
    :config
    (load-theme 'doom-challenger-deep t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

(use-package powerline
    :init
    (powerline-default-theme))

(use-package dimmer
    :config
    (dimmer-mode)
    :custom
    (dimmer-fraction 0.4))

(provide 'package_my)
