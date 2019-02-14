;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete trailing newlines
(defun my-other-delete-trailing-blank-lines ()
          "Deletes all blank lines at the end of the file."
          (interactive)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (delete-blank-lines)
              (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
                (if (> trailnewlines 1)
                    (progn
                      (delete-char trailnewlines)))))))
(add-hook 'before-save-hook 'my-other-delete-trailing-blank-lines)

;; Remove desktop after it's been read
(add-hook 'desktop-after-read-hook
      '(lambda ()
         ;; desktop-remove clears desktop-dirname
         (setq desktop-dirname-tmp desktop-dirname)
         (desktop-remove)
         (setq desktop-dirname desktop-dirname-tmp)))

;; winnow
(add-hook 'ag-mode-hook 'winnow-mode)
(add-hook 'compilation-mode-hook 'winnow-mode)

;; Shell mode
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; Default hook for dev files
(defun my-dev-hook ()
    "Hook for all development files."
    (linum-mode)
    (turn-on-eldoc-mode)
    (imenu-add-menubar-index)
    (dtrt-indent-mode)
    (flymake-mode)
    )

;; Company mode
(add-hook 'company-mode-hook 'company-quickhelp-local-mode)

;; LSP mode
;; Activate hooks for supported languages
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(add-hook 'lsp-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)))

;; Lisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'my-dev-hook)

;; Python
(add-hook 'python-mode-hook 'my-dev-hook)
(add-hook 'python-mode-hook #'lsp)

;; Go
(add-hook 'go-mode-hook 'my-dev-hook)
(add-hook 'go-mode-hook #'lsp)

;; Groovy
(add-hook 'groovy-mode-hook 'my-dev-hook)

;; YAML
(add-hook 'yaml-mode-hook 'my-dev-hook)

;; Web
(add-hook 'web-mode-hook 'my-dev-hook)
(add-hook 'typescript-mode-hook 'my-dev-hook)

;; C mode
(add-hook 'c-mode-common-hook 'my-dev-hook)
(defun my-clang-format ()
    (add-hook 'before-save-hook 'clang-format-buffer nil 'local))
(add-hook 'c++-mode-hook
          'my-clang-format)
(add-hook 'c-mode-hook
          'my-clang-format)

(provide 'hooks_my)
