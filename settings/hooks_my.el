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

;; Default hook for dev files
(defun my-dev-hook ()
        "Hook for all development files."
         (linum-mode)
         (turn-on-eldoc-mode)
         (imenu-add-menubar-index)
         (dtrt-indent-mode)
         (flymake-mode)
         (flycheck-mode)
         (with-eval-after-load 'flycheck
             (flycheck-pos-tip-mode))
         )

;; Company mode
(add-hook 'company-mode-hook 'company-quickhelp-mode)

;; LSP mode
;; Activate hooks for supported languages
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(add-hook 'lsp-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)))

(with-eval-after-load 'lsp-mode
    (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

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

(provide 'hooks_my)
