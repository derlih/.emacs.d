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

;; Default hook for dev files
(defun my-prog-mode-hook ()
    "Hook all modes based on 'prog-mode'."
    (linum-mode)
    (eldoc-mode)
    (dtrt-indent-mode)
    (flycheck-mode)
    (flycheck-pos-tip-mode)
    (condition-case nil (imenu-create-index-function) (error nil))
    (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
    (flyspell-prog-mode)

    (setq prettify-symbols-alist '(
                              ("<=" . ?≤)
                              (">=" . ?≥)
                              ("->" . ?→)))
    (prettify-symbols-mode 1))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; LSP mode
;; Activate hooks for supported languages
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(add-hook 'lsp-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)))

;; Lisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Python
(add-hook 'python-mode-hook #'lsp)

;; Go
(add-hook 'go-mode-hook #'lsp)

;; Groovy
(add-hook 'groovy-mode-hook 'my-prog-mode-hook)

;; C/C++ mode
(defun my-clang-format ()
    (add-hook 'before-save-hook 'clang-format-buffer nil 'local)
    (lsp))

(add-hook 'c++-mode-hook
          'my-clang-format)
(add-hook 'c-mode-hook
          'my-clang-format)

;; CSS
(add-hook 'css-mode-hook #'lsp)

;; Lua
(add-hook 'lua-mode-hook 'my-prog-mode-hook)

;; Yaml
(add-hook 'yaml-mode-hook 'my-prog-mode-hook)

;; web
(add-hook 'web-mode-hook #'lsp)

;; typescript
(add-hook 'typescript-mode-hook #'lsp)

(add-hook 'java-mode-hook #'lsp)

(provide 'hooks_my)
