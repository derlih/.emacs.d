;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete trailing newlines
(defun my-other-delete-trailing-blank-lines ()
          "Deletes all blank lines at the end of the file"
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

;; Magit
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-z") 'magit-status)

;; Lisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)

;; LSP mode
(add-hook 'lsp-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)
             ))

;; Python
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'python-mode-hook #'lsp)

;; Go
(add-hook 'go-mode-hook 'linum-mode)
(add-hook 'go-mode-hook #'lsp)

(provide 'hooks_my)
