;; Open buffer menu in same frame
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Open shell hotkey
(global-set-key (kbd "<f12>") 'shell)

;; Comment or uncomment
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Expand region
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q") 'er/expand-region)

;; Helm
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x x") 'execute-extended-command)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-p") 'helm-multi-files)
;; (global-set-key [f10] 'helm-semantic-or-imenu)
;; (global-set-key (kbd "M-p") 'helm-projectile-ag)
;; (global-set-key (kbd "C-M-p") 'helm-ag)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-c m") 'helm-all-mark-rings)

;;Helm-swoop
;; (global-unset-key (kbd "C-s"))
;; (global-unset-key (kbd "C-r"))
;; (global-set-key (kbd "C-s") 'helm-swoop)
;; (global-set-key (kbd "C-c s") 'search-forward)
;; (global-set-key (kbd "C-r") 'helm-multi-swoop-projectile)

;; Helm-git-grep
;; (global-unset-key (kbd "C-x g"))
;; (global-set-key (kbd "C-x g") 'helm-do-ag-project-root)

(provide 'keybindings_my)
