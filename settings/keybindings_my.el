;; Open buffer menu in same frame
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Open shell hotkey
(global-set-key (kbd "<f12>") 'shell)

;; Comment or uncomment
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Expand region
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q") 'er/expand-region)

;; Magit
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-z") 'magit-status)

(provide 'keybindings_my)
