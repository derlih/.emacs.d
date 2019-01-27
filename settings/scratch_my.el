;; Open your emacs config dir
(defun open-emacs-config-dir ()
  "Open emacs config directory"
  (interactive)
  (find-file (concat user-emacs-directory "settings"))
  )

;; Find the first non-ascii character
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

;; Move between frames
(windmove-default-keybindings 'meta)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

;; Show the time on the status bar.
(setq display-time-24hr-format t)
(display-time)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; In order to set emacs to delete the selected text when you press DEL, Ctrl-d,
;; or Backspace
(delete-selection-mode t)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable menu
(menu-bar-mode -1)

;; Turn on column mode
(setq column-number-mode t)

;; Set tab 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-default-style "linux"
          c-basic-offset 4)
(setq indent-line-function 'insert-tab)

;; Setup emacs backup directory
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


;; Session support
(setq
      desktop-files-not-to-save "^$"
      desktop-dirname           "~/.emacs.d/"
      desktop-path              (list desktop-dirname)
      desktop-base-file-name    "emacs-desktop"
      desktop-save              t)
(desktop-save-mode 1)

;; Disable *scratch* buffer
(setq initial-scratch-message nil)

;; See matching pairs of parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Automatically raises emacs frame when focused
(auto-raise-mode)

;; Automatically put closing quote or bracket
(electric-pair-mode 1)

;; Don't ask about opening unsafe files
(setq enable-local-variables :safe)

;; Don't ask about process on close
;; (add-hook 'comint-exec-hook
;;           (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;; Disable ask to kill process on exit
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (cl-letf (((symbol-function #'process-list) (lambda ())))
;;     ad-do-it))


(provide 'scratch_my)
