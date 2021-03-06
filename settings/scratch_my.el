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

;; Case insensetive sort line
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun align-to-colon (begin end)
  "Align region to colon (:) signs."
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))

(defun align-to-comma (begin end)
  "Align region to comma  signs."
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

;; Move between frames
(windmove-default-keybindings 'meta)

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

;; Show the time on the status bar.
(display-time)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; In order to set emacs to delete the selected text when you press DEL, Ctrl-d,
;; or Backspace
(delete-selection-mode t)

;; Disable menu
(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

;; Disable toolbar
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))

;; Disable scroll bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn on column mode
(setq column-number-mode t)

;; Reset fringle background
(set-face-attribute 'fringe nil :background nil)

;; Set tab 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq-default standart-indent 4)
(setq-default c-basic-offset 4)
(setq c-default-style "linux")

;; Setup emacs backup directory
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


;; Session support
(desktop-save-mode 1)

;; Disable *scratch* buffer
(setq initial-scratch-message nil)

;; See matching pairs of parentheses
(show-paren-mode 1)

;; Automatically raises emacs frame when focused
(auto-raise-mode)

;; Automatically put closing quote or bracket
(electric-pair-mode 1)

;; Don't ask about opening unsafe files
(setq enable-local-variables :safe)

;; Disable ask to kill process on exit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; IDO autocompletion for find-file and other stuff
(ido-mode 1)

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Scroll settings
(setq scroll-step 1)
(setq scroll-margin 10)

;; Ensure new line to the end
(setq require-final-newline t)

;; IBuffer
(setq ibuffer-expert t)

;; Other settings
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Confirm kill
(setq confirm-kill-processes nil)

(provide 'scratch_my)
