;;; -*- lexical-binding: t -*-

(use-package alert
  :vc (:url "https://github.com/jwiegley/alert")
  :custom
  (alert-default-style 'notifications)
  :ensure t)

(use-package pomm
  :vc (:url "https://github.com/SqrtMinusOne/pomm.el")
  :config
  (pomm-mode-line-mode)
  :custom
  (pomm-audio-enabled t)
  (pomm-audio-player-executable (executable-find "mpv"))
  :defer t
  :ensure t
  :hook
  (pomm-on-status-changed . pomm--sync-org-clock)
  (pomm-third-time-on-status-changed . pomm-third-time--sync-org-clock))

(use-package avy
  :vc (:url "https://github.com/abo-abo/avy")
  :bind
  ("M-g a" . avy-goto-char-timer)
  ([remap goto-char] . avy-goto-char)
  ([remap goto-line] . avy-goto-line)
  ("M-g w" . avy-goto-word-0)
  (:map isearch-mode-map
        ("M-j" . avy-isearch))
  :custom
  (avy-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
  ;; More uniform style, most jumps start with the same character.
  (avy-style 'de-bruijn)
  :config
  (defun karthinks/avy-action-kill-whole-line (pt)
    "Jump to target at marker PT, killing its whole line after the jump."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0))) t)

  (defun karthinks/avy-action-copy-whole-line (pt)
    "Jumpt to target at marker PT, copying its whole line to the kill ring,
without killing it."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0))) t)

  (defun karthinks/avy-action-yank-whole-line (pt)
    "Jump to target at marker PT, yanking its whole line to the current point."
    (karthinks/avy-action-kill-ring-save-whole-line pt)
    (save-excursion (yank)) t)

  (defun karthinks/avy-action-teleport-whole-line (pt)
    "Jump to target at marker PT, transposing it to the current point."
    (karthinks/avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun karthinks/avy-action-mark-to-char (pt)
    "Start mark at current point, then jump to target at marker PT
with the mark active. This sets an inclusive region selection between them."
    (activate-mark)
    (goto-char (+ pt 1)))

  (setf (alist-get ?w avy-dispatch-alist) #'avy-action-copy
        (alist-get ?W avy-dispatch-alist) #'karthinks/avy-action-copy-whole-line
        (alist-get ?k avy-dispatch-alist) #'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) #'karthinks/avy-action-kill-whole-line
        (alist-get ?  avy-dispatch-alist) #'karthinks/avy-action-mark-to-char ; This is bound to a space!
        (alist-get ?t avy-dispatch-alist) #'avy-action-teleport
        (alist-get ?y avy-dispatch-alist) #'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) #'karthinks/avy-action-yank-whole-line)
  :ensure t)

(use-package avy
  :after avy helpful
  :config
  (defun karthinks/avy-action-helpful (pt)
    "Jump to target at marker PT, and view its documentation
using Helpful."
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0))) t)

  (setf (alist-get ?H avy-dispatch-alist) #'karthinks/avy-action-helpful))

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap flymake-start] . consult-flymake)
   ([remap goto-line] . consult-goto-line)
   ([remap grep] . consult-grep)
   ([remap imenu] . consult-imenu)
   ([remap info] . consult-info)
   ([remap info-search] . consult-info)
   ([remap kmacro-menu] . consult-kmacro)
   ([remap list-buffers] . consult-buffer)
   ([remap load-theme] . consult-theme)
   ([remap locate] . consult-locate)
   ([remap man] . consult-man)
   ([remap pop-global-mark] . consult-global-mark)
   ([remap pop-to-mark-command] . consult-mark)
   ([remap project-find-regexp] . consult-grep)
   ([remap project-list-buffers] . consult-project-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap recentf] . consult-recent-file)
   ([remap rgrep] . consult-ripgrep)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap vc-git-grep] . consult-git-grep)
   ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
   ([remap yank-pop] . consult-yank-pop)
   ([remap execute-extended-command-for-buffer] . consult-mode-command)
   ("M-g I" . consult-imenu-multi))
  :config
  (defun oantolin/choose-completion-in-region ()
    "Use default `completion--in-region' unless we are not completing."
    (when minibuffer-completion-table
      (setq-local completion-in-region-function #'completion--in-region)))

  (advice-add #'register-preview :override #'consult-register-window)

  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring)

  (defvar-keymap minad/consult-line-map
    :doc "History keymap which is added to the local `consult-line' map."
    "C-s" #'previous-history-element)

  (consult-customize consult-line :keymap minad/consult-line-map)
  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (completion-in-region-function 'consult-completion-in-region)
  :ensure t
  :hook
  (minibuffer-setup . oantolin/choose-completion-in-region))

(use-package consult-dir
  :vc (:url "https://github.com/karthink/consult-dir")
  :after consult
  :bind
  ([remap list-directory] . consult-dir)
  ([remap dired-jump] . consult-dir-jump-file)
  :ensure t)

(use-package consult-notes
  :vc (:url "https://github.com/mclear-tools/consult-notes")
  :ensure t)

(use-package consult-notes
  :after org-roam
  :config
  (consult-notes-org-roam-mode))

(use-package consult-xref-stack
  :vc (:url "https://github.com/brett-lempereur/consult-xref-stack")
  :bind
  ([remap xref-go-back] . consult-xref-stack-backward)
  ([remap xref-go-forward] . consult-xref-stack-forward)
  :ensure t)

(use-package consult
  :after consult orderless
  :config
  (defun minad/consult--orderless-regexp-compiler (input type &rest _config)
    "Regular expression pattern compiler based on `orderless'."
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))
  :custom
  (consult--regexp-compiler #'minad/consult--orderless-regexp-compiler))

(use-package embark
  :vc (:url "https://github.com/gs-101/embark")
  :bind
  ([remap describe-bindings]. embark-bindings)
  ("C-;" . embark-act)
  (:map embark-collect-mode-map
        ("j" . goto-char))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  ;; Disable quitting after killing a buffer in an action
  (embark-quit-after-action '((kill-buffer . nil)))
  :ensure t)

(use-package embark
  :after embark avy
  :config
  (defun karthinks/avy-action-embark (pt)
    "Jump to target at marker PT, and act on it using Embark."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))))

  (setf (alist-get ?\; avy-dispatch-alist) 'karthinks/avy-action-embark))

(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :after embark
  :config
  (defun oantolin/embark-collect-resize-window (&rest _)
    "Resize the `embark-collect' window to match its contents."
    (when (memq embark-collect--kind '(:live :completions))
      (fit-window-to-buffer (get-buffer-window)
                            (floor (frame-height) 2) 1)))
  :hook
  (embark-collect-post-revert . oantolin/embark-collect-resize-window))

(use-package tinee
  :vc (:url "https://codeberg.org/tusharhero/tinee")
  :ensure t)

(use-package p-search
  :vc (:url "https://github.com/zkry/p-search")
  :bind
  ("M-s p" . p-search)
  :ensure t)

(use-package popper
  :vc (:url "https://github.com/karthink/popper")
  :bind
  ("C-z <up>" . popper-toggle)
  ("C-z <down>" . popper-cycle)
  :custom
  (popper-display-control t)
  (popper-reference-buffers '(cider-repl-mode
                              compilation-mode
                              eat-mode
                              eshell-mode
                              flutter-mode
                              geiser-repl-mode
                              vterm-mode
                              inferior-emacs-lisp-mode
                              inferior-lisp-mode
                              inferior-python-mode
                              inferior-scheme-mode
                              shell-mode
                              arei-connection-mode
                              "\\*ansi-term\\*"
                              "\\*Async Shell Command\\*"
                              "\\*Backtrace\\*"
                              "\\*compilation\\*"
                              "\\*Dtache Shell Command\\*"
                              "\\*eldoc\\*"
                              "\\*Ement Notifications\\*"
                              "*Flymake diagnostics.*"
                              "\\*GDB.*out\\*"
                              "\\*Messages\\*"
                              "\\*mu4e-update\\*"
                              "Output\\*$"
                              "^*tex"
                              "\\*Warnings\\*"
                              "\\*xref\\*"))
  :ensure t
  :demand t
  :init
  (popper-mode)
  (popper-echo-mode))

(use-package uniline
  :vc (:url "https://github.com/tbanel/uniline")
  :bind
  ("C-z i l" . uniline-mode)
  :ensure t)

(provide 'gs-utils)
