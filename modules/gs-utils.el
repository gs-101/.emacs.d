;;; -*- lexical-binding: t -*-

(use-package alert
  :custom
  (alert-default-style 'notifications)
  :ensure t
  )

(use-package pomm
  :config
  (pomm-mode-line-mode)
  :custom
  (pomm-audio-enabled t)
  :defer t
  :ensure t
  :hook
  (pomm-on-status-changed . pomm--sync-org-clock)
  (pomm-third-time-on-status-changed . pomm-third-time--sync-org-clock)
  )

(use-package consult
  :bind
  (
   ([remap bookmark-jump] . consult-bookmark)
   ([remap flymake-start] . consult-flymake)
   ([remap goto-line] . consult-goto-line)
   ([remap grep] . consult-grep)
   ([remap imenu] . consult-imenu)
   ([remap info] . consult-info)
   ([remap info-search] . consult-info)
   ([remap isearch-forward] . consult-line)
   ([remap isearch-forward-word] . consult-line)
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
   ([remap rgrep] . consult-ripgrep)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap vc-git-grep] . consult-git-grep)
   ([remap yank-from-kill-ring] . consult-yank-from-kill-ring)
   ([remap yank-pop] . consult-yank-pop)
   ("M-g I" . consult-imenu-multi)
   ("C-z M-x" . consult-mode-command)
   )
  :config
  (defun oantolin/choose-completion-in-region ()
    "Use default `completion--in-region' unless we are not completing."
    (when minibuffer-completion-table
      (setq-local completion-in-region-function #'completion--in-region)))
  (advice-add #'register-preview :override #'consult-register-window)
  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring)
  (defvar minad/consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map)
    "History keymap which is added to the local `consult-line' map.")
  (consult-customize consult-line :keymap minad/consult-line-map)
  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :ensure t
  :hook
  (minibuffer-setup . oantolin/choose-completion-in-region)
  )

(use-package consult-dir
  :after consult
  :bind
  ([remap list-directory] . consult-dir)
  ([remap dired-jump] . consult-dir-jump-file)
  :ensure t
  )

(use-package consult-gh
  :after consult
  :custom
  (consult-gh-code-action #'consult-gh--code-view-action)
  (consult-gh-default-clone-directory (expand-file-name gs-101/projects-code-directory))
  (consult-gh-default-orgs-list "gs-101")
  :defer t
  :ensure t
  )

(use-package consult-gh-embark
  :after consult-gh embark
  :ensure t
  )

(use-package consult-gh-forge
  :after consult-gh forge
  :custom
  (consult-gh-file-action #'consult-gh--files-view-action)
  (consult-gh-issue-action #'consult-gh-forge--issue-view-action)
  (consult-gh-pr-action #'consult-gh--forge-pr-view-action)
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  :ensure t
  )

(use-package consult-gh-transient
  :after consult-gh
  )

(use-package consult-notes
  :ensure t
  )

(use-package consult-notes
  :after org-roam
  :config
  (consult-notes-org-roam-mode)
  )

(use-package consult-xref-stack
  :vc (:url "https://github.com/brett-lempereur/consult-xref-stack")
  :bind
  ([remap xref-go-back] . consult-xref-stack-backward)
  ([remap xref-go-forward] . consult-xref-stack-forward)
  :ensure t
  )

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
  (consult--regexp-compiler 'minad/consult--orderless-regexp-compiler)
  )

(use-package embark
  :bind
  (
   ([remap describe-bindings]. embark-bindings)
   ("C-;" . embark-act)
   )
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  ;; Disable quitting after killing a buffer in an action
  (embark-quit-after-action '(
                              (kill-buffer . nil)
                              ))
  :ensure t
  )

(use-package embark-consult
  :defer t
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package embark
  :after embark
  :config
  (defun oantolin/embark-collect-resize-window (&rest _)
    "Resize the `embark-collect' window to match its contents."
    (when (memq embark-collect--kind '(:live :completions))
      (fit-window-to-buffer (get-buffer-window)
                             (floor (frame-height) 2) 1)))
  :hook
  (embark-collect-post-revert . oantolin/embark-collect-resize-window)
  )

(use-package embark
  :after embark
  :if (functionp 'karthinks/sudo-find-file)
  :bind
  (
   :map embark-file-map
   ("S" . karthinks/sudo-find-file)
   )
  )

(use-package embark
  :after embark
  :custom
  (embark-indicators '(
                       embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator
                       ))
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (car binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter :around #'embark-hide-which-key-indicator)
  )

(use-package gnosis
  :defer t
  :ensure t
  )

(use-package gnosis
  :after gnosis no-littering
  :custom
  (gnosis-dir (no-littering-expand-var-file-name "gnosis/"))
  )

(use-package p-search
  :vc (:url "https://github.com/zkry/p-search")
  :bind
  ("M-s p" . p-search)
  :ensure t
  )

(use-package popper
  :bind
  ("M-]" . popper-cycle)
  ("M-[" . popper-toggle)
  :custom
  (popper-display-control t)
  (popper-group-function 'popper-group-by-project)
  (popper-reference-buffers '(
                              compilation-mode
                              vterm-mode
                              inferior-emacs-lisp-mode
                              inferior-lisp-mode
                              inferior-python-mode
                              shell-mode
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
                              "\\*xref\\*"
                              ))
  :ensure t
  :demand t
  :init
  (popper-mode)
  (popper-echo-mode)
  )

(use-package uniline
  :bind
  ("C-z i l" . uniline-mode)
  :ensure t
  )

(provide 'gs-utils)
