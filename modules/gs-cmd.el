;;; -*- lexical-binding: t -*-

(use-package vterm
  :custom
  (vterm-shell "bash")
  (vterm-max-scrollback 10000)
  :ensure t
  )

(use-package vterm
  :requires vterm
  :preface
  (defun mocompute/project-shell ()
    "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
    (interactive)
    (require 'comint)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "shell"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if (and shell-buffer (not current-prefix-arg))
          (if (comint-check-proc shell-buffer)
              (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
            (vterm shell-buffer))
        (vterm (generate-new-buffer-name default-project-shell-name)))))
  :config
  (advice-add 'project-shell :override #'mocompute/project-shell)
  )

(use-package vterm-toggle
  :requires vterm
  :bind
  ("C-c t v" . vterm-toggle)
  :custom
  (vterm-toggle-reset-window-configration-after-exit t)
  :ensure t
  )

(provide 'gs-cmd)
