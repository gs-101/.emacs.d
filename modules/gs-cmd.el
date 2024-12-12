;;; -*- lexical-binding: t -*-

(use-package eat
  :bind
  ([remap shell] . eat)
  :ensure t
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  )

(use-package vterm
  :after vterm
  :bind
  ([remap project-shell] . mocompute/project-shell)
  :config
  (defun mocompute/project-shell ()
    "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "shell"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if (and shell-buffer (not current-prefix-arg))
          (if (comint-check-proc shell-buffer)
              (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
           (eat shell-buffer))
        (eat (generate-new-buffer-name default-project-shell-name)))))
  )

(provide 'gs-cmd)
