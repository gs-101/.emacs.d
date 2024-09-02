;;; -*- lexical-binding: t -*-

(use-package vterm
  :custom
  (vterm-shell "bash")
  (vterm-max-scrollback 10000)
  :ensure t
  )

(use-package vterm-toggle
  :bind
  ("C-c t v" . vterm-toggle)
  :custom
  (vterm-toggle-reset-window-configration-after-exit t)
  :ensure t
  )

(provide 'gs-cmd)
