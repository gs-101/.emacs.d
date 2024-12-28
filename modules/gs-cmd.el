;;; -*- lexical-binding: t -*-

(use-package eat
  :bind
  ([remap shell] . eat)
  :ensure t
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  )

(provide 'gs-cmd)
