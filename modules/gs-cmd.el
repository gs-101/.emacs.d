;;; -*- lexical-binding: t -*-

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :bind
  ([remap shell] . eat)
  :ensure t
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  )

(provide 'gs-cmd)
