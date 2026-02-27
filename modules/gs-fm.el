;;; -*- lexical-binding: t -*-

(use-package dired
  :custom
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "--all --author --color=auto --group-directories-first --human-readable --dereference-command-line -l")
  (dired-maybe-use-globstar t)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always))

(use-package dired-aux
  :after dired
  :custom
  (dired-create-destination-dirs 'always)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (dired-isearch-filenames 'dwim)
  (dired-vc-rename-file t))

(use-package dired-async
  :hook
  (dired-mode . dired-async-mode))

(use-package emacs
  :custom
  (delete-by-moving-to-trash t))

(use-package mouse
  :custom
  (mouse-drag-and-drop-region-cross-program t))

(use-package pale
  :vc (:url "https://codeberg.org/pale/pale")
  :ensure t)

(use-package ready-player
  :vc (:url "https://github.com/xenodium/ready-player")
  :ensure-system-package
  (ffmpeg mpv)
  :custom
  (ready-player-set-global-bindings nil)
  :ensure t
  :hook
  (dired-mode . ready-player-mode))

(provide 'gs-fm)
