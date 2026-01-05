;;; -*- lexical-binding: t -*-

(use-package autorevert
  :config
  (add-to-list 'window-state-change-functions
               (defun xenodium/window-state-state-change (state)
                 "Enable `global-auto-revert-mode' per active window."
                 (let* ((old-selected-window (old-selected-window))
                        (old-buffer (when old-selected-window
                                      (window-buffer old-selected-window)))
                        (selected-window (selected-window))
                        (new-buffer (when selected-window
                                      (window-buffer selected-window))))
                   (when old-buffer
                     (with-current-buffer old-buffer
                       (when buffer-file-name
                         (auto-revert-mode -1))))
                   (when new-buffer
                     (with-current-buffer new-buffer
                       (when buffer-file-name
                         (auto-revert-mode +1)))))))
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :init
  (global-auto-revert-mode t))

(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-agho --group-directories-first") ;;1
  (dired-maybe-use-globstar t)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always))

(use-package dired-aux
  :after dired
  :custom
  (dired-create-destination-dirs 'always)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (dired-do-revert-buffer t)
  (dired-isearch-filenames 'dwim))

(use-package dired-async
  :hook
  (dired-mode . dired-async-mode))

(use-package emacs
  :custom
  (delete-by-moving-to-trash t))

(use-package mouse
  :custom
  (mouse-drag-and-drop-region-cross-program t))

(use-package reader
  :vc (:url "https://codeberg.org/divyaranjan/emacs-reader")
  :ensure-system-package
  (mupdf-gl . mupdf)
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
