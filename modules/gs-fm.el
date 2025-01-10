;;; -*- lexical-binding: t -*-

(use-package autorevert
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :init
  (global-auto-revert-mode t)
  :preface
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
  )

(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-agho --group-directories-first") ;;1
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  :defer t
  )

(use-package dired-aux
  :after dired
  :custom
  (dired-do-revert-buffer t)
  )

(use-package dired-async
  :hook
  (dired-mode . dired-async-mode)
  )

(use-package emacs
  :custom
  (delete-by-moving-to-trash t)
  )

(use-package mouse
  :custom
  (mouse-drag-and-drop-region-cross-program t)
  )

(use-package nov
  :custom
  (nov-header-line-format nil)
  :ensure t
  )

(use-package ready-player
  :vc (:url "https://github.com/xenodium/ready-player")
  :custom
  (ready-player-previous-icon "󰒮")
  (ready-player-play-icon "󰐊")
  (ready-player-stop-icon "󰓛")
  (ready-player-next-icon "󰒭")
  (ready-player-search-icon "󰍉")
  (ready-player-set-global-bindings nil)
  (ready-player-shuffle-icon "󰒝")
  (ready-player-open-externally-icon "󰒖")
  (ready-player-repeat-icon "󰑖")
  (ready-player-autoplay-icon "󰼛")
  :ensure t
  :hook
  (dired-mode . ready-player-mode)
  )

(use-package show-font
  :vc (:url "https://github.com/protesilaos/show-font")
  :ensure t
  )

(provide 'gs-fm)
