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
  :commands
  (
   dired
   )
  :custom
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-agho --group-directories-first") ;;1
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  )

(use-package dired-aux
  :custom
  (dired-do-revert-buffer t)
  )

(use-package dired-async
  :init
  (dired-async-mode)
  )

(use-package dired-preview
  :vc (:url "https://github.com/protesilaos/dired-preview")
  :custom
  (dired-preview-delay 0.5)
  (dired-preview-ignored-extensions-regexp (concat
                                            "\\(gs\\|"
                                            "zst\\|"
                                            "tar\\|"
                                            "xz\\|"
                                            "rar\\|"
                                            "zip\\|"
                                            "iso\\|"
                                            "epub"
                                            "\\)"
                                            ))
  :ensure t
  :init
  (dired-preview-global-mode)
  )

(use-package dired-preview
  :if (package-installed-p 'ready-player)
  :bind
  (
   :map dired-preview-mode-map
   ("C-c C-p" . prot/ready-player-dired-preview-play-toggle)
   )
  :preface
  (defun prot/ready-player-dired-preview-play-toggle ()
    "Call `ready-player-toggle-play-stop' on the currently previewed media file."
    (interactive)
    (dired-preview-with-window
      (if-let ((file buffer-file-name)
               (media (concat "\\." (regexp-opt ready-player-supported-media t) "\\'"))
               (_ (string-match-p media file)))
          (call-interactively #'ready-player-toggle-play-stop)
        (user-error "Cannot do something useful with `ready-player' here"))))
  )

(use-package dired
  :custom
  (delete-by-moving-to-trash t)
  )

(use-package mouse
  :custom
  (mouse-drag-and-drop-region-cross-program t)
  )

(use-package pdf-tools
  :bind
  (
   :map pdf-view-mode-map
   ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
   ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page)
   )
  :defer t
  :ensure t
  :hook
  (pdf-view-mode . pdf-view-themed-minor-mode)
  )

(use-package ready-player
  :custom
  (ready-player-previous-icon "󰒮")
  (ready-player-play-icon "󰐊")
  (ready-player-stop-icon "󰓛")
  (ready-player-next-icon "󰒭")
  (ready-player-search-icon "󰍉")
  (ready-player-shuffle-icon "󰒝")
  (ready-player-open-externally-icon "󰒖")
  (ready-player-repeat-icon "󰑖")
  (ready-player-autoplay-icon "󰼛")
  :ensure t
  :init
  (ready-player-mode)
  )

(use-package show-font
  :ensure t
  )

(provide 'gs-fm)
