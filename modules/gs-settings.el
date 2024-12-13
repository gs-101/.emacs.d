;;; -*- lexical-binding: t -*-

(use-package advice
  :custom
  (ad-redefinition-action 'accept)
  )

(use-package bytecomp
  :custom
  (byte-compile-warnings '(not obsolete))
  )

(use-package comp-run
  :custom
  (native-comp-async-query-on-exit t)
  (native-comp-async-report-warnings-errors 'silent)
  )

(use-package ediff-wind
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  )

(use-package emacs
  :config
  (setq define-coding-system-alias '(UTF-8 'utf-8))
  :custom
  (auto-save-include-big-deletions)
  (auto-window-vscroll nil) ;; 3
  (bidi-inhibit-bpa t) ;; 3
  (completion-ignore-case t) ;; 4
  (cursor-in-non-selected-windows nil) ;; 3
  (debugger-stack-frame-as-list t) ;; 5
  (enable-recursive-minibuffers t)
  (fast-but-imprecise-scrolling t) ;; 3
  (frame-resize-pixelwise t) ;; 1
  (history-delete-duplicates t) ;; 5
  (hscroll-margin 2) ;; 3
  (hscroll-step 1) ;; 3
  (inhibit-compacting-font-caches t) ;; 3
  (load-prefer-newer t) ;; 2
  (locale-coding-system 'utf-8)
  ;; Disable the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(
                                  cursor-intangible t
                                  read-only t
                                  face minibuffer-prompt
                                  )) ;; 4
  (read-buffer-completion-ignore-case t) ;; 4
  (read-process-output-max (* 512 1024)) ;; 3
  (resize-mini-windows 'grow-only)
  (scroll-conservatively 100000)
  (scroll-margin 2)
  (scroll-step 1)
  (text-mode-ispell-word-completion nil)
  (user-full-name "Gabriel Santos")
  (window-resize-pixelwise nil) ;; 1
  (words-include-escapes t)
  :hook
  (minibuffer-setup . cursor-intangible-mode) ;; 4
  )

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback)
  )

(use-package files
  :config
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ;; 3
  (add-to-list 'find-file-not-found-functions #'xenodium/files-create-non-existent-directory) ;; 4
  :custom
  (auto-mode-case-fold nil)
  (backup-by-copying t) ;; 2
  (backup-by-copying-when-linked t) ;; 2
  (delete-old-versions t)
  (find-file-suppress-same-file-warnings t)
  (find-file-visit-truename t)
  (kept-new-versions 5) ;; 2
  (kept-old-versions 5) ;; 2
  (revert-without-query '("")) ;; 3
  (version-control t) ;; 2
  (view-read-only t) ;; 3
  :preface
  (defun xenodium/files-create-non-existent-directory ()
    "Create a non-existent directory."
    (when-let* ((file-name buffer-file-name)
                (parent-directory (file-name-parent-directory file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Create `%s' dir? " parent-directory)))
        (make-directory parent-directory t))))
  )

(use-package frame
  :bind
  ("C-z" . nil)
  )

(use-package grep
  :custom
  (grep-use-headings t)
  )

(use-package help
  :custom
  (help-window-select t)
  )

(use-package ibuf-ext
  :defer t
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  )

(use-package image-mode
  :after dired
  :custom
  (image-animate-loop t)
  )

(use-package imenu
  :custom
  (imenu-flatten 'annotation)
  (imenu-space-replacement nil)
  )

(use-package indent
  :custom
  (tab-always-indent 'complete)
  :defer t
  )

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-initial-delay 0.0)
  )

(use-package minibuffer
  :custom
  (completions-detailed t) ;; 1
  (completions-format 'vertical)
  (completions-group t) ;; 1
  (read-file-name-completion-ignore-case t) ;; 2
  (resize-mini-windows t)
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  )

(use-package mouse
  :custom
  (mouse-yank-at-point t)
  )

(use-package mule
  :config
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  :defer t
  )

(use-package mule-cmds
  :config
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  :custom
  (current-language-environment "UTF-8") ;; 2
  (default-input-method nil) ;; 1
  :defer t
  )

(use-package mule-util
  :custom
  (truncate-string-ellipsis "…")
  )

(use-package newcomment
  :custom
  (comment-empty-lines t)
  )

(use-package paren
  :config
  (set-face-attribute 'show-paren-match nil :box '(:line-width (-1 . -1)))
  :custom
  (show-paren-delay 0)
  :init
  (show-paren-mode)
  )

(use-package password-cache
  :custom
  (password-cache-expiry 60)
  )

(use-package pixel-scroll
  :init
  (pixel-scroll-precision-mode)
  )

(use-package savehist
  :custom
  (history-length 300)
  :init
  (savehist-mode)
  )

(use-package sendmail
  :custom
  (sendmail-program "msmtp")
  :defer t
  )

(use-package simple
  :custom
  (blink-matching-paren nil)
  (column-number-mode t)
  (completion-auto-select 'second-tab) ;; 1
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t) ;; 5
  (kill-read-only-ok t) ;; 5
  (kill-region-dwim 'emacs-word)
  (kill-whole-line t) ;; 5
  ;; Hides commands in completion that are not usable in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p) ;; 4
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t) ;; 5
  )

(use-package transient
  :custom
  (transient-mode-line-format nil)
  )

(use-package emacs
  :custom
  (undo-limit 67108864) ;; 64 mb
  (undo-strong-limit 100663296) ;; 96 mb
  (undo-outer-limit 1006632960) ;; 960 mb
  )

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  )

(use-package use-package
  :custom
  (use-package-compute-statistics t)
  (use-package-enable-imenu-support t)
  (use-package-vc-prefer-newest t)
  )

(use-package simple
  :hook
  (text-mode . visual-line-mode)
  )

(use-package warnings
  :custom
  (warning-suppress-log-types '((comp) (bytecomp)))
  )

(use-package disproject
  :bind
  (
   :map ctl-x-map
   ("p" . disproject-dispatch)
   )
  :ensure t
  )

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode)
  )

(use-package markdown-mode
  :defer t
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t)
  )

(use-package nil-mode
  :vc (:url "https://github.com/gs-101/nil-mode")
  :ensure t
  )

(use-package no-littering
  :ensure t
  :init
  (no-littering-theme-backups)
  )

(use-package no-littering
  :after no-littering
  :config
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  )

(use-package recentf
  :after no-littering
  :custom
  (recentf-max-saved-items 1000)
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)
               (recentf-expand-file-name no-littering-var-directory))
  :config
  (recentf-mode)
  )

(use-package no-littering
  :after no-littering
  :config
  (when (file-exists-p custom-file)
    (load-file custom-file))
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  )

(use-package olivetti
  :custom
  (olivetti-body-width 132)
  :defer t
  :ensure t
  )

(use-package otpp
  :after project
  :config
  (otpp-mode)
  (otpp-override-mode)
  :ensure t
  )

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize)
  )

(use-package emacs
  :init
  (defun karthinks/sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user-writable, aborting sudo"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|sudo@root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                 (concat "/sudo:root@localhost:" file))))
  )

(use-package vundo
  :bind
  (
   ([remap undo] . vundo)
   ([remap undo-redo] . vundo)
   :map vundo-mode-map
   ("C-n" . vundo-next)
   ("C-p" . vundo-previous)
   ("C-f" . vundo-forward)
   ("C-b" . vundo-previous)
   ("C-a" . vundo-stem-root)
   ("C-e" . vundo-stem-end)
   ("l" . nil)
   ("j" . vundo-goto-last-saved)
   )
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols)
  :ensure t
  )

(provide 'gs-settings)
