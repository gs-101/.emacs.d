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
  (native-comp-async-report-warnings-errors 'silent)
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
  ;; Disable the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(
                                  cursor-intangible t
                                  read-only t
                                  face minibuffer-prompt
                                  )) ;; 4
  (read-buffer-completion-ignore-case t) ;; 4
  (read-process-output-max (* 512 1024)) ;; 3
  (resize-mini-windows 'grow-only)
  (scroll-conservatively 10) ;; 3
  (text-mode-ispell-word-completion nil)
  (window-resize-pixelwise nil) ;; 1
  (words-include-escapes t)
  :hook
  (minibuffer-setup . cursor-intangible-mode) ;; 4
  )

(use-package files
  :config
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ;; 2
  (add-to-list 'find-file-not-found-functions #'xenodium/files-create-non-existent-directory) ;; 3
  :custom
  (auto-mode-case-fold nil)
  (confirm-kill-processes nil) ;; 3
  (find-file-suppress-same-file-warnings t)
  (find-file-visit-truename t)
  (revert-without-query '("")) ;; 2
  (view-read-only t) ;; 2
  :preface
  (defun xenodium/files-create-non-existent-directory ()
    "Create a non-existent directory."
    (when-let* ((file-name buffer-file-name)
                (parent-directory (file-name-parent-directory file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Create `%s' dir? " parent-directory)))
        (make-directory parent-directory t))))
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
  :commands
  (
   imenu
   )
  :custom
  (imenu-space-replacement nil)
  :defer t
  )

(use-package indent
  :custom
  (tab-always-indent 'complete)
  :defer t
  )

(use-package minibuffer
  :custom
  (completions-detailed t) ;; 1
  (completions-format 'one-column) ;; 1
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

(use-package mule-cmds
  :config
  (set-language-environment "UTF-8") ;; 2
  (setq prefer-coding-system 'utf-8) ;; 1
  :custom
  (current-language-environment "UTF-8") ;; 3
  (default-input-method nil) ;; 2
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

(use-package simple
  :custom
  (completion-auto-select 'second-tab) ;; 1
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t) ;; 5
  (kill-read-only-ok t) ;; 5
  (kill-whole-line t) ;; 5
  ;; Hides commands in completion that are not usable in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p) ;; 4
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t) ;; 5
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

(use-package warnings
  :custom
  (warning-suppress-log-types '((comp) (bytecomp)))
  )

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode)
  )

(use-package gnuplot
  :ensure t
  )

(use-package no-littering
  :vc (:url "https://github.com/gs-101/no-littering")
  :ensure t
  :init
  (no-littering-theme-backups)
  )

(use-package no-littering
  :requires no-littering
  :config
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  )

(use-package recentf
  :requires no-littering
  :custom
  (recentf-max-saved-items 1000)
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)
               (recentf-expand-file-name no-littering-var-directory))
  :init
  (recentf-mode)
  )

(use-package no-littering
  :requires no-littering
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  )

(provide 'gs-settings)
