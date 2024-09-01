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
  (completion-ignore-case t) ;; 3
  (cursor-in-non-selected-windows nil)
  (debugger-stack-frame-as-list t) ;; 4
  (enable-recursive-minibuffers t)
  (frame-resize-pixelwise t) ;; 1
  (history-delete-duplicates t) ;; 4
  (load-prefer-newer t) ;; 2
  ;; Disable the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(
                                  cursor-intangible t
                                  read-only t
                                  face minibuffer-prompt
                                  )) ;; 3
  (read-buffer-completion-ignore-case t) ;; 3
  (resize-mini-windows 'grow-only)
  (window-resize-pixelwise nil) ;; 1
  (words-include-escapes t)
  :hook
  (minibuffer-setup . cursor-intangible-mode) ;; 3
  )

(use-package files
  :config
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ;; 1
  :custom
  (find-file-suppress-same-file-warnings t)
  (find-file-visit-truename t)
  (revert-without-query '("")) ;; 1
  (view-read-only t) ;; 1
  )

(use-package help
  :custom
  (help-window-select t)
  )

(use-package ibuffer
  :defer t
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  )

(use-package imenu
  :custom
  (imenu-space-replacement nil)
  :defer t
  )

(use-package indent
  :custom
  (tab-always-indent 'complete)
  :defer t
  )

(use-package lisp
  :custom
  (narrow-to-defun-include-comments t)
  :defer t
  :ensure nil
  )

(use-package minibuffer
  :custom
  (completions-detailed t) ;; 1
  (completions-format 'one-column) ;; 1
  (completions-group t) ;; 1
  (read-file-name-completion-ignore-case t) ;; 2
  (resize-mini-windows t)
  :defer t
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
  (setq prefer-coding-system 'utf-8) ;; 1
  :custom
  (current-language-environment "UTF-8") ;; 2
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
  :init
  (savehist-mode)
  )

(use-package simple
  :custom
  (completion-auto-select 'second-tab) ;; 1
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t) ;; 4
  (kill-read-only-ok t) ;; 4
  (kill-whole-line t) ;; 4
  ;; Hides commands in completion that are not usable in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p) ;; 3
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t) ;; 4
  )

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  )

(use-package use-package
  :custom
  (use-package-enable-imenu-support t)
  (use-package-vc-prefer-newest t)
  )

(use-package warnings
  :custom
  (warning-suppress-log-types '((comp) (bytecomp)))
  )

(use-package window
  :custom
  (recenter-positions '(top middle bottom))
  (switch-to-buffer-obey-display-actions t)
  )

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode)
  )

(use-package no-littering
  :ensure t
  :init
  (no-littering-theme-backups)
  )

(use-package no-littering
  :config
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  )

(use-package recentf
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)
               (recentf-expand-file-name no-littering-var-directory))
  :init
  (recentf-mode)
  )

(use-package no-littering
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  )

(provide 'gs-settings)

(use-package autorevert
  :custom
  (auto-revert-stop-on-user-input nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  :init
  (global-auto-revert-mode t)
  )
