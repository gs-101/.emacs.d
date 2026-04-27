;;; -*- lexical-binding: t -*-

(use-package advice
  :custom
  (ad-redefinition-action 'accept))

(use-package auth-source
  :config
  (add-to-list 'auth-sources "~/.config/sops-nix/secrets/authinfo"))

(use-package bytecomp
  :custom
  (byte-compile-warnings '(not obsolete)))

(use-package comp-run
  :custom
  (native-comp-async-query-on-exit t)
  (native-comp-async-report-warnings-errors 'silent))

(use-package completion-preview
  :config
  (dolist (command '(org-self-insert-command
                     org-delete-backward-char))
    (add-to-list 'completion-preview-commands command))
  :init
  (global-completion-preview-mode))

(use-package ediff-wind
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package editorconfig
  :init
  (editorconfig-mode))

(use-package emacs
  :bind
  ("<WakeUp>" . gs-101/greeter)
  (:map emacs-lisp-mode-map
        ("C-c C-c" . gs-101/eval-dwim)
        ("C-c m e b" . eval-buffer)
        ("C-c m e r" . eval-region)
        ("C-c C-p" . ielm))
  :config
  (defun gs-101/eval-dwim (arg)
    "Evaluate region if it is active; if not, evaluate the buffer.
If the region is active, this function calls `eval-region'.
Otherwise, it calls `eval-buffer'.

If the character before point is a closed parenthesis,
this calls `eval-last-sexp'.

ARG is used for `eval-last-sexp'."
    (interactive "P")
    (cond
     ((use-region-p) (eval-region (region-beginning) (region-end) t)
      (message "Region evaluated"))
     ((eq (char-before) ?\)) (eval-last-sexp arg)
      (message "Sexp evaluated"))
     (t (eval-buffer nil nil)
        (message "Buffer evaluated"))))
  (defun gs-101/greeter ()
    "Print a \"Welcome back!\" message after waking up from suspend."
    (interactive)
    (message "Welcome back!"))
  :custom
  (auto-save-include-big-deletions)
  (auto-window-vscroll nil) ; 3
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t) ; 3
  (bidi-paragraph-direction 'left-to-right)
  (completion-ignore-case t) ; 4
  (cursor-in-non-selected-windows nil) ; 3
  (debugger-stack-frame-as-list t) ; 5
  (enable-recursive-minibuffers t)
  (fast-but-imprecise-scrolling t) ; 3
  (focus-follows-mouse t)
  (font-use-system-font t)
  (frame-resize-pixelwise t) ; 1
  (frame-title-format "GNU Emacs")
  (history-delete-duplicates t) ; 5
  (hscroll-margin 2) ; 3
  (hscroll-step 1) ; 3
  (inhibit-compacting-font-caches t) ; 3
  (load-prefer-newer t) ; 2
  ;; Disable the cursor in the minibuffer prompt
  ;; 4
  (minibuffer-prompt-properties
   '(cursor-intangible t read-only t face minibuffer-prompt))
  (mouse-autoselect-window t)
  (read-buffer-completion-ignore-case t) ; 4
  (read-process-output-max (* 512 1024 1024)) ; 3
  (redisplay-skip-fontification-on-input t)
  (scroll-conservatively 100000)
  (scroll-margin 2)
  (scroll-step 1)
  (text-mode-ispell-word-completion nil)
  (user-full-name "Gabriel Santos")
  (window-resize-pixelwise nil) ; 1
  (word-wrap t)
  (words-include-escapes t)
  :hook
  ;; 4
  (minibuffer-setup . cursor-intangible-mode))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package files
  :bind
  ("C-c f r" . recover-this-file)
  :config
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ; 3
  :custom
  (auto-mode-case-fold nil)
  (backup-by-copying t) ; 2
  (backup-by-copying-when-linked t) ; 2
  (delete-old-versions t)
  (find-file-suppress-same-file-warnings t)
  (find-file-visit-truename t)
  (kept-new-versions 5) ; 2
  (kept-old-versions 5) ; 2
  (trusted-content `(,user-emacs-directory
                     "~/Projects/"))
  (version-control t) ; 2
  ;; 3
  (view-read-only t))

(use-package frame
  :bind
  ("C-z" . nil))

(use-package grep
  :bind
  ("M-s g g" . grep)
  ("M-s g l" . lgrep)
  ("M-s g r" . rgrep)
  :custom
  (grep-use-headings t))

(use-package vc-git
  :bind
  ("M-s g v" . vc-git-grep))

(use-package help
  :bind
  ("C-h C-b" . describe-prefix-bindings)
  :custom
  (help-window-select t))

(use-package ibuf-ext
  :hook
  (ibuffer-mode . ibuffer-auto-mode))

(use-package image-mode
  :custom
  (image-animate-loop t))

(use-package imenu
  :custom
  (imenu-flatten 'annotation)
  (imenu-space-replacement nil))

(use-package indent
  :custom
  (tab-always-indent 'complete)
  :defer t)

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-initial-delay 0.0))

(use-package minibuffer
  :custom
  (completion-pcm-leading-wildcard t)
  (completions-detailed t) ; 1
  (completions-format 'vertical)
  (completions-group t) ; 1
  (completion-styles '(flex partial-completion basic))
  (read-file-name-completion-ignore-case t) ; 2
  (resize-mini-windows t)
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode))

(use-package mode-local)

(use-package mouse
  :custom
  (mouse-yank-at-point t))

(use-package mule
  :config
  (prefer-coding-system 'utf-8))

(use-package newcomment
  :custom
  (comment-empty-lines t))

(use-package paren
  :config
  (custom-set-faces
   '(show-paren-match ((t :background unspecified
                          :foreground unspecified
                          :box (:line-width (-1 . -1))))))
  :custom
  (show-paren-delay 0)
  :init
  (show-paren-mode))

(use-package repeat
  :init
  (repeat-mode))

(use-package savehist
  :custom
  (history-length 300)
  :init
  (savehist-mode))

(use-package secrets)

(use-package sendmail
  :custom
  (sendmail-program (executable-find "msmtp")))

(use-package server
  :demand t
  :ensure nil
  :init
  (defun positron-solutions/server ()
    "Start the Emacs server if it's not running."
    (unless (bound-and-true-p server-process)
      (server-start)))
  :init
  (positron-solutions/server))

(use-package simple
  :bind
  ("C-x M-h" . captainflasmr/copy-buffer-to-kill-ring)
  ("M-g M-c" . gs-101/switch-to-minibuffer-dwim)
  ("M-\\" . nil) ;; unbind `delete-horizontal-space', use `cycle-spacing' instead
  ([remap capitalize-word] . capitalize-dwim)
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  :config
  (defun gs-101/switch-to-minibuffer-dwim ()
    "Switch to minibuffer in a regular window. In minibuffer, switch to previous window.
If currently in the minibuffer, this function calls `previous-window-any-frame'.
Otherwise, it calls `switch-to-minibuffer'."
    (interactive)
    (if (minibufferp)
        (previous-window-any-frame)
      (switch-to-minibuffer)))
  :custom
  (blink-matching-paren nil)
  (column-number-mode t)
  (completion-auto-select 'second-tab) ; 1
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t) ; 5
  (kill-read-only-ok t) ; 5
  (kill-region-dwim 'emacs-word)
  (kill-whole-line t) ; 5
  ;; Hides commands in completion that are not usable in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p) ; 4
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t) ; 5
  :config
  ;; 6
  (defun captainflasmr/copy-buffer-to-kill-ring (arg)
    "Mark the whole buffer, then copy it to the kill-ring without moving point.
With a ARG prefix argument, copy the buffer to the other window."
    (interactive "P")
    (if arg
        (save-window-excursion
          (kill-ring-save (point-min) (point-max))
          (other-window 1)
          (erase-buffer)
          (yank))
      (progn
        (save-excursion
          (kill-ring-save (point-min) (point-max)))
        (message "Buffer copied to kill-ring"))))
  :demand t)

(use-package startup
  :custom
  (initial-major-mode 'fundamental-mode)
  :defer t)

(use-package text-mode
  :hook
  (text-mode . visual-line-mode))

(use-package transient
  :custom
  (transient-mode-line-format nil))

(use-package emacs
  :custom
  ;; 64 mb
  (undo-limit 67108864)
  ;; 96 mb
  (undo-strong-limit 100663296)
  ;; 960 mb
  (undo-outer-limit 1006632960))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package use-package
  :custom
  (use-package-compute-statistics t)
  (use-package-enable-imenu-support t)
  (use-package-vc-prefer-newest t))

(use-package vc-hooks
  :init
  (vc-auto-revert-mode))

(use-package warnings
  :custom
  (warning-suppress-log-types '((comp) (bytecomp))))

(use-package xref
  :config
  (when (executable-find "rg")
    (setopt xref-search-program 'ripgrep)))

(use-package diredfl
  :vc (:url "https://github.com/purcell/diredfl")
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

(use-package gcmh
  :vc (:url "https://github.com/emacsmirror/gcmh")
  :ensure t
  :init
  (gcmh-mode))

(use-package disproject
  :vc (:url "https://github.com/aurtzy/disproject")
  :bind
  (:map ctl-x-map
        ("p" . disproject-dispatch))
  :ensure t)

(use-package kkp
  :vc (:url "https://github.com/benotn/kkp")
  :ensure t
  :init
  (global-kkp-mode))

(use-package nil-mode
  :vc (:url "https://github.com/gs-101/nil-mode")
  :ensure t)

(use-package no-littering
  :vc (:url "https://github.com/emacscollective/no-littering")
  :ensure t
  :config
  ;; https://github.com/emacscollective/no-littering#lock-files
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  (when (file-exists-p custom-file)
    (load-file custom-file))
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :init
  (no-littering-theme-backups))

(use-package recentf
  :after no-littering
  :custom
  (recentf-max-saved-items 1000)
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)
               (recentf-expand-file-name no-littering-var-directory))
  :config
  (recentf-mode))

(use-package system-packages
  :vc (:url "https://gitlab.com/jabranham/system-packages")
  :config
  :ensure t)

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 101)
  (scroll-margin 0)
  :init
  (ultra-scroll-mode)
  :ensure t)

(use-package with-editor
  :vc (:url "https://github.com/magit/with-editor")
  :init
  (shell-command-with-editor-mode)
  :ensure t)

(use-package xdg-launcher
  :vc (:url "https://github.com/emacs-exwm/xdg-launcher")
  :ensure t)

(provide 'gs-settings)
