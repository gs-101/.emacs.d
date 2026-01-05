;;; -*- lexical-binding: t -*-

(use-package emacs
  :custom
  (menu-bar-mode nil)
  (ring-bell-function #'ignore)
  (scroll-preserve-screen-position t)
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (visible-bell nil)
  (x-stretch-cursor t)
  (x-underline-at-descent-line nil))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package frame
  :config (setq-mode-local doc-view-mode blink-cursor-mode nil)
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  :init
  (window-divider-mode))

(use-package mouse
  :if (display-graphic-p)
  :init
  (context-menu-mode))

(use-package org
  :custom
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars))

(use-package org
  :custom
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t))

(use-package org-modern
  :vc (:url "https://github.com/minad/org-modern")
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "󰪥󰪤󰪣󰪢󰪡󰪠󰪟")
  (org-modern-table-vertical 1)
  :ensure t
  :init
  (global-org-modern-mode))

(use-package org-agenda
  :custom
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "←──────────────")
  (org-agenda-time-grid
   '((daily today require-timed)
     (600 800 1000 1200 1400 1600 1800 2000 2200)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package startup
  :config
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)
  :custom
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-message t)
  (inhibit-startup-screen t)
  :defer t)

(use-package window
  :custom
  (recenter-positions '(top middle bottom)) ; 2
  (scroll-error-top-bottom t) ; 1
  (split-height-threshold nil) ;1
  (split-width-threshold 170) ; 1
  ;; 2
  (switch-to-buffer-obey-display-actions t))

(use-package catppuccin-theme
  :vc (:url "https://github.com/catppuccin/emacs")
  :when gs-101/nobara-p
  :config
  (load-theme 'catppuccin t)
  :ensure t)

(use-package crystal-point
  :vc (:url "https://github.com/laluxx/crystal-point")
  :init
  (crystal-point-enable)
  :ensure t)

(use-package doom-modeline
  :vc (:url "https://github.com/seagle0128/doom-modeline")
  :config
  (advice-add #'doom-modeline-lsp-icon :override
              (defun gs-101/doom-modeline-lsp-icon (text face)
                "Display LSP icon (or TEXT in terminal) with FACE.

This advice replaces the rocket icon with a electric plug icon."
                (if doom-modeline-lsp-icon
                    (doom-modeline-icon 'mdicon "nf-md-connection" "🔌" text :face face)
                  (propertize text 'face face))))
  :config
  ;; Enable only when there's a graphical environment available.
  (when (getenv "DISPLAY")
    (setopt doom-modeline-icon t))
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-icon nil)
  (doom-modeline-modal-modern-icon nil)
  :ensure t
  :init
  (doom-modeline-mode))

(use-package diredfl
  :vc (:url "https://github.com/purcell/diredfl")
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

(use-package eldoc-box
  :vc (:url "https://github.com/casouri/eldoc-box")
  :after eldoc
  :ensure t
  :hook
  (eldoc-box-buffer-setup . eldoc-box-prettify-ts-errors)
  (eldoc-mode . eldoc-box-hover-mode))

(use-package helpful
  :vc (:url "https://github.com/Wilfred/helpful")
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :config
  (custom-set-faces '(helpful-heading ((t :height 1.5))))
  :ensure t)

(use-package keycast
  :vc (:url "https://github.com/tarsius/keycast")
  :config
  (custom-set-faces
   '(keycast-key ((t :background nil
                     :foreground nil))))
  :custom
  (echo-keystrokes 0)
  :ensure t
  :init
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with `doom-modeline')."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (keycast-mode))

(use-package keycast
  :after keycast
  :config
  (dolist (substitute '((backward-delete-char-untabify "" "Erasing...")
                        (delete-backward-char "" "Erasing...")
                        (isearch-printing-char "" "Searching...")
                        (org-delete-backward-char "" "Erasing...")
                        (self-insert-command "" "Typing...")
                        (org-self-insert-command "" "Typing...")
                        (vertico-directory-enter nil nil)
                        (vertico-next nil nil)
                        (vertico-previous nil nil)))
    (add-to-list 'keycast-substitute-alist substitute)))

(use-package keycast
  :after embark
  :config
  (defun oantolin/keycast-store-action-key-cmd (cmd)
    "Store key and CMD command information for `keycast' use."
    (force-mode-line-update t)
    (setq this-command cmd
          keycast--this-command-keys (this-single-command-keys)
          keycast--this-command-desc cmd))
  (advice-add #'embark-keymap-prompter :filter-return #'oantolin/keycast-store-action-key-cmd)
  (defun oantolin/keycast--update-force (&rest _)
    "Version of `keycast--update' that accepts (and ignore) parameters."
    (keycast--update))
  (advice-add 'embark-act :before #'oantolin/keycast--update-force))

(use-package modus-themes
  :config
  (gs-101/run-at-time-daily
   "06:00"
   (modus-themes-load-theme 'modus-operandi))
  (gs-101/run-at-time-daily
   "18:00"
   (modus-themes-load-theme 'modus-vivendi))
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-completions
   '((matches . (semibold))
     (selection . (extrabold underline))))
  :init
  (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(use-package modus-themes
  :when dw/guix-p
  :config
  (when (file-exists-p (gs-101/filename "~/.emacs.d/var/matugen/matugen.el"))
    (load-file (gs-101/filename "~/.emacs.d/var/matugen/matugen.el"))))

(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el")
  :bind
  ("C-z i n" . nerd-icons-insert)
  :demand t
  :ensure t)

(use-package citar
  :after citar nerd-icons
  :config
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-record"
              :face 'nerd-icons-lgreen)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-file"
              :face 'nerd-icons-blue
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-link"
              :face 'nerd-icons-lblue
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-text"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (setq citar-indicators
        (list citar-indicator-cited-icons
              citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons)))

(use-package nerd-icons-completion
  :vc (:url "https://github.com/rainstormstudio/nerd-icons-completion")
  :after nerd-icons
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-completion
  :after nerd-icons
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :vc (:url "https://github.com/rainstormstudio/nerd-icons-dired")
  :after nerd-icons
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package eglot
  :after nerd-icons eglot
  :custom
  (eglot-code-action-indicator "󰌵"))

(use-package esh-mode
  :bind
  (:map eshell-mode-map
        ([remap completion-preview-insert] . thanos/eshell-preview-insert))
  :config
  (defun gs-101/eshell-lambda ()
    "This is just the code of the regular eshell prompt, but with a lambda
instead of $."
    (let ((prompt (concat (abbreviate-file-name (eshell/pwd))
                          (unless (eshell-exit-success-p)
                            (format " [%d]" eshell-last-command-status))
                          " λ ")))
      (propertize prompt 'face 'nerd-icons-lpurple)))

  (defun thanos/eshell-git-info ()
    "Return a string showing git information."
    (when (eq (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree") 0)
      (let* ((branch-raw (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))
             (branch (if (or (string-match-p "^fatal" branch-raw)
                             (string-match-p "^error" branch-raw))
                         "Unknown"
                       (string-trim branch-raw))))
        (concat (propertize "󰊢 " 'face 'nerd-icons-lred)
                (propertize branch 'face 'nerd-icons-lred)))))

  (defun thanos/eshell-prompt-multiline ()
    "Eshell Multiline Git prompt."
    (let ((separator (propertize " | " 'face 'shadow))
          (dir (propertize (format "%s" (abbreviate-file-name (eshell/pwd))) 'face 'dired-directory))
          (git-info (thanos/eshell-git-info))
          (sign (if (= (user-uid) 0)
                    (propertize "\#" 'face 'default)
                  (propertize (format "\n\nλ %s\n↳" user-login-name) 'face 'nerd-icons-lpurple))))
      (concat "\n" dir separator git-info sign " ")))

  (defun thanos/eshell-preview-insert ()
    "Alternative version of `completion-preview-insert' that doesn't insert an
additional space after completion."
    (interactive)
    (completion-preview-insert)
    (delete-char -1))
  :custom
  (eshell-banner-message "")
  (eshell-prompt-function 'gs-101/eshell-lambda))

(use-package emacs
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string '((error "󰃤" compilation-error)
                                      (warning "" compilation-warning)
                                      (note "󰎚" compilation-info))))

(use-package go-ts-mode
  :after nerd-icons go-ts-mode
  :hook
  (go-ts-mode . prettify-symbols-mode)
  (go-ts-mode . (lambda ()
                  (push '(":=" . ?) prettify-symbols-alist)
                  (push '("go" . ?󰟓) prettify-symbols-alist))))

(use-package nerd-icons-grep
  :vc (:url "https://github.com/hron/nerd-icons-grep")
  :after nerd-icons
  :ensure t
  :hook
  (grep-mode . nerd-icons-grep-mode))

(use-package nerd-icons-ibuffer
  :vc (:url "https://github.com/seagle0128/nerd-icons-ibuffer")
  :after nerd-icons
  :ensure t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  :after nerd-icons magit
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package ready-player
  :after nerd-icons ready-player
  :custom
  (ready-player-autoplay-icon "󰼛")
  (ready-player-help-icon "󰋖")
  (ready-player-next-icon "󰒭")
  (ready-player-open-externally-icon "󰒖")
  (ready-player-open-my-media-collection-icon "󰋜")
  (ready-player-play-icon "󰐊")
  (ready-player-previous-icon "󰒮")
  (ready-player-repeat-icon "󰑖")
  (ready-player-search-icon "󰍉")
  (ready-player-shuffle-icon "󰒝")
  (ready-player-stop-icon "󰓛"))

(use-package prism
  :vc (:url "https://github.com/alphapapa/prism.el")
  :config
  (defun gs-101/prism-mode-lisp ()
    "Enable `prism-mode' in Lisp modes."
    (when (string-match-p "clojure.*-mode\\'" (symbol-name major-mode))
      (prism-mode))
    (when (string-match-p "lisp.*-mode\\'" (symbol-name major-mode))
      (prism-mode))
    (when (derived-mode-p 'scheme-mode)
      (prism-mode)))
  :ensure t
  :hook
  (prog-mode . gs-101/prism-mode-lisp))

(use-package prism
  :after prism catppuccin-theme
  :config
  (defun prism-catppuccin-colors ()
    "Grab color definitions from catppuccin and use them to set prism's colors."
    (prism-set-colors
      :lightens '(0 5 10)
      :desaturations '(-2.5 0 2.5)
      :colors (mapcar #'catppuccin-get-color '(red
                                             peach
                                             yellow
                                             green
                                             sapphire
                                             lavender
                                             mauve))))
  (prism-catppuccin-colors))

(use-package rainbow-delimiters
  :vc (:url "https://github.com/Fanael/rainbow-delimiters")
  :config
  (defun gs-101/rainbow-delimiters-maybe ()
    "Enable `rainbow-delimiters-mode' only in non-lisp modes.

This is because I find `prism-mode' better for these modes."
    (unless (or (string-match-p "clojure.*-mode\\'" (symbol-name major-mode))
                (string-match-p "lisp.*-mode\\'" (symbol-name major-mode))
                (derived-mode-p 'scheme-mode))
      (rainbow-delimiters-mode)))
  :ensure t
  :hook
  (prog-mode . gs-101/rainbow-delimiters-maybe))

(provide 'gs-ui)
