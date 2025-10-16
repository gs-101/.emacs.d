;;; -*- lexical-binding: t -*-

(use-package ess
  :vc (:url "https://github.com/emacs-ess/ESS")
  :ensure t)

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(use-package org-src
  :config
  (gs-101/add-many-to-list 'org-src-lang-modes
                           '(("bash" . bash-ts)
                             ("c" . c-ts)
                             ("c++" . c++-ts)
                             ("clojure" . clojure-ts)
                             ("cmake" . cmake-ts)
                             ("csharp" . csharp-ts)
                             ("css" . css-ts)
                             ("dart" . dart-ts)
                             ("dockerfile" . dockerfile-ts)
                             ("go" . go-ts)
                             ("html" . mhtml-ts)
                             ("java" . java-ts)
                             ("js" . js-ts)
                             ("json" . json-ts)
                             ("nix" . nix-ts)
                             ("python" . python-ts)
                             ("ruby" . ruby-ts)
                             ("rust" . rust-ts)
                             ("toml" . toml-ts)
                             ("typescript" . typescript-ts)
                             ("yaml" . yaml-ts))))

(use-package treesit-auto
  :vc (:url "https://github.com/gs-101/treesit-auto" :branch custom)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :custom
  (treesit-auto-install t)
  :ensure t)

(use-package git-commit-ts-mode
  :vc (:url "https://github.com/danilshvalov/git-commit-ts-mode")
  :ensure t)

(use-package magit
  :after git-commit-ts-mode magit
  :custom
  (git-commit-major-mode 'git-commit-ts-mode))

(use-package mason
  :vc (:url "https://github.com/deirn/mason.el")
  :ensure t
  :init
  (mason-ensure))

(use-package compile
  :bind
  (:map compilation-mode-map
        ("n" . next-error-no-select)
        ("p" . previous-error-no-select)
        ("q" . kill-buffer-and-window))
  :custom
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
  (compilation-scroll-output t)
  (compilation-skip-threshold 2)
  :hook
  (compilation-mode . goto-address-mode)
  (compilation-filter . ansi-color-compilation-filter))

(use-package compile
  :after rust-ts-mode
  :config
  (push '(cargo "^\\ \\ -->\\ \\([/a-z_\\.]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist)
  (push 'cargo compilation-error-regexp-alist))

(use-package rmsbolt
  :vc (:url "https://gitlab.com/jgkamat/rmsbolt")
  :ensure t)

(use-package devcontainer
  :vc (:url "https://github.com/johannes-mueller/devcontainer.el")
  :ensure t)

(use-package devdocs
  :vc (:url "https://github.com/astoff/devdocs.el")
  :ensure t
  :bind
  (("C-h D" . devdocs-lookup)))

(use-package diff-mode
  :custom
  (diff-add-log-use-relative-names t))

(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd"
                                            "--all-scopes-completion"
                                            "--background-index"
                                            "--clang-tidy"
                                            "--completion-style=detailed")))
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout nil)
  (eglot-sync-connect nil))

(use-package eglot-codelens
  :vc (:url "https://github.com/Gavinok/eglot-codelens")
  :hook
  (eglot-managed-mode . eglot-codelens-mode)
  :ensure t)

(use-package eglot-x
  :vc (:url "https://github.com/nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup)
  :custom
  (eglot-x-enable-server-status nil)
  (eglot-x-enable-menu nil)
  :demand t
  :ensure t)

(use-package eglot-supplements
  :vc (:url "https://codeberg.org/harald/eglot-supplements")
  :ensure t)

(use-package eglot-cthier
  :after eglot
  :bind
  (:map eglot-mode-map
        ("C-c e H" . eglot-cthier-request-call-hierarchy)))

(use-package eglot-marocc
  :after eglot
  :bind
  (:map eglot-mode-map
        ("C-c e h" . eglot-marocc-request-highlights)
        ("C-c e n" . eglot-marocc-goto-next-highlight)
        ("C-c e p" . eglot-marocc-goto-previous-highlight)))

(use-package eglot-inactive-regions
  :vc (:url "https://github.com/fargiolas/eglot-inactive-regions")
  :ensure t
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.3)
  :hook
  (eglot-connect . eglot-inactive-regions-mode))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0))

(use-package eglot-signature-eldoc-talkative
  :vc (:url "https://codeberg.org/mekeor/eglot-signature-eldoc-talkative")
  :after eglot
  :config
  (advice-add #'eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative)
  :ensure t)

(use-package elec-pair
  :hook
  (minibuffer-mode . electric-pair-local-mode)
  (prog-mode . electric-pair-local-mode))

(use-package flymake
  :hook
  (prog-mode . flymake-mode))

(use-package flymake-collection
  :ensure t
  :config
  (push '((c-mode c-ts-mode) flymake-collection-gcc (flymake-collection-clang :disabled t)) flymake-collection-hook-config)
  (push '((python-mode python-ts-mode) flymake-collection-flake8 (flymake-collection-pycodestyle :disabled t)) flymake-collection-hook-config)
  :hook
  (flymake-mode . flymake-collection-hook-setup))

(use-package arduino-mode
  :vc (:url "https://repo.or.cz/arduino-mode.git")
  :ensure t)

(use-package clojure-ts-mode
  :vc (:url "https://github.com/clojure-emacs/clojure-ts-mode")
  :custom
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-toplevel-inside-comment-form t)
  :ensure t)

(use-package cider
  :vc (:url "https://github.com/clojure-emacs/cider")
  :after clojure-ts-mode
  :bind
  (:map cider-mode-map
        ("C-c C-p" . cider-jack-in-clj))
  :ensure t
  :hook
  (clojure-ts-mode . cider-mode))

(use-package css-mode
  :custom
  ;; It's two everywhere, really.
  (css-indent-offset 2))

(use-package dart-ts-mode
  :vc (:url "https://github.com/50ways2sayhard/dart-ts-mode")
  :ensure t)

(use-package flutter
  :bind
  (:map dart-ts-mode-map
        ([remap compile] . flutter-run-or-hot-reload)
        ("C-c C-c" . flutter-run-or-hot-reload))
  :config
  (defcustom gs-101/flutter-hot-reload-mode-lighter " Flutter Hot Reload"
    "Lighter for `gs-101/flutter-hot-reload-mode'."
    :type '(choice :tag "Lighter" (const :tag "No lighter" nil) string)
    :group 'flutter)

  (define-minor-mode gs-101/flutter-hot-reload-mode
    "Minor mode for running hot reload on save.

Only runs if a `flutter' buffer already exits."
    :lighter gs-101/flutter-hot-reload-mode-lighter
    (if (and gs-101/flutter-hot-reload-mode (get-buffer "*Flutter*"))
        (add-hook 'after-save-hook #'flutter-hot-reload nil 'local)
      (remove-hook 'after-save-hook #'flutter-hot-reload 'local)))
  :ensure t
  :hook
  (dart-ts-mode . gs-101/flutter-hot-reload-mode))

(use-package go-ts-mode
  :bind
  (:map go-ts-mode-map
        ("C-c m t f" . go-ts-mode-test-this-file)
        ("C-c m t p" . go-ts-mode-test-this-package)
        ("C-c m t ." . go-ts-mode-test-this-function-at-point)))

(use-package haskell-ts-mode
  :vc (:url "https://codeberg.org/pranshu/haskell-ts-mode")
  :ensure t)

(use-package hyprlang-ts-mode
  :vc (:url "https://github.com/Nathan-Melaku/hyprlang-ts-mode")
  :ensure t)

(use-package kotlin-ts-mode
  :vc (:url "https://gitlab.com/bricka/emacs-kotlin-ts-mode")
  :ensure t)

(use-package lisp
  :bind
  ("C-c d" . delete-pair)
  (:map lisp-mode-map
        ("C-c C-p" . run-lisp))
  :custom
  (inferior-lisp-program "sbcl")
  (narrow-to-defun-include-comments t))

(use-package slime
  :vc (:url "https://github.com/slime/slime")
  :ensure t)

(use-package cmuscheme
  :bind
  (:map scheme-mode-map
        ("C-c C-p" . "run-scheme")))

(use-package arei
  :when (gs-101/guix-p)
  :vc (:url "https://git.sr.ht/~abcdw/emacs-arei")
  :after scheme
  :ensure t)

(use-package nix-ts-mode
  :vc (:url "https://github.com/nix-community/nix-ts-mode")
  :ensure t)

;; Library
(use-package pg
  :vc (:url "https://github.com/emarsden/pg-el/")
  :ensure t)

(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs")
  :ensure t)

(use-package python
  :custom
  (python-indent-guess-indent-offset-verbose nil))

(use-package envrc
  :vc (:url "https://github.com/purcell/envrc")
  :ensure t
  :config
  (defun elfehr/advice-org-latex-preview-restart-envrc (&rest args)
    "Restart `envrc-global-mode' during Org startup."
    (if (and org-mode-loading envrc-global-mode)
        (envrc-mode 1)))
  (advice-add 'org-latex-preview :before 'elfehr/advice-org-latex-preview-restart-envrc)
  :init
  (envrc-global-mode))

(use-package cargo-transient
  :vc (:url "https://github.com/peterstuart/cargo-transient")
  :after rust-ts-mode
  :bind
  (:map rust-ts-mode-map
        ("C-c C-c" . compile)
        ("C-c C-p" . cargo-transient))
  :ensure t
  :custom
  (cargo-transient-buffer-name-function #'project-prefixed-buffer-name))

(use-package sh-script
  :bind
  (:map bash-ts-mode-map
        ("C-c C-p" . ansi-shell))
  (:map sh-mode-map
        ("C-c C-p" . ansi-shell)))

(use-package verilog-ts-mode
  :vc (:url "https://github.com/gmlarumbe/verilog-ts-mode")
  :ensure t)

(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode")
  :ensure t)

(use-package smerge-mode
  :init
  (smerge-mode))

(use-package subword
  :hook
  (prog-mode . superword-mode))

(use-package aggressive-indent
  :vc (:url "https://github.com/Malabarba/aggressive-indent-mode")
  :config
  (defun gs-101/aggressive-indent-mode-lisp ()
    "Enable `aggressive-indent-mode' in Lisp modes."
    (when (string-match-p "clojure.*-mode\\'" (symbol-name major-mode))
      (aggressive-indent-mode))
    (when (string-match-p "lisp.*-mode\\'" (symbol-name major-mode))
      (aggressive-indent-mode))
    (when (derived-mode-p 'scheme-mode)
      (aggressive-indent-mode)))
  :ensure t
  :hook
  (mhtml-ts-mode . aggressive-indent-mode)
  (prog-mode . gs-101/aggressive-indent-mode-lisp))

(use-package apheleia
  :vc (:url "https://github.com/radian-software/apheleia")
  :ensure t
  :config
  ;; Enabling code simplification for Go.
  (setf (alist-get 'gofmt apheleia-formatters) '("gofmt" "-s"))
  :hook
  (prog-mode . apheleia-mode))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-z t c")
  :ensure t
  :hook
  (prog-mode . combobulate-mode))

(use-package dape
  :vc (:url "https://github.com/svaante/dape")
  :ensure t
  :hook
  (dape-display-source . pulse-momentary-highlight-one-line))

(use-package debbugs
  :ensure t)

(use-package exercism
  :vc (:url "https://github.com/anonimitoraf/exercism.el")
  :commands
  (exercism)
  :custom
  (exercism--workspace
   (gs-101/filename "study/exercism/" gs-101/projects-code-directory))
  :ensure t)

(use-package git-modes
  :vc (:url "https://github.com/magit/git-modes")
  :ensure t)

(use-package leetcode
  :vc (:url "https://github.com/kaiwk/leetcode.el")
  :custom
  (leetcode-directory
   (gs-101/filename "study/leetcode-solutions/" gs-101/projects-code-directory))
  (leetcode--paid "$")
  (leetcode-save-solutions t)
  :ensure t)

(use-package magit
  :bind
  ("C-c v B" . magit-blame)
  ("C-c v C" . magit-clone)
  ("C-c v /" . magit-dispatch)
  ("C-c v F" . magit-fetch)
  ("C-c M-g" . magit-file-dispatch)
  ("C-c v x" . magit-file-delete)
  ("C-c v ." . magit-file-dispatch)
  ("C-c v L" . magit-log)
  ("C-c v g" . magit-status)
  ("C-c v G" . magit-status-here)
  ("C-c v c c" . magit-commit)
  ("C-c v c f" . magit-commit-fixup)
  ("C-c v l s" . magit-list-submodules)
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :ensure t
  :hook
  (magit-mode . magit-wip-mode)
  (magit-process-find-password-functions . magit-process-password-auth-source))

(use-package forge
  :after magit
  :ensure t)

(use-package orgit
  :vc (:url "https://github.com/magit/orgit")
  :after magit org
  :bind
  (:map magit-mode-map
        ("C-c m l s" . org-store-link))
  :ensure t)

(use-package package-lint
  :ensure t)

(use-package package-lint-flymake
  :after package-lint flymake
  :ensure t
  :hook
  (emacs-lisp-mode . package-lint-flymake-setup))

(use-package puni
  :vc (:url "https://github.com/AmaiKinono/puni")
  :bind
  (:map puni-mode-map
        ("M-h" . puni-expand-region)
        ("M-H" . puni-contract-region)
        ([remap mark-sexp] . puni-mark-sexp-at-point)
        ([remap transpose-sexps] . puni-transpose)
        ("C-)" . puni-slurp-forward)
        ("C-(" . puni-slurp-backward)
        ("C-}" . puni-barf-forward)
        ("C-{" . puni-barf-backward))
  :config
  (advice-add #'puni-kill-active-region :override
              (defun AmaiKinono/puni-kill-active-region ()
                "Kill active region.
When this will cause unbalanced state, ask the user to confirm,
unless `puni-confirm-when-delete-unbalanced-active-region' is
nil.
When `rectangle-mark-mode' is enabled, kill the marked
rectangular region instead."
                (interactive)
                (if (use-region-p)
                    (puni-kill-region)
                  ;; Fall back to Emacs default behavior which is signaling an error or what
                  ;; `kill-region-dwim' defines (since Emacs 31).
                  (call-interactively #'kill-region))))
  :ensure t
  :hook
  (minibuffer-mode . puni-disable-puni-mode)
  (text-mode . puni-disable-puni-mode)
  :init
  (puni-global-mode))

(use-package wakatime-mode
  :vc (:url "https://github.com/wakatime/wakatime-mode")
  :ensure t
  :init
  (defun gs-101/wakatime-api-key-from-auth ()
    "Get the Wakatime API key from either auth-source or secrets."
    (or (auth-source-pick-first-password :host "wakatime.com")
        (secrets-get-attribute "Keepass" "Wakatime" "api-key")))

  (defun gs-101/wakatime--enable-question ()
    "Let the user give one of the following answers regarding the value of
`wakatime-api-key':

- \"yes\" :: Get it from the system's secret tool
- \"from custom.el\" :: Get it from the value already saved in `custom-file'
- \"no\" :: Do not get the value"
    (let ((read-answer-short t))
      (read-answer "Enable wakatime tracking? "
                   '(("yes" ?y "Get API key from secrets")
                     ("no" ?n "Do not get a key, disabling wakatime")))))

  (defun gs-101/wakatime-enable-prompt ()
    "Prompt if the user wants to enable wakatime tracking.

Better asked on startup with an init hook:

(add-hook \'after-init-hook #\'gs-101/wakatime-enable-prompt)"
    ;; Disable dialog box
    (setq-local use-dialog-box nil)
    (unless wakatime-api-key
      (pcase (gs-101/wakatime--enable-question)
        ("yes"
         (progn
           (global-wakatime-mode)
           (setopt wakatime-api-key (gs-101/wakatime-api-key-from-auth))))
        ("no" (message "Ok, no tracking for you!")))))

  (add-hook 'after-init-hook #'gs-101/wakatime-enable-prompt))

(provide 'gs-dev)
