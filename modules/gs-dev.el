;;; -*- lexical-binding: t -*-

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  )

(use-package org-src
  :config
  (mapc (lambda (lang)
          (add-to-list 'org-src-lang-modes lang)) '(
          ("bash" . bash-ts)
          ("C" . c-ts)
          ("clojure" . clojure-ts)
          ("cmake" . cmake-ts)
          ("csharp" . csharp-ts)
          ("css" . css-ts)
          ("dart" . dart-ts)
          ("go" . go-ts)
          ("html" . html-ts)
          ("java" . java-ts)
          ("js" . js-ts)
          ("json" . json-ts)
          ("nix" . nix-ts)
          ("python" . python-ts)
          ("ruby" . ruby-ts)
          ("rust" . rust-ts)
          ("toml" . toml-ts)
          ("yaml" . yaml-ts)
          ))
  )

(use-package treesit-auto
  :vc (:url "https://github.com/gs-101/treesit-auto" :branch custom)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :custom
  (treesit-auto-install t)
  :ensure t
  )

(use-package git-commit-ts-mode
  :vc (:url "https://github.com/danilshvalov/git-commit-ts-mode")
  :ensure t
  )

(use-package magit
  :after git-commit-ts-mode magit
  :custom
  (git-commit-major-mode 'git-commit-ts-mode)
  )

(use-package compile
  :bind
  (
   :map compilation-mode-map
   ("n" . next-error-no-select)
   ("p" . previous-error-no-select)
   ("q" . kill-buffer-and-window)
   )
  :custom
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
  (compilation-scroll-output t)
  (compilation-skip-threshold 2)
  :defer t
  :hook
  (compilation-mode . goto-address-mode)
  (compilation-filter . ansi-color-compilation-filter)
  )

(use-package compile
  :after rust-ts-mode
  :config
  (push '(cargo "^\\ \\ -->\\ \\([/a-z_\\.]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist)
  (push 'cargo compilation-error-regexp-alist)
  )

(use-package diff-mode
  :custom
  (diff-add-log-use-relative-names t)
  :defer t
  )

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout nil)
  (eglot-sync-connect nil)
  :defer t
  )

(use-package eglot-codelens
  :vc (:url "https://github.com/Gavinok/eglot-codelens")
  :hook
  (eglot-managed-mode . eglot-codelens-mode)
  :ensure t
  )

(use-package eglot-x
  :vc (:url "https://github.com/nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup)
  :custom
  (eglot-x-enable-server-status nil)
  :demand t
  :ensure t
  )

(use-package eglot-supplements
  :vc (:url "https://codeberg.org/harald/eglot-supplements")
  :defer t
  :ensure t
  )

(use-package eglot-cthier
  :after eglot
  :bind
  (
   :map eglot-mode-map
   ("C-c e H" . eglot-cthier-request-call-hierarchy)
   )
  )

(use-package eglot-marocc
  :after eglot
  :bind
  (
   :map eglot-mode-map
   ("C-c e h" . eglot-marocc-request-highlights)
   ("C-c e n" . eglot-marocc-goto-next-highlight)
   ("C-c e p" . eglot-marocc-goto-previous-highlight)
   )
  )

(use-package eglot-marocc
  :after eglot-marocc catppuccin-theme
  :custom
  (set-face-attribute 'eglot-marocc-occurence-text nil :foreground (catppuccin-color 'green))
  )

(use-package eglot-inactive-regions
  :vc (:url "https://github.com/fargiolas/eglot-inactive-regions")
  :ensure t
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.3)
  :hook
  (eglot-connect . eglot-inactive-regions-mode)
  )

(use-package eldoc
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0)
  )

(use-package eglot-signature-eldoc-talkative
  :vc (:url "https://codeberg.org/mekeor/eglot-signature-eldoc-talkative")
  :after eglot
  :config
  (advice-add #'eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative)
  :ensure t
  )

(use-package eldoc-diffstat
  :vc (:url "https://github.com/kljohann/eldoc-diffstat/")
  :after eldoc
  :ensure t
  :hook
  (magit-log-mode . eldoc-diffstat-mode)
  )

(use-package elec-pair
  :hook
  (prog-mode . electric-pair-local-mode)
  )

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  )

(use-package flymake-collection
  :ensure t
  :config
  (push '((c-mode c-ts-mode) flymake-collection-gcc (flymake-collection-clang :disabled t)) flymake-collection-hook-config)
  (push '((python-mode python-ts-mode) flymake-collection-flake8 (flymake-collection-pycodestyle :disabled t)) flymake-collection-hook-config)
  :hook
  (flymake-mode . flymake-collection-hook-setup)
  )

(use-package clojure-ts-mode
  :vc (:url "https://github.com/clojure-emacs/clojure-ts-mode")
  :custom
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-toplevel-inside-comment-form t)
  :defer t
  :ensure t
  )

(use-package cider
  :vc (:url "https://github.com/clojure-emacs/cider")
  :after clojure-ts-mode
  :bind
  (
   :map cider-mode-map
   ("C-c C-p" . cider-jack-in-clj)
   )
  :ensure t
  :hook
  (clojure-ts-mode . cider-mode)
  )

(use-package dart-ts-mode
  :vc (:url "https://github.com/50ways2sayhard/dart-ts-mode")
  :defer t
  :ensure t
  )

(use-package flutter
  :bind
  (
   :map dart-ts-mode-map
   ([remap compile] . flutter-run-or-hot-reload)
   ("C-c C-c" . flutter-run-or-hot-reload)
   )
  :config
  (defcustom gs-101/flutter-hot-reload-mode-lighter " Flutter Hot Reload"
    "Lighter for `gs-101/flutter-hot-reload-mode'."
    :type '(choice :tag "Lighter" (const :tag "No lighter" nil) string)
    :group 'flutter)

  (define-minor-mode gs-101/flutter-hot-reload-mode
    "Minor mode for running hot reload on save.

Only runs if a Flutter buffer already exits."
    :lighter gs-101/flutter-hot-reload-mode-lighter
    (if (and gs-101/flutter-hot-reload-mode (get-buffer "*Flutter*"))
        (add-hook 'after-save-hook #'flutter-hot-reload nil 'local)
      (remove-hook 'after-save-hook #'flutter-hot-reload 'local)))
  :ensure t
  :hook
  (dart-ts-mode . gs-101/flutter-hot-reload-mode)
  )

(use-package go-ts-mode
  :bind
  (
   :map go-ts-mode-map
   ("C-c m t f" . go-ts-mode-test-this-file)
   ("C-c m t p" . go-ts-mode-test-this-package)
   ("C-c m t ." . go-ts-mode-test-this-function-at-point)
   )
  :defer t
  )

(use-package lisp
  :bind
  (
   :map lisp-mode-map
   ("C-c C-p" . run-lisp)
   )
  :custom
  (inferior-lisp-program "sbcl")
  (narrow-to-defun-include-comments t)
  :defer t
  )

(use-package nix-ts-mode
  :vc (:url "https://github.com/nix-community/nix-ts-mode")
  :ensure t
  :defer t
  )

;; Library
(use-package pg
  :vc (:url "https://github.com/emarsden/pg-el/")
  :ensure t
  )

(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs")
  :defer t
  :ensure t
  )

(use-package python
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :defer t
  )

(use-package envrc
  :vc (:url "https://github.com/purcell/envrc")
  :ensure t
  :init
  (envrc-global-mode)
  )

(use-package python-pytest
  :vc (:url "https://github.com/wbolster/emacs-python-pytest")
  :bind
  (
   :map python-base-mode-map
   ("C-c m t" . python-pytest-dispatch)
   )
  :ensure t
  )

(use-package cargo-transient
  :vc (:url "https://github.com/peterstuart/cargo-transient")
  :after rust-ts-mode
  :bind
  (
   :map rust-ts-mode-map
   ("C-c C-c" . compile)
   ("C-c C-p" . cargo-transient)
   )
  :ensure t
  :custom
  (cargo-transient-buffer-name-function #'project-prefixed-buffer-name)
  )

(use-package geiser
  :vc
  (
   :url "https://gitlab.com/emacs-geiser/geiser"
   :lisp-dir "elisp"
   )
  :after scheme
  :bind
  (
   :map scheme-mode-map
   ("C-c C-p" . geiser)
   ("C-c C-c" . gs-101/geiser-eval-dwim)
   )
  :config
  (defun gs-101/geiser-eval-dwim ()
    "Evaluate region if it is active; if not, evaluate the buffer.
If the region is active, this function calls `geiser-eval-region'.
Otherwise, it calls `geiser-eval-buffer'.

If the character before point is a closed parenthesis,
this calls `geiser-eval-last-sexp'."
    (interactive)
    (cond
     ((use-region-p) (geiser-eval-region (region-beginning) (region-end) t)
      (message "Region evaluated"))
     ((eq (char-before) ?\)) (eval-last-sexp nil)
      (message "Sexp evaluated"))
     (t (eval-buffer nil nil)
        (message "Buffer evaluated"))))
  :ensure t
  )

(use-package geiser-guile
  :vc (:url "https://gitlab.com/emacs-geiser/guile")
  :config
  (when (gs-101/nobara-p)
    (setq geiser-guile-binary "guile3.0"))
  :defer t
  :ensure t
  )

(use-package guix
  :if (gs-101/guix-p)
  :defer t
  :ensure t
  :hook
  (dired-mode . guix-prettify-mode)
  (scheme-mode . guix-devel-mode)
  (shell-mode . guix-prettify-mode)
  (shell-mode . guix-build-log-minor-mode)
  )

(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode")
  :defer t
  :ensure t
  )

(use-package sh-script
  :bind
  (
   :map bash-ts-mode-map
   ("C-c C-p" . ansi-shell)
   :map sh-mode-map
   ("C-c C-p" . ansi-shell)
   )
  :defer t
  )

(use-package smerge-mode
  :init
  (smerge-mode)
  )

(use-package subword
  :hook
  (prog-mode . subword-mode)
  )

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
  (css-mode . aggressive-indent-mode)
  (prog-mode . gs-101/aggressive-indent-mode-lisp)
  )

(use-package apheleia
  :vc (:url "https://github.com/radian-software/apheleia")
  :ensure t
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--style=microsoft" "--assume-filename"
          (or (apheleia-formatters-local-buffer-file-name)
              (apheleia-formatters-mode-extension)
              ".c"))
        )
  :hook
  (prog-mode . apheleia-mode)
  )

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :ensure t
  :bind
  (
   :map copilot-completion-map
   ("<tab>" . copilot-accept-completion)
   ("TAB" . copilot-accept-completion)
   ("C-<tab>" . copilot-accept-completion-by-word)
   ("C-TAB" . copilot-accept-completion-by-word)
   ("C-n" . copilot-next-completion)
   ("C-p" . copilot-previous-completion)
   )
  :config
  (dolist (pair '((clojure-ts-mode lisp-indent-offset)
                  (scheme-mode lisp-indent-offset)
                  (org-mode 2)))
    (add-to-list 'copilot-indentation-alist pair))
  )

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :bind
  (
   :map combobulate-key-map
   ([query-replace-regexp] . combobulate-cursor-edit-node-by-text-dwim)
   )
  :config
  (defun cxa/activate-combobulate-on-ts-mode ()
    "Enable `combobulate-mode' in tree-sitter modes."
    (when (string-match-p "-ts-mode\\'" (symbol-name major-mode))
      (combobulate-mode)))
  :custom
  (combobulate-key-prefix "C-z t c")
  :ensure t
  :hook
  (text-mode . cxa/activate-combobulate-on-ts-mode)
  (prog-mode . cxa/activate-combobulate-on-ts-mode)
  )

(use-package dape
  :vc (:url "https://github.com/svaante/dape")
  :defer t
  :ensure t
  :hook
  (dape-display-source . pulse-momentary-highlight-one-line)
  )

(use-package exercism
  :vc (:url "https://github.com/anonimitoraf/exercism.el")
  :commands
  (exercism)
  :custom
  (exercism--workspace (convert-standard-filename (expand-file-name "study/exercism/" gs-101/projects-code-directory)))
  :defer t
  :ensure t
  )

(use-package git-modes
  :vc (:url "https://github.com/magit/git-modes")
  :defer t
  :ensure t
  )

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel")
  :bind
  ("C-z g b" . gptel)
  ("C-z g DEL" . gptel-abort)
  ("C-z g a" . gptel-add)
  ("C-z g C-x C-f" . gptel-add-file)
  ("C-z g m" . gptel-menu)
  ("C-z g r" . gptel-rewrite)
  ("C-z g RET" . gptel-send)
  ("C-z g p" . gptel-system-prompt)
  :ensure t
  )

(use-package gptel-openai
  :after gptel
  :config
  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :stream t
    :key #'gptel-api-key
    :models '(
              gpt-4o
              gpt-4o-mini
              meta-llama-3.1-405b-instruct
              llama-3.2-90B-vision-instruct
              DeepSeek-R1
              ))
  )

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick")
  :after gptel
  :ensure t
  )

(use-package gptel-quick
  :after gptel-quick embark
  :bind
  (
   :map embark-general-map
   ("g" . gptel-quick)
   )
  )

(use-package leetcode
  :vc (:url "https://github.com/kaiwk/leetcode.el")
  :custom
  (leetcode-directory (convert-standard-filename (expand-file-name "study/leetcode-solutions/" gs-101/projects-code-directory)))
  (leetcode--paid "$")
  (leetcode-save-solutions t)
  (leetcode--User-Agent ("User Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:126.0) Gecko/20100101 Firefox/126.1"))
  :defer t
  :ensure t
  )

(use-package magit
  :vc (:url "https://github.com/magit/magit")
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
  (magit-process-find-password-functions . magit-process-password-auth-source)
  )

(use-package forge
  :vc (:url "https://github.com/magit/forge")
  :after magit
  :bind
  ("C-c v '" . forge-dispatch)
  ("C-c v c i" . forge-create-issue)
  ("C-c v c p" . forge-create-pullreq)
  ("C-c v f c" . forge-browse-commit)
  ("C-c v f i" . forge-browse-issue)
  ("C-c v f p" . forge-browse-pullreq)
  ("C-c v l i" . forge-list-issues)
  ("C-c v l n" . forge-list-notifications)
  ("C-c v l p" . forge-list-pullreqs)
  ("C-c v l r" . forge-list-repositories)
  :ensure t
  )

(use-package orgit
  :vc (:url "https://github.com/magit/orgit")
  :after magit
  :bind
  (
   :map magit-mode-map
   ("C-c m l s" . org-store-link)
   )
  :ensure t
  )

(use-package package-lint
  :ensure t
  )

(use-package package-lint-flymake
  :after package-lint flymake
  :ensure t
  :hook
  (emacs-lisp-mode . package-lint-flymake-setup)
  )

(use-package puni
  :vc (:url "https://github.com/AmaiKinono/puni")
  :bind
  (
   :map puni-mode-map
   ("M-h" . puni-expand-region)
   ("M-H" . puni-contract-region)
   ("C-k" . puni-kill-line)
   ("C-M-n" . puni-forward-sexp-or-up-list)
   ("C-M-p" . puni-backward-sexp-or-up-list)
   ("C-M-<right>" . puni-forward-sexp-or-up-list)
   ("C-M-<left>" . puni-backward-sexp-or-up-list)
   ("C-M-@" . puni-mark-sexp-at-point)
   ("C-M-SPC" . puni-mark-sexp-at-point)
   ("M-k" . Gavinok/puni-kill-thing-at-point)
   ("C-M-t" . puni-transpose)
   ("C-)" . puni-slurp-forward)
   ("C-(" . puni-slurp-backward)
   ("C-M-)" . puni-barf-forward)
   ("C-M-(" . puni-barf-backward)
   )
  :defer t
  :config
  (defun Gavinok/puni-kill-thing-at-point (&optional arg)
    "Kill the next puni based thing at point."
    (interactive)
    (unless buffer-read-only
      (puni-expand-region)
      (kill-region (region-beginning) (region-end))))
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
  :custom
  (cursor-type 'bar)
  :hook
  (text-mode . puni-disable-puni-mode)
  :init
  (puni-global-mode)
  )

(use-package wakatime-mode
  :vc (:url "https://github.com/wakatime/wakatime-mode")
  :custom
  (wakatime-api-key (auth-source-pick-first-password :host "wakatime.com"))
  :ensure t
  :hook
  (prog-mode . global-wakatime-mode)
  )

(provide 'gs-dev)
