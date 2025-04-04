;;; -*- lexical-binding: t -*-

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(use-package org-src
  :config
  (gs-101/add-many-to-list 'org-src-lang-modes
                           '(("bash" . bash-ts)
                             ("C" . c-ts)
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
  :defer t
  :hook
  (compilation-mode . goto-address-mode)
  (compilation-filter . ansi-color-compilation-filter))

(use-package compile
  :after rust-ts-mode
  :config
  (push '(cargo "^\\ \\ -->\\ \\([/a-z_\\.]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist)
  (push 'cargo compilation-error-regexp-alist))

(use-package devdocs
  :vc (:url "https://github.com/astoff/devdocs.el")
  :ensure t
  :bind
  (("C-h D" . devdocs-lookup)))

(use-package diff-mode
  :custom
  (diff-add-log-use-relative-names t)
  :defer t)

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
  (eglot-sync-connect nil)
  :defer t)

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
  :defer t
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

(use-package clojure-ts-mode
  :vc (:url "https://github.com/clojure-emacs/clojure-ts-mode")
  :custom
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-toplevel-inside-comment-form t)
  :defer t
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

(use-package dart-ts-mode
  :vc (:url "https://github.com/50ways2sayhard/dart-ts-mode")
  :defer t
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
        ("C-c m t ." . go-ts-mode-test-this-function-at-point))
  :defer t)

(use-package haskell-ts-mode
  :vc (:url "https://codeberg.org/pranshu/haskell-ts-mode")
  :ensure t)

(use-package hyprlang-ts-mode
  :vc (:url "https://github.com/Nathan-Melaku/hyprlang-ts-mode")
  :ensure t)

(use-package lisp
  :bind
  (:map lisp-mode-map
        ("C-c C-p" . run-lisp))
  :custom
  (inferior-lisp-program "sbcl")
  (narrow-to-defun-include-comments t)
  :defer t)

(use-package nix-ts-mode
  :vc (:url "https://github.com/nix-community/nix-ts-mode")
  :ensure t
  :defer t)

;; Library
(use-package pg
  :vc (:url "https://github.com/emarsden/pg-el/")
  :ensure t)

(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs")
  :defer t
  :ensure t)

(use-package python
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :defer t)

(use-package envrc
  :vc (:url "https://github.com/purcell/envrc")
  :ensure t
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

(use-package arei
  :when (gs-101/guix-p)
  :vc (:url "https://git.sr.ht/~abcdw/emacs-arei")
  :after scheme
  :ensure t)

(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode")
  :defer t
  :ensure t)

(use-package sh-script
  :bind
  (:map bash-ts-mode-map
        ("C-c C-p" . ansi-shell))
  (:map sh-mode-map
        ("C-c C-p" . ansi-shell))
  :defer t)

(use-package smerge-mode
  :init
  (smerge-mode))

(use-package emacs
  :ensure nil
  :config
  (defun gs-101/backward-symbol (&optional arg)
    "Move backward until encountering the beginning of a symbol.
  With argument ARG, do this that many times.
  If ARG is omitted or nil, move point backward one symbol.

  NOTE: This should definitely be upstreamed."
    (interactive "^p")
    (forward-symbol (- (or arg 1))))

  (defun gs-101/kill-symbol (arg)
    "Kill characters forward until encountering the end of a symbol.
  With argument ARG, do this that many times."
    (interactive "p")
    (kill-region (point) (progn (forward-symbol arg) (point))))

  (defun gs-101/mark-symbol (&optional arg allow-extend)
    "Set mark ARG symbols from point or move mark one symbol.
  When called from Lisp with ALLOW-EXTEND omitted or nil, mark is
  set ARG symbols from point.
  With ARG and ALLOW-EXTEND both non-nil (interactively with prefix
  argument), the place to which mark goes is the same place \\[forward-symbol]
  would move to with the same argument; if the mark is active, it moves
  ARG symbols from its current position, otherwise it is set ARG symbols
  from point.
  When invoked interactively without a prefix argument and no active
  region, mark moves one symbol forward.
  When invoked interactively without a prefix argument, and region
  is active, mark moves one symbol away of point (i.e., forward
  if mark is at or after point, back if mark is before point), thus
  extending the region by one symbol.  Since the direction of region
  extension depends on the relative position of mark and point, you
  can change the direction by \\[exchange-point-and-mark]."
    (interactive "P\np")
    (cond ((and allow-extend
                (or (and (eq last-command this-command) (mark t))
                    (region-active-p)))
           (setq arg (if arg (prefix-numeric-value arg)
                       (if (< (mark) (point)) -1 1)))
           (set-mark
            (save-excursion
              (goto-char (mark))
              (forward-symbol arg)
              (point))))
          (t
           (push-mark
            (save-excursion
              (forward-symbol (prefix-numeric-value arg))
              (point))
            nil t))))

  (defun gs-101/backward-kill-symbol (arg)
    "Kill characters backward until encountering the beginning of a symbol.
  With argument ARG, do this that many times."
    (interactive "p")
    (gs-101/kill-symbol (- arg)))

  (defun gs-101/left-symbol (&optional n)
    "Move point N symbols to the left (to the right if N is negative).

  Depending on the bidirectional context, this may move either backward
  of forward in the buffer.  This is contrast with \\[gs-101/backward-symbol]
  and \\[forward-symbol], which see.

  Value is normally t.

  If an edge of the buffer or a field boundary is reached, point is left there
  and the function returns nil.  Field boundaries are not noticed
  if `inhibit-field-text-motion' is non-nil."
    (interactive "^p")
    (if (eq (current-bidi-paragraph-direction) 'left-to-right)
        (gs-101/backward-symbol n)
      (forward-symbol n)))

  (defun gs-101/right-symbol (&optional n)
    "Move point N symbols to the right (to the left if N is negative).

  Depending on the bidirectional context, this may move either backward
  of forward in the buffer.  This is contrast with \\[forward-symbol]
  and \\[backward-symbol], which see.

  Value is normally t.

  If an edge of the buffer or a field boundary is reached, point is left there
  and the function returns nil.  Field boundaries are not noticed
  if `inhibit-field-text-motion' is non-nil."
    (interactive "^p")
    (if (eq (current-bidi-paragraph-direction) 'left-to-right)
        (forward-symbol n)
      (gs-101/backward-symbol n)))

  (defun gs-101/transpose-symbols (arg)
    "Interchange symbols around point, leaving point at the end of them.
  With prefix arg ARG, effect is to take word before or around point
  and drag it foward past ARG or other symbols (backward if ARG negative).
  If ARG is zero, the symbols around or arger point and around of after mark
  are interchanged."
    (interactive "*p")
    (transpose-subr 'forward-symbol arg))

  (defun gs-101/downcase-symbol (arg)
    "Convert to lower case from point to end of symbol, moving over.

If point is in the middle of a symbol, the part of that symbol before point
is ignored when moving forward.

With negative argument, convert previous symbols but do not move."
    (interactive "p")
    (let ((start (point)))
      (if (minusp arg)
          (progn
            (forward-symbol arg)
            (downcase-region start (point))
            (goto-char start))
        (downcase-region (point) (forward-symbol arg)))))

  (defun gs-101/upcase-symbol (arg)
    "Convert to upper case from point to end of symbol, moving over.

If point is in the middle of a symbol, the part of that symbol before point
is ignored when moving forward.

With negative argument, convert previous symbols but do not move.
See also `gs-101/capitalize-symbol'."
    (interactive "p")
    (let ((start (point)))
      (if (minusp arg)
          (progn
            (forward-symbol arg)
            (upcase-region start (point))
            (goto-char start))
        (upcase-region (point) (forward-symbol arg)))))

  (defun gs-101/capitalize-symbol (arg)
    "Capitalize from point to the end of symbol, moving over.

With numerical argument ARG, capitalize the next ARG-1 symbols as well.
This gives the symbol(s) a first character in upper case
and the rest lower case.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative arugment, capitalize previous words but do not move."
    (interactive "p")
    (let ((start (point)))
      (if (minusp arg)
          (progn
            (forward-symbol arg)
            (capitalize-region start (point))
            (goto-char start))
        (capitalize-region (point) (forward-symbol arg)))))

  ;; capitalization functions are defined in C. Check this later.
  (defvar-keymap gs-101/symbol-mode-map
    :doc "Keymap used for `gs-101/symbol-mode'."
    "<remap> <forward-word>" #'forward-symbol
    "<remap> <backward-word>" #'gs-101/backward-symbol
    "<remap> <kill-word>" #'gs-101/kill-symbol
    "<remap> <mark-word>" #'gs-101/mark-symbol
    "<remap> <backward-kill-word>" #'gs-101/backward-kill-symbol
    "<remap> <left-word>" #'gs-101/left-symbol
    "<remap> <right-word>" #'gs-101/right-symbol
    "<remap> <transpose-words>" #'gs-101/transpose-symbols
    "<remap> <downcase-word>" #'gs-101/downcase-symbol
    "<remap> <upcase-word>" #'gs-101/upcase-symbol
    "<remap> <capitalize-word>" #'gs-101/capitalize-symbol)

  (define-minor-mode gs-101/symbol-mode
    "Enable keybindings for symbol commands."
    :keymap gs-101/symbol-mode-map
    :after-hook
    (setq-local case-symbols-as-words 't))

  (add-hook 'prog-mode-hook #'gs-101/symbol-mode))

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
  :bind
  (:map combobulate-key-map
        ([query-replace-regexp] . combobulate-cursor-edit-node-by-text-dwim))
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
  (prog-mode . cxa/activate-combobulate-on-ts-mode))

(use-package dape
  :vc (:url "https://github.com/svaante/dape")
  :defer t
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
   (convert-standard-filename
    (expand-file-name "study/exercism/" gs-101/projects-code-directory)))
  :defer t
  :ensure t)

(use-package git-modes
  :vc (:url "https://github.com/magit/git-modes")
  :defer t
  :ensure t)

(use-package leetcode
  :vc (:url "https://github.com/kaiwk/leetcode.el")
  :custom
  (leetcode-directory
   (convert-standard-filename
    (expand-file-name "study/leetcode-solutions/" gs-101/projects-code-directory)))
  (leetcode--paid "$")
  (leetcode-save-solutions t)
  :defer t
  :ensure t)

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
  (magit-process-find-password-functions . magit-process-password-auth-source))

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
        ([remap kill-word] . gs-101/puni-forward-kill-symbol)
        ([remap backward-kill-word] . gs-101/puni-backward-kill-symbol)
        ("C-)" . puni-slurp-forward)
        ("C-(" . puni-slurp-backward)
        ("C-}" . puni-barf-forward)
        ("C-{" . puni-barf-backward))
  :defer t
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

  (defun gs-101/puni-backward-kill-symbol (&optional n)
    "Kill symbol backward while keeping expressions balanced.
With prefix argument N, kill that many symbols.  Negative argument
means kill symbols forward."
    (interactive "p")
    (setq n (or n 1))
    (if (< n 0)
        (gs-101/puni-forward-kill-symbol (- n))
      (dotimes (_ n)
        (puni-soft-delete-by-move #'gs-101/backward-symbol nil nil 'kill
                                  'jump-and-reverse-delete))))

  (defun gs-101/puni-forward-kill-symbol (&optional n)
    "Kill symbol forward while keeping expressions balanced.
With prefix argument N, kill that many symbols.  Negative argument
means kill symbols backward."
    (interactive "p")
    (setq n (or n 1))
    (if (< n 0)
        (gs-101/puni-backward-kill-symbol (- n))
      (dotimes (_ n)
        (puni-soft-delete-by-move #'forward-symbol nil nil 'kill
                                  'jump-and-reverse-delete))))
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
    "Get the Wakatime API key from either auth-source or password-store."
    (or (auth-source-pick-first-password :host "wakatime.com")
        (secrets-get-attribute "Keepass" "Wakatime" "api-key")))

  (defun gs-101/wakatime-enable-prompt ()
    "Prompt if the user wants to enable wakatime tracking.

Better asked on startup with an init hook:

(add-hook \'after-init-hook #\'gs-101/wakatime-enable-prompt)"
    ;; Disable dialog box
    (setq-local use-dialog-box nil)
    (when (y-or-n-p "Enable wakatime tracking?")
      (global-wakatime-mode)
      (setopt wakatime-api-key (gs-101/wakatime-api-key-from-auth))))

  (gs-101/wakatime-enable-prompt))

(provide 'gs-dev)
