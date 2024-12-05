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
          ("csharp" . csharp-ts)
          ("css" . css-ts)
          ("go" . go-ts)
          ("html" . html-ts)
          ("java" . java-ts)
          ("js" . js-ts)
          ("python" . python-ts)
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

(use-package compile-multi
  :bind
  ([remap compile] . compile-multi)
  :ensure t
  )

(use-package consult-compile-multi
  :after compile-multi consult
  :ensure t
  :config
  (consult-compile-multi-mode)
  )

(use-package compile-multi-embark
  :after compile-multi embark
  :ensure t
  :config
  (compile-multi-embark-mode)
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

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :ensure t
  :hook
  (eglot-managed-mode . eglot-booster-mode)
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
  :custom
  (eglot-x-enable-server-status nil)
  :ensure t
  )

(use-package eglot-supplements
  :vc (:url "https://codeberg.org/harald/eglot-supplements")
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

(use-package eldoc
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0)
  )

(use-package eglot-signature-eldoc-talkative
  :after eglot
  :config
  (advice-add #'eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative)
  :ensure t
  )

;; (use-package flymake
;;   :hook
;;   (prog-mode . flymake-mode)
;;   )

(use-package flymake-collection
  :ensure t
  :config
  (push '((c-mode c-ts-mode) flymake-collection-gcc (flymake-collection-clang :disabled t)) flymake-collection-hook-config)
  (push '((python-mode python-ts-mode) flymake-collection-flake8 (flymake-collection-pycodestyle :disabled t)) flymake-collection-hook-config)
  :hook
  (flymake-mode . flymake-collection-hook-setup)
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

(use-package python
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :defer t
  )

(use-package cargo-transient
  :vc (:url "https://github.com/gs-101/cargo-transient" :branch custom)
  :after rust-ts-mode
  :bind
  (
   :map rust-ts-mode-map
   ("C-c C-p" . cargo-transient)
   )
  :ensure t
  :custom
  (cargo-transient-buffer-name-function 'project-prefixed-buffer-name)
  )

(use-package cmuscheme
  :bind
  (
   :map scheme-mode-map
   ("C-c C-p" . run-scheme)
   )
  :defer t
  )

(use-package sh-script
  :bind
  (
   :map bash-ts-mode-map
   ("C-c C-p" . shell)
   :map sh-mode-map
   ("C-c C-p" . shell)
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
  :ensure t
  :hook
  (css-mode . aggressive-indent-mode)
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . agressive-indent-mode)
  )

(use-package apheleia
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

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :bind
  (
   :map combobulate-key-map
   ([query-replace-regexp] . combobulate-cursor-edit-node-by-text-dwim)
   )
  :config
  (defun cxa/activate-combobulate-on-ts-mode ()
    "Check if MAJOR MODE is a tree-sitter mode. If it is, enable `combobulate-mode'."
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
  :defer t
  :ensure t
  :hook
  (dape-display-source . pulse-momentary-highlight-one-line)
  )

(use-package exercism
  :commands
  (exercism)
  :custom
  (exercism--workspace (convert-standard-filename (expand-file-name "study/exercism/" gs-101/projects-code-directory)))
  :defer t
  :ensure t
  )

(use-package git-modes
  :defer t
  :ensure t
  )

(use-package gptel
  :bind
  ("C-z g b" . gptel)
  ("C-z g DEL" . gptel-abort)
  ("C-z g a" . gptel-add)
  ("C-z g C-x C-f" . gptel-add-file)
  ("C-z g m" . gptel-menu)
  ("C-z g r" . gptel-rewrite-menu)
  ("C-z g RET" . gptel-send)
  ("C-z g p" . gptel-system-prompt)
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
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

(use-package evedel
  :bind
  (
   :map prog-mode-map
   ("C-z e d"   . evedel-create-directive)
   ("C-z e m"   . evedel-modify-directive)
   ("C-z e D"   . evedel-modify-directive-tag-query)
   ("C-z e P"   . evedel-preview-directive-prompt)
   ("C-z e RET" . evedel-process-directives)
   ("C-z e TAB" . evedel-convert-instructions)
   ("C-z e r"   . evedel-create-reference)
   ("C-z e DEL" . evedel-delete-instructions)
   ("C-z e C-'" . evedel-modify-reference-commentary)
   ("C-z e n"   . evedel-next-instruction)
   ("C-z e p"   . evedel-previous-instruction)
   ("C-z e s"   . evedel-save-instructions)
   ("C-z e l"   . evedel-load-instructions)
   ("C-z e t"   . evedel-add-tags)
   ("C-z e T"   . evedel-remove-tags)
   )
  :ensure t
  )

(use-package leetcode
  :custom
  (leetcode-directory (convert-standard-filename (expand-file-name "study/leetcode-solutions/" gs-101/projects-code-directory)))
  (leetcode--paid "$")
  (leetcode-save-solutions t)
  (leetcode--User-Agent ("User Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:126.0) Gecko/20100101 Firefox/126.1"))
  :defer t
  :ensure t
  )

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
  (magit-process-find-password-functions . magit-process-password-auth-source)
  )

(use-package forge
  :after magit
  :bind
  ("C-c v '". forge-dispatch)
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

(use-package projection
  :ensure t
  :bind-keymap
  ("C-z p" . projection-map)
  :bind
  (
   :map projection-map
   ("C" . projection-commands-build-project)
   ("e" . projection-recentf)
   )
  :init
  (global-projection-hook-mode)
  )

(use-package projection-multi
  :after projection compile-multi
  :ensure t
  :bind
  (
   :map projection-map
   ("c" . projection-multi-compile)
   )
  )

(use-package projection-multi-embark
  :after projection compile-multi embark
  :ensure t
  :config
  (projection-multi-embark-setup-command-map)
  )

(use-package wakatime-mode
  :custom
  (wakatime-api-key (auth-source-pick-first-password :host "wakatime.com"))
  :ensure t
  :hook
  (prog-mode . global-wakatime-mode)
  )

(provide 'gs-dev)
