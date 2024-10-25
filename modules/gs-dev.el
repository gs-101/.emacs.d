;;; -*- lexical-binding: t -*-

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  )

(use-package org-src
  :config
  (add-to-list 'org-src-lang-modes '(
                                     ("C" . c-ts)
                                     ("css" . css-ts)
                                     ("html" . html-ts)
                                     ("js" . js-ts)
                                     ("python" . python-ts)
                                     ))
  )

(use-package treesit-auto
  :vc (:url "https://github.com/gs-101/treesit-auto" :branch "custom")
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :custom
  (treesit-auto-install t)
  :ensure t
  )

(use-package treesit-fold
  :after treesit
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :bind
  (
   :map prog-mode-map
   ("C-<tab>" . treesit-fold-toggle)
   )
  :custom
  (treesit-fold-replacement "…")
  :demand t
  :ensure t
  :init
  (global-treesit-fold-indicators-mode)
  (treesit-fold-line-comment-mode)
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
   ("}" . compilation-next-file)
   ("{" . compilation-previous-file)
   ("n" . next-error-no-select)
   ("p" . previous-error-no-select)
   ("q" . kill-buffer-and-window)
   )
  :commands
  (
   compile
   )
  :custom
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
  (compilation-scroll-output t)
  (compilation-skip-threshold 2)
  :functions
  (
   kill-buffer-and-window
   next-error-no-select
   previous-error-no-select
   )
  :hook
  (compilation-mode . goto-address-mode)
  (compilation-filter . xenodium/colorize-compilation-buffer)
  :preface
  (defun xenodium/colorize-compilation-buffer ()
    (let ((was-read-only buffer-read-only))
      (unwind-protect
          (progn
            (when was-read-only
              (read-only-mode -1))
            (ansi-color-apply-on-region (point-min) (point-max)))
        (when was-read-only
          (read-only-mode +1)))))
  )

(use-package compile-multi
  :vc (:url "https://github.com/mohkale/compile-multi")
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

(use-package eglot
  :commands
  (
   eglot
   )
  :custom
  (eglot-autoshutdown t)
  )

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  )

(use-package flymake-collection
  :ensure t
  :hook
  (flymake-mode . flymake-collection-hook-setup)
  )

(use-package lisp
  :custom
  (narrow-to-defun-include-comments t)
  :defer t
  )

(use-package python
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :defer t
  )

(use-package rust-mode
  :defer t
  :custom
  (rust-mode-treesitter-derive t)
  :ensure t
  )

(use-package rustic
  :after rust-mode
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-lsp-client 'eglot)
  :ensure t
  )

(use-package yaml-mode
  :defer t
  :ensure t
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
  :hook
  (prog-mode . apheleia-mode)
  )

(use-package cognitive-complexity
  :vc (:url "https://github.com/emacs-vs/cognitive-complexity")
  :ensure t
  :hook
  (prog-mode . cognitive-complexity-mode)
  )

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :bind
  (
   :map prog-mode-map
   ("C-c C-x c" . combobulate)
   )
  :ensure t
  :hook
  (
   (css-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (python-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   )
  )

(use-package git-modes
  :ensure t
  :mode
  ("\\.gitattributes\\'" . gitattributes-mode)
  ("\\.gitconfig\\'" . gitconfig-mode)
  ("\\.gitmodules\\'" . gitconfig-mode)
  ("\\.gitignore\\'" . gitignore-mode)
  )

(use-package gptel
  :commands
  (
   gptel
   gptel-menu
   )
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
  :after gptel
  :vc (:url "https://github.com/karthink/gptel-quick")
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

(use-package elysium
  :commands
  (
   elysium-query
   )
  :ensure t
  )

(use-package smerge-mode
  :after elysium
  :hook
  (prog-mode . smerge-mode)
  )

(use-package grip-mode
  :commands
  (
   grip-start-preview
   )
  :custom
  (grip-preview-use-webkit nil)
  :ensure t
  )

(use-package leetcode
  :commands
  (
   leetcode
   leetcode-daily
   )
  :custom
  (leetcode--paid "$")
  (leetcode--User-Agent ("User Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:126.0) Gecko/20100101 Firefox/126.1"))
  :ensure t
  )

(use-package leetcode
  :requires leetcode
  :custom
  (leetcode-save-solutions t)
  (leetcode-directory (convert-standard-filename (expand-file-name "leetcode-solutions/" projects-code-directory)))
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

(use-package magit-todos
  :requires (magit hl-todo)
  :ensure t
  )

(use-package package-lint
  :ensure t
  )

(use-package package-lint-flymake
  :after flymake
  :config
  (package-lint-flymake-setup)
  :ensure t
  )

(use-package projection
  :demand t
  :ensure t
  :bind-keymap
  ("C-c p" . projection-map)
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
  :requires projection
  :after compile-multi
  :ensure t
  :bind
  (
   :map projection-map
   ("c" . projection-multi-compile)
   )
  )

(use-package projection-multi-embark
  :requires projection
  :after compile-multi embark
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
