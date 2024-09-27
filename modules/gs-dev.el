;;; -*- lexical-binding: t -*-

(use-package treesit
  :mode
  (
   ("\\.sh\\'" . bash-ts-mode)
   ("\\.css\\'" . css-ts-mode)
   ("\\.Dockerfile\\'" . dockerfile-ts-mode)
   ("\\.go\\'" . go-ts-mode)
   ("\\.html\\'" . html-ts-mode)
   ("\\.json\\'" .  json-ts-mode)
   ("\\.lua\\'" . lua-ts-mode)
   ("\\.py\\'" . python-ts-mode)
   ("\\.rb\\'" . ruby-ts-mode)
   ("\\.rs\\'" . rust-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode)
   ("\\.jar\\'" . java-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode)
   ("\\.js\\'"  . typescript-ts-mode)
   ("\\.mjs\\'" . typescript-ts-mode)
   ("\\.mts\\'" . typescript-ts-mode)
   ("\\.cjs\\'" . typescript-ts-mode)
   ("\\.ts\\'"  . typescript-ts-mode)
   ("\\.yaml\\'" . yaml-ts-mode)
   )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist
        (grammar '(
                   (bash . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
                   (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
                   (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
                   (cmake . ("https://github.com/uyha/tree-sitter-cmake.git"))
                   (css . ("https://github.com/tree-sitter/tree-sitter-css.git"))
                   (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
                   (elisp . ("https://github.com/Wilfred/tree-sitter-elisp.git"))
                   (go . ("https://github.com/tree-sitter/tree-sitter-go.git"))
                   (html . ("https://github.com/tree-sitter/tree-sitter-html.git"))
                   (java . ("https://github.com/tree-sitter/tree-sitter-java.git"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json.git"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
                   (make . ("https://github.com/alemuller/tree-sitter-make.git"))
                   (markdown . ("https://github.com/ikatyang/tree-sitter-markdown.git"))
                   (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
                   (rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
                   (toml . ("https://github.com/tree-sitter-grammars/tree-sitter-toml.git"))
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml.git"))
                   ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  (dolist
      (mapping '(
                 (bash-mode . bash-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (css-mode . css-ts-mode)
                 (html-mode . html-ts-mode)
                 (java-mode . java-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (json-mode . json-ts-mode)
                 (lua-mode . lua-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (python-mode . python-ts-mode)
                 (ruby-mode . ruby-ts-mode)
                 (rust-mode . rust-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 ))
    (add-to-list 'major-mode-remap-alist mapping))
  :init
  (os/setup-install-grammars)
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
  :bind
  ([remap compile] . compile-multi)
  :ensure t
  )

(use-package consult-compile-multi
  :requires (compile-multi consult)
  :ensure t
  :init
  (consult-compile-multi-mode)
  )

(use-package compile-multi-embark
  :requires (compile-multi embark)
  :ensure t
  :init
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
  :mode
  ("\\.lisp\\'" . lisp-mode)
  ("\\.lsp\\'" . lisp-mode)
  :custom
  (narrow-to-defun-include-comments t)
  )

(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  )

(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-mode)
  :custom
  (rust-mode-treesitter-derive t)
  :ensure t
  )

(use-package rustic
  :requires rust-mode
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-format-on-save t)
  (rustic-lsp-client 'eglot)
  :ensure t
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
  :vc (:url "https://github.com/mickeynp/combobulate" :branch "development")
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
              "gpt-4o"
              "gpt-4o-mini"
              "meta-llama-3.1-405b-instruct"
              ))
)

(use-package gptel-quick
  :requires gptel
  :vc (:url "https://github.com/karthink/gptel-quick")
  :ensure t
  )

(use-package gptel-quick
  :after (gptel-quick embark)
  :bind
  (
   :map embark-general-map
   ("g" . gptel-quick)
   )
  )

(use-package elysium
  :requires gptel
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
  :commands
  (
   magit
   magit-clone
   magit-status
   )
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :ensure t
  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source)
  )

(use-package forge
  :after magit
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
  :config
  (package-lint-flymake-setup)
  :ensure t
  )

(use-package projection
  :ensure t
  :bind-keymap
  ("C-x P" . projection-map)
  :init
  (global-projection-hook-mode)
  )

(use-package projection-multi
  :requires (projection compile-multi)
  :ensure t
  :bind
  ([remap project-compile] . projection-multi-compile)
  )

(use-package projection-multi-embark
  :requires (projection compile-multi embark)
  :ensure t
  :init
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
