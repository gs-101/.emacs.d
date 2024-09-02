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
   ("\\.rs\\'" . rust-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode)
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
                   (bash "https://github.com/tree-sitter/tree-sitter-bash.git")
                   (c "https://github.com/tree-sitter/tree-sitter-c.git")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp.git")
                   (cmake "https://github.com/uyha/tree-sitter-cmake.git")
                   (css "https://github.com/tree-sitter/tree-sitter-css.git")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp.git")
                   (go "https://github.com/tree-sitter/tree-sitter-go.git")
                   (html "https://github.com/tree-sitter/tree-sitter-html.git")
                   (javascript "https://github.com/tree-sitter/tree-sitter-javascript.git")
                   (json "https://github.com/tree-sitter/tree-sitter-json.git")
                   (python "https://github.com/tree-sitter/tree-sitter-python.git")
                   (make "https://github.com/alemuller/tree-sitter-make.git")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown.git")
                   (rust "https://github.com/tree-sitter/tree-sitter-rust.git")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml.git")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml "https://github.com/ikatyang/tree-sitter-yaml.git")
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
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (json-mode . json-ts-mode)
                 (lua-mode . lua-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (python-mode . python-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 ))
    (add-to-list 'major-mode-remap-alist mapping))
  :init
  (os/setup-install-grammars)
  )

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  )

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  )

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
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

(use-package magit
  :commands
  (
   magit
   magit-clone
   magit-status
   )
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :ensure t
  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source)
  )

(use-package magit-todos
  :ensure t
  :hook
  (magit-mode . magit-todos-mode)
  )

(use-package forge
  :ensure t
  )

(use-package auth-source
  :custom
  (auth-sources (expand-file-name "gs-forge-magit-auth.gpg" modules-directory))
  )

(provide 'gs-dev)
