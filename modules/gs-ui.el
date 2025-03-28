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

(use-package faces
  :when (gs-101/nobara-p)
  :init
  (defun dw/set-font-faces ()
    ;; Set the default face
    (set-face-attribute 'default nil :font "Cascadia Code NF")
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Cascadia Mono NF")
    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Cascadia Code NF" :weight 'regular))
  (dw/set-font-faces))

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
  (org-ellipsis 'truncate-string-ellipsis)
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

(use-package which-key
  :custom
  (which-key-add-column-padding 1)
  (which-key-idle-delay 0.5)
  (which-key-min-display-lines 6)
  (which-key-separator " = ")
  (which-key-side-window-slot -10)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  :init
  (which-key-mode))

(use-package catppuccin-theme
  :vc (:url "https://github.com/catppuccin/emacs")
  :when (gs-101/nobara-p)
  :config
  (load-theme 'catppuccin t)
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
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-ellipsis 'truncate-string-ellipsis)
  (doom-modeline-enable-word-count t)
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
  :config
  (setq-mode-local typescript-ts-base-mode eldoc-box-buffer-setup-function #'eldoc-box-prettify-ts-errors-setup)
  (setq-mode-local tsx-ts-mode eldoc-box-buffer-setup-function #'eldoc-box-prettify-ts-errors-setup)
  :hook
  (eldoc-mode . eldoc-box-hover-mode))

(use-package ef-themes
  :vc (:url "https://github.com/protesilaos/ef-themes")
  :when (gs-101/guix-p)
  :config
  (load-theme 'ef-maris-dark)
  :ensure t)

(use-package helpful
  :vc (:url "https://github.com/Wilfred/helpful")
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :config
  (set-face-attribute 'helpful-heading nil :height 1.5)
  :ensure t)

(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo")
  :custom
  (hl-todo-keyword-faces '(("FIXME" . flymake-error-echo)
                           ("NOTE" . flymake-warning-echo)
                           ("QUESTION" . flymake-warning-echo)
                           ("ANSWER" . flymake-note-echo)
                           ("FIX" . flymake-error-echo)
                           ("FEAT" . flymake-warning-echo)
                           ("DOCS" . flymake-note-echo)
                           ("HACK" . flymake-note-echo)
                           ("STYLE" . flymake-note-echo)
                           ("REFACTOR" . flymake-note-echo)
                           ("REVIEW" . flymake-note-echo)
                           ("CHORE" . flymake-note-echo)
                           ("MERGED" . flymake-note-echo)
                           ("CLOSED" . flymake-error-echo)))
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode))

(use-package keycast
  :vc (:url "https://github.com/tarsius/keycast")
  :config
  (set-face-attribute 'keycast-key nil :background 'unspecified :foreground 'unspecified :box 'unspecified)
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
  (gs-101/add-many-to-list 'keycast-substitute-alist
                           '((backward-delete-char-untabify "" "Erasing...")
                             (delete-backward-char "" "Erasing...")
                             (org-delete-backward-char "" "Erasing...")
                             (self-insert-command "" "Typing...")
                             (org-self-insert-command "" "Typing...")
                             (vertico-next nil nil)
                             (vertico-previous nil nil))))

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

(use-package ligature
  :vc (:url "https://github.com/mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  :ensure t
  :hook
  (prog-mode . ligature-mode))

(use-package markdown-mode
  :after markdown-mode
  :custom
  (markdown-enable-highlighting-syntax t)
  (markdown-hide-markup t))

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

(use-package nerd-icons-corfu
  :vc (:url "https://github.com/LuigiPiucco/nerd-icons-corfu")
  :after nerd-icons corfu
  :custom
  (corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (nerd-icons-corfu-mapping '((array :style "md" :icon "code_array" :face font-lock-type-face)
                              (boolean :style "md" :icon "checkbox_intermediate" :face font-lock-builtin-face)
                              (class :style "md" :icon "file_tree" :face font-lock-type-face)
                              (color :style "md" :icon "format_paint" :face success)
                              (command :style "dev" :icon "terminal" :face default)
                              (constant :style "md" :icon "card_text_outline" :face font-lock-constant-face)
                              (constructor :style "md" :icon "arrow_right" :face font-lock-function-name-face)
                              (enummember :style "md" :icon "cards_outline" :face font-lock-builtin-face)
                              (enum-member :style "md" :icon "cards_outline" :face font-lock-builtin-face)
                              (enum :style "md" :icon "card_text_outline" :face font-lock-builtin-face)
                              (event :style "md" :icon "lightning_bolts" :face font-lock-warning-face)
                              (field :style "md" :icon "toy_brick" :face font-lock-variable-name-face)
                              (file :style "md" :icon "file" :face font-lock-string-face)
                              (folder :style "md" :icon "folder" :face font-lock-doc-face)
                              (interface :style "md" :icon "transit_connection_variant" :face font-lock-type-face)
                              (keyword :style "md" :icon "text" :face font-lock-keyword-face)
                              (macro :style "md" :icon "arrow_expand" :face font-lock-keyword-face)
                              (magic :style "md" :icon "magic_staff" :face font-lock-builtin-face)
                              (method :style "md" :icon "cube_outline" :face font-lock-function-name-face)
                              (function :style "md" :icon "function" :face font-lock-function-name-face)
                              (module :style "md" :icon "file_send" :face font-lock-preprocessor-face)
                              (numeric :style "md" :icon "numeric" :face font-lock-builtin-face)
                              (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
                              (param :style "md" :icon "code_parentheses" :face default)
                              (property :style "md" :icon "wrench" :face font-lock-variable-name-face)
                              (reference :style "md" :icon "file_link" :face font-lock-variable-name-face)
                              (snippet :style "md" :icon "note_text" :face font-lock-string-face)
                              (string :style "md" :icon "code_string" :face font-lock-string-face)
                              (struct :style "md" :icon "new_box" :face font-lock-variable-name-face)
                              (text :style "md" :icon "format_text" :face font-lock-doc-face)
                              (typeparameter :style "md" :icon "format_list_bulleted" :face font-lock-type-face)
                              (type-parameter :style "md" :icon "format_list_bulleted" :face font-lock-type-face)
                              (unit :style "md" :icon "ruler" :face font-lock-constant-face)
                              (value :style "md" :icon "toy_brick" :face font-lock-builtin-face)
                              (variable :style "md" :icon "toy_brick_outline" :face font-lock-variable-name-face)
                              (t :style "md" :icon "code_tags" :face font-lock-variable-name-face)))
  :ensure t)

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
  :defer t
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
  (prog-mode . gs-101/prism-mode-lisp)
  (python-base-mode . prism-whitespace-mode))

(use-package prism
  :after prism catppuccin-theme
  :config
  (defun prism-catppuccin-colors ()
    "Grab color definitions from catppuccin and use them to set prism's colors."
    (prism-set-colors
      :lightens '(0 5 10)
      :desaturations '(-2.5 0 2.5)
      :colors (-map #'catppuccin-get-color '(red
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

This is because I find `prism-mode' better for these modes.

Also disable in Python because I use `prism-whitespace-mode' there."
    (unless (or (string-match-p "clojure.*-mode\\'" (symbol-name major-mode))
                (string-match-p "lisp.*-mode\\'" (symbol-name major-mode))
                (derived-mode-p 'scheme-mode)
                (derived-mode-p 'python-base-mode))
      (rainbow-delimiters-mode)))
  :ensure t
  :hook
  (prog-mode . gs-101/rainbow-delimiters-maybe))

(use-package posframe
  :vc (:url "https://github.com/tumashu/posframe")
  :ensure t)

(use-package posframe
  :after transient
  :custom
  (transient-display-buffer-action
   (list
    (lambda (buffer _)
      (posframe-show
       buffer
       :poshandler #'posframe-poshandler-frame-center
       :min-width transient-minimal-frame-width ; Use the same minimal width as transient, to avoid weird resizing
       :lines-truncate t ; Truncate lines instead of wrapping them
       :internal-border-color (transient--prefix-color) ; Use transient colors to indicate that the current frame is a transient
       :internal-border-width 1)
      (get-buffer-window transient--buffer t)))))

(use-package vertico-posframe
  :vc (:url "https://github.com/tumashu/vertico-posframe")
  :after vertico
  :ensure t
  :custom
  (vertico-posframe-border-width 1)
  :config
  (vertico-posframe-mode))

(provide 'gs-ui)
