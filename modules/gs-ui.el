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
  (x-underline-at-descent-line nil)
  )

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  )

(use-package faces
  :preface
  (defun dw/set-font-faces ()
    ;; Set the default face
    (set-face-attribute 'default nil :font "Cascadia Code NF")
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Cascadia Mono NF")
    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Cascadia Code NF" :weight 'regular))
  :init
  (dw/set-font-faces)
  )

(use-package faces
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (dw/set-font-faces))))
  )

(use-package frame
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  :hook
  (doc-view-mode . (lambda () (setq-local blink-cursor-mode nil)))
  :init
  (window-divider-mode)
  )

(use-package hl-line
  :init
  (global-hl-line-mode)
  :hook
  (dashboard-mode . (lambda () (setq-local global-hl-line-mode nil)))
  (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package mouse
  :if (display-graphic-p)
  :init
  (context-menu-mode)
  )

(use-package mouse
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (context-menu-mode)))
  )

(use-package org-faces
  :config
  (dolist
      (face '(
              (org-level-1 . 1.30)
              (org-level-2 . 1.29)
              (org-level-3 . 1.28)
              (org-level-4 . 1.27)
              (org-level-5 . 1.26)
              (org-level-6 . 1.25)
              (org-level-7 . 1.24)
              (org-level-8 . 1.23)
              )
            )
    (set-face-attribute (car face) nil :font "Cascadia Code NF" :weight 'medium :height (cdr face)))
  (set-face-attribute 'org-document-title nil :font "Cascadia Code NF" :weight 'medium :height 1.40)
  )

(use-package org
  :custom
  (org-ellipsis "…")
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars)
  )

(use-package org
  :custom
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  )

(use-package org-modern
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "󰪥󰪤󰪣󰪢󰪡󰪠󰪟")
  (org-modern-table-vertical 1)
  :ensure t
  :init
  (global-org-modern-mode)
  )

(use-package org-agenda
  :custom
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "←──────────────")
  (org-agenda-time-grid
   '((daily today require-timed)
     (600 800 1000 1200 1400 1600 1800 2000 2200)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  )

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil)
  )

(use-package startup
  :config
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)
  :custom
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-message t)
  (inhibit-startup-screen t)
  :defer t
  )

(use-package window
  :custom
  (recenter-positions '(top middle bottom)) ;; 2
  (scroll-error-top-bottom t) ;; 1
  (split-height-threshold nil) ;;1
  (split-width-threshold 170) ;; 1
  (switch-to-buffer-obey-display-actions t) ;; 2
  )

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
  (which-key-mode)
  )

(use-package catppuccin-theme
  :preface
  (load-theme 'catppuccin t)
  :ensure t
  )

(use-package color-identifiers-mode
  :ensure t
  :config
  (defun gs-101/color-identifiers-toggle-on-ts-mode ()
    "Check if MAJOR MODE is a tree-sitter mode.
If it is, enable `color-identifiers-mode'."
    (when (string-match-p "-ts-mode\\'" (symbol-name major-mode))
      (color-identifiers-mode))
    (when (bound-and-true-p prism-mode)
      (setq-local color-identifiers-mode nil)))
  :hook
  (prog-mode . gs-101/color-identifiers-toggle-on-ts-mode)
  )

(use-package dashboard
  :bind
  ("C-z <home>" . dashboard-open)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-startupify-list '(
                               dashboard-insert-banner
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-footer
                               ))
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :preface
  (defun dashboard-create-scratch-buffer ()
    "Create a scratch buffer."
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*")))
  )

(use-package dashboard-widgets
  :after dashboard
  :custom
  (dashboard-agenda-prefix-format "%-12t% s ")
  (dashboard-banner-logo-title "The Extensible Computing Enviroment")
  (dashboard-items '(
                     (agenda . 5)
                     (projects . 5)
                     (recents . 5)
                     ))
  (dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory))
  (dashboard-week-agenda nil)
  )

(use-package dashboard-widgets
  :after dashboard nerd-icons
  :custom
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  :config
  (dashboard-modify-heading-icons '(
                                    (agenda . "nf-oct-calendar")
                                    (projects . "nf-oct-project")
                                    (recents . "nf-oct-clock")
                                    ))
  )

(use-package dashboard-widgets
  :after dashboard nerd-icons
  :custom
  (dashboard-navigator-buttons
   `(;; line1
     (
      (,(nerd-icons-mdicon "nf-md-github")
       "Source Repository"
       "Open the source repository in the browser"
       (lambda (&rest _) (browse-url "https://github.com/gs-101/.emacs.d"))
       'default)
      )
     ;;line 2
     (
      (,(nerd-icons-mdicon "nf-md-note_outline")
       "Open Scratch Buffer"
       "Switch to the scratch buffer"
       (lambda (&rest _) (dashboard-create-scratch-buffer))
       'default)
      (,(nerd-icons-mdicon "nf-md-calendar_outline")
       "Open Org Agenda"
       "Switch to the agenda buffer"
       (lambda (&rest _) (org-agenda))
       'default)
      (,(nerd-icons-mdicon "nf-md-cog")
       "Open Config"
       "Switch to the configuration file buffer"
       (lambda (&rest _) (interactive) (find-file (expand-file-name "emacs.org" user-emacs-directory)))
       'default)
      )
     ))
  )

(use-package startup
  :after dashboard
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :defer t
  )

(use-package doom-modeline
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
  (doom-modeline-ellipsis "…")
  (doom-modeline-enable-word-count t)
  (doom-modeline-modal-modern-icon nil)
  :ensure t
  :init
  (doom-modeline-mode)
  )

(use-package doom-modeline
  :after doom-modeline
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (setq doom-modeline-icon t)))
  )

(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode)
  )

(use-package eldoc-box
  :after eldoc
  :ensure t
  :hook
  (eldoc-mode . eldoc-box-hover-mode)
  )

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :ensure t
  )

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces '(
                           ("FIXME" . "red")
                           ("NOTE" . "yellow")
                           ("QUESTION" . "yellow")
                           ("ANSWER" . "green")
                           ("FIX" . "red")
                           ("FEAT" . "yellow")
                           ("HACK" . "green")
                           ("STYLE" . "orange")
                           ("REFACTOR" . "white")
                           ("REVIEW" . "white")
                           ("CHORE" . "grey")
                           ("MERGED" . "green")
                           ("CLOSED" . "red")
                           ))
  :ensure t
  :hook
  (markdown-mode . hl-todo-mode)
  (org-mode . hl-todo-mode)
  (prog-mode . hl-todo-mode)
  )

(use-package hl-todo
  :after hl-todo catppuccin-theme
  :custom
  (hl-todo-keyword-faces
   (mapcar (lambda (keyword-color)
             (cons (car keyword-color)
                   (catppuccin-get-color (cdr keyword-color))))
           '(
             ("FIXME" . red)
             ("NOTE" . yellow)
             ("QUESTION" . yellow)
             ("ANSWER" . green)
             ("FIX" . red)
             ("FEAT" . yellow)
             ("HACK" . green)
             ("STYLE" . lavender)
             ("REFACTOR" . sapphire)
             ("REVIEW" . sapphire)
             ("CHORE" . overlay0)
             ("MERGED" . green)
             ("CLOSED" . red)
             )))
)

(use-package keycast
  :config
  (set-face-attribute 'keycast-key nil :background nil :foreground "default" :box nil)
  (push '(self-insert-command nil nil) keycast-substitute-alist)
  (push '(org-self-insert-command nil nil) keycast-substitute-alist)
  :ensure t
  :init
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with `doom-modeline')."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (keycast-mode)
  )

(use-package keycast
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (set-face-attribute 'keycast-key nil :background nil :foreground "default" :box nil))))
  )

(use-package keycast
  :after embark
  :config
  (defun oantolin/keycast-store-action-key-cmd (cmd)
    "Store key and CMD command information for `keycast' use."
    (force-mode-line-update t)
    (setq this-command cmd
          keycast--this-command-keys (this-single-command-keys)
          keycast--this-command-desc cmd))
  (advice-add 'embark-keymap-prompter :filter-return #'oantolin/keycast-store-action-key-cmd)
  (defun oantolin/keycast--update-force (&rest _)
    "Version of `keycast--update' that accepts (and ignore) parameters."
    (keycast--update))
  (advice-add 'embark-act :before #'oantolin/keycast--update-force)
  )

(use-package ligature
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
  (prog-mode . ligature-mode)
  )

(use-package markdown-mode
  :after markdown-mode
  :custom
  (markdown-enable-highlighting-syntax t)
  (markdown-hide-markup t)
  )

(use-package nerd-icons
  :bind
  ("C-z i n" . nerd-icons-insert)
  :demand t
  :ensure t
  )

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
     (list
      citar-indicator-cited-icons
      citar-indicator-files-icons
      citar-indicator-links-icons
      citar-indicator-notes-icons
      ))
    )

(use-package nerd-icons-completion
  :after nerd-icons
  :ensure t
  :config
  (nerd-icons-completion-mode)
  )

(use-package nerd-icons-completion
  :after nerd-icons
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  )

(use-package compile-multi-nerd-icons
  :after nerd-icons compile-multi
  :ensure t
  )

(use-package nerd-icons-corfu
  :after nerd-icons corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :ensure t
  )

(use-package nerd-icons-dired
  :after nerd-icons
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode)
  )

(use-package emacs
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string '(
                                      (error "󰃤" compilation-error)
                                      (warning "" compilation-warning)
                                      (note "󰎚" compilation-info)
                                      ))
  )

(use-package go-ts-mode
  :after nerd-icons go-ts-mode
  :config
  (defvar gs-101/go-prettify-symbols-alist
    '((":=" . ?))
    "Value for `prettify-symbols-alist' in `go-ts-mode'.")
  :hook
  (go-ts-mode . (lambda () (setq-local prettify-symbols-alist gs-101/go-prettify-symbols-alist)))
  (go-ts-mode . prettify-symbols-mode)
  )

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :ensure t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  )

(use-package magit-file-icons
  :after nerd-icons magit
  :ensure t
  :hook
  (magit-mode . magit-file-icons-mode)
  )

(use-package popper
  :bind
  ("M-]" . popper-cycle)
  ("M-[" . popper-toggle)
  :custom
  (popper-display-control t)
  (popper-reference-buffers '(
                              compilation-mode
                              vterm-mode
                              inferior-emacs-lisp-mode
                              inferior-python-mode
                              shell-mode
                              "\\*Async Shell Command\\*"
                              "\\*Backtrace\\*"
                              "\\*compilation\\*"
                              "\\*Dtache Shell Command\\*"
                              "\\*eldoc\\*"
                              "\\*Embark Collect:"
                              "\\*Ement Notifications\\*"
                              "*Flymake diagnostics.*"
                              "\\*GDB.*out\\*"
                              "\\*Messages\\*"
                              "\\*mu4e-update\\*"
                              "Output\\*$"
                              "^*tex"
                              "\\*Warnings\\*"
                              "\\*xref\\*"
                              ))
  :ensure t
  :demand t
  :init
  (popper-mode)
  )

(use-package prism
  :config
  (defun gs-101/prism-mode-lisp ()
    "Check if MAJOR MODE is a Lisp mode.
If it is, enable `prism-mode'."
    (when (string-match-p "lisp.*-mode\\'" (symbol-name major-mode))
      (prism-mode))
    (when (string-match-p "scheme-mode\\'" (symbol-name major-mode))
      (prism-mode)))
  :ensure t
  :hook
  (prog-mode . gs-101/prism-mode-lisp)
  (python-base-mode . prism-whitespace-mode)
  )

(use-package prism
  :after prism catppuccin-theme
  :config
  (defun prism-catppuccin-colors ()
    "Grab color definitions from catppuccin and use them to set prism's colors."
    (prism-set-colors
      :lightens '(0 5 10)
      :desaturations '(-2.5 0 2.5)
      :colors (-map #'catppuccin-get-color '(
                                             red
                                             peach
                                             yellow
                                             green
                                             sapphire
                                             lavender
                                             mauve
                                             ))))
  (prism-catppuccin-colors)
  )

(use-package prism
  :if (daemonp)
  :after prism catppuccin-theme
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
              (prism-catppuccin-colors))))
  )

(use-package transient-posframe
  :after transient
  :vc (:url "https://github.com/tarsiiformes/transient-posframe" :branch fix-sizing)
  :ensure t
  :config
  (transient-posframe-mode)
  )

(provide 'gs-ui)
