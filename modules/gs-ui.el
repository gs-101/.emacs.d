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
  :init
  (window-divider-mode)
  )

(use-package hl-line
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (setq-local global-hl-line-mode nil))) ;; Disable highlight line in PDF mode
  :init
  (global-hl-line-mode)
  )

(use-package hl-line
  :if (package-installed-p 'dashboard)
  :config
  (add-hook 'dashboard-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package hl-line
  :if (package-installed-p 'pdf-tools)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package hl-line
  :if (package-installed-p 'vterm)
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
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
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  )

(use-package paragraphs
  :custom
  (sentence-end-double-space nil)
  :defer t
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
  (which-key-separator "  ")
  (which-key-side-window-slot -10)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  :init
  (which-key-mode)
  )

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t)
  :ensure t
  )

(use-package dashboard
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
  :defer nil
  :ensure t
  :preface
  (defun dashboard-create-scratch-buffer ()
    "Create a scratch buffer."
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*")))
  )

(use-package dashboard-widgets
  :config
  (dashboard-modify-heading-icons '(
                                    (agenda . "nf-oct-calendar")
                                    (projects . "nf-oct-project")
                                    (recents . "nf-oct-clock")
                                    ))
  :custom
  (dashboard-banner-logo-title "The Extensible Computing Enviroment")
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '(
                     (agenda . 5)
                     (projects . 5)
                     (recents . 5)
                     ))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory))
  (dashboard-week-agenda nil)
  :init
  (dashboard-setup-startup-hook)
  )

(use-package dashboard-widgets
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
       (lambda (&rest _) (snackon/create-scratch-buffer))
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
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :defer t
  )

(use-package doom-modeline
  :custom   
  (doom-modeline-ellipsis "…")
  (doom-modeline-enable-word-count t)
  :ensure t
  :init
  (doom-modeline-mode)
  )

(use-package doom-modeline
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (setq doom-modeline-icon t)))
  )

(use-package golden-ratio
  :ensure t
  :init
  (golden-ratio-mode)
  )

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :commands
  (
   helpful-callable
   helpful-command
   helpful-key
   helpful-variable
   )
  :ensure t
  )

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces '(
                           ("FIX" . "red")
                           ("FEAT" . "yellow")
                           ("STYLE" . "orange")
                           ("REFACTOR" . "white")
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
  :if (package-installed-p 'catppuccin-theme)
  :custom
  (hl-todo-keyword-faces '(
                           ("FIX" . "#f38ba8")
                           ("FEAT" . "#f9e2af")
                           ("STYLE" . "#fab387")
                           ("REFACTOR" . "#89b4fa")
                           ("CHORE" . "#a6adc8")
                           ("MERGED" . "#a6e3a1")
                           ("CLOSED" . "#f38ba8")
                           ))
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

(use-package nerd-icons
  :bind
  ("C-c i n" . nerd-icons-insert)
  :demand t
  :ensure t
  )

(use-package nerd-icons-completion
  :if (package-installed-p 'marginalia)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  :ensure t
  :init
  (nerd-icons-completion-mode)
  )

(use-package nerd-icons-corfu
  :if (package-installed-p 'marginalia)
  :after margnialia
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :custom
  (nerd-icons-corfu-mapping
   '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
     (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
     ;; ...
     (t :style "cod" :icon "code" :face font-lock-warning-face)))
  :ensure t
  )

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode)
  )

(use-package nerd-icons-ibuffer
  :ensure t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  )

(use-package popper
  :bind
  ("M-]" . popper-cycle)
  ("M-[" . popper-toggle)
  ("C-x M-[" . popper-toggle-type)
  :custom
  (popper-display-control t)
  (popper-reference-buffers '(
                              compilation-mode
                              help-mode
                              "\\*Async Shell Command\\*"
                              "\\*Backtrace\\*"
                              "\\*compilation\\*"
                              "\\*Dtache Shell Command\\*"
                              "\\*eldoc\\*"
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
  :init
  (popper-mode)
  )

(use-package prism
  :ensure t
  :hook
  (c-ts-mode . prism-mode)
  (c++-ts-mode . prism-mode)
  (csharp-ts-mode . prism-mode)
  (emacs-lisp-mode . prism-mode)
  (lisp-mode . prism-mode)
  (lisp-data-mode . prism-mode)
  (elisp-refs-mode . prism-mode)
  (common-lisp-mode . prism-mode)
  (elisp-byte-code-mode . prism-mode)
  (lisp-interaction-mode . prism-mode)
  (emacs-lisp-compilation-mode . prism-mode)
  (python-ts-mode . prism-whitespace-mode)
  (bash-ts-mode . prism-whitespace-mode)
  )

(use-package prism
  :if (package-installed-p 'catppuccin-theme)
  :config
  :preface
  (defun prism-catppuccin-colors ()
    "Grab color definitions from catppuccin and use them to set prism's colors."
    (interactive)
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
  )

(use-package prism
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (prism-catppuccin-colors)))
  )

(use-package transient-posframe
  :vc (:url "https://github.com/gs-101/transient-posframe")
  :ensure t
  :init
  (transient-posframe-mode)
  )

(provide 'gs-ui)
