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

(use-package faces
  :preface
  (defun dw/set-font-faces ()
    ;; Set the default face
    (set-face-attribute 'default nil :font "Cascadia Code NF")
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Cascadia Mono NF")
    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Cascadia Code NF" :weight 'regular))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (dw/set-font-faces))))
    (dw/set-font-faces))
  )

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  )

(use-package mouse
  :config
  ;; Make right click open the context menu
  (when (display-graphic-p)
    (context-menu-mode))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (context-menu-mode))))
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
  :custom
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-message t)
  (inhibit-startup-screen t)
  :defer t
  )

(use-package hl-line
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (setq-local global-hl-line-mode nil))) ;; Disable highlight line in PDF mode
  :init
  (global-hl-line-mode)
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

(use-package which-key
  :custom
  (which-key-add-column-padding 1)
  (which-key-idle-delay 0.5)
  (which-key-min-display-lines 6)
  (which-key-separator " | ")
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
                                           mauve)))
  )

(provide 'gs-ui)
