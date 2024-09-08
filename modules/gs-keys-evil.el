;;; -*- lexical-binding: t -*-

(use-package simple
  :bind 
  ("C-M-u" . universal-argument)
  )

(use-package emacs
  :custom
  (evil-want-keybinding nil)
  )

(use-package evil
  :ensure t
  )

(use-package evil-core
  :config
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  :init
  (evil-mode)
  )

(use-package evil-vars
  :custom
  (evil-disable-insert-state-bindings t) ;; 2
  (evil-respect-visual-line-mode t) ;; 1
  (evil-undo-system 'undo-redo)
  (evil-split-window-below t) ;; 1
  (evil-v$-excludes-newline t) ;; 1
  (evil-vsplit-window-right t) ;; 1
  (evil-want-C-i-jump nil) ;; 1
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t) ;; 2
  )

(use-package evil-commands
  :bind
  (
   :map evil-normal-state-map
   ("C-n" . evil-next-line)
   ("C-p" . evil-previous-line)
   (")" . evil-next-close-paren)
   ("(" . evil-previous-open-paren)
   )
  )

(use-package evil-search
  :custom
  (evil-search-module 'evil-search)
  )

(use-package evil-states
  :bind 
  (
   :map evil-insert-state-map
   ;; In insert state, use the default quit command to return to normal state
   ("C-g" . evil-normal-state)
   :map evil-motion-state-map
   ;; Disabled to avoid conflict with Org Mode
   ("RET" . nil)
   )
  )

(use-package evil-core
  :if (package-installed-p 'org-appear)
  :custom
  (org-appear-trigger 'manual) 
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
                                       #'org-appear-manual-start
                                       nil
                                       t)
                             (add-hook 'evil-insert-state-exit-hook
                                       #'org-appear-manual-stop
                                       nil
                                       t)))
  )

(use-package evil-core
  :if (package-installed-p 'vertico)
  :bind
  (
   :map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   )
  )

(use-package evil-core
  :config
  (evil-set-leader nil (kbd "SPC"))
  )

(use-package evil-core
  :config
  (evil-define-key 'normal 'global (kbd "<leader>B") 'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'switch-to-buffer)
  )

(use-package evil-core
  :config
  (evil-define-key 'normal 'global (kbd "<leader>ie") 'emoji-insert)
  (evil-define-key 'normal 'global (kbd "<leader>ic") 'insert-char)
  )

(use-package evil-core
  :config
  (evil-define-key 'normal 'global (kbd "<leader>fe") '(lambda () (interactive) (find-file (expand-file-name "emacs.org" user-emacs-directory))))
  )

(use-package evil-core
  :config
  (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda) 
  (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-clock-report)
  (evil-define-key 'normal 'global (kbd "<leader>od") 'org-deadline) 
  (evil-define-key 'normal 'global (kbd "<leader>ol") 'org-insert-link)
  (evil-define-key 'normal 'global (kbd "<leader>os") 'org-schedule)
  (evil-define-key 'normal 'global (kbd "<leader>op") 'org-set-property)
  (evil-define-key 'normal 'global (kbd "<leader>ot") 'org-todo)
  )

(use-package evil-core
  :config
  (evil-define-key 'normal 'global (kbd "<leader>wo") 'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
  )

(use-package evil-core
  :if (package-installed-p 'vterm-toggle)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>tv") 'vterm-toggle)
  )

(use-package evil-core
  :if (package-installed-p 'magit)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>mc") 'magit-clone)
  (evil-define-key 'normal 'global (kbd "<leader>ms") 'magit-status)
  )

(use-package evil-core
  :if (package-installed-p 'org-roam)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>rb") 'dw/org-roam-capture-inbox)
  (evil-define-key 'normal 'global (kbd "<leader>rd") 'org-roam-dailies-map)
  (evil-define-key 'normal 'global (kbd "<leader>rf") 'org-roam-node-find) 
  (evil-define-key 'normal 'global (kbd "<leader>ri") 'org-roam-node-insert)
  )

(use-package evil-core
  :if (package-installed-p 'org-roam-ui)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>ru") 'org-roam-ui-open)
  )

(use-package evil-core
  :if (package-installed-p 'nerd-icons)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>in") 'nerd-icons-insert)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init)
  )

(use-package evil-org
  :hook
  (
   (org-agenda-mode . evil-org-mode)
   (org-mode . evil-org-mode)
   )
  :config
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  :ensure t
  )

(use-package evil-org-agenda
  :after evil-org
  :config
  (evil-org-agenda-set-keys)
  )

(provide 'gs-keys-evil)
