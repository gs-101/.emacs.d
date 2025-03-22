;;; -*- lexical-binding: t -*-

(use-package simple
  :bind
  ("C-M-u" . universal-argument))

(use-package emacs
  :custom
  (evil-want-keybinding nil))

(use-package evil
  :ensure t)

(use-package evil-core
  :config
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  :init
  (evil-mode))

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
  (evil-want-Y-yank-to-eol t) ;; 2)

(use-package evil-commands
  :bind
  (:map evil-normal-state-map
        ("C-n" . evil-next-line)
        ("C-p" . evil-previous-line)
        (")" . evil-next-close-paren)
        ("(" . evil-previous-open-paren)))

(use-package evil-search
  :custom
  (evil-search-module 'evil-search))

(use-package evil-states
  :bind
  (:map evil-insert-state-map
        ;; In insert state, use the default quit command to return to normal state
        ("C-g" . evil-normal-state))
  (:map evil-motion-state-map
        ;; Disabled to avoid conflict with Org Mode
        ("RET" . nil)))

(use-package evil-core
  :after org-appear
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
                                       t))))

(use-package evil-core
  :after vertico
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package evil-core
  :config
  (evil-set-leader nil (kbd "SPC"))

  (defun gs-101/evil-define-leader-key (key function)
    "Define a new KEY for Evil bound to leader + KEY to call FUNCTION."
    (evil-define-key 'normal 'global `(kbd (kbd ,(string-append "<leader>" key))) function)))

(use-package evil-core
  :config
  (gs-101/evil-define-leader-key "B" #'ibuffer)
  (gs-101/evil-define-leader-key "b" #'switch-to-buffer))

(use-package evil-core
  :config
  (gs-101/evil-define-leader-key "ie" #'emoji-insert)
  (gs-101/evil-define-leader-key "ic" #'insert-char))

(use-package evil-core
  :config
  (gs-101/evil-define-leader-key "fe" #'lambda () (interactive) (find-file (expand-file-name "emacs.org" user-emacs-directory))))

(use-package evil-core
  :config
  (gs-101/evil-define-leader-key "oa" #'org-agenda)
  (gs-101/evil-define-leader-key "oc" #'org-clock-report)
  (gs-101/evil-define-leader-key "od" #'org-deadline)
  (gs-101/evil-define-leader-key "ol" #'org-insert-link)
  (gs-101/evil-define-leader-key "os" #'org-schedule)
  (gs-101/evil-define-leader-key "op" #'org-set-property)
  (gs-101/evil-define-leader-key "ot" #'org-todo))

(use-package evil-core
  :config
  (gs-101/evil-define-leader-key "wo" #'other-window)
  (gs-101/evil-define-leader-key "wk" #'windmove-up)
  (gs-101/evil-define-leader-key "wj" #'windmove-down)
  (gs-101/evil-define-leader-key "wh" #'windmove-left)
  (gs-101/evil-define-leader-key "wl" #'windmove-right))

(use-package evil-core
  :config
  (gs-101/evil-define-leader-key "ts" #'eshell))

(use-package evil-core
  :after magit
  :config
  (gs-101/evil-define-leader-key "mc" #'magit-clone)
  (gs-101/evil-define-leader-key "ms" #'magit-statusn))

(use-package evil-core
  :after org-roam
  :config
  (gs-101/evil-define-leader-key "rb" #'dw/org-roam-capture-inbox)
  (gs-101/evil-define-leader-key "rd" #'org-roam-dailies-map)
  (gs-101/evil-define-leader-key "rf" #'org-roam-node-find)
  (gs-101/evil-define-leader-key "ri" #'org-roam-node-insert))

(use-package evil-core
  :after nerd-icons
  :config
  (gs-101/evil-define-leader-key "in" #'nerd-icons-insert))

(use-package evil-collection
  :vc (:url "https://github.com/emacs-evil/evil-collection")
  :ensure t
  :init
  (evil-collection-init))

(use-package evil-org
  :vc (:url "https://github.com/Somelauw/evil-org-mode")
  :hook
  ((org-agenda-mode . evil-org-mode)
   (org-mode . evil-org-mode))
  :config
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  :ensure t)

(use-package evil-org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package which-key
  :custom
  (which-key-allow-evil-operators t))

(provide 'gs-keys-evil)
