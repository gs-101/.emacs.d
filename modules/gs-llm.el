;;; -*- lexical-binding: t -*-

(use-package ai-code
  :vc (:url "https://github.com/tninja/ai-code-interface.el")
  :ensure t
  :config
  (ai-code-set-backend 'antigravity)
  :custom
  (ai-code-menu-layout 'two-columns))

(use-package ai-code
  :after disproject
  :config
  (transient-insert-suffix 'disproject-dispatch "b"
    '("a" "Agent" ai-code-menu)))

(use-package ai-code
  :after magit
  :config
  (ai-code-magit-setup-transients))

(provide 'gs-llm)
