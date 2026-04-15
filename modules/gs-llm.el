;;; -*- lexical-binding: t -*-

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel")
  :ensure-system-package
  curl
  :ensure t
  :config
  (setq gptel-model 'claude-opus-4.6
        gptel-backend
        (gptel-make-gh-copilot "Copilot")))

(use-package gptel-agent
  :vc (:url "https://github.com/karthink/gptel-agent")
  :ensure t)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :ensure t
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion)
        ("C-<tab>" . copilot-accept-completion-by-word)
        ("C-TAB" . copilot-accept-completion-by-word)
        ("C-n" . copilot-next-completion)
        ("C-p" . copilot-previous-completion))
  :custom
  (copilot-indent-offset-warning-disable t))

(provide 'gs-llm)
