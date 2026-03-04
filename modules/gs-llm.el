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

(provide 'gs-llm)
