(use-package message
  :custom
  (message-kill-buffer-on-exit t)
  (message-mail-user-agent t)
  (message-send-mail-function #'smtpmail-send-it)
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-server "disroot.org")
  (smtpmail-local-domain "disroot.org"))

(use-package startup
  :custom
  (user-mail-address "gabrielsantosdesouza@disroot.org")
  :defer t)

(use-package org-msg
  :vc (:url "https://github.com/jeremy-compostella/org-msg")
  :after mu4e
  :custom
  (org-msg-greeting-fmt "\nGreetings,\n\n")
  (org-msg-default-alternatives '(
                                  (new . (text html))
                                  (reply-to-html . (text html))
                                  (reply-to-text . (text))
                                  ))
  (org-msg-convert-citation t)
  (org-msg-signature
   "Regards,

#+begin_signature
--
Gabriel Santos
#+end_signature"
   )
  :ensure t
  :config
  (org-msg-mode))

(provide 'gs-chat)
