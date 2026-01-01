;;; -*- lexical-binding: t -*-

(use-package message
  :custom
  (message-kill-buffer-on-exit t)
  (message-mail-user-agent t)
  (message-send-mail-function #'message-send-mail-with-sendmail))

(use-package smtpmail
  :custom
  (smtpmail-local-domain "gabrielsantosdesouza")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-server "disroot.org"))

(use-package startup
  :custom
  (user-mail-address "gabrielsantosdesouza@disroot.org")
  :defer t)

(use-package notmuch
  :ensure t)

(provide 'gs-chat)
