;;; ------------------------------------------------------------
;;; GNUS configuration file

;;; 1.1 Finding the news
(setq gnus-select-method '(nntp "news.otenet.gr"))
(setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl))))

;;; 1.4 New groups
(setq gnus-check-new-newsgroups nil)

;;; 1.6 Startup files
(setq gnus-save-killed-list nil)

;;; 1.7 Auto save
(setq gnus-always-read-dribble-file t)




;; (setq gnus-confirm-mail-reply-to-news t)
;; (setq gnus-always-read-dribble-file t)

;; ;; windows setup
;; (setq gnus-treat-display-smileys nil)
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;;                (horizontal 1.0
;;                            (summary 0.56 point)
;;                            (article 1.0)))))
;; (setq gnus-thread-indent-level 3)
;; (setq gnus-summary-line-format "%U%R%z%I%(%[ %-23,23f%]%) %s\n")

;; (setq gnus-read-active-file 'some)
;; (setq gnus-fetch-old-headers t)

;; (setq gnus-refer-article-method
;;       '(current
;;         (nnweb "google"
;;                (nnweb-type google))))


;; sending news
;; (setq message-send-mail-function 'smtpmail-send-it)
;; (setq message-send-mail-real-function 'smtpmail-send-it)
;; (setq smtpmail-default-smtp-server "mailgate.forthnet.gr")
;; (setq smtpmail-smtp-service 25)
;; (setq smtpmail-local-domain nil)
;; (setq message-required-mail-headers
;;       (remove 'Message-ID message-required-mail-headers))


;; cache
;; (setq gnus-use-cache 'passive)
;; (setq gnus-cache-enter-articles (quote (ticked dormant)))

;; (eval-after-load "mail-source"
;;   '(add-to-list 'mail-sources '(pop :server "pop.YourProvider.net"
;;                                     :user "yourUserName"
;;                                     :password "yourPassword")))


;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
