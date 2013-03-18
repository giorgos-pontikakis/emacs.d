;;; ------------------------------------------------------------
;;; GNUS configuration file

;;; 1.1 Finding the news
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-inbox "Inbox")
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nntp "news.otenet.gr")))

;;; 1.4 New groups
(setq gnus-check-new-newsgroups nil)

;;; 1.6 Startup files
(setq gnus-save-killed-list nil)

;;; 1.7 Auto save
(setq gnus-always-read-dribble-file t)

;;; 3.1 Summary Buffer format
(setq gnus-extract-address-components 'mail-extract-address-components)

;; 9.5 Window layout
(gnus-add-configuration
 '(article
   (horizontal 1.0
               (horizontal 1.0
                           (summary 0.56 point)
                           (article 1.0)))))

;;; 9.14.3 Smileys
(setq gnus-treat-display-smileys nil)



(setq gnus-posting-styles
  '((".*"
     (name "Giorgos Pontikakis")
     (address "g.pontikakis@extherm.gr")
     (organization "Extherm")
     (signature-file "~/.emacs.d/mail-signature"))
    ((message-news-p)
     (name "Giorgos Pontikakis")
     (address "giorgos.pontikakis.no-spam@gmail.com"))))






;; (setq gnus-thread-indent-level 3)
;; (setq gnus-summary-line-format "%U%R%z%I%(%[ %-23,23f%]%) %s\n")

;; (setq gnus-read-active-file 'some)
;; (setq gnus-fetch-old-headers t)

;; (setq gnus-refer-article-method
;;       '(current
;;         (nnweb "google"
;;                (nnweb-type google))))


;; cache
;; (setq gnus-use-cache 'passive)
;; (setq gnus-cache-enter-articles (quote (ticked dormant)))


;;; GMAIL
;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
