(require 'rx)
(require 'pophint)
(require 'ffap nil t)


;;;###autoload
(defvar pophint-config:regexp-url
  (rx-to-string `(and (any "a-z") (+ (any "a-z0-9")) "://" (+ (any "a-zA-Z0-9#%&-=~@+:./_")))))

;;;###autoload
(defvar pophint-config:regexp-file-head
  (rx-to-string `(and (or (and (any "a-zA-Z") ":/")
                          (and (? (or "." ".." "~")) "/")))))

;;;###autoload
(pophint:defsource
  :name "url-or-path"
  :description "Format like URL or Filepath."
  :source '((shown . "Url/Path")
            (method . (lambda ()
                        (let* ((u (save-excursion
                                    (when (re-search-forward pophint-config:regexp-url nil t)
                                      (let ((startpt (match-beginning 0))
                                            (endpt (match-end 0))
                                            (value (match-string-no-properties 0)))
                                        (pophint--trace "found url. pt:[%s] value:[%s]" startpt value)
                                        `(:startpt ,startpt :endpt ,endpt :value ,value)))))
                               (f (when (functionp 'ffap-guesser)
                                    (save-excursion
                                      (loop while (re-search-forward pophint-config:regexp-file-head nil t)
                                            for startpt = (match-beginning 0)
                                            for guess = (ffap-guesser)
                                            if guess
                                            return (progn
                                                     (pophint--trace "found path. pt:[%s] value:[%s]" startpt guess)
                                                     `(:startpt ,startpt
                                                                :endpt ,(+ startpt (length guess))
                                                                :value ,guess))))))
                               (next (cond ((not u)                                            f)
                                           ((not f)                                            u)
                                           ((<= (plist-get u :startpt) (plist-get f :startpt)) u)
                                           (t                                                  f))))
                          (when next (goto-char (plist-get next :endpt)))
                          next)))))

;;;###autoload
(add-to-list 'pophint:global-sources 'pophint:source-url-or-path t)


(provide 'pophint-config--url)
;;; pophint-config--url.el ends here
