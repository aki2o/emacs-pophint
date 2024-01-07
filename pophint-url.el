(require 'rx)
(require 'pophint)
(require 'ffap nil t)

;;;###autoload
(defcustom pophint-url:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defvar pophint-url--regexp-url
  (rx-to-string `(and (any "a-z") (+ (any "a-z0-9")) "://" (+ (any "a-zA-Z0-9#%&-=~@+:./_")))))

(defvar pophint-url--regexp-file-head
  (rx-to-string `(and (or (and (any "a-zA-Z") ":/")
                          (and (? (or "." ".." "~")) "/")))))

(pophint:defsource
  :name "url-or-path"
  :description "Format like URL or Filepath."
  :source '((shown . "Url/Path")
            (method . (lambda ()
                        (let* ((ffap-machine-p-known 'accept)
                               (u (save-excursion
                                    (when (re-search-forward pophint-url--regexp-url nil t)
                                      (let ((startpt (match-beginning 0))
                                            (endpt (match-end 0))
                                            (value (match-string-no-properties 0)))
                                        (pophint--trace "found url. pt:[%s] value:[%s]" startpt value)
                                        `(:startpt ,startpt :endpt ,endpt :value ,value)))))
                               (f (when (functionp 'ffap-guesser)
                                    (save-excursion
                                      (cl-loop while (re-search-forward pophint-url--regexp-file-head nil t)
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
(defun pophint-url:provision (activate)
  (interactive)
  (if activate
      (add-to-list 'pophint:global-sources 'pophint:source-url-or-path t)
    (setq pophint:global-sources (remove 'pophint:source-url-or-path pophint:global-sources))))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-url:enable (pophint-url:provision t)))


(provide 'pophint-url)
;;; pophint-url.el ends here
