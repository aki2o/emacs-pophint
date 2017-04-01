(require 'rx)
(require 'pophint)
(require 'pophint-config--quote)


(defvar pophint-config:regexp-sexp-start
  (rx-to-string `(and (or bos
                          (not (any "(")))
                      (group "(" (not (any ") \t\r\n"))))))


;;;###autoload
(defun pophint:do-sexp () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "sexp"
    :description "Sexp on emacs-lisp-mode."
    :source '((shown . "Sexp")
              (method .(lambda ()
                         (when (re-search-forward pophint-config:regexp-sexp-start nil t)
                           (save-excursion
                             (let* ((startpt (match-beginning 1))
                                    (endpt (ignore-errors (goto-char startpt) (forward-sexp) (point)))
                                    (value (when endpt (buffer-substring-no-properties startpt endpt))))
                               `(:startpt ,startpt :endpt ,endpt :value ,value))))))
              (highlight . nil))))


;;;###autoload
(defun pophint-config:elisp-setup ()
  (add-to-list 'pophint:sources 'pophint:source-sexp)
  (setq pophint-config:exclude-quote-chars '("'" "`")))


;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'pophint-config:elisp-setup t)


(provide 'pophint-config--el)
;;; pophint-config--el.el ends here
