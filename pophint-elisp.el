(require 'rx)
(require 'pophint)
(require 'pophint-quote)

(defcustom pophint-elisp:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defvar pophint-elisp--regexp-sexp-start
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
                         (when (re-search-forward pophint-elisp--regexp-sexp-start nil t)
                           (save-excursion
                             (let* ((startpt (match-beginning 1))
                                    (endpt (ignore-errors (goto-char startpt) (forward-sexp) (point)))
                                    (value (when endpt (buffer-substring-no-properties startpt endpt))))
                               `(:startpt ,startpt :endpt ,endpt :value ,value))))))
              (highlight . nil))))

(defun pophint-elisp:setup ()
  (add-to-list 'pophint:sources 'pophint:source-sexp)
  (setq pophint-quote:exclude-quote-chars '("'" "`")))
(define-obsolete-function-alias 'pophint-config:elisp-setup 'pophint-elisp:setup "1.1.0")

;;;###autoload
(defun pophint-elisp:provision (activate)
  (interactive)
  (if activate
      (add-hook 'emacs-lisp-mode-hook 'pophint-elisp:setup t)
    (remove-hook 'emacs-lisp-mode-hook 'pophint-elisp:setup)))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-elisp:enable (pophint-elisp:provision t)))


(provide 'pophint-elisp)
;;; pophint-elisp.el ends here
