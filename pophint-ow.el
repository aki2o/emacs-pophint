(require 'pophint)

;;;###autoload
(defcustom pophint-ow:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-do-when-other-window 'pophint-ow:enable "1.1.0")

;;;###autoload
(defun pophint:do-each-window () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "each-window"
    :description "Each window"
    :source `((action . (lambda (hint)
                          (funcall pophint--default-action hint)
                          (goto-char (pophint:hint-endpt hint))))
              ,@pophint--next-window-source)))

(pophint:set-allwindow-command pophint:do-each-window)


(defadvice other-window (around do-pophint disable)
  (if (and (called-interactively-p 'any)
           (> (length (window-list)) 2))
      (let ((pophint:use-pos-tip t))
        (pophint:do-each-window))
    ad-do-it))


;;;###autoload
(defun pophint-ow:provision (activate)
  (interactive)
  (if activate
      (ad-enable-advice 'other-window 'around 'do-pophint)
    (ad-disable-advice 'other-window 'around 'do-pophint))
  (ad-activate 'other-window))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-ow:enable (pophint-ow:provision t)))


(provide 'pophint-ow)
;;; pophint-ow.el ends here
