(require 'pophint)
(require 'pophint-config---util)


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


;;;###autoload
(defvar pophint-config:active-when-other-window-p pophint-config:effect-default-activated)

;;;###autoload
(defadvice other-window (around do-pophint disable)
  (if (and (interactive-p)
           pophint-config:active-when-other-window-p
           (> (length (window-list)) 2))
      (let ((pophint:use-pos-tip t))
        (pophint:do-each-window))
    ad-do-it))

;;;###autoload
(when pophint-config:active-when-other-window-p
  (ad-enable-advice 'other-window 'around 'do-pophint)
  (ad-activate 'other-window))

;;;###autoload
(defun pophint-config:set-do-when-other-window (activate)
  "Whether do pop-up when `other-window'."
  (if activate
      (ad-enable-advice 'other-window 'around 'do-pophint)
    (ad-disable-advice 'other-window 'around 'do-pophint))
  (ad-activate 'other-window)
  (setq pophint-config:active-when-other-window-p activate))


(provide 'pophint-config--ow)
;;; pophint-config--ow.el ends here
