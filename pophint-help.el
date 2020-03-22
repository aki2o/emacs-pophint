(require 'pophint)

;;;###autoload
(defcustom pophint-help:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

;;;###autoload
(defun pophint:do-help-btn () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "help-btn"
    :description "Button on help-mode."
    :source '((shown . "Link")
              (dedicated . (e2wm))
              (activebufferp . (lambda (b)
                                 (pophint--maybe-kind-mode-buffer-p b 'help-mode)))
              (method . (lambda ()
                          (when (forward-button 1)
                            (let* ((btn (button-at (point)))
                                   (startpt (when btn (button-start btn)))
                                   (endpt (when btn (button-end btn)))
                                   (value (when btn (buffer-substring-no-properties startpt endpt))))
                              (pophint--trace "found button. startpt:[%s] endpt:[%s] value:[%s]"
                                              startpt endpt value)
                              `(:startpt ,startpt :endpt ,endpt :value ,value)))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (push-button)))))))

(defun pophint-help:setup ()
  (add-to-list 'pophint:sources 'pophint:source-help-btn))
(define-obsolete-function-alias 'pophint-config:help-setup 'pophint-help:setup "1.1.0")

;;;###autoload
(defun pophint-help:provision (activate)
  (interactive)
  (if activate
      (add-hook 'help-mode-hook 'pophint-help:setup t)
    (remove-hook 'help-mode-hook 'pophint-help:setup)))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-help:enable (pophint-help:provision t)))


(provide 'pophint-help)
;;; pophint-help.el ends here
