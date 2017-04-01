(require 'pophint)


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


;;;###autoload
(defun pophint-config:help-setup ()
  (add-to-list 'pophint:sources 'pophint:source-help-btn))


;;;###autoload
(add-hook 'help-mode-hook 'pophint-config:help-setup t)


(provide 'pophint-config--help)
;;; pophint-config--help.el ends here
