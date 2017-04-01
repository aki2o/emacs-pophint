(require 'pophint)


;;;###autoload
(defun pophint:do-info-ref () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "info-ref"
    :description "Reference on info-mode."
    :source '((shown . "Link")
              (dedicated . (e2wm))
              (activebufferp . (lambda (b)
                                 (pophint--maybe-kind-mode-buffer-p b 'Info-mode)))
              (method . (lambda ()
                          (let* ((currpt (point))
                                 (startpt (progn (Info-next-reference) (point)))
                                 (endpt (next-property-change startpt))
                                 (value (buffer-substring-no-properties startpt endpt)))
                            (when (<= currpt startpt)
                              `(:startpt ,startpt :endpt ,endpt :value ,value)))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (Info-follow-nearest-node)))))))


;;;###autoload
(defun pophint-config:info-setup ()
  (add-to-list 'pophint:sources 'pophint:source-info-ref))


;;;###autoload
(add-hook 'Info-mode-hook 'pophint-config:info-setup t)


(provide 'pophint-config--info)
;;; pophint-config--info.el ends here
