(require 'pophint)

;;;###autoload
(defcustom pophint-info:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

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

(defun pophint-info:setup ()
  (add-to-list 'pophint:sources 'pophint:source-info-ref))
(define-obsolete-function-alias 'pophint-config:info-setup 'pophint-info:setup "1.1.0")

;;;###autoload
(defun pophint-info:provision (activate)
  (interactive)
  (if activate
      (add-hook 'Info-mode-hook 'pophint-info:setup t)
    (remove-hook 'Info-mode-hook 'pophint-info:setup)))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-info:enable (pophint-info:provision t)))


(provide 'pophint-info)
;;; pophint-info.el ends here
