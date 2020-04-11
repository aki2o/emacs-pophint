(require 'pophint)

;;;###autoload
(defcustom pophint-outline:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defcustom pophint-outline:heading-action 'outline-show-children
  "Function to invoke on it when hint selected."
  :type 'function
  :group 'pophint)

;;;###autoload
(defun pophint:do-outline-heading () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "outline-heading"
    :description "Heading of outline."
    :source '((shown . "Heading")
              (highlight . nil)
              (method . (lambda ()
                          (let* ((currpt (point))
                                 (startpt (progn (outline-next-visible-heading 1) (+ (point) (funcall outline-level))))
                                 (endpt (progn (outline-end-of-heading) (point)))
                                 (value (buffer-substring-no-properties startpt endpt)))
                            (when (and (<= currpt startpt)
                                       (outline-on-heading-p))
                              `(:startpt ,startpt :endpt ,endpt :value ,value)))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (when pophint-outline:heading-action
                              (funcall pophint-outline:heading-action))))))))

;;;###autoload
(defun pophint-outline:provision (activate)
  (interactive)
  (if activate
      (progn
        (add-to-list 'pophint:global-sources 'pophint:source-outline-heading t))
    (setq pophint:global-sources
          (remove 'pophint:source-outline-heading pophint:global-sources))))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-outline:enable (pophint-outline:provision t)))


(provide 'pophint-outline)
;;; pophint-outline.el ends here
