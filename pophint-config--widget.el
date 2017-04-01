(require 'cl-lib)
(require 'pophint)


(defun pophint-config:widget-value (w)
  (cl-loop for sexp in '((ignore-errors (widget-value w))
                         (ignore-errors (widget-get w :value)))
           for ret = (eval sexp)
           if (stringp ret) return ret
           finally return ""))

(defvar pophint-config:widget-not-invoke-types '(editable-field text))


;;;###autoload
(defun pophint:do-widget () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "widget"
    :description "Widget"
    :source '((shown . "Widget")
              (requires . 0)
              (highlight . nil)
              (dedicated . (e2wm))
              (activebufferp . (lambda (buff)
                                 (with-current-buffer buff
                                   (when (where-is-internal 'widget-forward (current-local-map))
                                     t))))
              (method . (lambda ()
                          (let* ((pt (point))
                                 (mpt (progn (widget-move 1) (point)))
                                 (w (when (> mpt pt) (widget-at))))
                            (when w
                              (pophint--trace "found widget. value:[%s] " (pophint-config:widget-value w))
                              `(:startpt ,(point)
                                         :endpt ,(+ (point) 1)
                                         :value ,(pophint-config:widget-value w))))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (let* ((w (widget-at))
                                   (type (when w (widget-type w))))
                              (when (and w
                                         (not (memq type pophint-config:widget-not-invoke-types)))
                                (widget-apply w :action)))))))))


;;;###autoload
(defun pophint-config:widget-setup ()
  (add-to-list 'pophint:sources 'pophint:source-widget))


;;;###autoload
(add-hook 'Custom-mode-hook 'pophint-config:widget-setup t)


(provide 'pophint-config--widget)
;;; pophint-config--widget.el ends here
