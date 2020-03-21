(require 'cl-lib)
(require 'pophint)

(defcustom pophint-widget:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defcustom pophint-widget:not-invoke-types '(editable-field text)
  "Types to not invoke `widget-apply' after selection by `pophint:source-widget'."
  :type '(repeat symbol)
  :group 'pophint)
(define-obsolete-function-alias 'pophint-config:widget-not-invoke-types 'pophint-widget:not-invoke-types "1.1.0")

(defun pophint-widget--value (w)
  (cl-loop for sexp in '((ignore-errors (widget-value w))
                         (ignore-errors (widget-get w :value)))
           for ret = (eval sexp)
           if (stringp ret) return ret
           finally return ""))

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
                              (pophint--trace "found widget. value:[%s] " (pophint-widget--value w))
                              `(:startpt ,(point)
                                         :endpt ,(+ (point) 1)
                                         :value ,(pophint-widget--value w))))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (let* ((w (widget-at))
                                   (type (when w (widget-type w))))
                              (when (and w
                                         (not (memq type pophint-widget:not-invoke-types)))
                                (widget-apply w :action)))))))))

(defun pophint-widget:setup ()
  (add-to-list 'pophint:sources 'pophint:source-widget))
(define-obsolete-function-alias 'pophint-config:widget-setup 'pophint-widget:setup "1.1.0")

;;;###autoload
(defun pophint-widget:provision (activate)
  (interactive)
  (if activate
      (add-hook 'Custom-mode-hook 'pophint-widget:setup t)
    (remove-hook 'Custom-mode-hook 'pophint-widget:setup)))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-widget:enable (pophint-widget:provision t)))


(provide 'pophint-widget)
;;; pophint-widget.el ends here
