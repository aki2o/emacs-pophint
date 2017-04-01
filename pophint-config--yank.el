(require 'pophint)
(require 'pophint-config---util)


(defvar pophint-config:yank-action
  '(lambda (hint)
     (kill-new (pophint:hint-value hint))))

(defvar pophint-config:yank-startpt nil)

(defvar pophint-config:relayout-when-yank-range-start-p nil)

(defvar pophint-config:yank-range-action
  (lambda (hint)
    (let ((wnd (pophint:hint-window hint)))
      (when (and (windowp wnd)
                 (window-live-p wnd))
        (save-window-excursion
          (save-excursion
            (with-selected-window wnd
              (goto-char (pophint:hint-startpt hint))
              (setq pophint-config:yank-startpt (point))
              (when pophint-config:relayout-when-yank-range-start-p
                (recenter 0)
                (delete-other-windows))
              (pophint:do :not-highlight t
                          :not-switch-window t
                          :use-pos-tip nil
                          :direction 'forward
                          :source `((shown . "Region")
                                    (requires . ,pophint-config:inch-length)
                                    (init . pophint-config:inch-forward)
                                    (method . pophint-config:make-hint-with-inch-forward))
                          :action-name "Yank"
                          :action (lambda (hint)
                                    (when (number-or-marker-p pophint-config:yank-startpt)
                                      (kill-new (buffer-substring-no-properties pophint-config:yank-startpt
                                                                                (pophint:hint-startpt hint)))))))))))))


;;;###autoload
(defun pophint:do-flexibly-yank () (interactive))
(with-no-warnings
  (pophint:defaction :key "y"
                     :name "Yank"
                     :description "Yank the text of selected hint-tip."
                     :action 'pophint-config:yank-action))

;;;###autoload
(defun pophint:do-flexibly-rangeyank () (interactive))
(with-no-warnings
  (pophint:defaction :key "Y"
                     :name "RangeYank"
                     :description "Yank the text getting end point by do pop-up at the selected point."
                     :action 'pophint-config:yank-range-action))

;;;###autoload
(defun pophint:do-rangeyank () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "RangeYank"
    :description "Yank the text getting end point by do pop-up at the selected point."
    :source `((shown . "RangeYank")
              (action . pophint-config:yank-range-action)
              ,@pophint--default-source)))


;;;###autoload
(defun pophint-config:set-relayout-when-rangeyank-start (activate)
  "Whether re-layouting window when start searching the end point of RangeYank."
  (setq pophint-config:relayout-when-yank-range-start-p activate))


(provide 'pophint-config--yank)
;;; pophint-config--yank.el ends here
