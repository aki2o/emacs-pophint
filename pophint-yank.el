(require 'pophint)

(defcustom pophint-yank:relayout-on-start-rangeyank-p nil
  "Whether relayout when select start of `pophint:source-rangeyank'."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-relayout-when-rangeyank-start 'pophint-yank:relayout-on-start-rangeyank-p "1.1.0")

(defvar pophint-yank--yank-action
  '(lambda (hint)
     (kill-new (pophint:hint-value hint))))

(defvar pophint-yank--startpt nil)

(defvar pophint-yank--rangeyank-action
  (lambda (hint)
    (let ((wnd (pophint:hint-window hint)))
      (when (and (windowp wnd)
                 (window-live-p wnd))
        (save-window-excursion
          (save-excursion
            (with-selected-window wnd
              (goto-char (pophint:hint-startpt hint))
              (setq pophint-yank--startpt (point))
              (when pophint-yank:relayout-on-start-rangeyank-p
                (recenter 0)
                (delete-other-windows))
              (pophint:do :not-highlight t
                          :not-switch-window t
                          :use-pos-tip nil
                          :direction 'forward
                          :source `((shown . "Region")
                                    (requires . ,pophint:inch-forward-length)
                                    (init . pophint:inch-forward)
                                    (method . pophint:make-hint-with-inch-forward))
                          :action-name "Yank"
                          :action (lambda (hint)
                                    (when (number-or-marker-p pophint-yank--startpt)
                                      (kill-new (buffer-substring-no-properties pophint-yank--startpt
                                                                                (pophint:hint-startpt hint)))))))))))))

;;;###autoload
(defun pophint:do-flexibly-yank () (interactive))
(with-no-warnings
  (pophint:defaction :key "y"
                     :name "Yank"
                     :description "Yank the text of selected hint-tip."
                     :action pophint-yank--yank-action))

;;;###autoload
(defun pophint:do-flexibly-rangeyank () (interactive))
(with-no-warnings
  (pophint:defaction :key "Y"
                     :name "RangeYank"
                     :description "Yank the text getting end point by do pop-up at the selected point."
                     :action pophint-yank--rangeyank-action))

;;;###autoload
(defun pophint:do-rangeyank () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "RangeYank"
    :description "Yank the text getting end point by do pop-up at the selected point."
    :source `((shown . "RangeYank")
              (action . ,pophint-yank--rangeyank-action)
              ,@pophint--default-source)))


(provide 'pophint-yank)
;;; pophint-yank.el ends here
