(require 'pophint)
(require 'pophint-config---util)


(defvar pophint-config:kill-region-kill-ring-save-p t)

(defvar pophint-config:region-start nil)
(defvar pophint-config:region-end nil)
(defvar pophint-config:region-user-start nil)

(defun pophint-config:do-with-narrow-or-wide-method (key)
  (let ((startpt (plist-get pophint-config:region-start key))
        (endpt (plist-get pophint-config:region-end key)))
    (when (< (point) startpt)
      (goto-char startpt))
    (when (= (point) pophint-config:region-user-start)
      (pophint-config:inch-forward))
    (pophint-config:make-hint-with-inch-forward endpt)))


;;;###autoload
(defun* pophint-config:do-with-narrow-or-wide (&key backward-p narrow-limit action-name action use-pos-tip)
  (let ((pophint:select-source-method 'nil)
        (pophint:switch-source-delay 0)
        (pophint-config:region-user-start (point))
        (pophint-config:region-start (if backward-p
                                         `(:narrow ,(or narrow-limit 0) :wide 0)
                                       `(:narrow ,(point) :wide ,(point))))
        (pophint-config:region-end (if backward-p
                                       `(:narrow ,(point) :wide ,(point))
                                     `(:narrow ,narrow-limit :wide nil))))
    (pophint:do :not-highlight t
                :not-switch-window t
                :use-pos-tip use-pos-tip
                :direction (if backward-p 'backward 'forward)
                :sources `(((shown . "Narrow")
                            (requires . ,pophint-config:inch-length)
                            (method . (lambda ()
                                        (pophint-config:do-with-narrow-or-wide-method :narrow))))
                           ((shown . "Wide")
                            (requires . ,pophint-config:inch-length)
                            (method . (lambda ()
                                        (pophint-config:do-with-narrow-or-wide-method :wide)))))
                :action-name action-name
                :action action)))

;;;###autoload
(defun pophint-config:forward-region ()
  "Forward region by selecting hint-tip."
  (interactive)
  (pophint-config:do-with-narrow-or-wide
   :narrow-limit (point-at-eol)
   :use-pos-tip t
   :action (lambda (hint) (goto-char (pophint:hint-startpt hint)))))

;;;###autoload
(defun pophint-config:backward-region ()
  "Backward region by selecting hint-tip."
  (interactive)
  (pophint-config:do-with-narrow-or-wide
   :backward-p t
   :narrow-limit (point-at-bol)
   :use-pos-tip t
   :action (lambda (hint) (goto-char (pophint:hint-startpt hint)))))

;;;###autoload
(defun pophint-config:kill-region ()
  "Kill/Delete region by selecting hint-tip."
  (interactive)
  (lexical-let ((func (if pophint-config:kill-region-kill-ring-save-p
                          'kill-region
                        'delete-region)))
    (pophint-config:do-with-narrow-or-wide
     :narrow-limit (point-at-eol)
     :use-pos-tip t
     :action-name (symbol-name func)
     :action (lambda (hint)
               (funcall func (point) (pophint:hint-startpt hint))))))

;;;###autoload
(defun pophint-config:backward-kill-region ()
  "Kill/Delete region by selecting hint-tip."
  (interactive)
  (lexical-let ((func (if pophint-config:kill-region-kill-ring-save-p
                          'kill-region
                        'delete-region)))
    (pophint-config:do-with-narrow-or-wide
     :backward-p t
     :narrow-limit (point-at-bol)
     :use-pos-tip t
     :action-name (symbol-name func)
     :action (lambda (hint)
               (funcall func (pophint:hint-startpt hint) (point))))))


;;;###autoload
(defun pophint-config:set-kill-region-kill-ring-save (activate)
  "Whether to save into kill ring by `pophint-config:kill-region'/`pophint-config:backward-kill-region'."
  (setq pophint-config:kill-region-kill-ring-save-p activate))


(provide 'pophint-config--region)
;;; pophint-config--region.el ends here
