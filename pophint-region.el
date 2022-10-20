(require 'pophint)

(defvar pophint-region--start nil)
(defvar pophint-region--end nil)
(defvar pophint-region--user-start nil)

(defun pophint-region--do-with-narrow-or-wide (key)
  (let ((startpt (plist-get pophint-region--start key))
        (endpt (plist-get pophint-region--end key)))
    (when (< (point) startpt)
      (goto-char startpt))
    (when (= (point) pophint-region--user-start)
      (pophint:inch-forward))
    (pophint:make-hint-with-inch-forward :limit endpt)))

;;;###autoload
(cl-defun pophint-region:narrow-or-wide (&key backward-p narrow-limit action-name action use-pos-tip)
  (let ((pophint:select-source-method 'nil)
        (pophint:switch-source-delay 0)
        (pophint-region--user-start (point))
        (pophint-region--start (if backward-p
                                   `(:narrow ,(or narrow-limit 0) :wide 0)
                                 `(:narrow ,(point) :wide ,(point))))
        (pophint-region--end (if backward-p
                                 `(:narrow ,(point) :wide ,(point))
                               `(:narrow ,narrow-limit :wide nil))))
    (pophint:do :not-highlight t
                :not-switch-window t
                :use-pos-tip use-pos-tip
                :direction (if backward-p 'backward 'forward)
                :sources `(((shown . "Narrow")
                            (requires . ,pophint:inch-forward-length)
                            (method . (lambda ()
                                        (pophint-region--do-with-narrow-or-wide :narrow))))
                           ((shown . "Wide")
                            (requires . ,pophint:inch-forward-length)
                            (method . (lambda ()
                                        (pophint-region--do-with-narrow-or-wide :wide)))))
                :action-name action-name
                :action action)))
(define-obsolete-function-alias 'pophint-config:do-with-narrow-or-wide 'pophint-region:narrow-or-wide "1.1.0")

;;;###autoload
(defun pophint-region:forward ()
  "Forward region by selecting hint-tip."
  (interactive)
  (pophint-region:narrow-or-wide
   :narrow-limit (point-at-eol)
   :use-pos-tip t
   :action (lambda (hint) (goto-char (pophint:hint-startpt hint)))))
(define-obsolete-function-alias 'pophint-config:forward-region 'pophint-region:forward "1.1.0")

;;;###autoload
(defun pophint-region:backward ()
  "Backward region by selecting hint-tip."
  (interactive)
  (pophint-region:narrow-or-wide
   :backward-p t
   :narrow-limit (point-at-bol)
   :use-pos-tip t
   :action (lambda (hint) (goto-char (pophint:hint-startpt hint)))))
(define-obsolete-function-alias 'pophint-config:backward-region 'pophint-region:backward "1.1.0")

;;;###autoload
(defun pophint-region:kill ()
  "Kill region by selecting hint-tip."
  (interactive)
  (pophint-region:narrow-or-wide
   :narrow-limit (point-at-eol)
   :use-pos-tip t
   :action-name "kill-region"
   :action (lambda (hint) (kill-region (point) (pophint:hint-startpt hint)))))
(define-obsolete-function-alias 'pophint-config:kill-region 'pophint-region:kill "1.1.0")

;;;###autoload
(defun pophint-region:backward-kill ()
  "Kill region by selecting hint-tip."
  (interactive)
  (pophint-region:narrow-or-wide
   :backward-p t
   :narrow-limit (point-at-bol)
   :use-pos-tip t
   :action-name "kill-region"
   :action (lambda (hint) (kill-region (pophint:hint-startpt hint) (point)))))
(define-obsolete-function-alias 'pophint-config:backward-kill-region 'pophint-region:backward-kill "1.1.0")

;;;###autoload
(defun pophint-region:delete ()
  "Delete region by selecting hint-tip."
  (interactive)
  (pophint-region:narrow-or-wide
   :narrow-limit (point-at-eol)
   :use-pos-tip t
   :action-name "delete-region"
   :action (lambda (hint) (delete-region (point) (pophint:hint-startpt hint)))))

;;;###autoload
(defun pophint-region:backward-delete ()
  "Delete region by selecting hint-tip."
  (interactive)
  (pophint-region:narrow-or-wide
   :backward-p t
   :narrow-limit (point-at-bol)
   :use-pos-tip t
   :action-name "delete-region"
   :action (lambda (hint) (delete-region (pophint:hint-startpt hint) (point)))))


(provide 'pophint-region)
;;; pophint-region.el ends here
