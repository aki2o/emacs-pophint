(require 'pophint)
(require 'pophint-config---util)


;;;###autoload
(defvar pophint-config:yank-immediately-when-marking-p nil)
;;;###autoload
(defvar pophint-config:mark-direction 'forward)


;;;###autoload
(defadvice set-mark-command (after do-pophint disable)
  (let ((pophint-config:mark-user-start (point))
        (action-name (if pophint-config:yank-immediately-when-marking-p
                         "Yank"
                       "Focus"))
        (action (lambda (hint)
                  (let ((currpt (point)))
                    (goto-char (pophint:hint-startpt hint))
                    (when pophint-config:yank-immediately-when-marking-p
                      (kill-ring-save currpt (point)))))))
    (case pophint-config:mark-direction
      (forward
       (pophint-config:do-with-narrow-or-wide :narrow-limit (point-at-eol)
                                              :use-pos-tip nil
                                              :action-name action-name
                                              :action action))
      (backward
       (pophint-config:do-with-narrow-or-wide :backward-p t
                                              :narrow-limit (point-at-bol)
                                              :use-pos-tip nil
                                              :action-name action-name
                                              :action action))
      (t
       (pophint:do :not-highlight t
                   :not-switch-window t
                   :use-pos-tip nil
                   :direction pophint-config:mark-direction
                   :source '((shown . "Region")
                             (method . (lambda ()
                                         (when (= (point) pophint-config:mark-user-start)
                                           (pophint-config:inch-forward))
                                         (pophint-config:make-hint-with-inch-forward))))
                   :action-name action-name
                   :action action)))))

;;;###autoload
(defadvice cua-set-mark (after do-pophint disable)
  (pophint:do :not-highlight t
              :not-switch-window t
              :use-pos-tip nil
              :direction pophint-config:mark-direction
              :source '((shown . "Region")
                        (regexp . "[^a-zA-Z0-9]+")
                        (action . (lambda (hint)
                                    (let* ((currpt (point)))
                                      (goto-char (pophint:hint-startpt hint))
                                      (when pophint-config:yank-immediately-when-marking-p
                                        (kill-ring-save currpt (point)))))))))


;;;###autoload
(when pophint-config:effect-default-activated
  (ad-enable-advice 'set-mark-command 'after 'do-pophint)
  (ad-enable-advice 'cua-set-mark 'after 'do-pophint)
  (ad-activate 'set-mark-command)
  (ad-activate 'cua-set-mark))


;;;###autoload
(defun pophint-config:set-yank-immediately-when-marking (activate)
  "Whether yank immediately when select hint-tip after `set-mark-command' or `cua-set-mark'."
  (setq pophint-config:yank-immediately-when-marking-p activate))


;;;###autoload
(defun pophint-config:set-mark-direction (direction)
  "Set direction when select hint-tip after `set-mark-command' or `cua-set-mark'."
  (setq pophint-config:mark-direction direction))

;;;###autoload
(defun pophint-config:set-automatically-when-marking (activate)
  "Whether do pop-up automatically when `set-mark-command' or `cua-set-mark'."
  (cond (activate
         (ad-enable-advice 'set-mark-command 'after 'do-pophint)
         (ad-enable-advice 'cua-set-mark 'after 'do-pophint))
        (t
         (ad-disable-advice 'set-mark-command 'after 'do-pophint)
         (ad-disable-advice 'cua-set-mark 'after 'do-pophint)))
  (ad-activate 'set-mark-command)
  (ad-activate 'cua-set-mark))


(provide 'pophint-config--mark)
;;; pophint-config--mark.el ends here
