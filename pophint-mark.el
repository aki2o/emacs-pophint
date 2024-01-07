(require 'pophint)

;;;###autoload
(defcustom pophint-mark:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-automatically-when-marking 'pophint-mark:enable "1.1.0")

(defcustom pophint-mark:yank-immediately-on-marking-p nil
  "Whether to yank immediately when select hint-tip
after `set-mark-command' or `cua-set-mark'."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-yank-immediately-when-marking 'pophint-mark:yank-immediately-on-marking-p "1.1.0")

(defcustom pophint-mark:direction 'forward
  "Set direction when select hint-tip after `set-mark-command' or `cua-set-mark'."
  :type 'symbol
  :group 'pophint)
(make-obsolete 'pophint-config:set-mark-direction 'pophint-mark:direction "1.1.0")


(defadvice set-mark-command (after do-pophint disable)
  (let ((pophint-mark--user-start (point))
        (action-name (if pophint-mark:yank-immediately-on-marking-p
                         "Yank"
                       "Focus"))
        (action (lambda (hint)
                  (let ((currpt (point)))
                    (goto-char (pophint:hint-startpt hint))
                    (when pophint-mark:yank-immediately-on-marking-p
                      (kill-ring-save currpt (point)))))))
    (cl-case pophint-mark:direction
      (forward
       (pophint-region:narrow-or-wide :narrow-limit (pos-eol)
                                      :use-pos-tip nil
                                      :action-name action-name
                                      :action action))
      (backward
       (pophint-region:narrow-or-wide :backward-p t
                                      :narrow-limit (pos-bol)
                                      :use-pos-tip nil
                                      :action-name action-name
                                      :action action))
      (t
       (pophint:do :not-highlight t
                   :not-switch-window t
                   :use-pos-tip nil
                   :direction pophint-mark:direction
                   :source '((shown . "Region")
                             (method . (lambda ()
                                         (when (= (point) pophint-mark--user-start)
                                           (pophint:inch-forward))
                                         (pophint:make-hint-with-inch-forward))))
                   :action-name action-name
                   :action action)))))

(defadvice cua-set-mark (after do-pophint disable)
  (pophint:do :not-highlight t
              :not-switch-window t
              :use-pos-tip nil
              :direction pophint-mark:direction
              :source '((shown . "Region")
                        (regexp . "[^a-zA-Z0-9]+")
                        (action . (lambda (hint)
                                    (let* ((currpt (point)))
                                      (goto-char (pophint:hint-startpt hint))
                                      (when pophint-mark:yank-immediately-on-marking-p
                                        (kill-ring-save currpt (point)))))))))


;;;###autoload
(defun pophint-mark:provision (activate)
  (interactive)
  (cond (activate
         (ad-enable-advice 'set-mark-command 'after 'do-pophint)
         (ad-enable-advice 'cua-set-mark 'after 'do-pophint))
        (t
         (ad-disable-advice 'set-mark-command 'after 'do-pophint)
         (ad-disable-advice 'cua-set-mark 'after 'do-pophint)))
  (ad-activate 'set-mark-command)
  (ad-activate 'cua-set-mark))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-mark:enable (pophint-mark:provision t)))


(provide 'pophint-mark)
;;; pophint-mark.el ends here
