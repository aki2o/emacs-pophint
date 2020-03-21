(require 'pophint)

(defcustom pophint-slack:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

;;;###autoload
(defun pophint:do-slack-link () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "slack-link"
    :description "Link on slack."
    :source '((shown . "Link")
              (dedicated . (e2wm))
              (activebufferp . (lambda (b)
                                 (pophint--maybe-kind-mode-buffer-p b 'slack-mode)))
              (requires . 1)
              (highlight . nil)
              (method . (lambda ()
                          (loop while (not (eobp))
                                for startpt = (point)
                                for endpt = (next-property-change startpt)
                                for face = (get-text-property startpt 'face)
                                for value = (buffer-substring-no-properties startpt endpt)
                                do (goto-char (next-property-change (point)))
                                do (pophint--trace "curr slack value: %s" value)
                                if (or (string= value "(load more message)")
                                       (memq face '(slack-message-output-header lui-button-face)))
                                return `(:startpt ,startpt :endpt ,endpt :value ,value))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          ;; do async to avoid freeze maybe slack.el trouble
                          (run-with-idle-timer 0.2 nil
                                               '(lambda (commands)
                                                  (loop for command in commands
                                                        if (ignore-errors (call-interactively command) t) return t))
                                               (if (string= (pophint:hint-value hint) "(load more message)")
                                                   '(slack-room-load-prev-messages)
                                                 (case (get-text-property (point) 'face)
                                                   (lui-button-face
                                                    '(push-button))
                                                   (slack-message-output-header
                                                    ;; error will be raised if pointed message is not a self one.
                                                    ;; in the case, do add reaction
                                                    '(slack-message-edit slack-message-add-reaction))))))))))

(pophint:set-allwindow-command pophint:do-slack-link)

(defun pophint-slack:setup ()
  (add-to-list 'pophint:sources 'pophint:source-slack-link))
(define-obsolete-function-alias 'pophint-config:slack-setup 'pophint-slack:setup "1.1.0")

;;;###autoload
(defun pophint-slack:provision (activate)
  (interactive)
  (if activate
      (add-hook 'slack-mode-hook 'pophint-slack:setup t)
    (remove-hook 'slack-mode-hook 'pophint-slack:setup)))

;;;###autoload
(with-eval-after-load 'slack
  (when pophint-slack:enable (pophint-slack:provision t)))


(provide 'pophint-slack)
;;; pophint-slack.el ends here
