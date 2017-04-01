(require 'pophint)
(require 'pophint-config--sym)


(defvar pophint-config:tag-jump-current-mode nil)


;;;###autoload
(defmacro pophint-config:set-tag-jump-command (command)
  "Set advice to move the point selected hint-tip before COMMAND."
  (declare (indent 0))
  `(defadvice ,command (around do-pophint activate)
     (pophint--trace "start as substitute for %s" (symbol-name ',command))
     (let ((pophint-config:tag-jump-current-mode major-mode)
           (currpt (point))
           (startpt (progn (skip-syntax-backward "w_") (point))))
       (pophint:do :allwindow t
                   :direction 'around
                   :source `((activebufferp . (lambda (b)
                                                (eq pophint-config:tag-jump-current-mode
                                                    (buffer-local-value 'major-mode b))))
                             ,@pophint:source-symbol)
                   :action-name "TagJump"
                   :action (lambda (hint)
                             (with-selected-window (pophint:hint-window hint)
                               (goto-char (pophint:hint-startpt hint))
                               ad-do-it
                               (ignore-errors
                                 (let (pt (point))
                                   (with-selected-window (get-buffer-window)
                                     (goto-char pt)))))))
       (when (and (eq major-mode pophint-config:tag-jump-current-mode)
                  (= (point) startpt))
         (goto-char currpt)))))


(provide 'pophint-config--tags)
;;; pophint-config--tags.el ends here
