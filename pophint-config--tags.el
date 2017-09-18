(require 'pophint)
(require 'pophint-config--sym)


(defvar pophint-config:tag-jump-current-mode nil)


;;;###autoload
(defmacro* pophint-config:set-tag-jump-command (command &key point-arg-index)
  "Set advice to move the point selected hint-tip before COMMAND.

If COMMAND receives point by interactive, give the argument index as POINT-ARG-INDEX."
  (declare (indent 0))
  `(defadvice ,command (around do-pophint activate)
     (pophint--trace "start as substitute for %s" (symbol-name ',command))
     (let ((pophint-config:tag-jump-current-mode major-mode))
       (lexical-let ((currwnd (get-buffer-window))
                     (currpt (point))
                     (startpt (progn
                                ;; move to head of current symbol
                                (skip-syntax-backward "w_")
                                (point))))
         (pophint:do :allwindow t
                     :direction 'around
                     :source `((activebufferp . (lambda (b)
                                                  (eq pophint-config:tag-jump-current-mode
                                                      (buffer-local-value 'major-mode b))))
                               ,@pophint:source-symbol)
                     :action-name "TagJump"
                     :action (lambda (hint)
                               (lexical-let* ((startwnd (pophint:hint-window hint))
                                              (beforept (window-point startwnd)))
                                 (with-selected-window startwnd
                                   (goto-char (pophint:hint-startpt hint))
                                   (when ,point-arg-index
                                     (ad-set-arg ,point-arg-index (point)))
                                   ad-do-it)
                                 (when (and (window-live-p startwnd)
                                            (eq (window-point startwnd) (pophint:hint-startpt hint)))
                                   ;; if jumped into other window, move active window point to before jump
                                   (set-window-point startwnd beforept))
                                 (when (and (window-live-p currwnd)
                                            (eq (window-point currwnd) startpt))
                                   ;; if jumped into other window, move active window point to before jump
                                   (set-window-point currwnd currpt)))))))))


(provide 'pophint-config--tags)
;;; pophint-config--tags.el ends here
