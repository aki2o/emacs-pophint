(require 'pophint)
(require 'pophint-sym)

(defvar pophint-tags--current-mode nil)

;;;###autoload
(cl-defmacro pophint-tags:advice-command (command &key (point-arg-index nil))
  "Set advice to move the point selected hint-tip before COMMAND.

If COMMAND receives the point by interactive,
  give the argument index as POINT-ARG-INDEX."
  (declare (indent 0))
  (let ((advice-name (intern (format "%s-advice-around-do-pophint" command))))
    `(progn
       (defun ,advice-name (orig &rest args)
         (pophint--trace "start as substitute for %s" (symbol-name ',command))
         (let* ((pophint-tags--current-mode major-mode)
                (currwnd (get-buffer-window))
                (currpt (point))
                (startpt (progn
                           ;; move to head of current symbol
                           (skip-syntax-backward "w_")
                           (point)))
                (hint (pophint:do :allwindow t
                                  :direction 'around
                                  :source `((activebufferp . (lambda (b)
                                                               (eq pophint-tags--current-mode
                                                                   (buffer-local-value 'major-mode b))))
                                            ,@pophint:source-symbol)
                                  :action-name "TagJump"
                                  :action 'hint))
                (wnd (when hint (pophint:hint-window hint)))
                (beforept (when wnd (window-point wnd))))
           (when wnd
             (with-selected-window wnd
               (goto-char (pophint:hint-startpt hint))
               (when ,point-arg-index
                 (setf (nth ,point-arg-index args) (point)))
               (apply orig args))
             (when (and (window-live-p wnd)
                        (eq (window-point wnd) (pophint:hint-startpt hint)))
               ;; if jumped into other window, move active window point to before jump
               (set-window-point wnd beforept))
             (when (and (window-live-p currwnd)
                        (eq (window-point currwnd) startpt))
               ;; if jumped into other window, move active window point to before jump
               (set-window-point currwnd currpt)))))
       (advice-add ',command :around ',advice-name))))
(define-obsolete-function-alias 'pophint-config:set-tag-jump-command 'pophint-tags:advice-command "1.1.0")


(provide 'pophint-tags)
;;; pophint-tags.el ends here
