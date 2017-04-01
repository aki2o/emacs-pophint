(require 'pophint)
(require 'pophint-config---util)


(defcustom pophint-config:thing-at-point-effect-enabled t
  "Whether to do pophint as substitute for the function like `thing-at-point'."
  :type 'boolean
  :group 'pophint-config)


;;;###autoload
(defmacro pophint-config:set-thing-at-point-function (function)
  "Set advice to get thing by hint-tip as substitute for COMMAND."
  (declare (indent 0))
  `(defadvice ,function (around do-pophint activate)
     (if (not pophint-config:thing-at-point-effect-enabled)
         ad-do-it
       (pophint--trace "start as substitute for %s" (symbol-name ',function))
       (setq ad-return-value
             (pophint:do-flexibly :action-name "SelectThing"
                                  :action (lambda (hint)
                                            (pophint:hint-value hint)))))))

;;;###autoload
(defmacro pophint-config:thing-def-command-with-toggle-effect (command)
  "Define a command named `pophint-config:thing-do-COMMAND-with-toggle-effect' to do COMMAND with toggle `pophint-config:thing-at-point-effect-enabled'."
  (declare (indent 0))
  (let ((func-sym (intern (format "pophint-config:thing-do-%s-with-toggle-effect" (symbol-name command))))
        (action-name (mapconcat 'identity
                                (mapcar 'capitalize (split-string (symbol-name command) "-+"))
                                ""))
        (doc (format "Do `%s' with toggle `pophint-config:thing-at-point-effect-enabled'." (symbol-name command))))
    `(defun ,func-sym ()
       ,doc
       (interactive)
       (let ((pophint-config:thing-at-point-effect-enabled (not pophint-config:thing-at-point-effect-enabled)))
         (call-interactively ',command)))))


(provide 'pophint-config--thing)
;;; pophint-config--thing.el ends here
