(require 'pophint)

(defcustom pophint-thing:enable-on-thing-at-point t
  "Whether to do pophint as substitute for the function like `thing-at-point'."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:thing-at-point-effect-enabled 'pophint-thing:enable-on-thing-at-point "1.1.0")

;;;###autoload
(cl-defmacro pophint-thing:advice-thing-at-point-function (function)
  "Set advice to get thing by hint-tip as substitute for COMMAND."
  (declare (indent 0))
  `(defadvice ,function (around do-pophint activate)
     (if (not pophint-thing:enable-on-thing-at-point)
         ad-do-it
       (pophint--trace "start as substitute for %s" (symbol-name ',function))
       (setq ad-return-value
             (pophint:do-flexibly :action-name "SelectThing"
                                  :action 'value)))))
(define-obsolete-function-alias 'pophint-config:set-thing-at-point-function 'pophint-thing:advice-thing-at-point-function "1.1.0")

;;;###autoload
(cl-defmacro pophint-thing:defcommand-noadvice (command)
  "Define a command named `pophint-thing:just-COMMAND' to do COMMAND without `pophint-thing:enable-on-thing-at-point'."
  (declare (indent 0))
  (let ((func-sym (intern (format "pophint-thing:just-%s" (symbol-name command))))
        (action-name (mapconcat 'identity
                                (mapcar 'capitalize (split-string (symbol-name command) "-+"))
                                ""))
        (doc (format "Do `%s' without `pophint-thing:enable-on-thing-at-point'." (symbol-name command))))
    `(defun ,func-sym ()
       ,doc
       (interactive)
       (let ((pophint-thing:enable-on-thing-at-point nil))
         (call-interactively ',command)))))
(define-obsolete-function-alias 'pophint-config:thing-def-command-with-toggle-effect 'pophint-thing:defcommand-noadvice "1.1.0")


(provide 'pophint-thing)
;;; pophint-thing.el ends here
