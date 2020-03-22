(require 'direx nil t)
(require 'rx)
(require 'pophint)

;;;###autoload
(defcustom pophint-direx:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defvar pophint-direx--node-regexp nil)

(defun pophint-direx:node-regexp ()
  (or pophint-direx--node-regexp
      (setq pophint-direx--node-regexp
            (rx-to-string `(and bol (* space)
                                (or ,direx:leaf-icon
                                    ,direx:open-icon
                                    ,direx:closed-icon)
                                (group (+ not-newline))
                                (* space) eol)))))
(define-obsolete-function-alias 'pophint-config:direx-node-regexp 'pophint-direx:node-regexp "1.1.0")

;;;###autoload
(defun pophint:do-direx-node () (interactive))
(with-no-warnings
  (pophint:defsource :name "direx-node"
                     :description "Node on DireX."
                     :source '((shown . "Node")
                               (regexp . pophint-direx:node-regexp)
                               (requires . 1)
                               (highlight . nil)
                               (dedicated . (e2wm))
                               (activebufferp . (lambda (b)
                                                  (pophint--maybe-kind-mode-buffer-p b 'direx:direx-mode)))
                               (action . (lambda (hint)
                                           (funcall pophint--default-action hint)
                                           (when (and (featurep 'e2wm)
                                                      (e2wm:managed-p))
                                             (direx:find-item-other-window)
                                             (e2wm:pst-window-select-main)))))))

(defun pophint-direx:setup ()
  (add-to-list 'pophint:sources 'pophint:source-direx-node))
(define-obsolete-function-alias 'pophint-config:direx-setup 'pophint-direx:setup "1.1.0")

;;;###autoload
(defun pophint-direx:provision (activate)
  (interactive)
  (if activate
      (add-hook 'direx:direx-mode-hook 'pophint-direx:setup t)
    (remove-hook 'direx:direx-mode-hook 'pophint-direx:setup)))

;;;###autoload
(with-eval-after-load 'direx
  (when pophint-direx:enable (pophint-direx:provision t)))


(provide 'pophint-direx)
;;; pophint-direx.el ends here
