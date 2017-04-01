(require 'direx nil t)
(require 'rx)
(require 'pophint)


(defvar pophint-config:regexp-direx-node nil)

(defun pophint-config:direx-node-regexp ()
  (or pophint-config:regexp-direx-node
      (setq pophint-config:regexp-direx-node
            (rx-to-string `(and bol (* space)
                                (or ,direx:leaf-icon
                                    ,direx:open-icon
                                    ,direx:closed-icon)
                                (group (+ not-newline))
                                (* space) eol)))))


;;;###autoload
(defun pophint:do-direx-node () (interactive))
(with-no-warnings
  (pophint:defsource :name "direx-node"
                     :description "Node on DireX."
                     :source '((shown . "Node")
                               (regexp . pophint-config:direx-node-regexp)
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


;;;###autoload
(defun pophint-config:direx-setup ()
  (add-to-list 'pophint:sources 'pophint:source-direx-node))


;;;###autoload
(add-hook 'direx:direx-mode-hook 'pophint-config:direx-setup t)


(provide 'pophint-config--direx)
;;; pophint-config--direx.el ends here
