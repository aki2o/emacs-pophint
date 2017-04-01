(require 'pophint)


;;;###autoload
(defun pophint:do-direx-node () (interactive))
(with-no-warnings
  (pophint:defsource :name "dired-node"
                     :description "Node in directory."
                     :source '((shown . "Node")
                               (regexp . "^ *[d-][r-][w-][x-].+ +\\([^ ]+\\)$")
                               (requires . 1)
                               (highlight . nil)
                               (dedicated . (e2wm))
                               (activebufferp . (lambda (b)
                                                  (pophint--maybe-kind-mode-buffer-p b 'dired-mode)))
                               (action . (lambda (hint)
                                           (funcall pophint--default-action hint)
                                           (when (and (featurep 'e2wm)
                                                      (e2wm:managed-p))
                                             (dired-find-file)
                                             (e2wm:pst-window-select-main)))))))


;;;###autoload
(defun pophint-config:dired-setup ()
  (add-to-list 'pophint:sources 'pophint:source-dired-node))


;;;###autoload
(add-hook 'dired-mode-hook 'pophint-config:dired-setup t)


(provide 'pophint-config--dired)
;;; pophint-config--dired.el ends here
