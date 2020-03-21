(require 'pophint)

(defcustom pophint-dired:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

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

(defun pophint-dired:setup ()
  (add-to-list 'pophint:sources 'pophint:source-dired-node))
(define-obsolete-function-alias 'pophint-config:dired-setup 'pophint-dired:setup "1.1.0")

;;;###autoload
(defun pophint-dired:provision (activate)
  (interactive)
  (if activate
      (add-hook 'dired-mode-hook 'pophint-dired:setup t)
    (remove-hook 'dired-mode-hook 'pophint-dired:setup)))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-dired:enable (pophint-dired:provision t)))


(provide 'pophint-dired)
;;; pophint-dired.el ends here
