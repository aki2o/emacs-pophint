(require 'yaol)
(require 'pophint)

;;;###autoload
(defcustom pophint-yaol:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defcustom pophint-yaol:head-action 'yaol-fold-clear-current
  "Function to invoke on it when hint selected."
  :type 'function
  :group 'pophint)

;;;###autoload
(defun pophint:do-yaol-head () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "yaol-head"
    :description "Head of yaol."
    :source '((shown . "Head")
              (highlight . nil)
              (requires . 1)
              (method . (lambda ()
                          (let* ((startpt (when (yaol-next-head)
                                            (point)))
                                 (endpt (when startpt
                                          (yaol-node-fold-beg (-first-item (yaol-find-deepest-nodes-at (point))))))
                                 (value (when endpt
                                          (buffer-substring-no-properties startpt endpt))))
                            (when (and startpt endpt
                                       (< startpt endpt))
                              `(:startpt ,startpt :endpt ,endpt :value ,value)))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (when pophint-yaol:head-action
                              (funcall pophint-yaol:head-action))))))))

;;;###autoload
(defun pophint-yaol:provision (activate)
  (interactive)
  (if activate
      (progn
        (add-to-list 'pophint:global-sources 'pophint:source-yaol-head t))
    (setq pophint:global-sources
          (remove 'pophint:source-yaol-head pophint:global-sources))))

;;;###autoload
(with-eval-after-load 'yaol
  (when pophint-yaol:enable (pophint-yaol:provision t)))


(provide 'pophint-yaol)
;;; pophint-yaol.el ends here
