(require 'pophint)

(defcustom pophint-sym:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(pophint:defsource :name "symbol"
                   :description "Symbol."
                   :source '((shown . "Sym")
                             (regexp . "\\_<.+?\\_>")))

;;;###autoload
(defun pophint-sym:provision (activate)
  (interactive)
  (if activate
      (add-to-list 'pophint:global-sources 'pophint:source-symbol t)
    (setq pophint:global-sources (remove 'pophint:source-symbol pophint:global-sources))))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-sym:enable (pophint-sym:provision t)))


(provide 'pophint-sym)
;;; pophint-sym.el ends here
