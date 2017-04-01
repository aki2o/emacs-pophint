(require 'pophint)


;;;###autoload
(pophint:defsource :name "symbol"
                   :description "Symbol."
                   :source '((shown . "Sym")
                             (regexp . "\\_<.+?\\_>")))

;;;###autoload
(add-to-list 'pophint:global-sources 'pophint:source-symbol t)


(provide 'pophint-config--sym)
;;; pophint-config--sym.el ends here
