(require 'rx)
(require 'pophint)


;;;###autoload
(defvar pophint-config:regexp-one-line
  (rx-to-string `(and bol (* (syntax whitespace)) (group (+ not-newline)))))


;;;###autoload
(pophint:defsource :name "one-line"
                   :description "One line."
                   :source '((shown . "Line")
                             (regexp . pophint-config:regexp-one-line)))

;;;###autoload
(add-to-list 'pophint:global-sources 'pophint:source-one-line t)


;;;###autoload
(pophint:defsource
  :name "comment-line"
  :description "Part of `font-lock-comment-face' in line"
  :source '((shown . "Cmt")
            (method . (lambda ()
                        (loop while (re-search-forward "\\s<+" nil t)
                              for startpt = (progn (skip-syntax-forward " ") (point))
                              for endpt = (when (and (eq (get-text-property (point) 'face) 'font-lock-comment-face)
                                                     (re-search-forward "\\s-*\\(\\s>+\\|$\\)"))
                                            (match-beginning 0))
                              for value = (when endpt (buffer-substring-no-properties startpt endpt))
                              if (and (stringp value)
                                      (not (string= value "")))
                              return `(:startpt ,startpt :endpt ,endpt :value ,value))))))

;;;###autoload
(add-to-list 'pophint:global-sources 'pophint:source-comment-line t)


(provide 'pophint-config--line)
;;; pophint-config--line.el ends here
