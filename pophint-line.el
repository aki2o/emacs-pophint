(require 'rx)
(require 'pophint)

;;;###autoload
(defcustom pophint-line:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defvar pophint-line--regexp-one-line
  (rx-to-string `(and bol (* (syntax whitespace)) (group (+ not-newline)))))

(pophint:defsource :name "one-line"
                   :description "One line."
                   :source '((shown . "Line")
                             (regexp . pophint-line--regexp-one-line)))

(pophint:defsource
  :name "comment-line"
  :description "Part of `font-lock-comment-face' in line"
  :source '((shown . "Cmt")
            (method . (lambda ()
                        (cl-loop while (re-search-forward "\\s<+" nil t)
                              for startpt = (progn (skip-syntax-forward " ") (point))
                              for endpt = (when (and (eq (get-text-property (point) 'face) 'font-lock-comment-face)
                                                     (re-search-forward "\\s-*\\(\\s>+\\|$\\)"))
                                            (match-beginning 0))
                              for value = (when endpt (buffer-substring-no-properties startpt endpt))
                              if (and (stringp value)
                                      (not (string= value "")))
                              return `(:startpt ,startpt :endpt ,endpt :value ,value))))))

;;;###autoload
(defun pophint-line:provision (activate)
  (interactive)
  (if activate
      (progn
        (add-to-list 'pophint:global-sources 'pophint:source-one-line t)
        (add-to-list 'pophint:global-sources 'pophint:source-comment-line t))
    (setq pophint:global-sources
          (remove 'pophint:source-one-line
                  (remove 'pophint:source-comment-line pophint:global-sources)))))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-line:enable (pophint-line:provision t)))


(provide 'pophint-line)
;;; pophint-line.el ends here
