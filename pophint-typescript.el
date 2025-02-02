(require 'rx)
(require 'pophint)

;;;###autoload
(defcustom pophint-typescript:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

;;;###autoload
(defun pophint:do-typescript-type () (interactive))
(with-no-warnings
  (pophint:defsource :name "typescript-type"
                     :description "Typescript Type."
                     :source `((shown . "Type")
                               (activebufferp . (lambda (b)
                                                  (eq 'typescript-mode
                                                      (buffer-local-value 'major-mode b))))
                               (regexp . ,(rx-to-string `(and bow (any "A-Z") (+ (any "a-zA-Z0-9.$"))))))))

(defun pophint-typescript:setup ()
  (add-to-list 'pophint:sources 'pophint:source-typescript-type))

;;;###autoload
(defun pophint-typescript:provision (activate)
  (interactive)
  (if activate
      (add-hook 'typescript-mode-hook 'pophint-typescript:setup t)
    (remove-hook 'typescript-mode-hook 'pophint-typescript:setup)))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-typescript:enable (pophint-typescript:provision t)))


(provide 'pophint-typescript)
;;; pophint-typescript.el ends here
