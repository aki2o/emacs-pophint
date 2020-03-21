(require 'pophint)
(require 'pophint-quote)

(defcustom pophint-vb:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defun pophint-vb:setup ()
  (setq pophint-quote:exclude-quote-chars '("'")))
(define-obsolete-function-alias 'pophint-config:vb-setup 'pophint-vb:setup "1.1.0")

;;;###autoload
(defun pophint-vb:provision (activate)
  (interactive)
  (if activate
      (add-hook 'visual-basic-mode-hook 'pophint-vb:setup t)
    (remove-hook 'visual-basic-mode-hook 'pophint-vb:setup)))

;;;###autoload
(with-eval-after-load 'visual-basic-mode
  (when pophint-vb:enable (pophint-vb:provision t)))


(provide 'pophint-vb)
;;; pophint-vb.el ends here
