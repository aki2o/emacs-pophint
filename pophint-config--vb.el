(require 'pophint)
(require 'pophint-config--quote)


;;;###autoload
(defun pophint-config:vb-setup ()
  (setq pophint-config:exclude-quote-chars '("'")))


;;;###autoload
(add-hook 'visual-basic-mode-hook 'pophint-config:vb-setup t)


(provide 'pophint-config--vb)
;;; pophint-config--vb.el ends here
