(require 'cl-lib)
(require 'pophint)


(defgroup pophint-config nil
  "A configuration set for pophint"
  :group 'popup
  :prefix "pophint-config:")

;;;###autoload
(defcustom pophint-config:effect-default-activated nil
  "Whether to activate various effects in Emacs basic functions."
  :type 'boolean
  :group 'pophint-config)

;;;###autoload
(defcustom pophint-config:run-defcommand-exhaustively-after-load nil
  "Whether to run `pophint-config:defcommand-exhaustively' after load each of `pophint-config:features'."
  :type 'boolean
  :group 'pophint-config)


;;;###autoload
(defvar pophint-config:inch-length 3)

;;;###autoload
(defun pophint-config:inch-forward ()
  (let* ((currpt (point))
         (pt1 (save-excursion
                (cl-loop for pt = (progn (forward-word 1) (point))
                         until (or (>= (- pt currpt) pophint-config:inch-length)
                                   (= pt (point-max))))
                (point)))
         (pt2 (save-excursion
                (cl-loop for re in '("\\w+" "\\s-+" "\\W+" "\\w+")
                         for pt = (progn (re-search-forward (concat "\\=" re) nil t)
                                         (point))
                         if (>= (- pt currpt) pophint-config:inch-length)
                         return pt
                         finally return pt1))))
    (goto-char (if (> pt1 pt2) pt2 pt1))))

;;;###autoload
(defun pophint-config:make-hint-with-inch-forward (&optional limitpt)
  (let ((currpt (point))
        (nextpt (progn (pophint-config:inch-forward) (point))))
    (when (and (or (not limitpt)
                   (<= currpt limitpt))
               (>= (- nextpt currpt) pophint-config:inch-length))
      `(:startpt ,currpt :endpt ,nextpt :value ,(buffer-substring-no-properties currpt nextpt)))))


(provide 'pophint-config---util)
;;; pophint-config---util.el ends here
