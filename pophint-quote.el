(require 'pophint)

(defcustom pophint-quote:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defvar pophint-quote:quote-chars '("\"" "'" "`"))
(make-variable-buffer-local 'pophint-quote:quote-chars)
(make-obsolete 'pophint-config:quote-chars 'pophint-quote:quote-chars "1.1.0")

(defvar pophint-quote:exclude-quote-chars nil)
(make-variable-buffer-local 'pophint-quote:exclude-quote-chars)
(make-obsolete 'pophint-config:exclude-quote-chars 'pophint-quote:exclude-quote-chars "1.1.0")

(defun pophint-quote--quoted-point-p (pt)
  (when (> pt 0)
    (memq (get-text-property pt 'face)
          '(font-lock-string-face font-lock-doc-face))))

;;;###autoload
(defun pophint:do-quoted () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "quoted"
    :description "Quoted range by `pophint-quote:quote-chars'.
If exist the character that not be used for quote, set `pophint-quote:exclude-quote-chars'.
It's a buffer local variable and list like `pophint-quote:quote-chars'."
    :source '((shown . "Quoted")
              (method . (lambda ()
                          (let* ((chars (loop for c in pophint-quote:quote-chars
                                              if (not (member c pophint-quote:exclude-quote-chars))
                                              collect c))
                                 (char-re (when chars (regexp-opt chars)))
                                 (re (when char-re (rx-to-string `(and (group (regexp ,char-re)))))))
                            (while (and (pophint-quote--quoted-point-p (point))
                                        re
                                        (re-search-forward re nil t)))
                            (loop while (and re (re-search-forward re nil t))
                                  for word = (match-string-no-properties 1)
                                  for startpt = (point)
                                  for endpt = (or (when (and (< (point) (point-max))
                                                             (string= (format "%c" (char-after)) word))
                                                    (forward-char)
                                                    (- (point) 1))
                                                  (when (re-search-forward (format "[^\\]%s" word) nil t)
                                                    (- (point) 1)))
                                  for value = (when (and endpt (< startpt endpt))
                                                (buffer-substring-no-properties startpt endpt))
                                  if (not endpt) return nil
                                  if (and (stringp value) (not (string= value "")))
                                  return `(:startpt ,startpt :endpt ,endpt :value ,value))))))))

;;;###autoload
(defun pophint-quote:provision (activate)
  (interactive)
  (if activate
      (add-to-list 'pophint:global-sources 'pophint:source-quoted t)
    (setq pophint:global-sources (remove 'pophint:source-quoted pophint:global-sources))))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-quote:enable (pophint-quote:provision t)))


(provide 'pophint-quote)
;;; pophint-quote.el ends here
