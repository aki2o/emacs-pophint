(require 'pophint)


;;;###autoload
(defvar pophint-config:quote-chars '("\"" "'" "`"))

;;;###autoload
(defvar pophint-config:exclude-quote-chars nil)
;;;###autoload
(make-variable-buffer-local 'pophint-config:exclude-quote-chars)

;;;###autoload
(defun pophint-config:quoted-point-p (pt)
  (when (> pt 0)
    (memq (get-text-property pt 'face)
          '(font-lock-string-face font-lock-doc-face))))


;;;###autoload
(pophint:defsource
  :name "quoted"
  :description "Quoted range by `pophint-config:quote-chars'.
If exist the character that not be used for quote, set `pophint-config:exclude-quote-chars'.
It's a buffer local variable and list like `pophint-config:quote-chars'."
  :source '((shown . "Quoted")
            (method . (lambda ()
                        (let* ((chars (loop for c in pophint-config:quote-chars
                                            if (not (member c pophint-config:exclude-quote-chars))
                                            collect c))
                               (char-re (when chars (regexp-opt chars)))
                               (re (when char-re (rx-to-string `(and (group (regexp ,char-re)))))))
                          (while (and (pophint-config:quoted-point-p (point))
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
                                return `(:startpt ,startpt :endpt ,endpt :value ,value)))))))

;;;###autoload
(add-to-list 'pophint:global-sources 'pophint:source-quoted t)


(provide 'pophint-config--quote)
;;; pophint-config--quote.el ends here
