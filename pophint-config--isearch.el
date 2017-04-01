(require 'pophint)
(require 'pophint-config---util)
(require 'pophint-config--region)


;;;###autoload
(defvar pophint-config:active-when-isearch-exit-p pophint-config:effect-default-activated)
;;;###autoload
(defvar pophint-config:index-of-isearch-overlays 0)

;;;###autoload
(defadvice isearch-exit (before do-pophint disable)
  (when pophint-config:active-when-isearch-exit-p
    (pophint:do :not-highlight t
                :not-switch-window t
                :source '((shown . "Cand")
                          (init . (lambda ()
                                    (setq pophint-config:index-of-isearch-overlays 0)))
                          (method . (lambda ()
                                      (pophint--trace "overlay count:[%s] index:[%s]"
                                                      (length isearch-lazy-highlight-overlays)
                                                      pophint-config:index-of-isearch-overlays)
                                      (let* ((idx pophint-config:index-of-isearch-overlays)
                                             (ov (when (< idx (length isearch-lazy-highlight-overlays))
                                                   (nth idx isearch-lazy-highlight-overlays)))
                                             (startpt (when ov (overlay-start ov)))
                                             (endpt (when ov (overlay-end ov)))
                                             (value (when ov (buffer-substring-no-properties startpt endpt)))
                                             (ret `(:startpt ,startpt :endpt ,endpt :value ,value)))
                                        (when ov (incf pophint-config:index-of-isearch-overlays))
                                        (when startpt (goto-char startpt))
                                        ret)))
                          (action . (lambda (hint)
                                      (goto-char (pophint:hint-startpt hint))))))))

;;;###autoload
(when pophint-config:active-when-isearch-exit-p
  (ad-enable-advice 'isearch-exit 'before 'do-pophint)
  (ad-activate 'isearch-exit))

;;;###autoload
(defun pophint-config:set-automatically-when-isearch (activate)
  "Whether do pop-up automatically when `isearch-exit'."
  (if activate
      (ad-enable-advice 'isearch-exit 'before 'do-pophint)
    (ad-disable-advice 'isearch-exit 'before 'do-pophint))
  (ad-activate 'isearch-exit)
  (setq pophint-config:active-when-isearch-exit-p activate))


;;;###autoload
(defun pophint-config:isearch-yank-region ()
  "Pull rest of region by selecting hint-tip from buffer into search string."
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (pophint-config:do-with-narrow-or-wide
      :narrow-limit (point-at-eol)
      :use-pos-tip nil
      :action (lambda (hint)
                (goto-char (pophint:hint-startpt hint))
                (point))))))

;;;###autoload
(defmacro pophint-config:set-isearch-yank-region-command (command)
  "Set advice to replace COMMAND with `pophint-config:isearch-yank-region'."
  (declare (indent 0))
  `(defadvice ,command (around do-pophint activate)
     (pophint--trace "start do as substitute for %s" (symbol-name ',command))
     (pophint-config:isearch-yank-region)))


;;;###autoload
(eval-after-load "anything-c-moccur"
  '(progn
     (defadvice anything-c-moccur-from-isearch (around disable-pophint activate)
       (let ((exitconf pophint-config:active-when-isearch-exit-p))
         (setq pophint-config:active-when-isearch-exit-p nil)
         ad-do-it
         (setq pophint-config:active-when-isearch-exit-p exitconf)))))

;;;###autoload
(eval-after-load "helm-c-moccur"
  '(progn
     (defadvice helm-c-moccur-from-isearch (around disable-pophint activate)
       (let ((exitconf pophint-config:active-when-isearch-exit-p))
         (setq pophint-config:active-when-isearch-exit-p nil)
         ad-do-it
         (setq pophint-config:active-when-isearch-exit-p exitconf)))))


(defvar pophint-config:isearch-action-result nil)

;;;###autoload
(defun pophint-config:isearch-setup ()
  (let ((hint pophint-config:isearch-action-result))
    (when (pophint:hint-p hint)
      (setq isearch-string (pophint:hint-value hint))
      (setq isearch-message (pophint:hint-value hint)))))

;;;###autoload
(add-hook 'isearch-mode-hook 'pophint-config:isearch-setup t)


;;;###autoload
(defun pophint:do-flexibly-isearch () (interactive))
(with-no-warnings
  (pophint:defaction :key "i"
                     :name "ISearch"
                     :description "Do `isearch' from the text of selected hint-tip."
                     :action (lambda (hint)
                               (let ((pophint-config:isearch-action-result hint))
                                 (with-selected-window (pophint:hint-window hint)
                                   (isearch-forward))))))

(defadvice pophint:do-flexibly-isearch (before set-pophint-condition activate)
  (let* ((ctx "pophint:do-flexibly-isearch")
         (lastc (pophint--get-last-condition-with-context ctx))
         (glastc pophint--last-condition))
    (when (and (not (pophint--condition-p lastc))
               (pophint--condition-p glastc))
      ;; 最初の実行時は、symbolをデフォルトにする
      (setf (pophint--condition-source glastc)
            (pophint--compile-source 'pophint:source-symbol))
      (pophint--set-last-condition glastc :context ctx))))


;;;###autoload
(defmacro pophint-config:def-isearch-command (command)
  (declare (indent 0))
  (let ((fnc-sym (intern (format "pophint-config:%s" (symbol-name command))))
        (fnc-doc (format "Start `%s' after move to selected hint-tip point." (symbol-name command))))
    `(progn
       (defun ,fnc-sym ()
         ,fnc-doc
         (interactive)
         (pophint:do :not-highlight t
                     :not-switch-window t
                     :use-pos-tip nil
                     :direction 'around
                     :source '((shown . "Region")
                               (action . (lambda (hint)
                                           (goto-char (pophint:hint-startpt hint))
                                           (call-interactively ',command)))))))))

;;;###autoload
(pophint-config:def-isearch-command isearch-forward)
;;;###autoload
(pophint-config:def-isearch-command isearch-backward)


(provide 'pophint-config--isearch)
;;; pophint-config--isearch.el ends here
