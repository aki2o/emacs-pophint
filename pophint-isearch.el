(require 'pophint)
(require 'pophint-region)

(defcustom pophint-isearch:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defcustom pophint-isearch:start-on-isearch-exit-p t
  "Whether to start pophint to move to hit text on `isearch-exit'."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-automatically-when-isearch 'pophint-isearch:start-on-isearch-exit-p "1.1.0")

(defvar pophint-isearch--overlay-index 0)
(defvar pophint-isearch--action-result nil)

;;;###autoload
(defun pophint-isearch:yank-region ()
  "Pull rest of region by selecting hint-tip from buffer into search string."
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (pophint-region:narrow-or-wide
      :narrow-limit (point-at-eol)
      :use-pos-tip nil
      :action (lambda (hint)
                (goto-char (pophint:hint-startpt hint))
                (point))))))
(define-obsolete-function-alias 'pophint-config:isearch-yank-region 'pophint-isearch:yank-region "1.1.0")

;;;###autoload
(defmacro pophint-isearch:replace-to-yank-region (command)
  "Set advice to replace COMMAND with `pophint-isearch:yank-region'."
  (declare (indent 0))
  `(defadvice ,command (around do-pophint activate)
     (pophint--trace "start do as substitute for %s" (symbol-name ',command))
     (pophint-isearch:yank-region)))
(define-obsolete-function-alias 'pophint-config:set-isearch-yank-region-command 'pophint-isearch:replace-to-yank-region "1.1.0")


;;;###autoload
(defun pophint:do-flexibly-isearch () (interactive))
(with-no-warnings
  (pophint:defaction :key "i"
                     :name "ISearch"
                     :description "Do `isearch' from the text of selected hint-tip."
                     :action (lambda (hint)
                               (let ((pophint-isearch--action-result hint))
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
(defmacro pophint-isearch:defcommand (command)
  (declare (indent 0))
  (let ((fnc-sym (intern (format "pophint-isearch:%s" (symbol-name command))))
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
(define-obsolete-function-alias 'pophint-config:def-isearch-command 'pophint-isearch:defcommand "1.1.0")

;;;###autoload
(defun pophint-isearch:isearch-forward () (interactive))
(with-no-warnings
  (pophint-isearch:defcommand isearch-forward))
(define-obsolete-function-alias 'pophint-config:isearch-forward 'pophint-isearch:isearch-forward "1.1.0")

;;;###autoload
(defun pophint-isearch:isearch-backward () (interactive))
(with-no-warnings
  (pophint-isearch:defcommand isearch-backward))
(define-obsolete-function-alias 'pophint-config:isearch-backward 'pophint-isearch:isearch-backward "1.1.0")


(defun pophint-isearch:setup ()
  (let ((hint pophint-isearch--action-result))
    (when (pophint:hint-p hint)
      (setq isearch-string (pophint:hint-value hint))
      (setq isearch-message (pophint:hint-value hint)))))
(define-obsolete-function-alias 'pophint-config:isearch-setup 'pophint-isearch:setup "1.1.0")


(defadvice isearch-exit (before do-pophint disable)
  (when pophint-isearch:start-on-isearch-exit-p
    (pophint:do :not-highlight t
                :not-switch-window t
                :source '((shown . "Cand")
                          (init . (lambda ()
                                    (setq pophint-isearch--overlay-index 0)))
                          (method . (lambda ()
                                      (pophint--trace "overlay count:[%s] index:[%s]"
                                                      (length isearch-lazy-highlight-overlays)
                                                      pophint-isearch--overlay-index)
                                      (let* ((idx pophint-isearch--overlay-index)
                                             (ov (when (< idx (length isearch-lazy-highlight-overlays))
                                                   (nth idx isearch-lazy-highlight-overlays)))
                                             (startpt (when ov (overlay-start ov)))
                                             (endpt (when ov (overlay-end ov)))
                                             (value (when ov (buffer-substring-no-properties startpt endpt)))
                                             (ret `(:startpt ,startpt :endpt ,endpt :value ,value)))
                                        (when ov (incf pophint-isearch--overlay-index))
                                        (when startpt (goto-char startpt))
                                        ret)))
                          (action . (lambda (hint)
                                      (goto-char (pophint:hint-startpt hint))))))))

(defadvice anything-c-moccur-from-isearch (around pophint:disable disable)
  (let ((exitconf pophint-isearch:start-on-isearch-exit-p))
    (setq pophint-isearch:start-on-isearch-exit-p nil)
    ad-do-it
    (setq pophint-isearch:start-on-isearch-exit-p exitconf)))

(defadvice helm-c-moccur-from-isearch (around pophint:disable disable)
  (let ((exitconf pophint-isearch:start-on-isearch-exit-p))
    (setq pophint-isearch:start-on-isearch-exit-p nil)
    ad-do-it
    (setq pophint-isearch:start-on-isearch-exit-p exitconf)))


;;;###autoload
(defun pophint-isearch:provision (activate)
  (interactive)
  (if activate
      (progn
        (add-hook 'isearch-mode-hook 'pophint-isearch:setup t)
        (ad-enable-advice 'isearch-exit 'before 'do-pophint)
        (ad-enable-advice 'anything-c-moccur-from-isearch 'around 'pophint:disable)
        (ad-enable-advice 'helm-c-moccur-from-isearch 'around 'pophint:disable))
    (remove-hook 'isearch-mode-hook 'pophint-isearch:setup)
    (ad-disable-advice 'isearch-exit 'before 'do-pophint)
    (ad-disable-advice 'anything-c-moccur-from-isearch 'around 'pophint:disable)
    (ad-disable-advice 'helm-c-moccur-from-isearch 'around 'pophint:disable))
  (ad-activate 'isearch-exit)
  (ad-activate 'anything-c-moccur-from-isearch)
  (ad-activate 'helm-c-moccur-from-isearch))

;;;###autoload
(with-eval-after-load 'pophint
  (when pophint-isearch:enable (pophint-isearch:provision t)))


(provide 'pophint-isearch)
;;; pophint-isearch.el ends here
