(ignore-errors (require 'w3m-search nil t))
(require 'pophint)
(require 'pophint-config--yank)


(defvar pophint-config:w3m-use-new-tab t)

;;;###autoload
(defun pophint-config:set-w3m-use-new-tab (activate)
  "Whether open new tab of w3m when action by w3m function."
  (setq pophint-config:w3m-use-new-tab activate))


;;;###autoload
(defun pophint:do-flexibly-search () (interactive))
(with-no-warnings
  (pophint:defaction :key "s"
                     :name "Search"
                     :description "Do `w3m-search' about the text of selected hint-tip."
                     :action (lambda (hint)
                               (if (not (functionp 'w3m-search-do-search))
                                   (message "Not exist function 'w3m-search-do-search'.")
                                 (let* ((engine (or w3m-search-default-engine
                                                    (completing-read "Select search engine: "
                                                                     w3m-search-engine-alist
                                                                     nil
                                                                     t
                                                                     nil
                                                                     'w3m-search-engine-history)))
                                        (str (read-string "Input search words: "
                                                          (pophint:hint-value hint)))
                                        (func (if pophint-config:w3m-use-new-tab
                                                  'w3m-goto-url-new-session
                                                'w3m-goto-url)))
                                   (w3m-search-do-search func engine str))))))


;;;###autoload
(defun pophint:do-w3m-anchor () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "w3m-anchor"
    :description "Anchor on w3m."
    :source '((shown . "Link")
              (dedicated . (e2wm))
              (activebufferp . (lambda (b)
                                 (pophint--maybe-kind-mode-buffer-p b 'w3m-mode)))
              (method . (lambda ()
                          (when (w3m-goto-next-anchor)
                            (let* ((a (w3m-anchor (point)))
                                   (title (w3m-anchor-title (point)))
                                   (seq (w3m-anchor-sequence (point))))
                              (pophint--trace "found anchor. a:[%s] title:[%s] seq:[%s]" a title seq)
                              `(:startpt ,(point) :endpt ,(+ (point) (length title)) :value ,a)))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (if pophint-config:w3m-use-new-tab
                                (w3m-view-this-url-new-session)
                              (w3m-view-this-url))))))))


(defun pophint-config:do-w3m-anchor-sentinel (method)
  (let ((pophint-config:w3m-use-new-tab (case method
                                          ('open   nil)
                                          ('tab    t)
                                          ('invert (not pophint-config:w3m-use-new-tab)))))
    (pophint:do-w3m-anchor)))

;;;###autoload
(defun pophint-config:w3m-anchor-open ()
  "Do `pophint:do-w3m-anchor' in current tab."
  (interactive)
  (pophint-config:do-w3m-anchor-sentinel 'open))

;;;###autoload
(defun pophint-config:w3m-anchor-open-new-tab ()
  "Do `pophint:do-w3m-anchor' in new tab."
  (interactive)
  (pophint-config:do-w3m-anchor-sentinel 'tab))

;;;###autoload
(defun pophint-config:w3m-anchor-open-invert ()
  "Do `pophint:do-w3m-anchor' inverting `pophint-config:w3m-use-new-tab'."
  (interactive)
  (pophint-config:do-w3m-anchor-sentinel 'invert))

;;;###autoload
(defun pophint-config:w3m-anchor-open-new-tab-continuously ()
  "Do `pophint:do-w3m-anchor' in new tab continuously."
  (interactive)
  (let ((buff (current-buffer))
        (pt (point)))
    (pophint-config:do-w3m-anchor-sentinel 'tab)
    (switch-to-buffer buff)
    (goto-char pt)
    (pophint-config:w3m-anchor-open-new-tab-continuously)))

;;;###autoload
(defun pophint-config:w3m-anchor-yank ()
  "Yank using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
              :action-name "Yank"
              :action pophint-config:yank-action))

;;;###autoload
(defun pophint-config:w3m-anchor-view-source ()
  "View source using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
              :action-name "ViewSource"
              :action (lambda (hint)
                        (let* ((sbuff (current-buffer))
                               (w3m-current-url (save-excursion
                                                  (goto-char (pophint:hint-startpt hint))
                                                  (or (w3m-url-valid (w3m-anchor))
                                                      (w3m-active-region-or-url-at-point t))))
                               (html (when w3m-current-url
                                       (w3m-copy-buffer)
                                       (w3m-view-source)
                                       (buffer-string)))
                               (mode (assoc-default "hoge.html" auto-mode-alist 'string-match))
                               (buff (generate-new-buffer "*w3m view source*")))
                          (w3m-delete-buffer)
                          (with-current-buffer buff
                            (insert html)
                            (goto-char (point-min))
                            (set-buffer-modified-p nil)
                            (when (functionp mode)
                              (funcall mode)))
                          (switch-to-buffer sbuff)
                          (display-buffer buff)))))

;;;###autoload
(defun pophint-config:w3m-anchor-focus ()
  "Focus using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
              :action-name "Focus"
              :action (lambda (hint)
                        (goto-char (pophint:hint-startpt hint)))))

(defun pophint-config:w3m-set-keys ()
  (local-set-key (kbd "f")       'pophint:do-w3m-anchor)
  (local-set-key (kbd "F")       'pophint-config:w3m-anchor-open-invert)
  (local-set-key (kbd "C-c C-e") 'pophint-config:w3m-anchor-open-new-tab-continuously)
  (local-set-key (kbd "; o")     'pophint-config:w3m-anchor-open)
  (local-set-key (kbd "; t")     'pophint-config:w3m-anchor-open-new-tab)
  (local-set-key (kbd "; F")     'pophint-config:w3m-anchor-open-new-tab-continuously)
  (local-set-key (kbd "; y")     'pophint-config:w3m-anchor-yank)
  (local-set-key (kbd "; v")     'pophint-config:w3m-anchor-view-source)
  (local-set-key (kbd "; RET")   'pophint-config:w3m-anchor-focus))

;;;###autoload
(defun pophint-config:w3m-setup ()
  (add-to-list 'pophint:sources 'pophint:source-w3m-anchor)
  (pophint-config:w3m-set-keys))


;;;###autoload
(add-hook 'w3m-mode-hook 'pophint-config:w3m-setup t)


(provide 'pophint-config--w3m)
;;; pophint-config--w3m.el ends here
