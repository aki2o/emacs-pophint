(ignore-errors (require 'w3m-search nil t))
(require 'pophint)
(require 'pophint-yank)

;;;###autoload
(defcustom pophint-w3m:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defcustom pophint-w3m:use-new-tab t
  "Whether open new tab of w3m when action by w3m function."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-w3m-use-new-tab 'pophint-w3m:use-new-tab "1.1.0")

(defun pophint-w3m--do-anchor-sentinel (method)
  (let ((pophint-w3m:use-new-tab (cl-case method
                                   ('open   nil)
                                   ('tab    t)
                                   ('invert (not pophint-w3m:use-new-tab)))))
    (pophint:do-w3m-anchor)))

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
                                        (func (if pophint-w3m:use-new-tab
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
                            (if pophint-w3m:use-new-tab
                                (w3m-view-this-url-new-session)
                              (w3m-view-this-url))))))))

;;;###autoload
(defun pophint-w3m:anchor-open ()
  "Do `pophint:do-w3m-anchor' in current tab."
  (interactive)
  (pophint-w3m--do-anchor-sentinel 'open))
(define-obsolete-function-alias 'pophint-config:w3m-anchor-open 'pophint-w3m:anchor-open "1.1.0")

;;;###autoload
(defun pophint-w3m:anchor-open-new-tab ()
  "Do `pophint:do-w3m-anchor' in new tab."
  (interactive)
  (pophint-w3m--do-anchor-sentinel 'tab))
(define-obsolete-function-alias 'pophint-config:w3m-anchor-open-new-tab 'pophint-w3m:anchor-open-new-tab "1.1.0")

;;;###autoload
(defun pophint-w3m:anchor-open-invert ()
  "Do `pophint:do-w3m-anchor' inverting `pophint-w3m:use-new-tab'."
  (interactive)
  (pophint-w3m--do-anchor-sentinel 'invert))
(define-obsolete-function-alias 'pophint-config:w3m-anchor-open-invert 'pophint-w3m:anchor-open-invert "1.1.0")

;;;###autoload
(defun pophint-w3m:anchor-open-new-tab-continuously ()
  "Do `pophint:do-w3m-anchor' in new tab continuously."
  (interactive)
  (let ((buff (current-buffer))
        (pt (point)))
    (pophint-w3m--do-anchor-sentinel 'tab)
    (switch-to-buffer buff)
    (goto-char pt)
    (pophint-w3m:anchor-open-new-tab-continuously)))
(define-obsolete-function-alias 'pophint-config:w3m-anchor-open-new-tab-continuously 'pophint-w3m:anchor-open-new-tab-continuously "1.1.0")

;;;###autoload
(defun pophint-w3m:anchor-yank ()
  "Yank using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
              :action-name "Yank"
              :action pophint-yank--yank-action))
(define-obsolete-function-alias 'pophint-config:w3m-anchor-yank 'pophint-w3m:anchor-yank "1.1.0")

;;;###autoload
(defun pophint-w3m:anchor-view-source ()
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
(define-obsolete-function-alias 'pophint-config:w3m-anchor-view-source 'pophint-w3m:anchor-view-source "1.1.0")

;;;###autoload
(defun pophint-w3m:anchor-focus ()
  "Focus using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
              :action-name "Focus"
              :action (lambda (hint)
                        (goto-char (pophint:hint-startpt hint)))))
(define-obsolete-function-alias 'pophint-config:w3m-anchor-focus 'pophint-w3m:anchor-focus "1.1.0")

(defun pophint-w3m:set-keys ()
  (local-set-key (kbd "f")       'pophint:do-w3m-anchor)
  (local-set-key (kbd "F")       'pophint-w3m:anchor-open-invert)
  (local-set-key (kbd "C-c C-e") 'pophint-w3m:anchor-open-new-tab-continuously)
  (local-set-key (kbd "; o")     'pophint-w3m:anchor-open)
  (local-set-key (kbd "; t")     'pophint-w3m:anchor-open-new-tab)
  (local-set-key (kbd "; F")     'pophint-w3m:anchor-open-new-tab-continuously)
  (local-set-key (kbd "; y")     'pophint-w3m:anchor-yank)
  (local-set-key (kbd "; v")     'pophint-w3m:anchor-view-source)
  (local-set-key (kbd "; RET")   'pophint-w3m:anchor-focus))

(defun pophint-w3m:setup ()
  (add-to-list 'pophint:sources 'pophint:source-w3m-anchor)
  (pophint-w3m:set-keys))
(define-obsolete-function-alias 'pophint-config:w3m-setup 'pophint-w3m:setup "1.1.0")

;;;###autoload
(defun pophint-w3m:provision (activate)
  (interactive)
  (if activate
      (add-hook 'w3m-mode-hook 'pophint-w3m:setup t)
    (remove-hook 'w3m-mode-hook 'pophint-w3m:setup)))

;;;###autoload
(with-eval-after-load 'w3m
  (when pophint-w3m:enable (pophint-w3m:provision t)))


(provide 'pophint-w3m)
;;; pophint-w3m.el ends here
