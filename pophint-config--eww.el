(require 'eww)
(require 'pophint)
(require 'pophint-config--yank)


(defvar pophint-config:eww-use-new-tab t)
(defvar pophint-config:eww-buffer-name "*eww*")

;;;###autoload
(defun pophint-config:set-eww-use-new-tab (activate)
  "Whether open new tab of eww when action by eww function."
  (setq pophint-config:eww-use-new-tab activate))


;;;###autoload
(defun pophint:do-eww-anchor () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "eww-anchor"
    :description "Anchor on eww."
    :source '((shown . "Link")
              (dedicated . (e2wm))
              (requires . 0)
              (activebufferp . (lambda (b)
                                 (pophint--maybe-kind-mode-buffer-p b 'eww-mode)))
              (method . (lambda ()
                          ;; Getting startpt by referring `shr-next-link' for silent execute
                          (let* ((startpt (pophint--awhen (text-property-any (point) (point-max) 'help-echo nil)
                                            (text-property-not-all it (point-max) 'help-echo nil)))
                                 (endpt (or (when startpt (next-single-property-change startpt 'face))
                                            (point-max)))
                                 (url (when startpt (get-text-property startpt 'shr-url))))
                            (when startpt
                              (goto-char startpt)
                              (pophint--trace "(eww)found anchor. url:[%s]" url)
                              `(:startpt ,startpt :endpt ,endpt :value ,url)))))
              (action . (lambda (hint)
                          (with-selected-window (pophint:hint-window hint)
                            (goto-char (pophint:hint-startpt hint))
                            (let ((cmd (key-binding "\r")))
                              (when (and (eq cmd 'eww-follow-link)
                                         (not pophint-config:eww-use-new-tab)
                                         (not (get-buffer pophint-config:eww-buffer-name)))
                                ;; If open link in same tab,
                                ;; back name to original buffer name before the open action
                                ;; because `eww-setup-buffer' gets target buffer by
                                ;; `get-buffer-create' to `pophint-config:eww-buffer-name'.
                                (rename-buffer pophint-config:eww-buffer-name))
                              (call-interactively cmd))))))))


(defun pophint-config:do-eww-anchor-sentinel (method)
  (let ((pophint-config:eww-use-new-tab (case method
                                          ('open   nil)
                                          ('tab    t)
                                          ('invert (not pophint-config:eww-use-new-tab)))))
    (pophint:do-eww-anchor)))

;;;###autoload
(defun pophint-config:eww-anchor-open ()
  "Do `pophint:do-eww-anchor' in current tab."
  (interactive)
  (pophint-config:do-eww-anchor-sentinel 'open))

;;;###autoload
(defun pophint-config:eww-anchor-open-new-tab ()
  "Do `pophint:do-eww-anchor' in new tab."
  (interactive)
  (pophint-config:do-eww-anchor-sentinel 'tab))

;;;###autoload
(defun pophint-config:eww-anchor-open-invert ()
  "Do `pophint:do-eww-anchor' inverting `pophint-config:eww-use-new-tab'."
  (interactive)
  (pophint-config:do-eww-anchor-sentinel 'invert))

;;;###autoload
(defun pophint-config:eww-anchor-open-new-tab-continuously ()
  "Do `pophint:do-eww-anchor' in new tab continuously."
  (interactive)
  (let ((buff (current-buffer))
        (pt (point)))
    (pophint-config:do-eww-anchor-sentinel 'tab)
    (switch-to-buffer buff)
    (goto-char pt)
    (pophint-config:eww-anchor-open-new-tab-continuously)))

;;;###autoload
(defun pophint-config:eww-anchor-open-with-external ()
  "Do `pophint:source-eww-anchor' with external browser."
  (interactive)
  (pophint:do :source pophint:source-eww-anchor
              :action-name "External Browser"
              :action (lambda (hint)
                        (eww-browse-with-external-browser (pophint:hint-value hint)))))

;;;###autoload
(defun pophint-config:eww-anchor-yank ()
  "Yank using `pophint:source-eww-anchor'."
  (interactive)
  (pophint:do :source pophint:source-eww-anchor
              :action-name "Yank"
              :action pophint-config:yank-action))

;;;###autoload
(defun pophint-config:eww-anchor-focus ()
  "Focus using `pophint:source-eww-anchor'."
  (interactive)
  (pophint:do :source pophint:source-eww-anchor
              :action-name "Focus"
              :action (lambda (hint)
                        (goto-char (pophint:hint-startpt hint)))))

(defun pophint-config:eww-set-keys ()
  (local-set-key (kbd "f")       'pophint:do-eww-anchor)
  (local-set-key (kbd "F")       'pophint-config:eww-anchor-open-invert)
  (local-set-key (kbd "C-c C-e") 'pophint-config:eww-anchor-open-new-tab-continuously)
  (local-set-key (kbd "; o")     'pophint-config:eww-anchor-open)
  (local-set-key (kbd "; t")     'pophint-config:eww-anchor-open-new-tab)
  (local-set-key (kbd "; F")     'pophint-config:eww-anchor-open-new-tab-continuously)
  (local-set-key (kbd "; e")     'pophint-config:eww-anchor-open-with-external)
  (local-set-key (kbd "; y")     'pophint-config:eww-anchor-yank)
  (local-set-key (kbd "; v")     'eww-view-source)
  (local-set-key (kbd "; RET")   'pophint-config:eww-anchor-focus))

;;;###autoload
(defun pophint-config:eww-setup ()
  (add-to-list 'pophint:sources 'pophint:source-eww-anchor)
  (pophint-config:eww-set-keys))


;;;###autoload
(add-hook 'eww-mode-hook 'pophint-config:eww-setup t)


;;;###autoload
(eval-after-load "eww"
  `(progn
     (defadvice eww-setup-buffer (after pophint-config:handle-tabs activate)
       ;; Always use not original buffer name for handling multiple tab
       (when (string= (buffer-name) pophint-config:eww-buffer-name)
         (rename-uniquely)))))


(provide 'pophint-config--eww)
;;; pophint-config--eww.el ends here
