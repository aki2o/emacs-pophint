(require 'eww)
(require 'pophint)
(require 'pophint-yank)

;;;###autoload
(defcustom pophint-eww:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)

(defcustom pophint-eww:use-new-tab t
  "Whether open new tab of eww when action by eww function."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-eww-use-new-tab 'pophint-eww:use-new-tab "1.1.0")

(defvar pophint-eww--eww-buffer-name "*eww*")

(defun pophint-eww--do-anchor-sentinel (method)
  (let ((pophint-eww:use-new-tab (cl-case method
                                   (open   nil)
                                   (tab    t)
                                   (invert (not pophint-eww:use-new-tab)))))
    (pophint:do-eww-anchor)))

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
                                         (not pophint-eww:use-new-tab)
                                         (not (get-buffer pophint-eww--eww-buffer-name)))
                                ;; If open link in same tab,
                                ;; back name to original buffer name before the open action
                                ;; because `eww-setup-buffer' gets target buffer by
                                ;; `get-buffer-create' to `pophint-eww--eww-buffer-name'.
                                (rename-buffer pophint-eww--eww-buffer-name))
                              (call-interactively cmd))))))))

;;;###autoload
(defun pophint-eww:anchor-open ()
  "Do `pophint:do-eww-anchor' in current tab."
  (interactive)
  (pophint-eww--do-anchor-sentinel 'open))
(define-obsolete-function-alias 'pophint-config:eww-anchor-open 'pophint-eww:anchor-open "1.1.0")

;;;###autoload
(defun pophint-eww:anchor-open-new-tab ()
  "Do `pophint:do-eww-anchor' in new tab."
  (interactive)
  (pophint-eww--do-anchor-sentinel 'tab))
(define-obsolete-function-alias 'pophint-config:eww-anchor-open-new-tab 'pophint-eww:anchor-open-new-tab "1.1.0")

;;;###autoload
(defun pophint-eww:anchor-open-invert ()
  "Do `pophint:do-eww-anchor' inverting `pophint-eww:use-new-tab'."
  (interactive)
  (pophint-eww--do-anchor-sentinel 'invert))
(define-obsolete-function-alias 'pophint-config:eww-anchor-open-invert 'pophint-eww:anchor-open-invert "1.1.0")

;;;###autoload
(defun pophint-eww:anchor-open-new-tab-continuously ()
  "Do `pophint:do-eww-anchor' in new tab continuously."
  (interactive)
  (let ((buff (current-buffer))
        (pt (point)))
    (pophint-eww--do-anchor-sentinel 'tab)
    (switch-to-buffer buff)
    (goto-char pt)
    (pophint-eww:anchor-open-new-tab-continuously)))
(define-obsolete-function-alias 'pophint-config:eww-anchor-open-new-tab-continuously 'pophint-eww:anchor-open-new-tab-continuously "1.1.0")

;;;###autoload
(defun pophint-eww:anchor-open-with-external ()
  "Do `pophint:source-eww-anchor' with external browser."
  (interactive)
  (pophint:do :source pophint:source-eww-anchor
              :action-name "External Browser"
              :action (lambda (hint)
                        (eww-browse-with-external-browser (pophint:hint-value hint)))))
(define-obsolete-function-alias 'pophint-config:eww-anchor-open-with-external 'pophint-eww:anchor-open-with-external "1.1.0")

;;;###autoload
(defun pophint-eww:anchor-yank ()
  "Yank using `pophint:source-eww-anchor'."
  (interactive)
  (pophint:do :source pophint:source-eww-anchor
              :action-name "Yank"
              :action pophint-yank--yank-action))
(define-obsolete-function-alias 'pophint-config:eww-anchor-yank 'pophint-eww:anchor-yank "1.1.0")

;;;###autoload
(defun pophint-eww:anchor-focus ()
  "Focus using `pophint:source-eww-anchor'."
  (interactive)
  (pophint:do :source pophint:source-eww-anchor
              :action-name "Focus"
              :action (lambda (hint)
                        (goto-char (pophint:hint-startpt hint)))))
(define-obsolete-function-alias 'pophint-config:eww-anchor-focus 'pophint-eww:anchor-focus "1.1.0")

(defun pophint-eww:set-keys ()
  (local-set-key (kbd "f")       'pophint:do-eww-anchor)
  (local-set-key (kbd "F")       'pophint-eww:anchor-open-invert)
  (local-set-key (kbd "C-c C-e") 'pophint-eww:anchor-open-new-tab-continuously)
  (local-set-key (kbd "; o")     'pophint-eww:anchor-open)
  (local-set-key (kbd "; t")     'pophint-eww:anchor-open-new-tab)
  (local-set-key (kbd "; F")     'pophint-eww:anchor-open-new-tab-continuously)
  (local-set-key (kbd "; e")     'pophint-eww:anchor-open-with-external)
  (local-set-key (kbd "; y")     'pophint-eww:anchor-yank)
  (local-set-key (kbd "; v")     'eww-view-source)
  (local-set-key (kbd "; RET")   'pophint-eww:anchor-focus))

(defun pophint-eww:setup ()
  (add-to-list 'pophint:sources 'pophint:source-eww-anchor)
  (pophint-eww:set-keys))
(define-obsolete-function-alias 'pophint-config:eww-setup 'pophint-eww:setup "1.1.0")


(defadvice eww-setup-buffer (after pophint:handle-tabs disable)
  ;; Always use not original buffer name for handling multiple tab
  (when (string= (buffer-name) pophint-eww--eww-buffer-name)
    (rename-uniquely)))


;;;###autoload
(defun pophint-eww:provision (activate)
  (interactive)
  (if activate
      (ad-enable-advice 'eww-setup-buffer 'after 'pophint:handle-tabs)
    (ad-disable-advice 'eww-setup-buffer 'after 'pophint:handle-tabs))
  (ad-activate 'eww-setup-buffer)
  (if activate
      (add-hook 'eww-mode-hook 'pophint-eww:setup t)
    (remove-hook 'eww-mode-hook 'pophint-eww:setup)))

;;;###autoload
(with-eval-after-load 'eww
  (when pophint-eww:enable (pophint-eww:provision t)))


(provide 'pophint-eww)
;;; pophint-eww.el ends here
