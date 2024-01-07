(require 'e2wm)
(require 'e2wm-perspb nil t)
(require 'e2wm-sww nil t)
(require 'e2wm-term nil t)
(require 'pophint)
(require 'pophint-ow)
(require 'pophint-widget)

;;;###autoload
(defcustom pophint-e2wm:enable t
  "Whether to enable feature."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-automatically-when-e2wm-array 'pophint-e2wm:enable "1.1.0")

(defcustom pophint-e2wm:array-quit-immediately t
  "Whether to do `e2wm:dp-array-goto-prev-pst-command' immediately
in array perspective."
  :type 'boolean
  :group 'pophint)
(make-obsolete 'pophint-config:set-goto-immediately-when-e2wm-array 'pophint-e2wm:array-quit-immediately "1.1.0")

;;;###autoload
(defun pophint:do-situationally-e2wm () (interactive))
(with-no-warnings
  (pophint:defsituation e2wm))

;;;###autoload
(defun pophint:do-e2wm-files () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-files"
    :description "Node in files plugin of e2wm."
    :source '((dedicated . e2wm)
              (regexp . "^\\([^ ]+\\)")
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (pophint--maybe-kind-mode-buffer-p b 'e2wm:def-plugin-files-mode))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm:def-plugin-files-select-command))))))

;;;###autoload
(defun pophint:do-e2wm-history () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-history"
    :description "Entry in history list plugin of e2wm."
    :source '((dedicated . e2wm)
              (regexp . "^ +[0-9]+ +\\([^ ]+\\)")
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (pophint--maybe-kind-mode-buffer-p b 'e2wm:def-plugin-history-list-mode))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm:def-plugin-history-list-select-command))))))

;;;###autoload
(defun pophint:do-e2wm-history2 () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-history2"
    :description "Entry in history list2 plugin of e2wm."
    :source '((dedicated . e2wm)
              (regexp . "^\\(?:<-\\)?\\(?:->\\)? +[0-9]+ +\\([^ ]+\\)")
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (pophint--maybe-kind-mode-buffer-p b 'e2wm:def-plugin-history-list2-mode))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm:def-plugin-history-list2-select-command)
                          (e2wm:pst-window-select-main))))))

;;;###autoload
(defun pophint:do-e2wm-imenu () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-imenu"
    :description "Entry in imenu plugin of e2wm."
    :source '((dedicated . e2wm)
              (regexp . "^\\(.+\\) *$")
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (pophint--maybe-kind-mode-buffer-p b 'e2wm:def-plugin-imenu-mode))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm:def-plugin-imenu-jump-command))))))

;;;###autoload
(defun pophint:do-e2wm-perspb () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-perspb"
    :description "Entry in perspb plugin of e2wm."
    :source '((dedicated . e2wm)
              (regexp . "^..\\(.+\\) *$")
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (pophint--maybe-kind-mode-buffer-p b 'e2wm-perspb:mode))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm-perspb:select-command))))))

;;;###autoload
(defun pophint:do-e2wm-sww () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-sww"
    :description "Entry in sww plugin of e2wm."
    :source `((init . (lambda ()
                        (goto-char (point-min))))
              (dedicated . e2wm)
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (widget-apply (widget-at) :action)
                          (e2wm:pst-window-select-main)))
              ,@pophint:source-widget)))

;;;###autoload
(defun pophint:do-e2wm-term-history () (interactive))
(with-no-warnings
  (pophint:defsource
    :name "e2wm-term-history"
    :description ""
    :source '((dedicated . e2wm)
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (pophint--maybe-kind-mode-buffer-p b 'e2wm-term:history-mode))))
              (method . (lambda ()
                          (let* ((startpt (e2wm-term::history-currpt))
                                 (endpt (progn (e2wm-term:history-move-next t t) (point)))
                                 (value (buffer-substring-no-properties startpt endpt)))
                            `(:startpt ,startpt :endpt ,endpt :value ,value))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm-term:history-highlight)
                          (e2wm-term:history-sync)
                          (e2wm-term:history-send-pt-point))))))

;;;###autoload
(defun pophint-e2wm:array-other-window ()
  "Do `pophint:do-each-window' in array perspective of `e2wm.el'."
  (interactive)
  (if (<= (length (window-list)) 3)
      (e2wm:dp-array-move-right-command)
    (let ((pophint:use-pos-tip t))
      (if (and (pophint:do-each-window)
               pophint-e2wm:array-quit-immediately)
          (e2wm:dp-array-goto-prev-pst-command)
        (e2wm:dp-array-update-summary)))))
(define-obsolete-function-alias 'pophint-config:e2wm-array-other-window 'pophint-e2wm:array-other-window "1.1.0")


(defadvice e2wm:dp-array (after do-pophint disable)
  (when (interactive-p)
    (pophint-e2wm:array-other-window)))


(defun pophint-e2wm:setup-array-key (activate)
  (eval-after-load "e2wm"
    `(progn
       (let ((key (ignore-errors
                    (key-description (nth 0 (where-is-internal 'other-window global-map))))))
         (when (and key
                    (keymapp e2wm:dp-array-minor-mode-map))
           (define-key
             e2wm:dp-array-minor-mode-map
             (read-kbd-macro key)
             (if ,activate 'pophint-e2wm:array-other-window 'other-window)))))))

;;;###autoload
(defun pophint-e2wm:provision (activate)
  (interactive)
  (if activate
      (ad-enable-advice 'e2wm:dp-array 'after 'do-pophint)
    (ad-disable-advice 'e2wm:dp-array 'after 'do-pophint))
  (ad-activate 'e2wm:dp-array)
  (pophint-e2wm:setup-array-key activate))

;;;###autoload
(with-eval-after-load 'e2wm
  (when pophint-e2wm:enable (pophint-e2wm:provision t)))


(provide 'pophint-e2wm)
;;; pophint-e2wm.el ends here
