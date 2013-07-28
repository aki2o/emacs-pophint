;;; pophint-config.el --- provide configuration for pophint.el.

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-pophint
;; Version: 0.3.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides configuration for pophint.el.

;;; Dependency:
;; 
;; - pophint.el ( see <https://github.com/aki2o/emacs-pophint> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'pophint-config)

;;; Configuration:
;; 
;; ;; When set-mark-command, start pop-up hint automatically.
;; (pophint-config:set-automatically-when-marking t)
;; 
;; ;; When you select the shown hint-tip after set-mark-command, do yank immediately.
;; (pophint-config:set-yank-immediately-when-marking t)
;; 
;; ;; When isearch, start pop-up hint automatically after exit isearch.
;; (pophint-config:set-automatically-when-isearch t)
;; 
;; ;; When start searching the end point of RangeYank, layout the window position temporarily.
;; (pophint-config:set-relayout-when-rangeyank-start t)
;; 
;; ;; If you want to start some action immediately, bind key for the action.
;; (define-key global-map (kbd "M-y") 'pophint:do-flexibly-yank)
;; (define-key global-map (kbd "C-M-y") 'pophint:do-rangeyank)

;;; Customization:
;; 
;; Nothing

;;; API:
;; 
;; Nothing
;; 
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'pophint)
(require 'rx)
(require 'regexp-opt)
(require 'ffap nil t)


;;;;;;;;;;;;;;;;;;;;;
;; Add yank action

(defvar pophint-config:yank-action (lambda (hint)
                                     (kill-new (pophint:hint-value hint))))

(pophint:defaction :key "y"
                   :name "Yank"
                   :description "Yank the text of selected hint-tip."
                   :action 'pophint-config:yank-action)

(defvar pophint-config:yank-startpt nil)

(defvar pophint-config:relayout-when-yank-range-start-p nil)

(defvar pophint-config:collect-word-methods
  '((lambda ()
      (when (not (eq (pophint:get-current-direction) 'backward))
        (let* ((currpt (point))
               (startpt (progn (forward-word) (point)))
               (endpt (save-excursion (forward-word) (point)))
               (value (buffer-substring-no-properties startpt endpt)))
          (when (and (< currpt startpt)
                     (< startpt endpt))
            (make-pophint:hint :startpt startpt :endpt endpt :value value)))))
    (lambda ()
      (when (not (eq (pophint:get-current-direction) 'forward))
        (let* ((currpt (point))
               (endpt (progn (backward-word) (point)))
               (startpt (save-excursion (backward-word) (point)))
               (value (buffer-substring-no-properties startpt endpt)))
          (when (and (> currpt endpt)
                     (> endpt startpt))
            (make-pophint:hint :startpt startpt :endpt endpt :value value)))))))

(defvar pophint-config:yank-range-action
  (lambda (hint)
    (let* ((buff (pophint:hint-buffer hint))
           (wnd (get-buffer-window buff)))
      (when (and (buffer-live-p buff)
                 (windowp wnd)
                 (window-live-p wnd))
        (save-window-excursion
          (save-excursion
            (with-selected-window wnd
              (goto-char (pophint:hint-startpt hint))
              (setq pophint-config:yank-startpt (point))
              (when pophint-config:relayout-when-yank-range-start-p
                (switch-to-buffer buff)
                (recenter 0)
                (delete-other-windows))
              (pophint:do :source '((method . pophint-config:collect-word-methods))
                          :direction 'forward
                          :not-highlight t
                          :not-switch-window t
                          :action (lambda (hint)
                                    (when (number-or-marker-p pophint-config:yank-startpt)
                                      (kill-new (buffer-substring-no-properties pophint-config:yank-startpt
                                                                                (pophint:hint-startpt hint)))))))))))))

(defun pophint-config:set-relayout-when-rangeyank-start (activate)
  "Whether re-layouting window or not when start searching the end point of RangeYank."
  (setq pophint-config:relayout-when-yank-range-start-p activate))

(pophint:defaction :key "Y"
                   :name "RangeYank"
                   :description "Yank the text getting end point by do pop-up at the selected point."
                   :action 'pophint-config:yank-range-action)

(pophint:defsource
  :name "RangeYank"
  :description "Yank the text getting end point by do pop-up at the selected point."
  :source '((shown . "RangeYank")
            (regexp . pophint--default-search-regexp)
            (requires . 1)
            (highlight . nil)
            (action . pophint-config:yank-range-action)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Add global source

;; Quoted range
(defvar pophint-config:quote-chars '("\"" "'" "`"))
(defvar pophint-config:exclude-quote-chars nil)
(make-variable-buffer-local 'pophint-config:exclude-quote-chars)
(pophint:defsource
  :name "quoted"
  :description "Quoted range by `pophint-config:quote-chars'.
If exist the character that not be used for quote, set `pophint-config:exclude-quote-chars'.
It's a buffer local variable and list like `pophint-config:quote-chars'."
  :source '((shown . "Quoted")
            (limit . 50)
            (method . ((lambda ()
                         (when (not (eq (pophint:get-current-direction) 'backward))
                           (let* ((chars (loop for c in pophint-config:quote-chars
                                               if (not (member c pophint-config:exclude-quote-chars))
                                               collect c))
                                  (char-re (when chars (regexp-opt chars)))
                                  (re (when char-re (rx-to-string `(and (group (regexp ,char-re)))))))
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
                                   return (make-pophint:hint :startpt startpt :endpt endpt :value value)))))
                       (lambda ()
                         (when (not (eq (pophint:get-current-direction) 'forward))
                           (let* ((chars (loop for c in pophint-config:quote-chars
                                               if (not (member c pophint-config:exclude-quote-chars))
                                               collect c))
                                  (char-re (when chars (regexp-opt chars)))
                                  (re (when char-re (rx-to-string `(and (group (regexp ,char-re)))))))
                             (loop while (and re (re-search-backward re nil t))
                                   for word = (match-string-no-properties 1)
                                   for endpt = (point)
                                   for startpt = (or (when (and (> (point) (point-min))
                                                                (string= (format "%c" (char-before)) word))
                                                       (forward-char -1)
                                                       (+ (point) 2))
                                                     (when (re-search-backward (format "[^\\]%s" word) nil t)
                                                       (+ (point) 2)))
                                   for value = (when (and startpt (< startpt endpt))
                                                 (buffer-substring-no-properties startpt endpt))
                                   if (not startpt) return nil
                                   if (and (stringp value) (not (string= value "")))
                                   return (make-pophint:hint :startpt startpt :endpt endpt :value value)))))))))
(add-to-list 'pophint:global-sources 'pophint:source-quoted t)

;; URL or Filepath
(defvar pophint-config:regexp-url-or-path (rx-to-string `(and (or bow bol) (group (? (+ (any "a-zA-Z")) ":") "/"))))
(pophint:defsource
  :name "url-or-path"
  :description "Format like URL or Filepath."
  :source '((shown . "Url/Path")
            (method . ((lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'backward))
                                    (functionp 'ffap-guesser))
                           (loop while (re-search-forward pophint-config:regexp-url-or-path nil t)
                                 for startpt = (match-beginning 1)
                                 for guess = (ffap-guesser)
                                 if guess
                                 return (progn
                                          (pophint--trace "found url/path. pt:[%s] value:[%s]" startpt guess)
                                          (make-pophint:hint :startpt startpt
                                                             :endpt (+ startpt (length guess))
                                                             :value guess)))))
                       (lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'forward))
                                    (functionp 'ffap-guesser))
                           (loop while (re-search-backward pophint-config:regexp-url-or-path nil t)
                                 for startpt = (match-beginning 1)
                                 for guess = (ffap-guesser)
                                 if guess
                                 return (progn
                                          (pophint--trace "found url/path. pt:[%s] value:[%s]" startpt guess)
                                          (make-pophint:hint :startpt startpt
                                                             :endpt (+ startpt (length guess))
                                                             :value guess)))))))))
(add-to-list 'pophint:global-sources 'pophint:source-url-or-path t)

;; Comment
(pophint:defsource
  :name "comment-line"
  :description "Part of `font-lock-comment-face' in line"
  :source '((shown . "Cmt")
            (limit . 100)
            (method . ((lambda ()
                         (when (not (eq (pophint:get-current-direction) 'backward))
                           (loop while (re-search-forward "\\s<+" nil t)
                                 for startpt = (progn (skip-syntax-forward " ") (point))
                                 for endpt = (when (and (eq (get-text-property (point) 'face) 'font-lock-comment-face)
                                                        (re-search-forward "\\s-*\\(\\s>+\\|$\\)"))
                                               (match-beginning 0))
                                 for value = (when endpt (buffer-substring-no-properties startpt endpt))
                                 if (and (stringp value)
                                         (not (string= value "")))
                                 return (make-pophint:hint :startpt startpt :endpt endpt :value value))))
                       (lambda ()
                         (when (not (eq (pophint:get-current-direction) 'forward))
                           (loop while (re-search-backward "\\s>+" nil t)
                                 for endpt = (progn (skip-syntax-backward " ") (point))
                                 for startpt = (when (and (eq (get-text-property (point) 'face) 'font-lock-comment-face)
                                                          (re-search-backward "\\(\\s<+\\|^\\)\\s-*"))
                                                 (match-end 0))
                                 for value = (when startpt (buffer-substring-no-properties startpt endpt))
                                 if (and (stringp value)
                                         (not (string= value "")))
                                 return (make-pophint:hint :startpt startpt :endpt endpt :value value))))))))
(add-to-list 'pophint:global-sources 'pophint:source-comment-line t)

;; One Line
(defvar pophint-config:regexp-one-line (rx-to-string `(and bol (* (syntax whitespace)) (group (+ not-newline)))))
(pophint:defsource :name "one-line"
                   :description "One line."
                   :source '((shown . "Line")
                             (limit . 100)
                             (regexp . pophint-config:regexp-one-line)))
(add-to-list 'pophint:global-sources 'pophint:source-one-line t)

;; Symbol
(pophint:defsource :name "symbol"
                   :description "Symbol."
                   :source '((shown . "Sym")
                             (regexp . "\\_<.+?\\_>")))
(add-to-list 'pophint:global-sources 'pophint:source-symbol t)


;;;;;;;;;;;;;;
;; For mark

(defvar pophint-config:yank-immediately-when-marking-p nil)
(defun pophint-config:set-yank-immediately-when-marking (activate)
  "Whether yank immediately or not when select hint-tip after set mark."
  (setq pophint-config:yank-immediately-when-marking-p activate))

(defadvice set-mark-command (after do-pophint disable)
  (pophint--trace "start do when set-mark")
  (pophint:do :not-highlight t
              :not-switch-window t
              :source '((method . pophint-config:collect-word-methods)
                        (action . (lambda (hint)
                                    (let* ((currpt (point)))
                                      (goto-char (pophint:hint-startpt hint))
                                      (when pophint-config:yank-immediately-when-marking-p
                                        (kill-ring-save currpt (point)))))))))

(defadvice cua-set-mark (after do-pophint disable)
  (pophint--trace "start do when cua-set-mark")
  (pophint:do :not-highlight t
              :not-switch-window t
              :source '((regexp . "[^a-zA-Z0-9]+")
                        (action . (lambda (hint)
                                    (let* ((currpt (point)))
                                      (goto-char (pophint:hint-startpt hint))
                                      (when pophint-config:yank-immediately-when-marking-p
                                        (kill-ring-save currpt (point)))))))))

(defun pophint-config:set-automatically-when-marking (activate)
  "Whether the pop-up is automatically or not when set mark."
  (cond (activate
         (ad-enable-advice 'set-mark-command 'after 'do-pophint)
         (ad-enable-advice 'cua-set-mark 'after 'do-pophint))
        (t
         (ad-disable-advice 'set-mark-command 'after 'do-pophint)
         (ad-disable-advice 'cua-set-mark 'after 'do-pophint)))
  (ad-activate 'set-mark-command)
  (ad-activate 'cua-set-mark))


;;;;;;;;;;;;;;;;;
;; For isearch

(defvar pophint-config:active-when-isearch-exit-p nil)
(defvar pophint-config:index-of-isearch-overlays 0)
(defadvice isearch-exit (before do-pophint disable)
  (when pophint-config:active-when-isearch-exit-p
    (pophint--trace "start do when isearch-exit")
    (pophint:do :not-highlight t
                :not-switch-window t
                :source '((init . (lambda ()
                                    (setq pophint-config:index-of-isearch-overlays 0)))
                          (method . (lambda ()
                                      (pophint--trace "overlay count:[%s] index:[%s]"
                                                      (length isearch-lazy-highlight-overlays)
                                                      pophint-config:index-of-isearch-overlays)
                                      (let* ((idx pophint-config:index-of-isearch-overlays)
                                             (ov (when (< idx (length isearch-lazy-highlight-overlays))
                                                   (nth idx isearch-lazy-highlight-overlays)))
                                             (pt (when ov (overlay-start ov)))
                                             (ret (when pt
                                                    (make-pophint:hint :startpt (overlay-start ov)
                                                                       :endpt (overlay-end ov)
                                                                       :value (buffer-substring-no-properties
                                                                               (overlay-start ov)
                                                                               (overlay-end ov))))))
                                        (when ov (incf pophint-config:index-of-isearch-overlays))
                                        (when pt (goto-char pt))
                                        ret)))
                          (action . (lambda (hint)
                                      (goto-char (pophint:hint-startpt hint))))))))

(defun pophint-config:set-automatically-when-isearch (activate)
  "Whether the pop-up is automatically or not when exit isearch."
  (if activate
      (ad-enable-advice 'isearch-exit 'before 'do-pophint)
    (ad-disable-advice 'isearch-exit 'before 'do-pophint))
  (ad-activate 'isearch-exit)
  (setq pophint-config:active-when-isearch-exit-p activate))

(when (featurep 'anything-c-moccur)
  (defadvice anything-c-moccur-from-isearch (around disable-pophint activate)
    (let ((exitconf pophint-config:active-when-isearch-exit-p))
      (setq pophint-config:active-when-isearch-exit-p nil)
      ad-do-it
      (setq pophint-config:active-when-isearch-exit-p exitconf)))
  )


;;;;;;;;;;;;;;;
;; For elisp

(defun pophint-config:elisp-setup ()
  (setq pophint-config:exclude-quote-chars '("'" "`")))

(add-hook 'emacs-lisp-mode-hook 'pophint-config:elisp-setup t)


;;;;;;;;;;;;;;
;; For Help

(pophint:defsource
  :name "help-btn"
  :description "Button on help-mode."
  :source '((shown . "Link")
            (limit . 50)
            (method . ((lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'backward))
                                    (forward-button 1))
                           (let* ((btn (button-at (point)))
                                  (startpt (when btn (button-start btn)))
                                  (endpt (when btn (button-end btn)))
                                  (value (when btn (buffer-substring-no-properties startpt endpt))))
                             (pophint--trace "found button. startpt:[%s] endpt:[%s] value:[%s]"
                                             startpt endpt value)
                             (when btn (make-pophint:hint :startpt startpt :endpt endpt :value value)))))
                       (lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'forward))
                                    (backward-button 1))
                           (let* ((btn (button-at (point)))
                                  (startpt (when btn (button-start btn)))
                                  (endpt (when btn (button-end btn)))
                                  (value (when btn (buffer-substring-no-properties startpt endpt))))
                             (pophint--trace "found button. startpt:[%s] endpt:[%s] value:[%s]"
                                             startpt endpt value)
                             (when btn (make-pophint:hint :startpt startpt :endpt endpt :value value)))))))
            (action . (lambda (hint)
                        (goto-char (pophint:hint-startpt hint))
                        (push-button)))))

(defun pophint-config:help-setup ()
  (add-to-list 'pophint:sources 'pophint:source-help-btn))

(add-hook 'help-mode-hook 'pophint-config:help-setup t)


;;;;;;;;;;;;;;
;; For Info

(pophint:defsource
  :name "info-ref"
  :description "Reference on info-mode."
  :source '((shown . "Link")
            (limit . 50)
            (method . ((lambda ()
                         (when (not (eq (pophint:get-current-direction) 'backward))
                           (let* ((currpt (point))
                                  (startpt (progn (Info-next-reference) (point)))
                                  (endpt (next-property-change startpt))
                                  (value (buffer-substring-no-properties startpt endpt)))
                             (when (< currpt startpt)
                               (make-pophint:hint :startpt startpt :endpt endpt :value value)))))
                       (lambda ()
                         (when (not (eq (pophint:get-current-direction) 'forward))
                           (let* ((currpt (point))
                                  (startpt (progn (Info-prev-reference) (point)))
                                  (endpt (next-property-change startpt))
                                  (value (buffer-substring-no-properties startpt endpt)))
                             (when (> currpt startpt)
                               (make-pophint:hint :startpt startpt :endpt endpt :value value)))))))
            (action . (lambda (hint)
                        (goto-char (pophint:hint-startpt hint))
                        (Info-follow-nearest-node)))))

(defun pophint-config:info-setup ()
  (add-to-list 'pophint:sources 'pophint:source-info-ref))

(add-hook 'Info-mode-hook 'pophint-config:info-setup t)


;;;;;;;;;;;;;
;; For w3m

(pophint:defsource
  :name "w3m-anchor"
  :description "Anchor on w3m."
  :source '((shown . "Link")
            (limit . 50)
            (method . ((lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'backward))
                                    (w3m-goto-next-anchor))
                           (let* ((a (w3m-anchor (point)))
                                  (title (w3m-anchor-title (point)))
                                  (seq (w3m-anchor-sequence (point))))
                             (pophint--trace "found anchor. a:[%s] title:[%s] seq:[%s]" a title seq)
                             (make-pophint:hint :startpt (point)
                                                :endpt (+ (point) (length title))
                                                :value a))))
                       (lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'forward))
                                    (w3m-goto-previous-anchor))
                           (let* ((a (w3m-anchor (point)))
                                  (title (w3m-anchor-title (point)))
                                  (seq (w3m-anchor-sequence (point))))
                             (pophint--trace "found anchor. a:[%s] title:[%s] seq:[%s]" a title seq)
                             (make-pophint:hint :startpt (point)
                                                :endpt (+ (point) (length title))
                                                :value a))))))
            (action . (lambda (hint)
                        (goto-char (pophint:hint-startpt hint))
                        (w3m-view-this-url)))))

(defun pophint-config:w3m-setup ()
  (add-to-list 'pophint:sources 'pophint:source-w3m-anchor))

(add-hook 'w3m-mode-hook 'pophint-config:w3m-setup t)


;;;;;;;;;;;;;;;
;; For dired

(pophint:defsource :name "dired-node"
                   :description "Node in directory."
                   :source '((shown . "Node")
                             (regexp . "^ *[d-][r-][w-][x-].+ +\\([^ ]+\\)$")
                             (requires . 1)))

(defun pophint-config:dired-setup ()
  (add-to-list 'pophint:sources 'pophint:source-dired-node))

(add-hook 'dired-mode-hook 'pophint-config:dired-setup t)


(provide 'pophint-config)
;;; pophint-config.el ends here
