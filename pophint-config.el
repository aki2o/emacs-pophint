;;; pophint-config.el --- provide configuration for pophint.el.

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-pophint
;; Version: 0.9.4

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
;; see <https://github.com/aki2o/emacs-pophint/blob/master/README.md>.

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
(require 'w3m-search nil t)
;; (require 'anything-c-moccur nil t)
;; (require 'helm-c-moccur nil t)
(require 'direx nil t)


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
    (let ((wnd (pophint:hint-window hint)))
      (when (and (windowp wnd)
                 (window-live-p wnd))
        (save-window-excursion
          (save-excursion
            (with-selected-window wnd
              (goto-char (pophint:hint-startpt hint))
              (setq pophint-config:yank-startpt (point))
              (when pophint-config:relayout-when-yank-range-start-p
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
  "Whether re-layouting window when start searching the end point of RangeYank."
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
                                                       (+ (point) 1))
                                                     (when (re-search-backward (format "[^\\]%s" word) nil t)
                                                       (+ (point) 2)))
                                   for value = (when (and startpt (< startpt endpt))
                                                 (buffer-substring-no-properties startpt endpt))
                                   if (not startpt) return nil
                                   if (and (stringp value) (not (string= value "")))
                                   return (make-pophint:hint :startpt startpt :endpt endpt :value value)))))))))
(add-to-list 'pophint:global-sources 'pophint:source-quoted t)

;; URL or Filepath
(defvar pophint-config:regexp-url-or-path (rx-to-string `(and (or blank bol "'" "\"") (group (* (any "a-zA-Z.~:"))) "/")))
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
                                 do (goto-char startpt)
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
  "Whether yank immediately when select hint-tip after `set-mark-command' or `cua-set-mark'."
  (setq pophint-config:yank-immediately-when-marking-p activate))

(defadvice set-mark-command (after do-pophint disable)
  (pophint--trace "start do when set-mark")
  (pophint:do :not-highlight t
              :not-switch-window t
              :use-pos-tip nil
              :source '((shown . "Region")
                        (method . pophint-config:collect-word-methods)
                        (action . (lambda (hint)
                                    (let* ((currpt (point)))
                                      (goto-char (pophint:hint-startpt hint))
                                      (when pophint-config:yank-immediately-when-marking-p
                                        (kill-ring-save currpt (point)))))))))

(defadvice cua-set-mark (after do-pophint disable)
  (pophint--trace "start do when cua-set-mark")
  (pophint:do :not-highlight t
              :not-switch-window t
              :use-pos-tip nil
              :source '((shown . "Region")
                        (regexp . "[^a-zA-Z0-9]+")
                        (action . (lambda (hint)
                                    (let* ((currpt (point)))
                                      (goto-char (pophint:hint-startpt hint))
                                      (when pophint-config:yank-immediately-when-marking-p
                                        (kill-ring-save currpt (point)))))))))

(defun pophint-config:set-automatically-when-marking (activate)
  "Whether do pop-up automatically when `set-mark-command' or `cua-set-mark'."
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
  "Whether do pop-up automatically when `isearch-exit'."
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

(when (featurep 'helm-c-moccur)
  (defadvice helm-c-moccur-from-isearch (around disable-pophint activate)
    (let ((exitconf pophint-config:active-when-isearch-exit-p))
      (setq pophint-config:active-when-isearch-exit-p nil)
      ad-do-it
      (setq pophint-config:active-when-isearch-exit-p exitconf)))
  )

(defvar pophint-config:isearch-action-result nil)
(defun pophint-config:isearch-setup ()
  (let ((hint pophint-config:isearch-action-result))
    (when (pophint:hint-p hint)
      (setq isearch-string (pophint:hint-value hint))
      (setq isearch-message (pophint:hint-value hint)))))

(add-hook 'isearch-mode-hook 'pophint-config:isearch-setup t)

(pophint:defaction :key "i"
                   :name "ISearch"
                   :description "Do `isearch' from the text of selected hint-tip."
                   :action (lambda (hint)
                             (let ((currpt (point))
                                   (wnd (pophint:hint-window hint))
                                   (pophint-config:isearch-action-result hint))
                               (when (and (windowp wnd)
                                          (window-live-p wnd))
                                 (cond ((eq (get-buffer-window) wnd)
                                        (goto-char (pophint:hint-startpt hint))
                                        (or (isearch-forward)
                                            (goto-char currpt)))
                                       (t
                                        (let ((retpt (save-window-excursion
                                                       (with-selected-window wnd
                                                         (goto-char (pophint:hint-startpt hint))
                                                         (and (isearch-forward)
                                                              (point))))))
                                          (when retpt
                                            (select-window wnd)
                                            (goto-char retpt)))))))))


;;;;;;;;;;;;;;;;;;;;;;
;; For other window

(defvar pophint-config:active-when-other-window-p nil)
(defvar pophint-config:current-window nil)
(defadvice other-window (around do-pophint disable)
  (if (and (interactive-p)
           pophint-config:active-when-other-window-p
           (> (length (window-list)) 2))
      (pophint:do :not-highlight t
                  :direction 'around
                  :allwindow t
                  :use-pos-tip t
                  :source '((shown . "Wnd")
                            (requires . 0)
                            (method . (lambda ()
                                        (if (eq pophint-config:current-window
                                                (selected-window))
                                            (setq pophint-config:current-window nil)
                                          (setq pophint-config:current-window (selected-window))
                                          (make-pophint:hint :startpt (point-min) :endpt (point) :value ""))))
                            (action . (lambda (hint)
                                        (funcall pophint--default-action hint)
                                        (goto-char (pophint:hint-endpt hint))))))
    ad-do-it))

(defun pophint-config:set-do-when-other-window (activate)
  "Whether do pop-up when `other-window'."
  (if activate
      (ad-enable-advice 'other-window 'around 'do-pophint)
    (ad-disable-advice 'other-window 'around 'do-pophint))
  (ad-activate 'other-window)
  (setq pophint-config:active-when-other-window-p activate))


;;;;;;;;;;;;;;;
;; For elisp

(defvar pophint-config:regexp-sexp-start (rx-to-string `(and (or bos
                                                                 (not (any "(")))
                                                             (group "(" (not (any ") \t\r\n"))))))

(pophint:defsource
  :name "sexp"
  :description "Sexp on emacs-lisp-mode."
  :source '((shown . "Sexp")
            (method . ((lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'backward))
                                    (re-search-forward pophint-config:regexp-sexp-start nil t))
                           (save-excursion
                             (let* ((startpt (match-beginning 1))
                                    (endpt (ignore-errors (goto-char startpt) (forward-sexp) (point)))
                                    (value (when endpt (buffer-substring-no-properties startpt endpt))))
                               (when (and startpt endpt value)
                                 (make-pophint:hint :startpt startpt :endpt endpt :value value))))))
                       (lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'forward))
                                    (re-search-backward pophint-config:regexp-sexp-start nil t))
                           (save-excursion
                             (let* ((startpt (match-beginning 1))
                                    (endpt (ignore-errors (goto-char startpt) (forward-sexp) (point)))
                                    (value (when endpt (buffer-substring-no-properties startpt endpt))))
                               (when (and startpt endpt value)
                                 (make-pophint:hint :startpt startpt :endpt endpt :value value))))))))
            (highlight . nil)))

(defun pophint-config:elisp-setup ()
  (add-to-list 'pophint:sources 'pophint:source-sexp)
  (setq pophint-config:exclude-quote-chars '("'" "`")))

(add-hook 'emacs-lisp-mode-hook 'pophint-config:elisp-setup t)


;;;;;;;;;;;;;;;;;;;;;;
;; For visual-basic

(defun pophint-config:vb-setup ()
  (setq pophint-config:exclude-quote-chars '("'")))

(add-hook 'visual-basic-mode-hook 'pophint-config:vb-setup t)


;;;;;;;;;;;;;;
;; For Help

(pophint:defsource
  :name "help-btn"
  :description "Button on help-mode."
  :source '((shown . "Link")
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

(defvar pophint-config:w3m-use-new-tab t)
(defun pophint-config:set-w3m-use-new-tab (activate)
  "Whether open new tab of w3m when action by w3m function."
  (setq pophint-config:w3m-use-new-tab activate))

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
                                 (w3m-search-do-search func engine str)))))

(pophint:defsource
  :name "w3m-anchor"
  :description "Anchor on w3m."
  :source '((shown . "Link")
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
                        (if pophint-config:w3m-use-new-tab
                            (w3m-view-this-url-new-session)
                          (w3m-view-this-url))))))

(defun pophint-config:do-w3m-anchor-sentinel (method)
  (let ((pophint-config:w3m-use-new-tab (case method
                                          ('open   nil)
                                          ('tab    t)
                                          ('invert (not pophint-config:w3m-use-new-tab)))))
    (pophint:do-w3m-anchor)))

(defun pophint-config:w3m-anchor-open ()
  "Do `pophint:do-w3m-anchor' in current tab."
  (interactive)
  (pophint-config:do-w3m-anchor-sentinel 'open))

(defun pophint-config:w3m-anchor-open-new-tab ()
  "Do `pophint:do-w3m-anchor' in new tab."
  (interactive)
  (pophint-config:do-w3m-anchor-sentinel 'tab))

(defun pophint-config:w3m-anchor-open-invert ()
  "Do `pophint:do-w3m-anchor' inverting `pophint-config:w3m-use-new-tab'."
  (interactive)
  (pophint-config:do-w3m-anchor-sentinel 'invert))

(defun pophint-config:w3m-anchor-open-new-tab-continuously ()
  "Do `pophint:do-w3m-anchor' in new tab continuously."
  (interactive)
  (let ((buff (current-buffer))
        (pt (point)))
    (pophint-config:do-w3m-anchor-sentinel 'tab)
    (switch-to-buffer buff)
    (goto-char pt)
    (pophint-config:w3m-anchor-open-new-tab-continuously)))

(defun pophint-config:w3m-anchor-yank ()
  "Yank using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
              :action pophint-config:yank-action))

(defun pophint-config:w3m-anchor-view-source ()
  "View source using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
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

(defun pophint-config:w3m-anchor-focus ()
  "Focus using `pophint:source-w3m-anchor'."
  (interactive)
  (pophint:do :source pophint:source-w3m-anchor
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

(defun pophint-config:w3m-setup ()
  (add-to-list 'pophint:sources 'pophint:source-w3m-anchor)
  (pophint-config:w3m-set-keys))

(add-hook 'w3m-mode-hook 'pophint-config:w3m-setup t)


;;;;;;;;;;;;;
;; For eww

(pophint:defsource
  :name "eww-anchor"
  :description "Anchor on eww."
  :source '((shown . "Link")
            (method . ((lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'backward))
                                    (not (string= (shr-next-link) "No next link")))
                           (let* ((url (get-text-property (point) 'shr-url)))
                             (pophint--trace "(eww)found anchor. url:[%s] " url)
                             (make-pophint:hint :startpt (point)
                                                :endpt (text-property-any (point) (point-max)  'help-echo nil)
                                                :value url))))
                       (lambda ()
                         (when (and (not (eq (pophint:get-current-direction) 'forward))
                                    (not (string= (shr-previous-link) "No previous link")))
                           (let* ((url (get-text-property (point) 'shr-url)))
                             (pophint--trace "(eww)found anchor. url:[%s]" url)
                             (make-pophint:hint :startpt (point)
                                                :endpt (text-property-any (point) (point-max)  'help-echo nil)
                                                :value url))))))
            (action . (lambda (hint)
                        (goto-char (pophint:hint-startpt hint))
                        (shr-browse-url)))))

(defun pophint-config:eww-setup ()
  (add-to-list 'pophint:sources 'pophint:source-eww-anchor))

(add-hook 'eww-mode-hook 'pophint-config:eww-setup t)


;;;;;;;;;;;;;;;
;; For dired

(pophint:defsource :name "dired-node"
                   :description "Node in directory."
                   :source '((shown . "Node")
                             (regexp . "^ *[d-][r-][w-][x-].+ +\\([^ ]+\\)$")
                             (requires . 1)
                             (highlight . nil)))

(defun pophint-config:dired-setup ()
  (add-to-list 'pophint:sources 'pophint:source-dired-node))

(add-hook 'dired-mode-hook 'pophint-config:dired-setup t)


;;;;;;;;;;;;;;;
;; For direx

(when (featurep 'direx)

  (defvar pophint-config:regexp-direx-node nil)
  (defun pophint-config:direx-node-regexp ()
    (or pophint-config:regexp-direx-node
        (setq pophint-config:regexp-direx-node (rx-to-string `(and bol (* space)
                                                                   (or ,direx:leaf-icon
                                                                       ,direx:open-icon
                                                                       ,direx:closed-icon)
                                                                   (group (+ not-newline))
                                                                   (* space) eol)))))
  (pophint:defsource :name "direx-node"
                     :description "Node on DireX."
                     :source '((shown . "Node")
                               (regexp . pophint-config:direx-node-regexp)
                               (requires . 1)
                               (highlight . nil)
                               (dedicated . (e2wm))
                               (activebufferp . (lambda (buff)
                                                  (with-current-buffer buff
                                                    (eq major-mode 'direx:direx-mode))))
                               (action . (lambda (hint)
                                           (funcall pophint--default-action hint)
                                           (when (and (featurep 'e2wm)
                                                      (e2wm:managed-p))
                                             (direx:find-item-other-window)
                                             (e2wm:pst-window-select-main))))))

  (defun pophint-config:direx-setup ()
    (add-to-list 'pophint:sources 'pophint:source-direx-node))

  (add-hook 'direx:direx-mode-hook 'pophint-config:direx-setup t)

  )


;;;;;;;;;;;;;;;;
;; For Widget

(defun pophint-config:widget-value (w)
  (loop for sexp in '((ignore-errors (widget-value w))
                      (ignore-errors (widget-get w :value)))
        for ret = (eval sexp)
        if (stringp ret) return ret
        finally return ""))

(pophint:defsource
  :name "widget"
  :description "Widget"
  :source '((shown . "Widget")
            (requires . 0)
            (highlight . nil)
            (activebufferp . (lambda (buff)
                               (with-current-buffer buff
                                 (when (where-is-internal 'widget-forward (current-local-map))
                                   t))))
            (method . ((lambda ()
                         (when (not (eq (pophint:get-current-direction) 'backward))
                           (let* ((pt (point))
                                  (mpt (progn (widget-move 1) (point)))
                                  (w (when (> mpt pt) (widget-at))))
                             (when w
                               (pophint--trace "found widget. value:[%s] " (pophint-config:widget-value w))
                               (make-pophint:hint :startpt (point)
                                                  :endpt (+ (point) 1)
                                                  :value (pophint-config:widget-value w))))))
                       (lambda ()
                         (when (not (eq (pophint:get-current-direction) 'forward))
                           (let* ((pt (point))
                                  (mpt (progn (widget-move -1) (point)))
                                  (w (when (< mpt pt) (widget-at))))
                             (when w
                               (pophint--trace "found widget. value:[%s] " (pophint-config:widget-value w))
                               (make-pophint:hint :startpt (point)
                                                  :endpt (+ (point) 1)
                                                  :value (pophint-config:widget-value w))))))))
            (action . (lambda (hint)
                        (select-window (pophint:hint-window hint))
                        (goto-char (pophint:hint-startpt hint))
                        (widget-apply (widget-at) :action)))))

(defun pophint-config:widget-setup ()
  (add-to-list 'pophint:sources 'pophint:source-widget))

(add-hook 'Custom-mode-hook 'pophint-config:widget-setup t)


;;;;;;;;;;;;;;
;; For e2wm

(pophint:defsituation e2wm)

(pophint:defsource
  :name "e2wm-files"
  :description "Node in files plugin of e2wm."
  :source '((dedicated . e2wm)
            (regexp . "^\\([^ ]+\\)")
            (requires . 1)
            (highlight . nil)
            (activebufferp . (lambda (buff)
                               (with-current-buffer buff
                                 (and (e2wm:managed-p)
                                      (eq major-mode 'e2wm:def-plugin-files-mode)))))
            (action . (lambda (hint)
                        (select-window (pophint:hint-window hint))
                        (goto-char (pophint:hint-startpt hint))
                        (e2wm:def-plugin-files-select-command)))))

(pophint:defsource
  :name "e2wm-history"
  :description "Entry in history list plugin of e2wm."
  :source '((dedicated . e2wm)
            (regexp . "^ +[0-9]+ +\\([^ ]+\\)")
            (requires . 1)
            (highlight . nil)
            (activebufferp . (lambda (buff)
                               (with-current-buffer buff
                                 (and (e2wm:managed-p)
                                      (eq major-mode 'e2wm:def-plugin-history-list-mode)))))
            (action . (lambda (hint)
                        (select-window (pophint:hint-window hint))
                        (goto-char (pophint:hint-startpt hint))
                        (e2wm:def-plugin-history-list-select-command)))))

(pophint:defsource
  :name "e2wm-history2"
  :description "Entry in history list2 plugin of e2wm."
  :source '((dedicated . e2wm)
            (regexp . "^\\(?:<-\\)?\\(?:->\\)? +[0-9]+ +\\([^ ]+\\)")
            (requires . 1)
            (highlight . nil)
            (activebufferp . (lambda (buff)
                               (with-current-buffer buff
                                 (and (e2wm:managed-p)
                                      (eq major-mode 'e2wm:def-plugin-history-list2-mode)))))
            (action . (lambda (hint)
                        (select-window (pophint:hint-window hint))
                        (goto-char (pophint:hint-startpt hint))
                        (e2wm:def-plugin-history-list2-select-command)
                        (e2wm:pst-window-select-main)))))

(pophint:defsource
  :name "e2wm-imenu"
  :description "Entry in imenu plugin of e2wm."
  :source '((dedicated . e2wm)
            (regexp . "^\\(.+\\) *$")
            (requires . 1)
            (highlight . nil)
            (activebufferp . (lambda (buff)
                               (with-current-buffer buff
                                 (and (e2wm:managed-p)
                                      (eq major-mode 'e2wm:def-plugin-imenu-mode)))))
            (action . (lambda (hint)
                        (select-window (pophint:hint-window hint))
                        (goto-char (pophint:hint-startpt hint))
                        (e2wm:def-plugin-imenu-jump-command)))))

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
            ,@pophint:source-widget))


(provide 'pophint-config)
;;; pophint-config.el ends here
