;;; pophint.el --- Provide navigation using pop-up tips, like Firefox's Vimperator Hint Mode

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-pophint
;; Version: 0.8.10
;; Package-Requires: ((popup "0.5.0") (log4e "0.2.0") (yaxception "0.1"))

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
;; This extension provides navigation like the Vimperator Hint Mode of Firefox.
;; The interface has the following flow.
;;  1. pop-up tip about the matched point for some action which user want.
;;  2. do some action for the user selecting.
;; 
;; For more infomation, see <https://github.com/aki2o/emacs-pophint/blob/master/README.md>

;;; Dependencies:
;; 
;; - popup.el ( bundled auto-complete.el. see <https://github.com/auto-complete/auto-complete> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'pophint)

;;; Configuration:
;; 
;; ;; Key Binding
;; (define-key global-map (kbd "C-;") 'pophint:do-flexibly)
;; (define-key global-map (kbd "C-+") 'pophint:do)
;; (define-key global-map (kbd "M-;") 'pophint:redo)
;; (define-key global-map (kbd "C-M-;") 'pophint:do-interactively)
;; 
;; For more information, see Configuration section in <https://github.com/aki2o/emacs-pophint/wiki>

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "pophint:" :docstring t)
;; `pophint:popup-chars'
;; Characters for pop-up hint.
;; `pophint:select-source-chars'
;; Characters for selecting source.
;; `pophint:select-source-method'
;; Method to select source.
;; `pophint:switch-source-char'
;; Character for switching source used to pop-up.
;; `pophint:switch-source-reverse-char'
;; Character for switching source used to pop-up in reverse.
;; `pophint:switch-source-delay'
;; Second for delay to switch source used to pop-up.
;; `pophint:switch-source-selectors'
;; List of dedicated selector for source.
;; `pophint:switch-direction-char'
;; Character for switching direction of pop-up.
;; `pophint:switch-direction-reverse-char'
;; Character for switching direction of pop-up in reverse.
;; `pophint:switch-window-char'
;; Character for switching window of pop-up.
;; `pophint:popup-max-tips'
;; Maximum counts of pop-up hint.
;; `pophint:default-require-length'
;; Default minimum length of matched text for pop-up.
;; `pophint:switch-direction-p'
;; Whether switch direction of pop-up.
;; `pophint:do-allwindow-p'
;; Whether do pop-up at all windows.
;; `pophint:use-pos-tip'
;; Whether use pos-tip.el to show prompt.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "pophint:" :docstring t)
;; `pophint:defsource'
;; Define the variable and command to pop-up hint-tip by using given source.
;; `pophint:defaction'
;; Define the action that called when finish hint-tip selection and the command using it.
;; `pophint:defsituation'
;; Define the command to pop-up hint-tip in SITUATION.
;; `pophint:set-allwindow-command'
;; Define advice to FUNC for doing pop-up at all windows.
;; `pophint:set-not-allwindow-command'
;; Define advice to FUNC for doing pop-up at one window.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "pophint:" :docstring t)
;; `pophint:get-current-direction'
;; Get current direction of searching next point for pop-up hint-tip.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "pophint:" :docstring t)
;; `pophint:do'
;; Do pop-up hint-tip using given source on target to direction.
;; `pophint:do-flexibly'
;; Do pop-up hint-tip using source in `pophint:sources'.
;; `pophint:do-interactively'
;; Do pop-up hint-tip asking about what to do after select hint-tip.
;; `pophint:do-situationally'
;; Do pop-up hint-tip for SITUATION.
;; `pophint:redo'
;; Redo last pop-up hint-tip using any sources.
;; `pophint:toggle-use-pos-tip'
;; Toggle the status of `pophint:use-pos-tip'.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
;; - auto-complete.el ... Version 1.4
;; - yaxception.el ... Version 0.1
;; - log4e.el ... Version 0.1


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'popup)
(require 'yaxception)
(require 'log4e)
(require 'pos-tip nil t)


(defgroup pophint nil
  "Pop-up the hint tip of candidates for doing something"
  :group 'popup
  :prefix "pophint:")

(defcustom pophint:popup-chars "hjklyuiopnm"
  "Characters for pop-up hint."
  :type 'string
  :group 'pophint)

(defcustom pophint:select-source-chars "123456789"
  "Characters for selecting source."
  :type 'string
  :group 'pophint)

(defcustom pophint:select-source-method 'use-source-char
  "Method to select source.

This value is one of the following symbols.
 - use-source-char ... Push the key bound to each of sources from `pophint:select-source-chars'
                       without pushing `pophint:switch-source-char'.
 - use-popup-char  ... Push the key bound to each of sources from `pophint:popup-chars'
                       after pushing `pophint:switch-source-char'.
 - nil             ... Push `pophint:switch-source-char' only."
  :type '(choice (const use-source-char)
                 (const use-popup-char)
                 (const nil))
  :group 'pophint)

(defcustom pophint:switch-source-char "s"
  "Character for switching source used to pop-up."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-source-reverse-char "S"
  "Character for switching source used to pop-up in reverse."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-source-delay 0.5
  "Second for delay to switch source used to pop-up.

If nil, it means not delay."
  :type 'number
  :group 'pophint)

(defcustom pophint:switch-source-selectors nil
  "List of dedicated selector for source.

Example:
 '((\"Quoted\"   . \"q\")
   (\"Url/Path\" . \"u\"))
"
  :type '(repeat (cons string string))
  :group 'pophint)

(defcustom pophint:switch-direction-char "d"
  "Character for switching direction of pop-up."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-direction-reverse-char "D"
  "Character for switching direction of pop-up in reverse."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-window-char "w"
  "Character for switching window of pop-up."
  :type 'string
  :group 'pophint)

(defcustom pophint:popup-max-tips 200
  "Maximum counts of pop-up hint.

If nil, it means limitless."
  :type 'integer
  :group 'pophint)

(defcustom pophint:default-require-length 2
  "Default minimum length of matched text for pop-up."
  :type 'integer
  :group 'pophint)

(defcustom pophint:switch-direction-p t
  "Whether switch direction of pop-up."
  :type 'boolean
  :group 'pophint)

(defcustom pophint:do-allwindow-p nil
  "Whether do pop-up at all windows."
  :type 'boolean
  :group 'pophint)

(defcustom pophint:use-pos-tip nil
  "Whether use pos-tip.el to show prompt."
  :type 'boolean
  :group 'pophint)

(defface pophint:tip-face
  '((t (:background "khaki1" :foreground "black" :bold t)))
  "Face for the pop-up hint."
  :group 'pophint)

(defface pophint:match-face
  '((t (:background "steel blue" :foreground "white")))
  "Face for matched hint text."
  :group 'pophint)

(defface pophint:pos-tip-face
  '((((class color) (background dark))  (:background "ivory" :foreground "black"))
    (((class color) (background light)) (:background "gray10" :foreground "white"))
    (t                                  (:background "ivory" :foreground "black")))
  "Face for the tip of pos-tip.el"
  :group 'pophint)

(defface pophint:prompt-bind-part-face
  '((t (:inherit font-lock-keyword-face :bold t)))
  "Face for the part of bound key in prompt."
  :group 'pophint)

(defface pophint:prompt-active-part-face
  '((t (:bold t)))
  "Face for the part of active source/direction in prompt."
  :group 'pophint)

(defvar pophint:sources nil
  "Buffer local sources for pop-up hint tip flexibly.")
(make-variable-buffer-local 'pophint:sources)

(defvar pophint:global-sources nil
  "Global sources for pop-up hint tip flexibly")

(defvar pophint:dedicated-sources nil
  "Dedicated sources for pop-up hint tip in particular situation.")

(defstruct pophint:hint window popup overlay (startpt 0) (endpt 0) (value ""))
(defstruct pophint:action name action)


(log4e:deflogger "pophint" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                     (error . "error")
                                                     (warn  . "warn")
                                                     (info  . "info")
                                                     (debug . "debug")
                                                     (trace . "trace")))
(pophint--log-set-level 'trace)


(defstruct pophint--condition
  source sources action action-name direction window allwindow use-pos-tip
  not-highlight not-switch-direction not-switch-window not-switch-source)

(defvar pophint--action-hash (make-hash-table :test 'equal))
(defvar pophint--enable-allwindow-p nil)
(defvar pophint--disable-allwindow-p nil)

(defvar pophint--last-hints nil)
(defvar pophint--last-condition nil)

(defvar pophint--default-search-regexp
  "\\(?:[^a-zA-Z0-9]\\([a-zA-Z0-9][a-zA-Z0-9]\\)\\|[a-zA-Z0-9 ]\\([^a-zA-Z0-9 ][^a-zA-Z0-9 ]\\)\\|\\([^ \t\n]\\)\\s-*\n\\)")
(defvar pophint--default-source '((shown . "Default")
                                  (regexp . pophint--default-search-regexp)
                                  (requires . 1)
                                  (highlight . nil)))
(defvar pophint--default-action (lambda (hint)
                                  (let ((wnd (pophint:hint-window hint)))
                                    (push-mark)
                                    (when (and (windowp wnd)
                                               (window-live-p wnd)
                                               (not (eq (selected-window) wnd)))
                                      (select-window wnd))
                                    (goto-char (pophint:hint-startpt hint)))))
(defvar pophint--default-action-name "Go/SrcAct")


;;;;;;;;;;;;;
;; Utility

(defmacro pophint--aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro pophint--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun* pophint--show-message (msg &rest args)
  (apply 'message (concat "[PopHint] " msg) args)
  nil)

(defsubst pophint--current-not-highlight-p (cond)
  (or (pophint--condition-not-highlight cond)
      (pophint--awhen (assq 'highlight (pophint--condition-source cond))
        (not (cdr-safe it)))))

(defsubst pophint--current-not-switch-source-p (cond)
  (or (pophint--condition-not-switch-source cond)
      (< (length (pophint--condition-sources cond)) 2)))

(defsubst pophint--current-action (cond)
  (or (pophint--condition-action cond)
      (assoc-default 'action (pophint--condition-source cond))
      pophint--default-action))

(defsubst pophint--delete (hint)
  (when (pophint:hint-p hint)
    (let* ((tip (pophint:hint-popup hint))
           (ov (pophint:hint-overlay hint)))
      (when ov (delete-overlay ov))
      (when tip (popup-delete tip))
      nil)))

(defun pophint--deletes (hints)
  (pophint--trace "start delete hints. hints:[%s]" (length hints))
  (dolist (hint hints)
    (pophint--delete hint))
  nil)

(defun pophint--delete-last-hints ()
  (when pophint--last-hints
    (pophint--deletes pophint--last-hints)
    (setq pophint--last-hints nil)))

(defun pophint--compile-to-function (something)
  (cond ((functionp something)
         something)
        ((and (symbolp something)
              (boundp something))
         (symbol-value something))
        ((listp something)
         something)
        (t
         nil)))

(defsubst pophint--compile-source (source)
  (cond ((symbolp source)
         (symbol-value source))
        ((listp source)
         source)))

(defun pophint--compile-sources (sources)
  (loop for s in sources
        collect (pophint--compile-source s)))

(defsubst pophint--make-index-char-string (idx char-list)
  (if (or (not (stringp char-list))
          (string= char-list ""))
      ""
    (let* ((basei (length char-list)))
      (loop with ret = ""
            with n = idx
            for i = (/ n basei)
            for r = (- n (* basei i))
            until (= i 0)
            do (setq n i)
            do (setq ret (concat (substring char-list r (+ r 1)) ret))
            finally return (concat (substring char-list r (+ r 1)) ret)))))

(defsubst pophint--make-unique-char-strings (count char-list &optional not-upcase exclude-strings)
  (loop with reth = (make-hash-table :test 'equal)
        with idx = 0
        with currcount = 0
        while (< currcount count)
        for currstr = (pophint--make-index-char-string idx char-list)
        for currstr = (if not-upcase currstr (upcase currstr))
        do (incf idx)
        do (when (not (member currstr exclude-strings))
             (puthash currstr t reth)
             (incf currcount))
        do (let ((chkvalue (substring currstr 0 (- (length currstr) 1))))
             (when (gethash chkvalue reth)
               (remhash chkvalue reth)
               (decf currcount)))
        finally return (loop for k being the hash-keys in reth collect k)))
      
(defun pophint--set-selector-sources (sources)
  (loop with char-list = (case pophint:select-source-method
                           (use-popup-char  pophint:popup-chars)
                           (use-source-char pophint:select-source-chars)
                           (t               nil))
        with excludes = (loop for src in sources
                              for s = (assoc-default (assoc-default 'shown src) pophint:switch-source-selectors)
                              if s
                              append (loop with idx = (length s)
                                           while (> idx 0)
                                           collect (substring s 0 idx)
                                           do (decf idx)))
        with selectors = (when char-list
                           (pophint--make-unique-char-strings (length sources) char-list t excludes))
        for src in sources
        for selector = (or (assoc-default (assoc-default 'shown src) pophint:switch-source-selectors)
                           (when selectors (pop selectors)))
        do (pophint--awhen (assq 'selector src)
             (setq src (delq it src)))
        if selector
        do (add-to-list 'src `(selector . ,selector) t)
        collect src))

(defun pophint--get-available-sources (window)
  (let* ((sources (with-current-buffer (or (and (windowp window)
                                                (window-live-p window)
                                                (window-buffer window))
                                           (current-buffer))
                    (pophint--compile-sources pophint:sources))))
    (loop for src in (pophint--compile-sources pophint:global-sources)
          do (add-to-list 'sources src t))
    ;; (add-to-list 'sources pophint--default-source t)
    sources))

(defun pophint--do-action (hint action)
  (when (pophint:hint-p hint)
    (let* ((tip (pophint:hint-popup hint))
           (selected (pophint:hint-value hint))
           (action (pophint--compile-to-function action)))
      (pophint--debug "start action. selected:[%s] action:%s" selected action)
      (pophint--delete hint)
      (when (functionp action) (funcall action hint)))))


;;;;;;;;;;;;;;;;;;;;;
;; For Interactive

(defun pophint--menu-read-key-sequence (prompt use-pos-tip &optional timeout)
  (pophint--trace "start menu read key sequence. prompt[%s] use-pos-tip[%s] timeout[%s]"
                  prompt use-pos-tip timeout)
  ;; Coding by referring to popup-menu-read-key-sequence
  (catch 'timeout
    (let ((timer (and timeout
                      (run-with-timer timeout nil
                                      (lambda ()
                                        (if (zerop (length (this-command-keys)))
                                            (throw 'timeout nil))))))
          (old-global-map (current-global-map))
          (temp-global-map (make-sparse-keymap))
          (overriding-terminal-local-map (make-sparse-keymap)))
      (substitute-key-definition 'keyboard-quit 'keyboard-quit temp-global-map old-global-map)
      (define-key temp-global-map [menu-bar] (lookup-key old-global-map [menu-bar]))
      (define-key temp-global-map [tool-bar] (lookup-key old-global-map [tool-bar]))
      (when (current-local-map)
        (define-key overriding-terminal-local-map [menu-bar] (lookup-key (current-local-map) [menu-bar])))
      (yaxception:$
        (yaxception:try
          (use-global-map temp-global-map)
          (clear-this-command-keys)
          (if (and use-pos-tip
                   window-system
                   (featurep 'pos-tip))
              (progn (pophint--pos-tip-show prompt)
                     (read-key-sequence nil))
            (with-temp-message prompt
              (read-key-sequence nil))))
        (yaxception:finally
          (use-global-map old-global-map)
          (when timer (cancel-timer timer))
          (when (and use-pos-tip
                     (featurep 'pos-tip))
            (pos-tip-hide)))))))

(defun* pophint--make-source-selection-prompt (sources &key
                                                       (delimiter "|")
                                                       highlight-source)
  (let ((hsrcnm (or (assoc-default 'shown highlight-source)
                    "")))
    (mapconcat (lambda (src)
                 (let* ((srcnm (or (assoc-default 'shown src) "*None*"))
                        (selector (assoc-default 'selector src)))
                   (concat (if selector
                               (concat (propertize selector 'face 'pophint:prompt-bind-part-face) ":")
                             "")
                           (if (string= hsrcnm srcnm)
                               (propertize srcnm 'face 'pophint:prompt-active-part-face)
                             srcnm))))
               sources
               delimiter)))

(defun pophint--make-prompt (cond hint-count)
  (let* ((source (pophint--condition-source cond))
         (sources (pophint--condition-sources cond))
         (actdesc (pophint--condition-action-name cond))
         (direction (pophint--condition-direction cond))
         (not-switch-direction (pophint--condition-not-switch-direction cond))
         (not-switch-window (pophint--condition-not-switch-window cond))
         (not-switch-source (pophint--current-not-switch-source-p cond))
         (swsrctext (cond ((not not-switch-source)
                           (format "%s%s:SwSrc(%s) "
                                   (propertize pophint:switch-source-char 'face 'pophint:prompt-bind-part-face)
                                   (if pophint:switch-source-reverse-char
                                       (concat "/"
                                               (propertize pophint:switch-source-reverse-char 'face 'pophint:prompt-bind-part-face))
                                     "")
                                   (pophint--make-source-selection-prompt sources
                                                                          :highlight-source source)))
                          ((loop for s in (append (list source) sources)
                                 always (assoc-default 'dedicated s))
                           "")
                          (t
                           (format "Src[%s] " (or (assoc-default 'shown source)
                                                  "*None*")))))
         (swdirtext (cond ((not not-switch-direction)
                           (format "%s%s:SwDrct(%s) "
                                   (propertize pophint:switch-direction-char 'face 'pophint:prompt-bind-part-face)
                                   (if pophint:switch-direction-reverse-char
                                       (concat "/"
                                               (propertize pophint:switch-direction-reverse-char 'face 'pophint:prompt-bind-part-face))
                                     "")
                                   (mapconcat (lambda (d)
                                                (let* ((s (format "%s" d)))
                                                  (if (eq d direction)
                                                      (propertize s 'face 'pophint:prompt-active-part-face)
                                                    s)))
                                              '(around forward backward)
                                              "|")))
                          (t
                           "")))
         (swwndtext (cond ((not not-switch-window)
                           (format "%s:SwWnd "
                                   (propertize pophint:switch-window-char 'face 'pophint:prompt-bind-part-face)))
                          (t
                           ""))))
    (format "Select ch. Hints[%s] Act[%s] %s%s%s" hint-count actdesc swsrctext swdirtext swwndtext)))

(defun pophint--make-prompt-interactively ()
  (let* ((count 1)
         (acttext (loop with ret = ""
                        for k being the hash-keys in pophint--action-hash using (hash-values act)
                        for desc = (pophint:action-name act)
                        do (incf count)
                        do (setq ret (concat ret
                                             (format "%s:%s "
                                                     (propertize k 'face 'pophint:prompt-bind-part-face)
                                                     desc)))
                        finally return ret))
         (defact (propertize "<RET>" 'face 'pophint:prompt-bind-part-face)))
    (format "Select ch. Actions[%s] %s:Default %s" count defact acttext)))


;;;;;;;;;;;;;;;;;
;; Pop-up Hint

(defun pophint--let-user-select (cond)
  (when (not (pophint--condition-source cond))
    (let ((sources (pophint--condition-sources cond)))
      (setf (pophint--condition-source cond)
            (or (and (> (length sources) 0) (nth 0 sources))
                pophint--default-source))))
  (let ((hints (pophint--get-hints cond)))
    (pophint--show-hint-tips hints
                             (pophint--current-not-highlight-p cond))
    (pophint--event-loop hints cond)))

(defsubst pophint--get-max-tips (source direction)
  (let ((ret (or (assoc-default 'limit source)
                 pophint:popup-max-tips)))
    (when (and (eq direction 'around)
               ret)
      (setq ret (/ ret 2)))
    ret))

(defsubst pophint--get-hint-regexp (source)
  (let ((re (or (assoc-default 'regexp source)
                pophint--default-search-regexp)))
    (cond ((stringp re)   re)
          ((boundp re)    (symbol-value re))
          ((functionp re) (funcall re))
          (t              (eval re)))))

(defsubst pophint--get-search-functions (srcmtd direction)
  (cond ((functionp srcmtd)
         (list srcmtd))
        ((and (listp srcmtd)
              (> (length srcmtd) 0))
         srcmtd)
        (t
         (case direction
           (forward  '(re-search-forward))
           (backward '(re-search-backward))
           (around   '(re-search-forward re-search-backward))))))

(defun* pophint--get-hints (cond)
  (let* ((source (pophint--condition-source cond))
         (direction (pophint--condition-direction cond))
         (window (pophint--condition-window cond))
         (allwindow (pophint--condition-allwindow cond))
         (init (pophint--compile-to-function (assoc-default 'init source)))
         (requires (or (assoc-default 'requires source)
                       pophint:default-require-length))
         (re (pophint--get-hint-regexp source))
         (srcmtd (pophint--compile-to-function (assoc-default 'method source)))
         (srchfuncs (pophint--get-search-functions srcmtd direction))
         (maxtips (pophint--get-max-tips source direction))
         hints)
    (dolist (wnd (or (when allwindow (window-list nil t))
                     (and (windowp window) (window-live-p window) (list window))
                     (list (nth 0 (get-buffer-window-list)))))
      (with-selected-window wnd
        (save-restriction
          (yaxception:$
            (yaxception:try (narrow-to-region (window-start) (window-end)))
            (yaxception:catch 'error e
              (pophint--warn "failed narrow region : window:%s startpt:%s endpt:%s" wnd (window-start) (window-end))))
          (dolist (srchfnc srchfuncs)
            (save-excursion
              (loop initially (progn
                                (pophint--trace
                                 "start searching hint. require:[%s] max:[%s] buffer:[%s] point:[%s]\nregexp: %s\nfunc: %s"
                                 requires maxtips (current-buffer) (point) re srchfnc)
                                (when (functionp init) (funcall init)))
                    with cnt = 0
                    with orghint
                    while (and (yaxception:$
                                 (yaxception:try
                                   (cond (srcmtd (pophint:hint-p (setq orghint (funcall srchfnc))))
                                         (t      (funcall srchfnc re nil t))))
                                 (yaxception:catch 'error e
                                   (pophint--error "failed seek next popup point : %s\n%s"
                                                   (yaxception:get-text e) (yaxception:get-stack-trace-string e))))
                               (or (not maxtips)
                                   (< cnt maxtips)))
                    for hint = (or orghint
                                   (make-pophint:hint :startpt (or (match-beginning 1) (match-beginning 0))
                                                      :endpt (or (match-end 1) (match-end 0))
                                                      :value (or (when (match-beginning 1) (match-string-no-properties 1))
                                                                 (match-string-no-properties 0))))
                    do (setf (pophint:hint-window hint) (selected-window))
                    if (and (>= (length (pophint:hint-value hint)) requires)
                            (not (invisible-p (pophint:hint-startpt hint))))
                    do (progn (pophint--trace "found hint. text:[%s] startpt:[%s] endpt:[%s]"
                                              (pophint:hint-value hint) (pophint:hint-startpt hint) (pophint:hint-endpt hint))
                              (incf cnt)
                              (setq hints (append hints (list hint))))))))))
    hints))

(defun pophint--show-hint-tips (hints not-highlight)
  (pophint--trace "start show hint tips. count:[%s] not-highlight:[%s]" (length hints) not-highlight)
  (pophint--delete-last-hints)
  (yaxception:$
    (yaxception:try
      (loop with orgwnd = (selected-window)
            with wnd = orgwnd
            with tiptexts = (pophint--make-unique-char-strings (length hints) pophint:popup-chars)
            for hint in hints
            for tiptext = (or (when tiptexts (pop tiptexts)) "")
            for nextwnd = (pophint:hint-window hint)
            if (string= tiptext "")
            return nil
            if (not (eq wnd nextwnd))
            do (progn (select-window nextwnd t)
                      (setq wnd (selected-window)))
            do (let ((tip (popup-create (pophint:hint-startpt hint)
                                        (string-width tiptext)
                                        1
                                        :around nil
                                        :margin-left 0
                                        :margin-right 0
                                        :face 'pophint:tip-face))
                     (ov (when (not not-highlight)
                           (make-overlay (pophint:hint-startpt hint)
                                         (pophint:hint-endpt hint)))))
                 (when ov
                   (overlay-put ov 'window (selected-window))
                   (overlay-put ov 'face 'pophint:match-face)
                   (setf (pophint:hint-overlay hint) ov))
                 (setf (pophint:hint-popup hint) tip)
                 (popup-set-list tip (list tiptext))
                 (popup-draw tip))
            finally do (select-window orgwnd t)))
    (yaxception:catch 'error e
      (pophint--deletes hints)
      (pophint--error "failed show hint tips : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defsubst pophint--get-next-source (source sources &optional reverse)
  (loop with maxidx = (- (length sources) 1)
        with i = (if reverse maxidx 0)
        with endi = (if reverse 0 maxidx)
        while (not (= i endi))
        for currsrc = (nth i sources)
        if (equal source currsrc)
        return (let* ((nidx (if reverse (- i 1) (+ i 1)))
                      (nidx (cond ((< nidx 0)      maxidx)
                                  ((> nidx maxidx) 0)
                                  (t               nidx))))
                 (pophint--trace "got next source index : %s" nidx)
                 (nth nidx sources))
        do (if reverse (decf i) (incf i))
        finally return (let ((nidx (if reverse maxidx 0)))
                         (nth nidx sources))))

(defsubst pophint--get-next-window (window)
  (let ((pophint--last-condition nil)
        (currwnd nil)
        (nextwnd nil)
        (basic-getter (lambda (w)
                        (with-selected-window (or (and (windowp w) (window-live-p w) w)
                                                  (get-buffer-window))
                          (next-window)))))
    (if (<= (length (window-list)) 2)
        (funcall basic-getter window)
      (pophint:do :not-highlight t
                  :direction 'around
                  :allwindow t
                  :use-pos-tip t
                  :source '((shown . "Wnd")
                            (requires . 0)
                            (method . (lambda ()
                                        (if (eq currwnd (selected-window))
                                            (setq currwnd nil)
                                          (setq currwnd (selected-window))
                                          (make-pophint:hint :startpt (point-min) :endpt (point) :value ""))))
                            (action . (lambda (hint)
                                        (setq nextwnd (pophint:hint-window hint))))))
      (pophint--aif nextwnd
          it
        (pophint--warn "failed get next window by pophint:do")
        (funcall basic-getter window)))))

(defun* pophint--event-loop (hints cond &optional (inputed "") source-selection)
  (yaxception:$
    (yaxception:try
      (if (and (= (length hints) 1)
               (not (string= inputed "")))
          (pop hints)
        (setq pophint--last-hints hints)
        (let* ((source (pophint--condition-source cond))
               (sources (pophint--condition-sources cond))
               (action-name (pophint--condition-action-name cond))
               (window (pophint--condition-window cond))
               (allwindow (pophint--condition-allwindow cond))
               (not-switch-direction (pophint--condition-not-switch-direction cond))
               (not-switch-window (pophint--condition-not-switch-window cond))
               (not-switch-source (pophint--current-not-switch-source-p cond))
               (key (pophint--menu-read-key-sequence (pophint--make-prompt cond (length hints))
                                                     (pophint--condition-use-pos-tip cond)))
               (gbinding (when key (lookup-key (current-global-map) key)))
               (binding (or (when (and key (current-local-map))
                              (lookup-key (current-local-map) key))
                            gbinding)))
          (pophint--trace "got user input. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
          (setq pophint--last-hints nil)
          ;; Case by user input
          (cond
           ;; Error
           ((or (null key) (zerop (length key)))
            (pophint--warn "can't get user input")
            (pophint--deletes hints))
           ;; Quit
           ((eq gbinding 'keyboard-quit)
            (pophint--debug "user inputed keyboard-quit")
            (pophint--deletes hints)
            (keyboard-quit)
            nil)
           ;; Return first hint
           ((eq gbinding 'newline)
            (pophint--debug "user inputed newline")
            (let* ((hint (when (not (string= inputed "")) (pop hints))))
              (pophint--deletes hints)
              hint))
           ;; Restart loop
           ((or (eq gbinding 'backward-delete-char-untabify)
                (eq gbinding 'delete-backward-char))
            (pophint--debug "user inputed delete command")
            (pophint--deletes hints)
            (pophint--let-user-select cond))
           ((or (eq gbinding 'self-insert-command)
                (string-match key (mapconcat (lambda (s) (or s ""))
                                             (list pophint:popup-chars
                                                   pophint:select-source-chars
                                                   pophint:switch-source-char
                                                   pophint:switch-source-reverse-char
                                                   pophint:switch-direction-char
                                                   pophint:switch-direction-reverse-char
                                                   pophint:switch-window-char)
                                             "")))
            (cond
             ;; Grep hints
             ((and (string-match key pophint:popup-chars)
                   (not source-selection))
              (pophint--debug "user inputed hint char")
              (let* ((currinputed (concat inputed (upcase key)))
                     (nhints (loop with re = (concat "\\`" currinputed)
                                   for hint in hints
                                   for tip = (pophint:hint-popup hint)
                                   for tiptextlist = (when (popup-p tip) (popup-list tip))
                                   for tiptext = (or (when tiptextlist (nth 0 tiptextlist))
                                                     "")
                                   if (and (string-match re tiptext)
                                           (popup-live-p tip))
                                   collect hint
                                   else
                                   do (pophint--delete hint))))
                (pophint--event-loop nhints cond currinputed)))
             ;; Select source
             ((and (or source-selection
                       (and (string-match key pophint:select-source-chars)
                            (eq pophint:select-source-method 'use-source-char)))
                   (not not-switch-source))
              (pophint--debug "user inputed select source char")
              (when (not source-selection) (setq inputed ""))
              (let* ((currinputed (concat inputed key))
                     (nsource (loop for src in sources
                                    if (string= currinputed (or (assoc-default 'selector src) ""))
                                    return src)))
                (if (not nsource)
                    (pophint--event-loop hints cond currinputed t)
                  (pophint--deletes hints)
                  (setf (pophint--condition-source cond) nsource)
                  (pophint--let-user-select cond))))
             ;; Switch source
             ((and (or (string= key pophint:switch-source-char)
                       (string= key pophint:switch-source-reverse-char))
                   (not not-switch-source))
              (pophint--debug "user inputed switch source")
              (if (eq pophint:select-source-method 'use-popup-char)
                  (pophint--event-loop hints cond "" t)
                (loop with reverse = (string= key pophint:switch-source-reverse-char)
                      do (setf (pophint--condition-source cond)
                               (pophint--get-next-source (pophint--condition-source cond) sources reverse))
                      while (and pophint:switch-source-delay
                                 (string= key (pophint--menu-read-key-sequence
                                               (pophint--make-prompt cond (length hints))
                                               (pophint--condition-use-pos-tip cond)
                                               pophint:switch-source-delay))))
                (pophint--deletes hints)
                (pophint--let-user-select cond)))
             ;; Switch direction
             ((and (or (string= key pophint:switch-direction-char)
                       (string= key pophint:switch-direction-reverse-char))
                   (not not-switch-direction))
              (pophint--debug "user inputed switch direction")
              (pophint--deletes hints)
              (let* ((reverse (string= key pophint:switch-direction-reverse-char))
                     (ndirection (case (pophint--condition-direction cond)
                                   (forward  (if reverse 'around 'backward))
                                   (backward (if reverse 'forward 'around))
                                   (around   (if reverse 'backward 'forward))
                                   (t        'around))))
                (setf (pophint--condition-direction cond) ndirection)
                (pophint--let-user-select cond)))
             ;; Switch window
             ((and (string= key pophint:switch-window-char)
                   (not not-switch-window))
              (pophint--debug "user inputed switch window")
              (pophint--deletes hints)
              (let* ((nwindow (pophint--get-next-window window))
                     (nsources (when (not not-switch-source)
                                 (pophint--set-selector-sources (pophint--get-available-sources nwindow)))))
                (setf (pophint--condition-window cond) nwindow)
                ;; (when (and (> (length nsources) 0)
                ;;            (not (member source nsources)))
                ;;   (setf (pophint--condition-source cond) nil))
                (setf (pophint--condition-sources cond) nsources)
                (pophint--let-user-select cond)))
             ;; Warning
             (t
              (pophint--debug "user inputed worthless char")
              (pophint--show-message "Inputed not hint char.")
              (sleep-for 2)
              (pophint--event-loop hints cond inputed source-selection))))
           ;; Pass inputed command
           ((commandp binding)
            (pophint--debug "user inputed command : %s" binding)
            (pophint--deletes hints)
            (call-interactively binding)
            nil)
           ;; Abort
           (t
            (pophint--deletes hints))))))
    (yaxception:catch 'error e
      (pophint--deletes hints)
      (setq pophint--last-hints nil)
      (pophint--error "failed event loop : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))


;;;;;;;;;;;;;;;;;;;;
;; For pos-tip.el

(defun pophint--pos-tip-show (string)
  (copy-face 'pophint:pos-tip-face 'pos-tip-temp)
  (when (eq (face-attribute 'pos-tip-temp :font) 'unspecified)
    (set-face-font 'pos-tip-temp (frame-parameter nil 'font)))
  (set-face-bold-p 'pos-tip-temp (face-bold-p 'pophint:pos-tip-face))
  (multiple-value-bind (wnd rightpt bottompt) (pophint--get-pos-tip-location)
    (let* ((max-width (pos-tip-x-display-width))
           (max-height (pos-tip-x-display-height))
           (tipsize (pophint--get-pos-tip-size string))
           (tipsize (cond ((or (> (car tipsize) max-width)
                               (> (cdr tipsize) max-height))
                           (setq string (pos-tip-truncate-string string max-width max-height))
                           (pophint--get-pos-tip-size string))
                          (t
                           tipsize)))
           (tipwidth (car tipsize))
           (tipheight (cdr tipsize))
           (dx (- rightpt tipwidth 10))
           (dy (- bottompt tipheight)))
      (pos-tip-show-no-propertize
       string 'pos-tip-temp 1 wnd 300 tipwidth tipheight nil dx dy))))

(defun pophint--get-pos-tip-size (string)
  "Return (WIDTH . HEIGHT) of the tip of pos-tip.el generated from STRING."
  (let* ((w-h (pos-tip-string-width-height string))
         (width (pos-tip-tooltip-width (car w-h) (frame-char-width)))
         (height (pos-tip-tooltip-height (cdr w-h) (frame-char-height))))
    (cons width height)))

(defun pophint--get-pos-tip-location ()
  "Return (WND RIGHT BOTTOM) as the location to show the tip of pos-tip.el."
  (let ((leftpt 0)
        (toppt 0)
        wnd rightpt bottompt)
    (dolist (w (window-list))
      (let* ((edges (when (not (minibufferp (window-buffer w)))
                      (window-pixel-edges w)))
             (currleftpt (or (nth 0 edges) -1))
             (currtoppt (or (nth 1 edges) -1)))
        (when (and (= currleftpt 0)
                   (= currtoppt 0))
          (setq wnd w))
        (when (or (not rightpt)
                  (> currleftpt leftpt))
          (setq rightpt (nth 2 edges))
          (setq leftpt currleftpt))
        (when (or (not bottompt)
                  (> currtoppt toppt))
          (setq bottompt (nth 3 edges))
          (setq toppt currtoppt))))
    (list wnd rightpt bottompt)))


;;;;;;;;;;;;;;;;;;;
;; User Function

(defmacro* pophint:defsource (&key name description source)
  "Define the variable and command to pop-up hint-tip by using given source.

Arguments:
NAME is string. It's used for define variable and command as part of the name.
DESCRIPTION is string. It's used for define variable as part of the docstring.
SOURCE is alist. The member is the following.

 - shown         ... It's string.
                     It's used for message in minibuffer when get user input.
                     If nil, its value is NAME.
                 
 - regexp        ... It's string.
                     It's used for finding next point of pop-up.
                     If nil, its value is `pophint--default-search-regexp'.
                     If exist group of matches, next point is beginning of group 1, else it's beginning of group 0.
                 
 - requires      ... It's integer.
                     It's minimum length of matched text as next point.
                     If nil, its value is 0.
                 
 - limit         ... It's integer.
                     If non-nil, replace `pophint:popup-max-tips' with it while using the source.
                 
 - action        ... It's function.
                     It's called when finish hint-tip selection.
                     If nil, its value is `pophint--default-action'.
                     It receive the object of `pophint:hint' selected by user.
                 
 - method        ... It's function or list of function.
                     It's called when find next point of pop-up.
                     If nil, its value is `re-search-forward' or `re-search-backward', and regexp is used.
                 
 - init          ... It's function.
                     It's called before start to find pop-up point at each of window/direction.
                     If the method is multiple, It's called before start each method.
                 
 - highlight     ... It's t or nil. Default is t.
                     If nil, don't highlight matched text when pop-up hint.
                 
 - dedicated     ... It's symbol or list of symbol means the particular situation that SOURCE is dedicated for.
                     If non-nil, added to `pophint:dedicated-sources' which has relation with `pophint:do-situationally'.

 - activebufferp ... It's function called for checking if SOURCE is activated in the buffer.
                     It's no need if 'dedicated' option is not used.
                     This function receives a buffer object and
                     needs to return non-nil if the buffer is the target of itself.

Example:
 (pophint:defsource :name \"sexp-head\"
                    :description \"Head word of sexp.\"
                    :source '((shown . \"SexpHead\")
                              (regexp . \"(+\\([^() \t\n]+\\)\")
                              (requires . 1)))
"
  (declare (indent 0))
  (let* ((symnm (downcase (replace-regexp-in-string " +" "-" name)))
         (var-sym (intern (format "pophint:source-%s" symnm)))
         (var-doc (format "Source for pop-up hint-tip of '%s'.\n\nDescription:\n%s"
                          name (or description "Not documented.")))
         (fnc-sym (intern (format "pophint:do-%s" symnm)))
         (fnc-doc (format "Do pop-up hint-tip using `%s'." var-sym)))
    `(progn
       (defvar ,var-sym nil
         ,var-doc)
       (setq ,var-sym ,source)
       (when (not (assoc-default 'shown ,var-sym))
         (add-to-list ',var-sym '(shown . ,name)))
       (if (assoc-default 'dedicated ,var-sym)
           (add-to-list 'pophint:dedicated-sources ',var-sym t)
         (defun ,fnc-sym ()
           ,fnc-doc
           (interactive)
           (pophint:do :source ',var-sym))))))

(defmacro* pophint:defaction (&key key name description action)
  "Define the action that called when finish hint-tip selection and the command using it.

Arguments:
KEY is string. It's one character. It's the key when read user input by `pophint:do-interactively'.
NAME is string. It's used for define command as part of the name and show message in minibuffer when get user input.
DESCRIPTION is string. It's used for define command as part of the docstring.
ACTION is function. For detail, see action of SOURCE for `pophint:defsource'.

Example:
 (pophint:defaction :key \"y\"
                    :name \"Yank\"
                    :description \"Yank the text of selected hint-tip.\"
                    :action (lambda (hint)
                              (kill-new (pophint:hint-value hint))))
"
  (declare (indent 0))
  `(progn
     (let ((key ,key)
           (name ,name)
           (action ,action))
       (if (or (not (stringp key))
               (string= key "")
               (not (= (length key) 1)))
           (pophint--show-message "Failed pophint:defaction : key is not one character.")
         (puthash key
                  (make-pophint:action :name name :action action)
                  pophint--action-hash)
         (defun ,(intern (format "pophint:do-flexibly-%s" (downcase (replace-regexp-in-string " " "-" name)))) ()
           ,(format "Do pop-up hint-tip using source in `pophint:sources' and do %s.\n\nDescription:\n%s"
                    name (or description "Not documented."))
           (interactive)
           (let ((act (gethash ,key pophint--action-hash)))
             (pophint:do-flexibly :action (pophint:action-action act)
                                  :action-name (pophint:action-name act))))))))

(defmacro pophint:defsituation (situation)
  "Define the command to pop-up hint-tip in SITUATION.

Arguments:
SITUATION is symbol. It's used for finding the sources that is dedicated
for SITUATION from `pophint:dedicated-sources'.

Example:
 (pophint:defsituation e2wm)
"
  (declare (indent 0))
  (let* ((symnm (downcase (replace-regexp-in-string " +" "-" (symbol-name situation))))
         (fnc-sym (intern (format "pophint:do-situationally-%s" symnm)))
         (fnc-doc (format "Do `pophint:do-situationally' for '%s'." symnm)))
    `(progn
       (defun ,fnc-sym ()
         ,fnc-doc
         (interactive)
         (pophint:do-situationally ',situation)))))

(defmacro pophint:set-allwindow-command (func)
  "Define advice to FUNC for doing pop-up at all windows.

FUNC is symbol not quoted. e.g. (pophint:set-allwindow-command pophint:do-flexibly)"
  `(defadvice ,func (around pophint-allwindow activate)
     (let ((pophint--enable-allwindow-p t))
       ad-do-it)))

(defmacro pophint:set-not-allwindow-command (func)
  "Define advice to FUNC for doing pop-up at one window.

FUNC is symbol not quoted. e.g. (pophint:set-not-allwindow-command pophint:do-flexibly)"
  `(defadvice ,func (around pophint-not-allwindow activate)
     (let ((pophint--disable-allwindow-p t))
       ad-do-it)))

(defun pophint:get-current-direction ()
  "Get current direction of searching next point for pop-up hint-tip."
  (when (pophint--condition-p pophint--last-condition)
    (pophint--condition-direction pophint--last-condition)))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun* pophint:do (&key source
                         sources
                         action
                         action-name
                         direction
                         not-highlight
                         window
                         not-switch-window
                         allwindow
                         (use-pos-tip 'global))
  "Do pop-up hint-tip using given source on target to direction.

SOURCE is alist or symbol of alist. About its value, see `pophint:defsource'.
 If nil, its value is the first of SOURCES or `pophint--default-source'.
 If non-nil, `pophint--default-source' isn't used for SOURCES.
SOURCES is list of SOURCE. If this length more than 1, enable switching SOURCE when pop-up hint.
ACTION is function. About this, see action of SOURCE for `pophint:defsource'. If nil, it's used.
ACTION-NAME is string. About this, see name of `pophint:defaction'.
DIRECTION is symbol. The allowed value is the following.
 - forward  ... seek the pop-up point moving forward until `pophint:popup-max-tips'.
 - backward ... seek the pop-up point moving backward until `pophint:popup-max-tips'.
 - around   ... seek the pop-up point moving both until half of `pophint:popup-max-tips'.
 If nil, enable switching DIRECTION when pop-up hint.
NOT-HIGHLIGHT is t or nil. If non-nil, don't highlight matched text when pop-up hint.
WINDOW is window. find next point of pop-up in the window. If nil, its value is `selected-window'.
NOT-SWITCH-WINDOW is t or nil. If non-nil, disable switching window when select shown hint.
ALLWINDOW is t or nil. If non-nil, pop-up at all windows in frame.
USE-POS-TIP is t or nil. If omitted, inherit `pophint:use-pos-tip'."
  (interactive)
  (yaxception:$
    (yaxception:try
      (pophint--debug
       "start do.\ndirection:%s\nnot-highlight:%s\nwindow:%s\nnot-switch-window:%s\nallwindow:%s\naction-name:%s\naction:%s\nsource:%s\nsources:%s"
       direction not-highlight window not-switch-window allwindow action-name action source sources)
      (let* ((current-input-method nil)
             (allwindow-p (and (or allwindow
                                   pophint--enable-allwindow-p
                                   pophint:do-allwindow-p)
                               (not pophint--disable-allwindow-p)
                               (not window)))
             (c (setq pophint--last-condition
                      (make-pophint--condition :source (pophint--compile-source source)
                                               :sources (pophint--set-selector-sources (pophint--compile-sources sources))
                                               :action action
                                               :action-name (or action-name pophint--default-action-name)
                                               :direction (or direction
                                                              (when (not pophint:switch-direction-p) 'around)
                                                              (pophint:get-current-direction)
                                                              'around)
                                               :window window
                                               :allwindow allwindow-p
                                               :use-pos-tip (if (eq use-pos-tip 'global) pophint:use-pos-tip use-pos-tip)
                                               :not-highlight not-highlight
                                               :not-switch-direction (or (when direction t)
                                                                         (not pophint:switch-direction-p))
                                               :not-switch-window (or not-switch-window (one-window-p) allwindow-p)
                                               :not-switch-source (and source (not sources)))))
             (hint (pophint--let-user-select c)))
        (pophint--do-action hint (pophint--current-action c))))
    (yaxception:catch 'error e
      (pophint--show-message "Failed pophint:do : %s" (yaxception:get-text e))
      (pophint--fatal "failed do : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

;;;###autoload
(defun* pophint:do-flexibly (&key action action-name window)
  "Do pop-up hint-tip using source in `pophint:sources'.

For detail, see `pophint:do'."
  (interactive)
  (pophint--debug "start do flexibly. window:[%s] action-name:[%s]\naction:%s" window action-name action)
  (let* ((lastc pophint--last-condition)
         (window (or window
                     (when (pophint--condition-p lastc)
                       (pophint--condition-window lastc))))
         (sources (pophint--get-available-sources window))
         (lastsrc (when (pophint--condition-p lastc)
                    (pophint--condition-source lastc)))
         (compsrc (pophint--aif (assq 'selector lastsrc)
                      (delq it (copy-sequence lastsrc))
                    lastsrc))
         (source (when (and compsrc
                            (member compsrc sources))
                   lastsrc)))
    (pophint:do :source source
                :sources sources
                :action action
                :action-name action-name
                :window window)))

;;;###autoload
(defun pophint:do-interactively ()
  "Do pop-up hint-tip asking about what to do after select hint-tip."
  (interactive)
  (yaxception:$
    (yaxception:try
      (let* ((key (pophint--menu-read-key-sequence (pophint--make-prompt-interactively)
                                                   pophint:use-pos-tip))
             (gbinding (lookup-key (current-global-map) key))
             (binding (or (when (current-local-map)
                            (lookup-key (current-local-map) key))
                          gbinding)))
        (pophint--trace "got user input. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
        (cond ((or (null key) (zerop (length key)))
               (pophint--warn "can't get user input"))
              ((eq gbinding 'keyboard-quit)
               (pophint--debug "user inputed keyboard-quit")
               (pophint--show-message "Quit do-interactively."))
              ((eq gbinding 'newline)
               (pophint--debug "user inputed newline")
               (pophint:do-flexibly))
              ((eq gbinding 'self-insert-command)
               (let* ((action (gethash key pophint--action-hash)))
                 (cond ((pophint:action-p action)
                        (pophint:do-flexibly :action (pophint:action-action action)
                                             :action-name (pophint:action-name action)))
                       (t
                        (pophint--show-message "Inputed not start key of action.")
                        (sleep-for 2)
                        (pophint:do-interactively)))))
              ((commandp binding)
               (pophint--debug "user inputed command : %s" binding)
               (call-interactively binding)
               (pophint:do-interactively)))))
    (yaxception:catch 'error e
      (pophint--show-message "Failed pophint:do-interactively : %s" (yaxception:get-text e))
      (pophint--fatal "failed do-interactively : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

;;;###autoload
(defun pophint:do-situationally (situation)
  "Do pop-up hint-tip for SITUATION.

SITUATION is symbol used for finding active sources from `pophint:dedicated-sources'."
  (interactive
   (list (intern
          (completing-read "Select situation: "
                           (loop with ret = nil
                                 for src in (pophint--compile-sources pophint:dedicated-sources)
                                 for dedicated = (assoc-default 'dedicated src)
                                 if dedicated
                                 do (cond ((symbolp dedicated) (pushnew dedicated ret))
                                          ((listp dedicated)   (loop for e in dedicated do (pushnew e ret))))
                                 finally return ret)
                           nil t nil '()))))
  (yaxception:$
    (yaxception:try
      (pophint--trace "start do situationally. situation[%s]" situation)
      (let* ((current-input-method nil)
             (sources (loop for src in (pophint--compile-sources pophint:dedicated-sources)
                            for dedicated = (assoc-default 'dedicated src)
                            if (or (and dedicated
                                        (symbolp dedicated)
                                        (eq dedicated situation))
                                   (and dedicated
                                        (listp dedicated)
                                        (memq situation dedicated)))
                            collect src))
             (not-highlight (loop for src in sources
                                  always (and (assq 'highlight src)
                                              (not (assoc-default 'highlight src)))))
             (actionh (make-hash-table :test 'equal))
             (cond (make-pophint--condition :sources sources
                                            :action-name (upcase (symbol-name situation))
                                            :direction 'around
                                            :use-pos-tip pophint:use-pos-tip
                                            :not-highlight not-highlight
                                            :not-switch-direction t
                                            :not-switch-window t
                                            :not-switch-source t))
             (hints (loop for wnd in (window-list nil nil)
                          do (setf (pophint--condition-window cond) wnd)
                          append (with-selected-window wnd
                                    (loop with buff = (window-buffer)
                                          for src in sources
                                          for chker = (assoc-default 'activebufferp src)
                                          if (and (functionp chker)
                                                  (funcall chker buff))
                                          return (progn
                                                   (puthash (buffer-name buff) (assoc-default 'action src) actionh)
                                                   (setf (pophint--condition-source cond) src)
                                                   (pophint--get-hints cond))))))
             (hint (progn (pophint--show-hint-tips hints not-highlight)
                          (pophint--event-loop hints cond)))
             (action (or (when hint
                           (gethash (buffer-name (window-buffer (pophint:hint-window hint))) actionh))
                         pophint--default-action)))
        (pophint--do-action hint action)))
    (yaxception:catch 'error e
      (pophint--show-message "Failed pophint:do situationally : %s" (yaxception:get-text e))
      (pophint--fatal "failed do situationally : %s\n%s"
                      (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

;;;###autoload
(defun pophint:redo ()
  "Redo last pop-up hint-tip using any sources."
  (interactive)
  (yaxception:$
    (yaxception:try
      (if (not (pophint--condition-p pophint--last-condition))
          (pophint--show-message "Failed pophint:redo : Maybe pophint:do done not yet")
        (let ((hint (pophint--let-user-select pophint--last-condition)))
          (pophint--do-action hint (pophint--current-action pophint--last-condition)))))
    (yaxception:catch 'error e
      (pophint--show-message "Failed pophint:redo : %s" (yaxception:get-text e))
      (pophint--fatal "failed redo : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

;;;###autoload
(defun pophint:toggle-use-pos-tip ()
  "Toggle the status of `pophint:use-pos-tip'."
  (interactive)
  (setq pophint:use-pos-tip (not pophint:use-pos-tip)))


(defadvice keyboard-quit (before pophint:delete-last-hints activate)
  (pophint--delete-last-hints))


(provide 'pophint)
;;; pophint.el ends here
