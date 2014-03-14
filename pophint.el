;;; pophint.el --- Provide navigation using pop-up tips, like Firefox's Vimperator Hint Mode

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-pophint
;; Version: 0.6.1
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

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "pophint:" :docstring t)
;; `pophint:popup-chars'
;; Characters for the pop-up hint.
;; `pophint:switch-source-char'
;; Character for switching using source.
;; `pophint:switch-direction-char'
;; Character for switching direction of pop-up.
;; `pophint:switch-window-char'
;; Character for switching window of pop-up.
;; `pophint:popup-max-tips'
;; Maximum counts of the pop-up hint.
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
;; `pophint:toggle-use-pos-tip'
;; Toggle the status of `pophint:use-pos-tip'.
;; `pophint:redo'
;; Redo last pop-up hint-tip using any sources.
;; `pophint:do-interactively'
;; Do pop-up hint-tip asking about what to do after select hint-tip.
;; `pophint:do-flexibly'
;; Do pop-up hint-tip using source in `pophint:sources'.
;; `pophint:do'
;; Do pop-up hint-tip using given source on target to direction.
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
  "Characters for the pop-up hint."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-source-char "s"
  "Character for switching using source."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-direction-char "d"
  "Character for switching direction of pop-up."
  :type 'string
  :group 'pophint)

(defcustom pophint:switch-window-char "w"
  "Character for switching window of pop-up."
  :type 'string
  :group 'pophint)

(defcustom pophint:popup-max-tips 200
  "Maximum counts of the pop-up hint.

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

(defvar pophint:sources nil
  "Buffer local sources for pop-up hint tip flexibly.")
(make-variable-buffer-local 'pophint:sources)

(defvar pophint:global-sources nil
  "Global sources for pop-up hint tip flexibly")


(defstruct pophint:hint window popup overlay (startpt 0) (endpt 0) (value ""))
(defstruct pophint:action name action)


(defvar pophint--default-search-regexp
  "\\(?:[^a-zA-Z0-9]\\([a-zA-Z0-9][a-zA-Z0-9]\\)\\|[a-zA-Z0-9 ]\\([^a-zA-Z0-9 ][^a-zA-Z0-9 ]\\)\\|\\([^ \t\n]\\)\\s-*\n\\)")
(defvar pophint--current-direction 'around)
(defvar pophint--current-highlight nil)
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
(defvar pophint--action-hash (make-hash-table :test 'equal))
(defvar pophint--last-source nil)
(defvar pophint--last-sources nil)
(defvar pophint--last-action nil)
(defvar pophint--last-action-name nil)
(defvar pophint--last-window nil)
(defvar pophint--last-hints nil)
(defvar pophint--enable-allwindow-p nil)
(defvar pophint--disable-allwindow-p nil)


(log4e:deflogger "pophint" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                     (error . "error")
                                                     (warn  . "warn")
                                                     (info  . "info")
                                                     (debug . "debug")
                                                     (trace . "trace")))
(pophint--log-set-level 'trace)


(defmacro* pophint:defsource (&key name description source)
  "Define the variable and command to pop-up hint-tip by using given source.

Arguments:
NAME is string. It's used for define variable and command as part of the name.
DESCRIPTION is string. It's used for define variable as part of the docstring.
SOURCE is alist. The member is the following.

 - shown     ... It's string.
                 It's used for message in minibuffer when get user input.
                 If nil, its value is NAME.

 - regexp    ... It's string.
                 It's used for finding next point of pop-up.
                 If nil, its value is `pophint--default-search-regexp'.
                 If exist group of matches, next point is beginning of group 1, else it's beginning of group 0.

 - requires  ... It's integer.
                 It's minimum length of matched text as next point.
                 If nil, its value is 0.

 - limit     ... It's integer.
                 If non-nil, replace `pophint:popup-max-tips' with it while using the source.

 - action    ... It's function.
                 It's called when finish hint-tip selection.
                 If nil, its value is `pophint--default-action'.
                 It receive the object of `pophint:hint' selected by user.
            
 - method    ... It's function or list of function.
                 It's called when find next point of pop-up.
                 If nil, its value is `re-search-forward' or `re-search-backward', and regexp is used.

 - init      ... It's function.
                 It's called before start to find pop-up point at each of window/direction.
                 If the method is multiple, It's called before start each method.

 - highlight ... It's t or nil. Default is t.
                 If nil, don't highlight matched text when pop-up hint.

Example:
 (pophint:defsource :name \"sexp-head\"
                    :description \"Head word of sexp.\"
                    :source '((shown . \"SexpHead\")
                              (regexp . \"(+\\([^() \t\n]+\\)\")
                              (requires . 1)))
"
  (declare (indent 0))
  `(progn
     (defvar ,(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " +" "-" name)))) nil
       ,(format "Source for pop-up hint-tip of '%s'.\n\nDescription:\n%s" name (or description "Not documented.")))
     (setq ,(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " +" "-" name)))) ,source)
     (when (not (assoc-default 'shown ,(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " +" "-" name))))))
       (add-to-list ',(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " +" "-" name)))) '(shown . ,name)))
     (defun ,(intern (format "pophint:do-%s" (downcase (replace-regexp-in-string " +" "-" name)))) ()
       ,(format "Do pop-up hint-tip using `pophint:source-%s'." (downcase (replace-regexp-in-string " +" "-" name)))
       (interactive)
       (pophint:do :source ',(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " +" "-" name))))))))

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
           (message "[PopHint] Failed pophint:defaction : key is not one character.")
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
  "Get current direction of searching next point for pop-up hint-tip.

It return 'around or 'forward or 'backward."
  pophint--current-direction)

;;;###autoload
(defun pophint:toggle-use-pos-tip ()
  "Toggle the status of `pophint:use-pos-tip'."
  (interactive)
  (setq pophint:use-pos-tip (not pophint:use-pos-tip)))

;;;###autoload
(defun pophint:redo ()
  "Redo last pop-up hint-tip using any sources."
  (interactive)
  (pophint:do :source pophint--last-source
              :sources pophint--last-sources
              :action pophint--last-action
              :action-name pophint--last-action-name
              :window pophint--last-window))

;;;###autoload
(defun pophint:do-interactively ()
  "Do pop-up hint-tip asking about what to do after select hint-tip."
  (interactive)
  (yaxception:$
    (yaxception:try
      (let* ((key (pophint--menu-read-key-sequence (pophint--get-read-prompt-interactively)))
             (gbinding (lookup-key (current-global-map) key))
             (binding (or (when (current-local-map)
                            (lookup-key (current-local-map) key))
                          gbinding)))
        (cond ((or (null key) (zerop (length key)))
               (pophint--trace "inputed null. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding))
              ((eq gbinding 'keyboard-quit)
               (pophint--trace "inputed quit. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
               (message "[PopHint] Quit do-interactively."))
              ((eq gbinding 'newline)
               (pophint--trace "inputed ret. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
               (pophint:do-flexibly))
              ((eq gbinding 'self-insert-command)
               (let* ((action (gethash key pophint--action-hash)))
                 (cond ((pophint:action-p action)
                        (pophint:do-flexibly :action (pophint:action-action action)
                                             :action-name (pophint:action-name action)))
                       (t
                        (message "[PopHint] Inputed not start key of action.")
                        (sleep-for 2)
                        (pophint:do-interactively)))))
              ((commandp binding)
               (pophint--trace "inputed cmd. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
               (call-interactively binding)
               (pophint:do-interactively))
              (t
               (pophint--trace "inputed else. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)))))
    (yaxception:catch 'error e
      (message "[PopHint] Failed pophint:do-interactively : %s" (yaxception:get-text e))
      (pophint--fatal "failed do-interactively : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

;;;###autoload
(defun* pophint:do-flexibly (&key action action-name window)
  "Do pop-up hint-tip using source in `pophint:sources'.

For detail, see `pophint:do'."
  (interactive)
  (pophint--debug "start do flexibly. window:[%s] action-name:[%s]\naction:%s" window action-name action)
  (let* ((window (or window pophint--last-window))
         (sources (pophint--get-available-sources window))
         (source (when (member pophint--last-source sources)
                   pophint--last-source)))
    (pophint:do :source source
                :sources sources
                :action action
                :action-name action-name
                :window window)))

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
 If nil, enable switching DIRECTION when pop-up hint. The default is `pophint--current-direction'.
NOT-HIGHLIGHT is t or nil. If non-nil, don't highlight matched text when pop-up hint.
WINDOW is window. find next point of pop-up in the window. If nil, its value is `selected-window'.
NOT-SWITCH-WINDOW is t or nil. If non-nil, disable switching window when select shown hint.
ALLWINDOW is t or nil. If non-nil, pop-up at all windows in frame.
USE-POS-TIP is t or nil. If omitted, inherit `pophint:use-pos-tip'."
  (interactive)
  (yaxception:$
    (yaxception:try
      (pophint--debug
       "start do. direction:[%s] not-highlight:[%s] window:[%s] not-switch-window:[%s] allwindow:[%s] action-name:[%s]\naction:%s\nsource:%s\nsources:%s"
       direction not-highlight window not-switch-window allwindow action-name action source sources)
      (pophint--delete-last-hints)
      (when (not pophint:switch-direction-p)
        (setq pophint--current-direction 'around))
      (setq pophint--current-highlight (not not-highlight))
      (let* ((sources (pophint--expand-sources sources))
             (source (or (pophint--expand-source source)
                         (when (> (length sources) 0) (nth 0 sources))
                         pophint--default-source))
             (currdirection (or direction
                                pophint--current-direction))
             (not-switch-direction (or (when direction t)
                                       (not pophint:switch-direction-p)))
             (not-highlight (or not-highlight
                                (and (assq 'highlight source)
                                     (not (assoc-default 'highlight source)))))
             (not-switch-window (or not-switch-window
                                    (one-window-p)))
             (allwindow (and (or allwindow pophint--enable-allwindow-p pophint:do-allwindow-p)
                             (not pophint--disable-allwindow-p)
                             (not window)
                             (not not-switch-window)))
             (pophint:use-pos-tip (if (eq use-pos-tip 'global) pophint:use-pos-tip use-pos-tip))
             (hints (pophint--get-hints :source source
                                        :direction currdirection
                                        :not-highlight not-highlight
                                        :window window
                                        :allwindow allwindow))
             (hint (pophint--event-loop :hints hints
                                        :source source
                                        :sources sources
                                        :action action
                                        :action-name (or action-name "Go/SrcAct")
                                        :not-highlight not-highlight
                                        :direction currdirection
                                        :not-switch-direction not-switch-direction
                                        :not-switch-window not-switch-window
                                        :window window
                                        :allwindow allwindow))
             (action (or action
                         (assoc-default 'action source)
                         pophint--default-action)))
        (when (and (pophint:hint-p hint)
                   (> (length sources) 0))
          (setq pophint--last-source source)
          (setq pophint--last-sources sources)
          (setq pophint--last-action action)
          (setq pophint--last-action-name action-name)
          (setq pophint--last-window window))
        (pophint--do-action hint action)))
    (yaxception:catch 'error e
      (message "[PopHint] Failed pophint:do : %s" (yaxception:get-text e))
      (pophint--fatal "failed do : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))


(defun pophint--expand-sources (sources)
  (loop for s in sources
        collect (pophint--expand-source s)))

(defun pophint--expand-source (source)
  (cond ((symbolp source)
         (symbol-value source))
        ((listp source)
         source)))

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
    (cond ((symbolp re) (symbol-value re))
          (t            re))))

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

(defsubst pophint--get-popup-text (idx)
  (if (or (not (stringp pophint:popup-chars))
          (string= pophint:popup-chars ""))
      ""
    (let* ((basei (length pophint:popup-chars)))
      (loop with ret = ""
            with n = idx
            for i = (/ n basei)
            for r = (- n (* basei i))
            until (= i 0)
            do (setq n i)
            do (setq ret (concat (substring pophint:popup-chars r (+ r 1)) ret))
            finally return (concat (substring pophint:popup-chars r (+ r 1)) ret)))))

(defsubst pophint--get-popup-text-list (hint-count)
  (loop with tiptexth = (make-hash-table :test 'equal)
        with idx = 0
        with tip-count = 0
        while (< tip-count hint-count)
        for tiptext = (upcase (pophint--get-popup-text idx))
        do (incf idx)
        do (progn (puthash tiptext t tiptexth)
                  (incf tip-count))
        do (let ((chktext (substring tiptext 0 (- (length tiptext) 1))))
             (when (gethash chktext tiptexth)
               (remhash chktext tiptexth)
               (decf tip-count)))
        finally return (loop for k being the hash-keys in tiptexth collect k)))
      
(defsubst pophint--show-tip (hints not-highlight)
  (pophint--trace "start show hint tip. count:[%s] not-highlight:[%s]" (length hints) not-highlight)
  (loop with tiptexts = (pophint--get-popup-text-list (length hints))
        for hint in hints
        for tiptext = (or (pop tiptexts) "")
        if (not (string= tiptext ""))
        do (with-selected-window (pophint:hint-window hint)
             (let ((tip (popup-create (pophint:hint-startpt hint)
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
               (popup-draw tip)))))

(defun* pophint--get-hints (&key source direction not-highlight window allwindow)
  (pophint--debug "start get hints direction:[%s] not-highlight:[%s] window:[%s] allwindow:[%s]\nsource:%s"
                  direction not-highlight window allwindow source)
  (let ((hints))
    (yaxception:$
      (yaxception:try
        (let* ((init (pophint--expand-function-symbol (assoc-default 'init source)))
               (requires (or (assoc-default 'requires source)
                             pophint:default-require-length))
               (re (pophint--get-hint-regexp source))
               (srcmtd (pophint--expand-function-symbol (assoc-default 'method source)))
               (srchfuncs (pophint--get-search-functions srcmtd direction))
               (maxtips (pophint--get-max-tips source direction)))
          (dolist (wnd (or (when allwindow (window-list nil t))
                           (and (windowp window) (window-live-p window) (list window))
                           (list (nth 0 (get-buffer-window-list)))))
            (with-selected-window wnd
              (save-restriction
                (narrow-to-region (window-start) (window-end))
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
          (pophint--show-tip hints not-highlight))
        hints)
      (yaxception:catch 'error e
        (pophint--deletes hints)
        (message "[PopHint] Failed pophint:do : %s" (yaxception:get-text e))
        (pophint--error "failed get hints : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
        (pophint--log-open-log-if-debug)))))

(defun* pophint--event-loop (&key hints
                                  (inputed "")
                                  source
                                  sources
                                  action
                                  action-name
                                  not-highlight
                                  direction
                                  not-switch-direction
                                  not-switch-window
                                  window
                                  allwindow)
  (yaxception:$
    (yaxception:try
      (pophint--debug
       "start event loop. hints:[%s] inputed:[%s] not-highlight:[%s] not-switch-direction:[%s] not-switch-window:[%s] window:[%s] allwindow:[%s] action-name:[%s]\naction:%s\nsource:%s\nsources:%s"
       (length hints) inputed not-highlight not-switch-direction not-switch-window window allwindow action-name action source sources)
      (if (and (= (length hints) 1)
               (not (string= inputed "")))
          (pop hints)
        (let* ((not-switch-source (or (not (listp sources))
                                      (< (length sources) 2)))
               (key (progn
                      (setq pophint--last-hints hints)
                      (pophint--menu-read-key-sequence 
                       (pophint--get-read-prompt :count (length hints)
                                                 :actdesc action-name
                                                 :source source
                                                 :sources sources
                                                 :not-switch-direction not-switch-direction
                                                 :not-switch-source not-switch-source
                                                 :not-switch-window (or not-switch-window allwindow)))))
               (gbinding (progn (pophint--debug "got input key")
                                (lookup-key (current-global-map) key)))
               (binding (or (when (current-local-map)
                              (lookup-key (current-local-map) key))
                            gbinding)))
          (setq pophint--last-hints nil)
          (cond ((or (null key) (zerop (length key)))
                 (pophint--trace "inputed null. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints))
                ((eq gbinding 'keyboard-quit)
                 (pophint--trace "inputed quit. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints)
                 (keyboard-quit)
                 nil)
                ((eq gbinding 'newline)
                 (pophint--trace "inputed ret. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (let* ((hint (when (not (string= inputed ""))
                                (pop hints))))
                   (pophint--deletes hints)
                   hint))
                ((or (eq gbinding 'backward-delete-char-untabify)
                     (eq gbinding 'delete-backward-char))
                 (pophint--trace "inputed del. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints)
                 (pophint:do :source source
                             :sources sources
                             :action action
                             :action-name action-name
                             :direction (when not-switch-direction direction)
                             :not-highlight (not pophint--current-highlight)
                             :window window
                             :not-switch-window not-switch-window
                             :allwindow allwindow)
                 nil)
                ((eq gbinding 'self-insert-command)
                 (cond ((string-match key pophint:popup-chars)
                        (pophint--trace "inputed hintchar. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--event-loop :hints (loop for hint in hints
                                                          for tip = (pophint:hint-popup hint)
                                                          for tiptextlist = (when (popup-p tip) (popup-list tip))
                                                          for tiptext = (or (when tiptextlist (nth 0 tiptextlist))
                                                                            "")
                                                          if (and (string-match (concat "\\`" inputed (upcase key)) tiptext)
                                                                  (popup-live-p tip))
                                                          collect hint
                                                          else
                                                          do (pophint--delete hint))
                                             :inputed (concat inputed (upcase key))
                                             :source source
                                             :sources sources
                                             :action action
                                             :action-name action-name
                                             :not-highlight not-highlight
                                             :direction direction
                                             :not-switch-direction not-switch-direction
                                             :not-switch-window not-switch-window
                                             :window window
                                             :allwindow allwindow))
                       ((and (string= key pophint:switch-direction-char)
                             (not not-switch-direction))
                        (pophint--trace "inputed switch direction. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--deletes hints)
                        (setq pophint--current-direction (case pophint--current-direction
                                                           (forward 'backward)
                                                           (backward 'around)
                                                           (around 'forward)
                                                           (t 'around)))
                        (pophint:do :source source
                                    :sources sources
                                    :action action
                                    :action-name action-name
                                    :not-highlight (not pophint--current-highlight)
                                    :window window
                                    :not-switch-window not-switch-window
                                    :allwindow allwindow)
                        nil)
                       ((and (string= key pophint:switch-source-char)
                             (not not-switch-source))
                        (pophint--trace "inputed switch source. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--deletes hints)
                        (pophint:do :source (loop for i from 1 to (length sources)
                                                  for currsrc = (nth (- i 1) sources)
                                                  if (equal source currsrc)
                                                  return (cond ((= i (length sources)) (nth 0 sources))
                                                               (t                      (nth i sources)))
                                                  finally return (nth 0 sources))
                                    :sources sources
                                    :action action
                                    :action-name action-name
                                    :not-highlight (not pophint--current-highlight)
                                    :direction (when not-switch-direction direction)
                                    :window window
                                    :not-switch-window not-switch-window
                                    :allwindow allwindow))
                       ((and (string= key pophint:switch-window-char)
                             (not not-switch-window)
                             (not allwindow))
                        (pophint--trace "inputed switch window. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--deletes hints)
                        (let* ((nwindow (with-selected-window (or (and (windowp window) (window-live-p window) window)
                                                                  (get-buffer-window))
                                          (next-window)))
                               (nsources (pophint--get-available-sources nwindow)))
                          (pophint:do :source (or (when (= (length sources) 0) source)
                                                  (when (member source nsources) source))
                                      :sources (when (not not-switch-source) nsources)
                                      :action action
                                      :action-name action-name
                                      :not-highlight (not pophint--current-highlight)
                                      :direction (when not-switch-direction direction)
                                      :window nwindow))
                        nil)
                       (t
                        (pophint--trace "inputed not hintchar. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (message "[PopHint] Inputed not hint char.")
                        (sleep-for 2)
                        (pophint--event-loop :hints hints
                                             :inputed inputed
                                             :source source
                                             :sources sources
                                             :action action
                                             :action-name action-name
                                             :not-highlight not-highlight
                                             :direction direction
                                             :not-switch-direction not-switch-direction
                                             :not-switch-window not-switch-window
                                             :window window
                                             :allwindow allwindow))))
                ((commandp binding)
                 (pophint--trace "inputed cmd. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints)
                 (call-interactively binding)
                 nil)
                (t
                 (pophint--trace "inputed else. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints))))))
    (yaxception:catch 'error e
      (pophint--deletes hints)
      (message "[PopHint] Failed pophint:do : %s" (yaxception:get-text e))
      (pophint--error "failed event loop : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

(defun pophint--menu-read-key-sequence (prompt &optional timeout)
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
          (if (and pophint:use-pos-tip
                   window-system
                   (featurep 'pos-tip))
              (progn (pophint--pos-tip-show prompt)
                     (read-key-sequence nil))
            (with-temp-message prompt
              (read-key-sequence nil))))
        (yaxception:finally
          (use-global-map old-global-map)
          (when timer (cancel-timer timer))
          (when (and pophint:use-pos-tip
                     (featurep 'pos-tip))
            (pos-tip-hide)))))))

(defun* pophint--get-read-prompt (&key (count 0)
                                       (actdesc "")
                                       source
                                       sources
                                       not-switch-direction
                                       not-switch-source
                                       not-switch-window)
  (let* ((swsrctext (or (when (not not-switch-source)
                          (concat "'" pophint:switch-source-char "':SwitchSrc("
                                  (mapconcat (lambda (src)
                                               (let* ((currsrcnm (or (assoc-default 'shown source)
                                                                     ""))
                                                      (srcnm (or (assoc-default 'shown src)
                                                                 "NONAME"))
                                                      (srcnm (format "%s" srcnm)))
                                                 (when (string= currsrcnm srcnm)
                                                   (put-text-property 0 (length srcnm) 'face 'bold srcnm))
                                                 srcnm))
                                             sources
                                             "|")
                                  ") "))
                        (format "Src[%s] " (or (assoc-default 'shown source)
                                               "NONAME"))))
         (swdirtext (or (when (not not-switch-direction)
                          (concat "'" pophint:switch-direction-char "':SwitchDrct("
                                  (mapconcat (lambda (d)
                                               (let* ((s (format "%s" d)))
                                                 (when (eq d pophint--current-direction)
                                                   (put-text-property 0 (length s) 'face 'bold s))
                                                 s))
                                             '(around forward backward)
                                             "|")
                                  ") "))
                        ""))
         (swwndtext (or (when (not not-switch-window)
                          (concat "'" pophint:switch-window-char "':SwitchWnd "))
                        "")))
    (format "Select ch. Hints[%s] Act[%s] %s%s%s" count actdesc swsrctext swdirtext swwndtext)))

(defun pophint--get-available-sources (window)
  (let* ((sources (with-current-buffer (or (and (windowp window)
                                                (window-live-p window)
                                                (window-buffer window))
                                           (current-buffer))
                    (pophint--expand-sources pophint:sources))))
    (loop for src in (pophint--expand-sources pophint:global-sources)
          do (add-to-list 'sources src t))
    ;; (add-to-list 'sources pophint--default-source t)
    sources))

(defun pophint--deletes (hints)
  (pophint--trace "start delete hints. hints:[%s]" (length hints))
  (dolist (hint hints)
    (pophint--delete hint))
  nil)

(defun pophint--delete (hint)
  (when (pophint:hint-p hint)
    (let* ((tip (pophint:hint-popup hint))
           (ov (pophint:hint-overlay hint)))
      (when ov
        (delete-overlay ov))
      (when tip
        (popup-delete tip))
      nil)))

(defun pophint--delete-last-hints ()
  (pophint--deletes pophint--last-hints)
  (setq pophint--last-hints nil))

(defun pophint--do-action (hint action)
  (when (pophint:hint-p hint)
    (let* ((tip (pophint:hint-popup hint))
           (selected (pophint:hint-value hint))
           (action (pophint--expand-function-symbol action)))
      (pophint--debug "start action. selected:[%s] action:%s" selected action)
      (pophint--delete hint)
      (when (functionp action) (funcall action hint)))))

(defun pophint--get-read-prompt-interactively ()
  (let* ((count 1)
         (acttext (loop with ret = ""
                        for k being the hash-keys in pophint--action-hash using (hash-values act)
                        for desc = (pophint:action-name act)
                        do (incf count)
                        do (setq ret (concat ret (format "'%s':%s " k desc)))
                        finally return ret)))
    (format "Select ch. Actions[%s] <RET>:Default %s" count acttext)))

(defun pophint--expand-function-symbol (something)
  (cond ((functionp something)
         something)
        ((and (symbolp something)
              (boundp something))
         (symbol-value something))
        ((listp something)
         something)
        (t
         nil)))

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

(defadvice keyboard-quit (before delete-pophint-last-hints activate)
  (pophint--delete-last-hints))


(provide 'pophint)
;;; pophint.el ends here
