;;; pophint.el --- provide navigation like the Vimperator Hint Mode of Firfox that pop-up tip and do some action for the selected.

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-pophint
;; Version: 0.1

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
;; This extension provides navigation like the Vimperator Hint Mode of Firfox.
;; The interface has the following flow.
;;  1. pop-up tip about the matched point for some action which user want.
;;  2. do some action for the user selecting.

;;; Dependency:
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
;; (define-key global-map (kbd "M-;") 'pophint:do-interactively)

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
;; Default length of matched text for pop-up.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "pophint:" :docstring t)
;; `pophint:defsource'
;; Define the variable and command to pop-up hint-tip by using given source.
;; `pophint:defaction'
;; Define the action that called when finish hint-tip selection and the command using it.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "pophint:" :docstring t)
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
  "Maximum counts of the pop-up hint."
  :type 'integer
  :group 'pophint)

(defcustom pophint:default-require-length 4
  "Default length of matched text for pop-up."
  :type 'integer
  :group 'pophint)

(defface pophint:tip-face
  '((t (:background "khaki1" :foreground "black" :bold t)))
  "Face for the pop-up hint."
  :group 'pophint)

(defface pophint:match-face
  '((t (:background "cornflower blue" :foreground "white")))
  "Face for matched hint text."
  :group 'pophint)

(defvar pophint:sources nil
  "Buffer local sources for pop-up hint tip flexibly.")
(make-variable-buffer-local 'pophint:sources)

(defvar pophint:global-sources nil
  "Global sources for pop-up hint tip flexibly")


(defstruct pophint:hint buffer popup overlay (startpt 0) (endpt 0) (value ""))
(defstruct pophint:action description action)


(defvar pophint--default-search-regexp "[^a-zA-Z0-9_]+\\([a-zA-Z0-9_]+\\)")
(defvar pophint--current-direction 'around)
(defvar pophint--default-source '((shown . "Word")
                                  (regexp . pophint--default-search-regexp)))
(defvar pophint--default-action (lambda (hint)
                                  (let* ((buff (pophint:hint-buffer hint)))
                                    (push-mark)
                                    (when (and (buffer-live-p buff)
                                               (get-buffer-window buff)
                                               (not (eq (current-buffer) buff)))
                                      (select-window (get-buffer-window buff)))
                                    (goto-char (pophint:hint-startpt hint)))))
(defvar pophint--action-hash (make-hash-table :test 'equal))


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

 - regexp   ... It's string.
                It's used for finding next point of pop-up.
                If nil, its value is `pophint--default-search-regexp'.
                If exist group of matches, next point is beginning of group 1, else it's beginning of group 0.

 - requires ... It's integer.
                It's minimum length of matched text as next point.
                If nil, its value is 0.

 - action   ... It's function.
                It's called when finish hint-tip selection.
                If nil, its value is `pophint--default-action'.
                It receive the object of `pophint:hint' selected by user.
            
 - method   ... It's function or list of function.
                It's called when find next point of pop-up.
                If nil, its value is `re-search-forward' or `re-search-backward', and regexp is used.

 - init     ... It's function.
                It's called before start the loop that find next point of pop-up.
                If the method is multiple, It's called before start each method.

 - shown    ... It's string.
                It's used for message in minibuffer when get user input.
                If nil, its value is NAME.

Example:
 (pophint:defsource :name \"sexp-head\"
                    :description \"Head word of sexp.\"
                    :source '((shown . \"SexpHead\")
                              (regexp . \"(+\\([^() \t\n]+\\)\")
                              (requires . 1)))
"
  (declare (indent 0))
  `(progn
     (defvar ,(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " " "" name)))) nil
       ,(format "Source for pop-up hint-tip of '%s'.\n\nDescription:\n%s" name (or description "Not documented.")))
     (setq ,(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " " "" name)))) ,source)
     (when (not (assoc-default 'shown ,(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " " "" name))))))
       (add-to-list ',(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " " "" name)))) '(shown . ,name)))
     (defun ,(intern (format "pophint:do-%s" (downcase (replace-regexp-in-string " " "" name)))) ()
       ,(format "Do pop-up hint-tip using `pophint:source-%s'." (downcase (replace-regexp-in-string " " "" name)))
       (interactive)
       (pophint:do :source ',(intern (format "pophint:source-%s" (downcase (replace-regexp-in-string " " "" name))))))))

(defmacro* pophint:defaction (&key key name description action)
  "Define the action that called when finish hint-tip selection and the command using it.

Arguments:
KEY is string. It's one character. It's the key when read user input by `pophint:do-interactively'.
NAME is string. It's used for define command as part of the name and show message in minibuffer when get user input.
DESCRIPTION is string. It's used for define command as part of the docstring.
ACTION is function.

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
                  (make-pophint:action :description name :action action)
                  pophint--action-hash)
         (defun ,(intern (format "pophint:do-flexibly-%s" (downcase (replace-regexp-in-string " " "" name)))) ()
           ,(format "Do pop-up hint-tip using source in `pophint:sources' and do %s.\n\nDescription:\n%s"
                    name (or description "Not documented."))
           (interactive)
           (let ((act (gethash ,key pophint--action-hash)))
             (pophint:do-flexibly :action (pophint:action-action act)
                                  :action-description (pophint:action-description act))))))))

(defun pophint:do-interactively ()
  "Do pop-up hint-tip asking about what to do after select hint-tip."
  (interactive)
  (yaxception:$
    (yaxception:try
      (let* ((key (popup-menu-read-key-sequence nil (pophint--get-read-prompt-interactively)))
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
                                             :action-description (pophint:action-description action)))
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

(defun* pophint:do-flexibly (&key action action-description window)
  "Do pop-up hint-tip using source in `pophint:sources'.

For detail, see `pophint:do'."
  (interactive)
  (pophint--debug "start do flexibly. action-description:[%s]\naction:%s" action-description action)
  (pophint:do :sources (pophint--get-available-sources window)
              :action action
              :action-description action-description
              :window window))

(defun* pophint:do (&key source
                         sources
                         action
                         action-description
                         direction
                         not-highlight
                         window
                         not-switch-window)
  "Do pop-up hint-tip using given source on target to direction.

SOURCE is alist or symbol of alist. About its value, see `pophint:defsource'.
 If nil, its value is the first of SOURCES or `pophint--default-source'.
 If non-nil, `pophint--default-source' isn't used for SOURCES.
SOURCES is list of SOURCE. If this length more than 1, enable switching SOURCE when pop-up hint.
ACTION is function. About this, see action of `pophint:defsource'. If nil, it's used.
DIRECTION is symbol. The allowed value is the following.
 - forward  ... seek the pop-up point moving forward until `pophint:popup-max-tips'.
 - backward ... seek the pop-up point moving backward until `pophint:popup-max-tips'.
 - around   ... seek the pop-up point moving both until half of `pophint:popup-max-tips'.
 If nil, enable switching DIRECTION when pop-up hint. The default is `pophint--current-direction'.
NOT-HIGHLIGHT is t or nil. If non-nil, don't highlight matched text when pop-up hint.
WINDOW is window. find next point of pop-up in the window. If nil, its value is `selected-window'.
NOT-SWITCH-WINDOW is t or nil. If non-nil, disable switching window when select shown hint."
  (interactive)
  (yaxception:$
    (yaxception:try
      (pophint--debug "start do. direction:[%s] not-highlight:[%s] window:[%s] not-switch-window:[%s] action-description:[%s]\naction:%s\nsource:%s\nsources:%s"
                      direction not-highlight window not-switch-window action-description action source sources)
      (let* ((sources (pophint--expand-sources sources))
             (source (or (pophint--expand-source source)
                         (when (> (length sources) 0) (nth 0 sources))
                         pophint--default-source))
             (currdirection (or direction pophint--current-direction))
             (not-switch-direction (and direction t))
             (hints (pophint--get-hints :source source
                                        :direction currdirection
                                        :not-highlight not-highlight
                                        :window window))
             (hint (pophint--event-loop :hints hints
                                        :source source
                                        :sources sources
                                        :action action
                                        :action-description (or action-description "Go/SrcAct")
                                        :not-highlight not-highlight
                                        :not-switch-direction not-switch-direction
                                        :not-switch-window not-switch-window
                                        :window window)))
        (pophint--do-action hint (or action
                                     (assoc-default 'action source)
                                     pophint--default-action))))
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

(defun* pophint--get-hints (&key source direction not-highlight window)
  (pophint--debug "start get hints direction:[%s] not-highlight:[%s] window:[%s]\nsource:%s"
                  direction not-highlight window source)
      (loop with idx = 0
            with method = (assoc-default 'method source)
            with init = (assoc-default 'init source)
            with requires = (or (assoc-default 'requires source)
                                pophint:default-require-length)
            with re = (or (assoc-default 'regexp source)
                          pophint--default-search-regexp)
            with re = (cond ((symbolp re) (symbol-value re))
                            (t            re))
            with max = (case direction
                         (around (/ pophint:popup-max-tips 2))
                         (t      pophint:popup-max-tips))
            for srchfnc in (cond ((functionp method)
                                  (list method))
                                 ((and (listp method)
                                       (> (length method) 0))
                                  method)
                                 (t
                                  (case direction
                                    (forward  '(re-search-forward))
                                    (backward '(re-search-backward))
                                    (around   '(re-search-forward re-search-backward)))))
            append (with-selected-window (or (and (windowp window) window)
                                             (get-buffer-window))
                     (save-excursion
                       (loop initially (when (functionp init) (funcall init))
                             with cnt = 0
                             with ret
                             while (and (yaxception:$
                                          (yaxception:try
                                            (cond (method (pophint:hint-p (setq ret (funcall srchfnc))))
                                                  (t      (funcall srchfnc re nil t))))
                                          (yaxception:catch 'error e
                                            (pophint--error "failed seek next popup point : %s\n%s"
                                                            (yaxception:get-text e)
                                                            (yaxception:get-stack-trace-string e))))
                                        (< cnt max))
                             for mtext = (cond (method (pophint:hint-value ret))
                                               ((match-beginning 1) (match-string-no-properties 1))
                                               (t                   (match-string-no-properties 0)))
                             for mstartpt = (cond (method (pophint:hint-startpt ret))
                                                  (t      (or (match-beginning 1) (match-beginning 0))))
                             for mendpt = (cond (method (pophint:hint-endpt ret))
                                                (t      (or (match-end 1) (match-end 0))))
                             for found = (or method (<= requires (length mtext)))
                             for tiptext = (upcase (pophint--get-popup-text idx))
                             for tip = (when (and found
                                                  (not (string= tiptext "")))
                                         (popup-create mstartpt
                                                       (string-width tiptext)
                                                       1
                                                       :around nil
                                                       :margin-left 0
                                                       :margin-right 0
                                                       :face 'pophint:tip-face))
                             for ov = (when (and tip
                                                 (not not-highlight))
                                        (make-overlay mstartpt mendpt))
                             for hint = (when tip
                                          (make-pophint:hint :buffer (current-buffer)
                                                             :popup tip
                                                             :overlay ov
                                                             :startpt mstartpt
                                                             :endpt mendpt
                                                             :value mtext))
                             do (progn (when hint
                                         (popup-set-list tip (list tiptext))
                                         (popup-draw tip)
                                         (incf cnt)
                                         (incf idx)
                                         (pophint--trace "found hint. tip:[%s] text:[%s] startpt:[%s] endpt:[%s]"
                                                         tiptext mtext mstartpt mendpt)
                                         (when ov
                                           (overlay-put ov 'window (selected-window))
                                           (overlay-put ov 'face 'pophint:match-face))))
                             if hint collect hint)))))

(defun pophint--get-popup-text (idx)
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

(defun* pophint--event-loop (&key hints
                                  (inputed "")
                                  source
                                  sources
                                  action
                                  action-description
                                  not-highlight
                                  not-switch-direction
                                  not-switch-window
                                  window)
  (yaxception:$
    (yaxception:try
      (pophint--debug "start event loop. hints:[%s] inputed:[%s] not-highlight:[%s] not-switch-direction:[%s] not-switch-window:[%s] window:[%s] action-description:[%s]\naction:%s\nsource:%s\nsources:%s"
                      (length hints) inputed not-highlight not-switch-direction not-switch-window action-description action source sources)
      (if (= (length hints) 1)
          (pop hints)
        (let* ((not-switch-source (or (not (listp sources))
                                      (< (length sources) 2)))
               (not-switch-window (or not-switch-window
                                      (one-window-p)))
               (key (popup-menu-read-key-sequence nil
                                                  (pophint--get-read-prompt :count (length hints)
                                                                            :actdesc action-description
                                                                            :source source
                                                                            :sources sources
                                                                            :not-switch-direction not-switch-direction
                                                                            :not-switch-source not-switch-source
                                                                            :not-switch-window not-switch-window)))
               (gbinding (lookup-key (current-global-map) key))
               (binding (or (when (current-local-map)
                              (lookup-key (current-local-map) key))
                            gbinding)))
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
                ((eq gbinding 'self-insert-command)
                 (cond ((string-match key pophint:popup-chars)
                        (pophint--trace "inputed hintchar. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--event-loop :hints (loop for hint in hints
                                                          for tip = (pophint:hint-popup hint)
                                                          for tiptextlist = (popup-list tip)
                                                          for tiptext = (nth 0 tiptextlist)
                                                          if (and (string-match (concat "\\`" inputed (upcase key)) tiptext)
                                                                  (popup-live-p tip))
                                                          collect hint
                                                          else
                                                          do (pophint--delete hint))
                                             :inputed (concat inputed (upcase key))
                                             :source source
                                             :sources sources
                                             :action action
                                             :action-description action-description
                                             :not-highlight not-highlight
                                             :not-switch-direction not-switch-direction
                                             :not-switch-window not-switch-window))
                       ((and (string= key pophint:switch-direction-char)
                             (not not-switch-direction))
                        (pophint--trace "inputed switch direction. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--deletes hints)
                        (setq pophint--current-direction (case pophint--current-direction
                                                           (forward 'backward)
                                                           (backward 'around)
                                                           (around 'forward)))
                        (pophint:do :source source
                                    :sources sources
                                    :action action
                                    :action-description action-description
                                    :not-switch-window not-switch-window)
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
                                    :action-description action-description
                                    :not-switch-window not-switch-window))
                       ((and (string= key pophint:switch-window-char)
                             (not not-switch-window))
                        (pophint--trace "inputed switch window. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                        (pophint--deletes hints)
                        (let* ((nwindow (with-selected-window (or (and (windowp window) window)
                                                                  (get-buffer-window))
                                          (next-window)))
                               (nsources (pophint--get-available-sources nwindow)))
                          (pophint:do :source (when (member source nsources) source)
                                      :sources nsources
                                      :action action
                                      :action-description action-description
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
                                             :action-description action-description
                                             :not-highlight not-highlight
                                             :not-switch-direction not-switch-direction
                                             :not-switch-window not-switch-window))))
                ((commandp binding)
                 (pophint--trace "inputed cmd. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints)
                 (call-interactively binding)
                 nil)
                (t
                 (pophint--trace "inputed else. key:[%s] gbinding:[%s] binding:[%s]" key gbinding binding)
                 (pophint--deletes hints))))))
    (yaxception:catch 'error e
      (message "[PopHint] Failed pophint:do : %s" (yaxception:get-text e))
      (pophint--deletes hints)
      (pophint--error "failed event loop : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (pophint--log-open-log-if-debug))))

(defun* pophint--get-read-prompt (&key count
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
                                                (window-buffer window))
                                           (current-buffer))
                    pophint:sources)))
    (loop for src in pophint:global-sources
          do (add-to-list 'sources src t))
    (add-to-list 'sources pophint--default-source t)
    sources))

(defun pophint--deletes (hints)
  (dolist (hint hints)
    (pophint--delete hint))
  nil)

(defun pophint--delete (hint)
  (let* ((tip (pophint:hint-popup hint))
         (ov (pophint:hint-overlay hint)))
    (when ov
      (delete-overlay ov))
    (when tip
      (popup-delete tip))
    nil))

(defun pophint--do-action (hint action)
  (when (pophint:hint-p hint)
    (let* ((tip (pophint:hint-popup hint))
           (selected (pophint:hint-value hint))
           (action (cond ((functionp action)
                          action)
                         ((and (symbolp action)
                               (boundp action))
                          (symbol-value action))
                         (t
                          action))))
      (pophint--debug "start action. selected:[%s] action:%s" selected action)
      (when (popup-live-p tip) (pophint--delete hint))
      (when (functionp action) (funcall action hint)))))

(defun pophint--get-read-prompt-interactively ()
  (let* ((count 1)
         (acttext (loop with ret = ""
                        for k being the hash-keys in pophint--action-hash using (hash-values act)
                        for desc = (pophint:action-description act)
                        do (incf count)
                        do (setq ret (concat ret (format "'%s':%s " k desc)))
                        finally return ret)))
    (format "Select ch. Actions[%s] <RET>:Default %s" count acttext)))


(provide 'pophint)
;;; pophint.el ends here
