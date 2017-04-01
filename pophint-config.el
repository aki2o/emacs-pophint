;;; pophint-config.el --- provide configuration for pophint.el.

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-pophint
;; Version: 1.0.0

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


(require 'cl-lib)


(defvar pophint-config:features
  (cl-loop for f in (directory-files (file-name-directory (locate-library "pophint")))
           if (and (string-match "\\`pophint-config--" f)
                   (string-match "\\.el\\'" f)
                   (not (string-match "-autoloads\\.el\\'" f))
                   (not (string= f "pophint-config---util.el")))
           collect (file-name-sans-extension f)))


;; (dolist (feat pophint-config:features)
;;   (eval-after-load feat
;;     `(progn
;;        (when pophint-config:run-defcommand-exhaustively-after-load
;;          (pophint:defcommand-exhaustively :feature ,feat)))))


(or (require 'pophint-autoloads nil t)
    (progn
      (require 'pophint-config---util)
      (mapc 'require pophint-config:features)))


(provide 'pophint-config)
;;; pophint-config.el ends here
