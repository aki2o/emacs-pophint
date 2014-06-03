(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "make-index-char-string first")
  (expect "h"
    (pophint--make-index-char-string 0 "hjkl"))
  (desc "make-index-char-string next")
  (expect "j"
    (pophint--make-index-char-string 1 "hjkl"))
  (desc "make-index-char-string 2 length")
  (expect "kk"
    (pophint--make-index-char-string 10 "hjkl"))
  (desc "make-index-char-string 3 length")
  (expect "jkjh"
    (pophint--make-index-char-string 100 "hjkl"))
  (desc "make-index-char-string not char-list")
  (expect ""
    (pophint--make-index-char-string 1 ""))
  )

