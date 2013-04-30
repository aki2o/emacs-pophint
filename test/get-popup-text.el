(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "get-popup-text first")
  (expect "h"
    (setq pophint:popup-chars "hjkl")
    (pophint--get-popup-text 0)))

(expectations
  (desc "get-popup-text next")
  (expect "j"
    (setq pophint:popup-chars "hjkl")
    (pophint--get-popup-text 1)))

(expectations
  (desc "get-popup-text 2 length")
  (expect "kk"
    (setq pophint:popup-chars "hjkl")
    (pophint--get-popup-text 10)))

(expectations
  (desc "get-popup-text 3 length")
  (expect "jkjh"
    (setq pophint:popup-chars "hjkl")
    (pophint--get-popup-text 100)))

(expectations
  (desc "get-popup-text")
  (expect ""
    (setq pophint:popup-chars "")
    (pophint--get-popup-text 1)))

