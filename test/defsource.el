(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "defsource have not shown")
  (expect "hoge"
    (pophint:defsource :name "hoge"
                       :source '((regexp . "^hoge$")))
    (and (boundp 'pophint:source-hoge)
         (fboundp 'pophint:do-hoge)
         (listp pophint:source-hoge)
         (assoc-default 'shown pophint:source-hoge))))

(expectations
  (desc "defsource has shown")
  (expect "bar"
    (pophint:defsource :name "fuga"
                       :source '((shown . "bar")
                                 (regexp . "^fuga$")))
    (and (boundp 'pophint:source-fuga)
         (fboundp 'pophint:do-fuga)
         (listp pophint:source-fuga)
         (assoc-default 'shown pophint:source-fuga))))

(expectations
  (desc "defsource has space in name")
  (expect "this is  test"
    (pophint:defsource :name "this is  test"
                       :description ""
                       :source '((regexp . "^ThisIsTest$")))
    (and (boundp 'pophint:source-this-is-test)
         (fboundp 'pophint:do-this-is-test)
         (listp pophint:source-this-is-test)
         (assoc-default 'shown pophint:source-this-is-test))))

