(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "defsource have not shown")
  (expect "hoge"
    (let ((pophint:dedicated-sources nil))
      (pophint:defsource :name "hoge"
                         :source '((regexp . "^hoge$")))
      (and (boundp 'pophint:source-hoge)
           (listp pophint:source-hoge)
           (commandp 'pophint:do-hoge)
           (not pophint:dedicated-sources)
           (assoc-default 'shown pophint:source-hoge))))
  (desc "defsource has shown")
  (expect "bar"
    (pophint:defsource :name "fuga"
                       :source '((shown . "bar")
                                 (regexp . "^fuga$")))
    (and (boundp 'pophint:source-fuga)
         (listp pophint:source-fuga)
         (commandp 'pophint:do-fuga)
         (assoc-default 'shown pophint:source-fuga)))
  (desc "defsource has space in name")
  (expect "this is  test"
    (pophint:defsource :name "this is  test"
                       :description ""
                       :source '((regexp . "^ThisIsTest$")))
    (and (boundp 'pophint:source-this-is-test)
         (listp pophint:source-this-is-test)
         (commandp 'pophint:do-this-is-test)
         (assoc-default 'shown pophint:source-this-is-test)))
  (desc "defsource have dedicated")
  (expect '(pophint:source-test-dedicated)
    (let ((pophint:dedicated-sources nil))
      (pophint:defsource :name "test dedicated"
                         :source '((regexp . "^Dedicated$")
                                   (dedicated . bar)))
      (and (boundp 'pophint:source-test-dedicated)
           (listp pophint:source-test-dedicated)
           (commandp 'pophint:do-test-dedicated)
           pophint:dedicated-sources)))
  )

