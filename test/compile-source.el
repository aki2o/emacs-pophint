(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "compile-source symbol")
  (expect '((shown . "hoge") (regexp . "\\`hoge\\'"))
    (pophint:defsource :name "hoge"
                       :source '((regexp . "\\`hoge\\'")))
    (pophint--compile-source 'pophint:source-hoge))
  (desc "compile-source alist")
  (expect '((shown . "hoge") (regexp . "\\`hoge\\'"))
    (pophint:defsource :name "hoge"
                       :source '((regexp . "\\`hoge\\'")))
    (pophint--compile-source pophint:source-hoge))
  (desc "compile-source list of symbol")
  (expect '(((shown . "hoge") (regexp . "\\`hoge\\'")))
    (pophint:defsource :name "hoge"
                       :source '((regexp . "\\`hoge\\'")))
    (setq pophint:sources '(pophint:source-hoge))
    (pophint--compile-sources pophint:sources))
  (desc "compile-source nil")
  (expect nil
    (pophint--compile-source nil))
  (desc "compile-source symbol of nil")
  (expect nil
    (setq hoge nil)
    (pophint--compile-source 'hoge))
  )

