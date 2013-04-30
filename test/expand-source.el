(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "expand-source symbol")
  (expect '((shown . "hoge") (regexp . "\\`hoge\\'"))
    (pophint:defsource :name "hoge"
                       :source '((regexp . "\\`hoge\\'")))
    (pophint--expand-source 'pophint:source-hoge)))

(expectations
  (desc "expand-source alist")
  (expect '((shown . "hoge") (regexp . "\\`hoge\\'"))
    (pophint:defsource :name "hoge"
                       :source '((regexp . "\\`hoge\\'")))
    (pophint--expand-source pophint:source-hoge)))

(expectations
  (desc "expand-source list of symbol")
  (expect '(((shown . "hoge") (regexp . "\\`hoge\\'")))
    (pophint:defsource :name "hoge"
                       :source '((regexp . "\\`hoge\\'")))
    (setq pophint:sources '(pophint:source-hoge))
    (pophint--expand-sources pophint:sources)))

(expectations
  (desc "expand-source nil")
  (expect nil
    (pophint--expand-source nil)))

(expectations
  (desc "expand-source symbol of nil")
  (expect nil
    (setq hoge nil)
    (pophint--expand-source 'hoge)))

