(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "defsituation")
  (expect t
    (pophint:defsituation hoge)
    (commandp 'pophint:do-situationally-hoge))
  )

