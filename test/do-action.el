(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "do-action not hint")
  (expect nil
    (pophint--do-action nil '(lambda (hint) (forward-char)))))

(expectations
  (desc "do-action hint")
  (expect (mock (forward-char))
    (pophint--do-action (make-pophint:hint) '(lambda (hint) (forward-char)))))

