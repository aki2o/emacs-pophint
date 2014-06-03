(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "make-prompt-interactively default")
  (expect "Select ch. Actions[1] <RET>:Default "
    (setq pophint--action-hash (make-hash-table :test 'equal))
    (pophint--make-prompt-interactively))
  (desc "make-prompt-interactively 1 action")
  (expect "Select ch. Actions[2] <RET>:Default y:Yank "
    (pophint:defaction :key "y"
                       :name "Yank"
                       :action '(lambda (hint) (yank)))
    (pophint--make-prompt-interactively))
  (desc "make-prompt-interactively 2 action")
  (expect "Select ch. Actions[3] <RET>:Default y:Yank T:Test of PopHint "
    (pophint:defaction :key "T"
                       :name "Test of PopHint"
                       :action '(lambda (hint) (message "test")))
    (pophint--make-prompt-interactively))
  )

