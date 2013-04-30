(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "get-read-prompt-interactively default")
  (expect "Select ch. Actions[1] <RET>:Default "
    (setq pophint--action-hash (make-hash-table :test 'equal))
    (pophint--get-read-prompt-interactively)))

(expectations
  (desc "get-read-prompt-interactively 1 action")
  (expect "Select ch. Actions[2] <RET>:Default 'y':Yank "
    (pophint:defaction :key "y"
                       :name "Yank"
                       :action '(lambda (hint) (yank)))
    (pophint--get-read-prompt-interactively)))

(expectations
  (desc "get-read-prompt-interactively 2 action")
  (expect "Select ch. Actions[3] <RET>:Default 'y':Yank 'T':Test of PopHint "
    (pophint:defaction :key "T"
                       :name "Test of PopHint"
                       :action '(lambda (hint) (message "test")))
    (pophint--get-read-prompt-interactively)))

