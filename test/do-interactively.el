(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "do-interactively null")
  (expect nil
    (stub pophint--menu-read-key-sequence => nil)
    (pophint:do-interactively)))

(expectations
  (desc "do-interactively empty")
  (expect nil
    (stub pophint--menu-read-key-sequence => (kbd ""))
    (pophint:do-interactively)))

(expectations
  (desc "do-interactively quit")
  (expect (mock (message *))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'keyboard-quit)
    (pophint:do-interactively)))

(expectations
  (desc "do-interactively return")
  (expect (mock (pophint:do-flexibly *))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'newline)
    (pophint:do-interactively)))

(expectations
  (desc "do-interactively action")
  (expect (mock (pophint:do-flexibly :action '(lambda (hint) (message "test"))
                                     :action-name "test"))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (pophint:defaction :key "q"
                       :name "test"
                       :action (lambda (hint) (message "test")))
    (pophint:do-interactively)))

