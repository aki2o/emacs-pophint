(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "do-flexibly not argument")
  (expect (mock (pophint:do :source nil
                            :sources '(pophint--default-source)
                            :action nil
                            :action-name nil
                            :window nil))
    (stub pophint--get-available-sources => '(pophint--default-source))
    (let* ((pophint--last-source)
           (pophint--last-window))
      (pophint:do-flexibly))))

(expectations
  (desc "do-flexibly has argument")
  (expect (mock (pophint:do :source nil
                            :sources '(pophint--default-source)
                            :action '(lambda (hint) (message "test"))
                            :action-name "It'sTest"
                            :window 'hogewindow))
    (stub pophint--get-available-sources => '(pophint--default-source))
    (let* ((pophint--last-source)
           (pophint--last-window))
      (pophint:do-flexibly :action '(lambda (hint) (message "test"))
                           :action-name "It'sTest"
                           :window 'hogewindow))))

