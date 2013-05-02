(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "do get-hint not argument")
  (expect (mock (pophint--get-hints :source pophint--default-source
                                    :direction 'hogedirection
                                    :not-highlight t
                                    :window nil))
    (stub pophint--event-loop => nil)
    (let* ((pophint--current-direction 'hogedirection))
      (pophint:do))))

(expectations
  (desc "do get-hint has sources")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'hogedirection
                                    :not-highlight nil
                                    :window nil))
    (stub pophint--event-loop => nil)
    (let* ((pophint--current-direction 'hogedirection))
      (pophint:do :sources '(((regexp . "hoge"))
                             ((regexp . "fuga")))))))

(expectations
  (desc "do get-hint has argument")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :not-highlight t
                                    :window 'hogewnd))
    (stub pophint--event-loop => nil)
    (let* ((pophint--current-direction 'hogedirection))
      (pophint:do :source '((regexp . "hoge"))
                  :sources '(((regexp . "fuga"))
                             ((regexp . "bar")))
                  :direction 'around
                  :not-highlight t
                  :window 'hogewnd))))

(expectations
  (desc "do event-loop not argument")
  (expect (mock (pophint--event-loop :hints 'hogehints
                                     :source pophint--default-source
                                     :sources nil
                                     :action nil
                                     :action-name "Go/SrcAct"
                                     :not-highlight t
                                     :direction 'hogedirect
                                     :not-switch-direction nil
                                     :not-switch-window nil
                                     :window nil))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--do-action => nil)
    (setq pophint--current-direction 'hogedirect)
    (pophint:do)))

(expectations
  (desc "do event-loop has argument")
  (expect (mock (pophint--event-loop :hints 'hogehints
                                     :source '((regexp . "hoge"))
                                     :sources '(((regexp . "fuga")) ((regexp . "bar")))
                                     :action '(lambda (hint) (message "hoge"))
                                     :action-name "Test"
                                     :not-highlight t
                                     :direction 'around
                                     :not-switch-direction t
                                     :not-switch-window t
                                     :window (next-window)))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--do-action => nil)
    (pophint:do :source '((regexp . "hoge"))
                :sources '(((regexp . "fuga"))
                           ((regexp . "bar")))
                :action '(lambda (hint) (message "hoge"))
                :action-name "Test"
                :direction 'around
                :not-highlight t
                :window (next-window)
                :not-switch-window t)))

(expectations
  (desc "do do-action not argument")
  (expect (mock (pophint--do-action 'hogehint pophint--default-action))
    (stub pophint--get-hints => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do)))

(expectations
  (desc "do do-action not has action source")
  (expect (mock (pophint--do-action 'hogehint pophint--default-action))
    (stub pophint--get-hints => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")))))

(expectations
  (desc "do do-action has action source")
  (expect (mock (pophint--do-action 'hogehint '(lambda (hint) (message "hoge"))))
    (stub pophint--get-hints => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")
                          (action . (lambda (hint) (message "hoge")))))))

(expectations
  (desc "do do-action has action and has action source")
  (expect (mock (pophint--do-action 'hogehint '(lambda (hint) (message "fuga"))))
    (stub pophint--get-hints => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")
                          (action . (lambda (hint) (message "hoge"))))
                :action '(lambda (hint) (message "fuga")))))

