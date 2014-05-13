(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "do get-hint not argument")
  (expect (mock (pophint--get-hints :source '((shown . "Default") (regexp . pophint--default-search-regexp) (requires . 1) (highlight))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do)))
  (desc "do get-hint has sources")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :sources '(((regexp . "hoge"))
                             ((regexp . "fuga"))))))
  (desc "do get-hint has source")
  (expect (mock (pophint--get-hints :source '((regexp . "fuga"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "fuga")))))
  (desc "do get-hint has sources/source")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge"))
                  :sources '(((regexp . "fuga"))
                             ((regexp . "bar"))))))
  (desc "do get-hint auto set around direction when not pophint:switch-direction-p")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil)
          (pophint--current-direction 'hogedirection))
      (pophint:do :source '((regexp . "hoge")))))
  (desc "do get-hint use current direction when pophint:switch-direction-p")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'hogedirection
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint--current-direction 'hogedirection))
      (pophint:do :source '((regexp . "hoge")))))
  (desc "do get-hint has direction")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'hogedirection
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil)
          (pophint--current-direction 'hogedirection))
      (pophint:do :source '((regexp . "hoge"))
                  :direction 'hogedirection)))
  (desc "do get-hint has not-highlight")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge"))
                  :not-highlight t)))
  (desc "do get-hint has nil value of highlight option")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge") (highlight))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge") (highlight)))))
  (desc "do get-hint has t value of highlight option")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge") (highlight . t))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge") (highlight . t)))))
  (desc "do get-hint has window")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window 'hogewnd
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge"))
                  :window 'hogewnd)))
  (desc "do get-hint active allwindow when pophint:do-allwindow-p and multi windows")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow t))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p t))
      (split-window)
      (pophint:do :source '((regexp . "hoge")))))
  (desc "do get-hint active allwindow when enable allwindow and multi windows")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow t))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil)
          (pophint--enable-allwindow-p t))
      (pophint:do :source '((regexp . "hoge")))))
  (desc "do get-hint active allwindow when allwindow option and multi windows")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow t))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge"))
                  :allwindow t)))
  (desc "do get-hint deactive allwindow when disable allwindow")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p t)
          (pophint--disable-allwindow-p t))
      (pophint:do :source '((regexp . "hoge")))))
  (desc "do get-hint deactive allwindow when window option")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window 'hogewnd
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p t))
      (pophint:do :source '((regexp . "hoge"))
                  :window 'hogewnd)))
  (desc "do get-hint deactive allwindow when not-switch-window option")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p t))
      (pophint:do :source '((regexp . "hoge"))
                  :not-switch-window t)))
  (desc "do get-hint deactive allwindow when single window")
  (expect (mock (pophint--get-hints :source '((regexp . "hoge"))
                                    :direction 'around
                                    :window nil
                                    :allwindow nil))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p t))
      (delete-other-windows)
      (pophint:do :source '((regexp . "hoge")))))
  )


(expectations
  (desc "do event-loop not argument")
  (expect (mock (pophint--event-loop :hints 'hogehints
                                     :source '((shown . "Default") (regexp . pophint--default-search-regexp) (requires . 1) (highlight))
                                     :sources nil
                                     :action nil
                                     :action-name "Go/SrcAct"
                                     :not-highlight t
                                     :direction 'around
                                     :not-switch-direction t
                                     :not-switch-window t
                                     :window nil
                                     :allwindow nil))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--show-tip => nil)
    (stub pophint--do-action => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (delete-other-windows)
      (pophint:do)))
  (desc "do event-loop has argument")
  (expect (mock (pophint--event-loop :hints 'hogehints
                                     :source '((regexp . "hoge"))
                                     :sources '(((regexp . "fuga")) ((regexp . "bar")))
                                     :action '(lambda (hint) (message "hoge"))
                                     :action-name "Test"
                                     :not-highlight nil
                                     :direction 'hogedirection
                                     :not-switch-direction t
                                     :not-switch-window t
                                     :window 'hogewnd
                                     :allwindow nil))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--show-tip => nil)
    (stub pophint--do-action => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil))
      (split-window)
      (pophint:do :source '((regexp . "hoge"))
                  :sources '(((regexp . "fuga")) ((regexp . "bar")))
                  :action '(lambda (hint) (message "hoge"))
                  :action-name "Test"
                  :direction 'hogedirection
                  :window 'hogewnd
                  :not-switch-window t)))
  (desc "do event-loop change argument by config")
  (expect (mock (pophint--event-loop :hints 'hogehints
                                     :source '((regexp . "hoge"))
                                     :sources nil
                                     :action nil
                                     :action-name "Go/SrcAct"
                                     :not-highlight nil
                                     :direction 'around
                                     :not-switch-direction nil
                                     :not-switch-window nil
                                     :window nil
                                     :allwindow nil))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--show-tip => nil)
    (stub pophint--do-action => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil))
      (pophint:do :source '((regexp . "hoge"))))))


(expectations
  (desc "do do-action not argument")
  (expect (mock (pophint--do-action 'hogehint pophint--default-action))
    (stub pophint--get-hints => nil)
    (stub pophint--show-tip => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do))
  (desc "do do-action not has action source")
  (expect (mock (pophint--do-action 'hogehint pophint--default-action))
    (stub pophint--get-hints => nil)
    (stub pophint--show-tip => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge"))))
  (desc "do do-action has action source")
  (expect (mock (pophint--do-action 'hogehint '(lambda (hint) (message "hoge"))))
    (stub pophint--get-hints => nil)
    (stub pophint--show-tip => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")
                          (action . (lambda (hint) (message "hoge"))))))
  (desc "do do-action has action and has action source")
  (expect (mock (pophint--do-action 'hogehint '(lambda (hint) (message "fuga"))))
    (stub pophint--get-hints => nil)
    (stub pophint--show-tip => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")
                          (action . (lambda (hint) (message "hoge"))))
                :action '(lambda (hint) (message "fuga")))))

