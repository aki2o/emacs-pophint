(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "do get-hint not argument")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do)))
  (desc "do get-hint has sources")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :sources '(((regexp . "hoge"))
                                                     ((regexp . "fuga")))
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do :sources '(((regexp . "hoge"))
                             ((regexp . "fuga"))))))
  (desc "do get-hint has source")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source '((regexp . "fuga"))
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t
                                          :not-switch-source t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do :source '((regexp . "fuga")))))
  (desc "do get-hint has sources/source")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :sources '(((regexp . "fuga"))
                                                     ((regexp . "bar")))
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do :source '((regexp . "hoge"))
                  :sources '(((regexp . "fuga"))
                             ((regexp . "bar"))))))
  (desc "do get-hint not-switch-direction when not pophint:switch-direction-p")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-direction t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do)))
  (desc "do get-hint use current direction when pophint:switch-direction-p")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'forward
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (stub pophint:get-current-direction => 'forward)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do)))
  (desc "do get-hint has direction")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'hogedirection
                                          :allwindow t
                                          :not-switch-direction t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do :direction 'hogedirection)))
  (desc "do get-hint has window")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :window 'hogewnd
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (setq pophint--last-condition nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do :window 'hogewnd)))
  (desc "do get-hint in single window when not pophint:do-allwindow-p")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint:select-source-method nil))
      (pophint:do)))
  (desc "do get-hint in multi window when not pophint:do-allwindow-p")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint:select-source-method nil))
      (split-window)
      (pophint:do)))
  (desc "do get-hint in multi window when pophint:do-allwindow-p")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do)))
  (desc "do get-hint pophint--enable-allwindow-p")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint--enable-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do)))
  (desc "do get-hint allwindow option")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :allwindow t
                                          :not-switch-window t)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint:select-source-method nil))
      (pophint:do :allwindow t)))
  (desc "do get-hint allwindow option")
  (expect (mock (pophint--get-hints
                 (make-pophint--condition :source pophint--default-source
                                          :action-name pophint--default-action-name
                                          :direction 'around)))
    (stub pophint--event-loop => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint--disable-allwindow-p t)
          (pophint:select-source-method nil))
      (pophint:do :allwindow t)))
  )


(expectations
  (desc "do event-loop not argument")
  (expect (mock (pophint--event-loop
                 'hogehints
                 (make-pophint--condition :source pophint--default-source
                                          :sources nil
                                          :action nil
                                          :action-name pophint--default-action-name
                                          :direction 'around
                                          :not-switch-direction t
                                          :not-switch-window t)))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--show-hint-tips => nil)
    (stub pophint--do-action => nil)
    (let ((pophint:switch-direction-p nil)
          (pophint:do-allwindow-p nil))
      (delete-other-windows)
      (pophint:do)))
  (desc "do event-loop has argument")
  (expect (mock (pophint--event-loop
                 'hogehints
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :sources '(((regexp . "fuga"))
                                                     ((regexp . "bar")))
                                          :action '(lambda (hint) (message "hoge"))
                                          :action-name "Test"
                                          :direction 'hogedirection
                                          :window 'hogewnd
                                          :not-switch-direction t
                                          :not-switch-window t)))
    (stub pophint--get-hints => 'hogehints)
    (stub pophint--show-hint-tips => nil)
    (stub pophint--do-action => nil)
    (let ((pophint:switch-direction-p t)
          (pophint:do-allwindow-p nil)
          (pophint:select-source-method nil))
      (split-window)
      (pophint:do :source '((regexp . "hoge"))
                  :sources '(((regexp . "fuga"))
                             ((regexp . "bar")))
                  :action '(lambda (hint) (message "hoge"))
                  :action-name "Test"
                  :direction 'hogedirection
                  :window 'hogewnd
                  :not-switch-window t)))
  )


(expectations
  (desc "do do-action not argument")
  (expect (mock (pophint--do-action 'hogehint pophint--default-action))
    (stub pophint--get-hints => nil)
    (stub pophint--show-hint-tips => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do))
  (desc "do do-action not has action source")
  (expect (mock (pophint--do-action 'hogehint pophint--default-action))
    (stub pophint--get-hints => nil)
    (stub pophint--show-hint-tips => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge"))))
  (desc "do do-action has action source")
  (expect (mock (pophint--do-action 'hogehint '(lambda (hint) (message "hoge"))))
    (stub pophint--get-hints => nil)
    (stub pophint--show-hint-tips => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")
                          (action . (lambda (hint) (message "hoge"))))))
  (desc "do do-action has action and has action source")
  (expect (mock (pophint--do-action 'hogehint '(lambda (hint) (message "fuga"))))
    (stub pophint--get-hints => nil)
    (stub pophint--show-hint-tips => nil)
    (stub pophint--event-loop => 'hogehint)
    (pophint:do :source '((regexp . "hoge")
                          (action . (lambda (hint) (message "hoge"))))
                :action '(lambda (hint) (message "fuga"))))
  )

