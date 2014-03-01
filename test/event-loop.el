(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "event-loop nil")
  (expect (mock (pophint--deletes *))
    (stub pophint--menu-read-key-sequence => nil)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))))

(expectations
  (desc "event-loop empty")
  (expect (mock (pophint--deletes *))
    (stub pophint--menu-read-key-sequence => (kbd ""))
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))))

(expectations
  (desc "event-loop quit")
  (expect (mock (keyboard-quit))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'keyboard-quit)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))))

(expectations
  (desc "event-loop ret")
  (expect nil
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'newline)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)
                                      (make-pophint:hint :value "fuga" :startpt 1 :endpt 2)))))

(expectations
  (desc "event-loop ret has inputed")
  (expect "hoge"
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'newline)
    (let* ((ret (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)
                                                  (make-pophint:hint :value "fuga" :startpt 1 :endpt 2))
                                     :inputed "q")))
      (and (pophint:hint-p ret)
           (pophint:hint-value ret)))))

(expectations
  (desc "event-loop delete-backward-char")
  (expect (mock (pophint:do :source '((regexp . "hoge"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :direction nil
                            :not-highlight 'hogehigh
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'delete-backward-char)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         :source '((regexp . "hoge"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop delete-backward-char not-switch-direction")
  (expect (mock (pophint:do :source '((regexp . "hoge"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :direction 'hogedirect
                            :not-highlight 'hogehigh
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'delete-backward-char)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         :source '((regexp . "hoge"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-switch-direction t
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop backward-delete-char-untabify")
  (expect (mock (pophint:do :source '((regexp . "hoge"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :direction nil
                            :not-highlight 'hogehigh
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'backward-delete-char-untabify)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         :source '((regexp . "hoge"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop switch direction")
  (expect (mock (pophint:do :source '((regexp . "hoge"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (setq pophint--current-direction 'around)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         :source '((regexp . "hoge"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop switch direction around")
  (expect 'forward
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (stub pophint:do => t)
    (setq pophint--current-direction 'around)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))
    pophint--current-direction))

(expectations
  (desc "event-loop switch direction forward")
  (expect 'backward
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (stub pophint:do => t)
    (setq pophint--current-direction 'forward)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))
    pophint--current-direction))

(expectations
  (desc "event-loop switch direction backward")
  (expect 'around
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (stub pophint:do => t)
    (setq pophint--current-direction 'backward)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))
    pophint--current-direction))

(expectations
  (desc "event-loop switch source not match")
  (expect (mock (pophint:do :source '((regexp . "fuga"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :direction nil
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "s"))
    (pophint--event-loop :source '((regexp . "hoge"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop switch source match")
  (expect (mock (pophint:do :source '((regexp . "bar"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :direction nil
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "s"))
    (pophint--event-loop :source '((regexp . "fuga"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop switch source direction")
  (expect (mock (pophint:do :source '((regexp . "bar"))
                            :sources '(((regexp . "fuga")) ((regexp . "bar")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :direction 'hogedirect
                            :window 'hogewnd
                            :not-switch-window 'hogeswwnd))
    (stub pophint--menu-read-key-sequence => (kbd "s"))
    (pophint--event-loop :source '((regexp . "fuga"))
                         :sources '(((regexp . "fuga")) ((regexp . "bar")))
                         :action '(lambda (hint) (message "hoge"))
                         :action-name "TestAct"
                         :direction 'hogedirect
                         :not-switch-direction t
                         :not-highlight 'hogehigh
                         :window 'hogewnd
                         :not-switch-window 'hogeswwnd)))

(expectations
  (desc "event-loop switch window not exist source")
  (expect (mock (pophint:do :source nil
                            :sources '(((regexp . "HOGEGE")) ((regexp . "FUGAGA")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :direction nil
                            :window *))
    (stub pophint--menu-read-key-sequence => (kbd "w"))
    (stub pophint--get-available-sources => '(((regexp . "HOGEGE")) ((regexp . "FUGAGA"))))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer-other-window "*Messages*")
      (pophint--event-loop :source '((regexp . "fuga"))
                           :sources '(((regexp . "fuga")) ((regexp . "bar")))
                           :action '(lambda (hint) (message "hoge"))
                           :action-name "TestAct"
                           :direction 'hogedirect
                           :not-highlight 'hogehigh))))

(expectations
  (desc "event-loop switch window exist source")
  (expect (mock (pophint:do :source '((regexp . "FUGAGA"))
                            :sources '(((regexp . "HOGEGE")) ((regexp . "FUGAGA")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :direction nil
                            :window *))
    (stub pophint--menu-read-key-sequence => (kbd "w"))
    (stub pophint--get-available-sources => '(((regexp . "HOGEGE")) ((regexp . "FUGAGA"))))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer-other-window "*Messages*")
      (pophint--event-loop :source '((regexp . "FUGAGA"))
                           :sources '(((regexp . "fuga")) ((regexp . "bar")))
                           :action '(lambda (hint) (message "hoge"))
                           :action-name "TestAct"
                           :direction 'hogedirect
                           :not-highlight 'hogehigh))))

(expectations
  (desc "event-loop switch window exist source")
  (expect (mock (pophint:do :source '((regexp . "FUGAGA"))
                            :sources '(((regexp . "HOGEGE")) ((regexp . "FUGAGA")))
                            :action '(lambda (hint) (message "hoge"))
                            :action-name "TestAct"
                            :not-highlight 'hogehigh
                            :direction 'hogedirect
                            :window *))
    (stub pophint--menu-read-key-sequence => (kbd "w"))
    (stub pophint--get-available-sources => '(((regexp . "HOGEGE")) ((regexp . "FUGAGA"))))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer-other-window "*Messages*")
      (pophint--event-loop :source '((regexp . "FUGAGA"))
                           :sources '(((regexp . "fuga")) ((regexp . "bar")))
                           :action '(lambda (hint) (message "hoge"))
                           :action-name "TestAct"
                           :direction 'hogedirect
                           :not-switch-direction t
                           :not-highlight 'hogehigh))))

(expectations
  (desc "event-loop some command")
  (expect (mock (call-interactively 'forward-char))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'forward-char)
    (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)))))

(expectations
  (desc "event-loop exit")
  (expect "hoge"
    (let* ((ret (pophint--event-loop :hints (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                                     :inputed "q")))
      (and (pophint:hint-p ret)
           (pophint:hint-value ret)))))

