(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "event-loop nil")
  (expect (mock (pophint--deletes *))
    (stub pophint--menu-read-key-sequence => nil)
    (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         (make-pophint--condition)))
  (desc "event-loop empty")
  (expect (mock (pophint--deletes *))
    (stub pophint--menu-read-key-sequence => (kbd ""))
    (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         (make-pophint--condition)))
  (desc "event-loop quit")
  (expect (mock (keyboard-quit))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'keyboard-quit)
    (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         (make-pophint--condition)))
  (desc "event-loop return first hint no return from no input")
  (expect nil
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'newline)
    (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)
                               (make-pophint:hint :value "fuga" :startpt 1 :endpt 2))
                         (make-pophint--condition)))
  (desc "event-loop return first hint return when inputed")
  (expect "hoge"
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'newline)
    (let* ((ret (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2)
                                           (make-pophint:hint :value "fuga" :startpt 1 :endpt 2))
                                     (make-pophint--condition)
                                     "q")))
      (and (pophint:hint-p ret)
           (pophint:hint-value ret))))
  (desc "event-loop restart hint")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "hoge")))))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'delete-backward-char)
    (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         (make-pophint--condition :source '((regexp . "hoge")))))
  (desc "event-loop switch direction from forward")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :direction 'backward)))
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (pophint--event-loop nil
                         (make-pophint--condition :source '((regexp . "hoge"))
                                                  :direction 'forward)))
  (desc "event-loop switch direction from backward")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :direction 'around)))
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (pophint--event-loop nil
                         (make-pophint--condition :source '((regexp . "hoge"))
                                                  :direction 'backward)))
  (desc "event-loop switch direction from around")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :direction 'forward)))
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (pophint--event-loop nil
                         (make-pophint--condition :source '((regexp . "hoge"))
                                                  :direction 'around)))
  (desc "event-loop switch direction from else")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "hoge"))
                                          :direction 'around)))
    (stub pophint--menu-read-key-sequence => (kbd "d"))
    (pophint--event-loop nil
                         (make-pophint--condition :source '((regexp . "hoge")))))
  ;; (desc "event-loop switch window not exist source")
  ;; (expect (mock (pophint--let-user-select
  ;;                (make-pophint--condition :source nil
  ;;                                         :sources '(((regexp . "HOGEGE"))
  ;;                                                    ((regexp . "FUGAGA")))
  ;;                                         :window 'hogewnd)))
  ;;   (stub pophint--menu-read-key-sequence => (kbd "w"))
  ;;   (stub next-window => 'hogewnd)
  ;;   (stub pophint--get-available-sources => '(((regexp . "HOGEGE"))
  ;;                                             ((regexp . "FUGAGA"))))
  ;;   (save-window-excursion
  ;;     (delete-other-windows)
  ;;     (switch-to-buffer-other-window "*Messages*")
  ;;     (pophint--event-loop nil
  ;;                          (make-pophint--condition :source '((regexp . "fuga"))
  ;;                                                   :sources '(((regexp . "fuga"))
  ;;                                                              ((regexp . "bar")))))))
  (desc "event-loop switch window exist source")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "FUGAGA"))
                                          :sources '(((regexp . "HOGEGE"))
                                                     ((regexp . "FUGAGA")))
                                          :window 'hogewnd)))
    (stub pophint--menu-read-key-sequence => (kbd "w"))
    (stub next-window => 'hogewnd)
    (stub pophint--get-available-sources => '(((regexp . "HOGEGE"))
                                              ((regexp . "FUGAGA"))))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer-other-window "*Messages*")
      (pophint--event-loop nil
                           (make-pophint--condition :source '((regexp . "FUGAGA"))
                                                    :sources '(((regexp . "fuga"))
                                                               ((regexp . "bar")))))))
  (desc "event-loop switch source not match")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "fuga"))
                                          :sources '(((regexp . "fuga"))
                                                     ((regexp . "bar"))))))
    (stub pophint--menu-read-key-sequence => (kbd "s"))
    (let ((pophint:select-source-method nil)
          (pophint:switch-source-delay nil))
      (pophint--event-loop nil
                           (make-pophint--condition :source '((regexp . "hoge"))
                                                    :sources '(((regexp . "fuga"))
                                                               ((regexp . "bar")))))))
  (desc "event-loop switch source match")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "bar"))
                                          :sources '(((regexp . "fuga"))
                                                     ((regexp . "bar"))))))
    (stub pophint--menu-read-key-sequence => (kbd "s"))
    (let ((pophint:select-source-method nil)
          (pophint:switch-source-delay nil))
      (pophint--event-loop nil
                           (make-pophint--condition :source '((regexp . "fuga"))
                                                    :sources '(((regexp . "fuga"))
                                                               ((regexp . "bar")))))))
  (desc "event-loop select source use-source-char")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "bar") (selector . "2"))
                                          :sources '(((regexp . "fuga") (selector . "1"))
                                                     ((regexp . "bar") (selector . "2"))))))
    (stub pophint--menu-read-key-sequence => (kbd "2"))
    (let ((pophint:select-source-method 'use-source-char))
      (pophint--event-loop nil
                           (make-pophint--condition :source '((regexp . "hoge"))
                                                    :sources '(((regexp . "fuga") (selector . "1"))
                                                               ((regexp . "bar") (selector . "2")))))))
  (desc "event-loop select source use-popup-char")
  (expect (mock (pophint--let-user-select
                 (make-pophint--condition :source '((regexp . "fuga") (selector . "H"))
                                          :sources '(((regexp . "fuga") (selector . "H"))
                                                     ((regexp . "bar") (selector . "J"))))))
    (stub pophint--menu-read-key-sequence => (kbd "h"))
    (let ((pophint:select-source-method 'use-popup-char))
      (pophint--event-loop nil
                           (make-pophint--condition :source '((regexp . "hoge"))
                                                    :sources '(((regexp . "fuga") (selector . "H"))
                                                               ((regexp . "bar") (selector . "J"))))
                           ""
                           t)))
  (desc "event-loop some command")
  (expect (mock (call-interactively 'forward-char))
    (stub pophint--menu-read-key-sequence => (kbd "q"))
    (stub lookup-key => 'forward-char)
    (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                         (make-pophint--condition)))
  (desc "event-loop detect hint")
  (expect "hoge"
    (let* ((ret (pophint--event-loop (list (make-pophint:hint :value "hoge" :startpt 1 :endpt 2))
                                     (make-pophint--condition)
                                     "q")))
      (and (pophint:hint-p ret)
           (pophint:hint-value ret))))
  )

