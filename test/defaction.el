(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "defaction name")
  (expect "Hoge"
    (pophint:defaction :key "h"
                       :name "Hoge"
                       :action (lambda (hint) (message "hoge")))
    (let* ((ret (gethash "h" pophint--action-hash)))
      (and (pophint:action-p ret)
           (pophint:action-name ret)))))

(expectations
  (desc "defaction action")
  (expect '(lambda (hint) (message "hoge"))
    (pophint:defaction :key "h"
                       :name "Hoge"
                       :action (lambda (hint) (message "hoge")))
    (let* ((ret (gethash "h" pophint--action-hash)))
      (and (pophint:action-p ret)
           (pophint:action-action ret)))))

(expectations
  (desc "defaction function")
  (expect t
    (pophint:defaction :key "h"
                       :name "Hoge"
                       :action (lambda (hint) (message "hoge")))
    (fboundp 'pophint:do-flexibly-hoge)))

