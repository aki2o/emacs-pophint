(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "expand-function-symbol nil")
  (expect nil
    (pophint--expand-function-symbol nil)))

(expectations
  (desc "expand-function-symbol lambda")
  (expect '(lambda () (message ""))
    (and (functionp (lambda () (message "")))
         (pophint--expand-function-symbol (lambda () (message ""))))))

(expectations
  (desc "expand-function-symbol quoted lambda")
  (expect '(lambda () (message ""))
    (and (functionp '(lambda () (message "")))
         (pophint--expand-function-symbol '(lambda () (message ""))))))

(expectations
  (desc "expand-function-symbol variable")
  (expect '(lambda () (message ""))
    (let* ((var (lambda () (message ""))))
      (pophint--expand-function-symbol var))))

(expectations
  (desc "expand-function-symbol quoted variable")
  (expect '(lambda () (message ""))
    (let* ((var (lambda () (message ""))))
      (pophint--expand-function-symbol 'var))))

(expectations
  (desc "expand-function-symbol function")
  (expect 'hoge
    (defun hoge () (message ""))
    (and (functionp 'hoge)
         (pophint--expand-function-symbol 'hoge))))

(expectations
  (desc "expand-function-symbol lambda list")
  (expect '((lambda nil (message "a")) (lambda nil (message "b")))
    (pophint--expand-function-symbol '((lambda () (message "a"))
                                       (lambda () (message "b"))))))

(expectations
  (desc "expand-function-symbol variable list")
  (expect '((lambda nil (message "a")) (lambda nil (message "b")))
    (let* ((var '((lambda () (message "a"))
                  (lambda () (message "b")))))
      (pophint--expand-function-symbol var))))

(expectations
  (desc "expand-function-symbol quoted variable list")
  (expect '((lambda nil (message "a")) (lambda nil (message "b")))
    (let* ((var '((lambda () (message "a"))
                  (lambda () (message "b")))))
      (pophint--expand-function-symbol 'var))))

