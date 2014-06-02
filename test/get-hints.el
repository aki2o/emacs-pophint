(require 'pophint)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-hints around")
  (expect 3
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" t t))
           (buff (progn (tenv-update-file tfile t
                                          "This is a hoge.\n"
                                          "This is a fuga.\n"
                                          "This is a bar.\n")
                        (find-file-noselect tfile)))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+[a-z]+"))
                                              :direction 'around))))))
      (length ret)))
  (desc "get-hints forward")
  (expect 2
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (buff (find-file-noselect tfile))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+[a-z]+"))
                                              :direction 'forward))))))
      (length ret)))
  (desc "get-hints backward")
  (expect 1
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (buff (find-file-noselect tfile))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+[a-z]+"))
                                              :direction 'backward))))))
      (length ret)))
  (desc "get-hints value startpt endpt")
  (expect '("a hoge" 9 15)
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (buff (find-file-noselect tfile))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+[a-z]+"))
                                              :direction 'backward)))))
           (hint (pop ret)))
      (and (pophint:hint-p hint)
           (list (pophint:hint-value hint)
                 (pophint:hint-startpt hint)
                 (pophint:hint-endpt hint)))))
  (desc "get-hints has group regexp")
  (expect '("hoge" 11 15)
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (buff (find-file-noselect tfile))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+\\([a-z]+\\)"))
                                              :direction 'backward)))))
           (hint (pop ret)))
      (and (pophint:hint-p hint)
           (list (pophint:hint-value hint)
                 (pophint:hint-startpt hint)
                 (pophint:hint-endpt hint)))))
  (desc "get-hints use method")
  (expect '("fuga" 2 7)
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (buff (find-file-noselect tfile))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+\\([a-z]+\\)")
                                                        (method . (lambda ()
                                                                    (when (re-search-backward "hoge" nil t)
                                                                      (make-pophint:hint :value "fuga"
                                                                                         :startpt 2
                                                                                         :endpt 7)))))
                                              :direction 'backward)))))
           (hint (pop ret)))
      (and (pophint:hint-p hint)
           (list (pophint:hint-value hint)
                 (pophint:hint-startpt hint)
                 (pophint:hint-endpt hint)))))
  (desc "get-hints window")
  (expect 0
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (tfile2 (tenv-get-tmp-file "pophint" "test2.txt" t t))
           (buff (find-file-noselect tfile))
           (buff2 (find-file-noselect tfile2))
           (ret (save-window-excursion
                  (switch-to-buffer-other-window buff2)
                  (switch-to-buffer-other-window buff)
                  (with-current-buffer buff
                    (goto-char (point-min))
                    (forward-line)
                    (pophint--get-hints
                     (make-pophint--condition :source '((regexp . "a\\s-+\\([a-z]+\\)"))
                                              :direction 'backward
                                              :window (get-buffer-window buff2)))))))
      (length ret)))
  (desc "get-hints popup created")
  (expect t
    (let* ((tfile (tenv-get-tmp-file "pophint" "test.txt" nil nil))
           (tfile2 (tenv-get-tmp-file "pophint" "test2.txt" t t))
           (buff (find-file-noselect tfile))
           (buff2 (find-file-noselect tfile2))
           (hints (save-window-excursion
                    (switch-to-buffer-other-window buff2)
                    (switch-to-buffer-other-window buff)
                    (with-current-buffer buff
                      (goto-char (point-min))
                      (forward-line)
                      (let ((hints (pophint--get-hints
                                    (make-pophint--condition :source '((regexp . "a\\s-+\\([a-z]+\\)"))
                                                             :direction 'backward))))
                        (pophint--show-hint-tips hints nil)
                        hints))))
           (hint (pop hints)))
      (and (pophint:hint-p hint)
           (popup-p (pophint:hint-popup hint)))))
  )

