(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "get-read-prompt default")
  (expect "Select ch. Hints[0] Act[] 's':SwitchSrc() 'd':SwitchDrct(around|forward|backward) 'w':SwitchWnd "
    (pophint--get-read-prompt)))

(expectations
  (desc "get-read-prompt has argument")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] 's':SwitchSrc(hoge|fuga|bar) 'd':SwitchDrct(around|forward|backward) 'w':SwitchWnd "
    (pophint--get-read-prompt :count 789
                              :actdesc "Go/SrcAct"
                              :source '((shown . "hoge"))
                              :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar"))))))

(expectations
  (desc "get-read-prompt has not shown source")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] 's':SwitchSrc(hoge|NONAME|bar) 'd':SwitchDrct(around|forward|backward) 'w':SwitchWnd "
    (pophint--get-read-prompt :count 789
                              :actdesc "Go/SrcAct"
                              :source '((regexp . "hoge"))
                              :sources '(((shown . "hoge")) ((regexp . "fuga")) ((shown . "bar"))))))

(expectations
  (desc "get-read-prompt not-switch-source")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] Src[hoge] 'd':SwitchDrct(around|forward|backward) 'w':SwitchWnd "
    (pophint--get-read-prompt :count 789
                              :actdesc "Go/SrcAct"
                              :source '((shown . "hoge"))
                              :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                              :not-switch-source t)))

(expectations
  (desc "get-read-prompt not-switch-source and has not shown")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] Src[NONAME] 'd':SwitchDrct(around|forward|backward) 'w':SwitchWnd "
    (pophint--get-read-prompt :count 789
                              :actdesc "Go/SrcAct"
                              :source '((regexp . "hoge"))
                              :sources '(((shown . "hoge")) ((regexp . "fuga")) ((shown . "bar")))
                              :not-switch-source t)))

(expectations
  (desc "get-read-prompt not-switch-direction")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] 's':SwitchSrc(hoge|fuga|bar) 'w':SwitchWnd "
    (pophint--get-read-prompt :count 789
                              :actdesc "Go/SrcAct"
                              :source '((shown . "hoge"))
                              :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                              :not-switch-direction t)))

(expectations
  (desc "get-read-prompt not-switch-window")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] 's':SwitchSrc(hoge|fuga|bar) 'd':SwitchDrct(around|forward|backward) "
    (pophint--get-read-prompt :count 789
                              :actdesc "Go/SrcAct"
                              :source '((shown . "hoge"))
                              :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                              :not-switch-window t)))


