(require 'pophint)
(require 'el-expectations)

(expectations
  (desc "make-prompt default")
  (expect "Select ch. Hints[0] Act[Go/SrcAct] Src[*None*] d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :action-name pophint--default-action-name)
                          0))
  (desc "make-prompt action-name")
  (expect "Select ch. Hints[0] Act[HogeAct] Src[*None*] d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :action-name "HogeAct")
                          0))
  (desc "make-prompt sources")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] s/S:SwSrc(hoge|fuga|bar) d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :source '((shown . "hoge"))
                           :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                           :action-name pophint--default-action-name)
                          789))
  (desc "make-prompt source has not shown")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] s/S:SwSrc(hoge|*None*|bar) d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :source '((regexp . "hoge"))
                           :sources '(((shown . "hoge")) ((regexp . "fuga")) ((shown . "bar")))
                           :action-name pophint--default-action-name)
                          789))
  (desc "make-prompt not-switch-source")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] Src[hoge] d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :source '((shown . "hoge"))
                           :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                           :action-name pophint--default-action-name
                           :not-switch-source t)
                          789))
  (desc "make-prompt not-switch-source and source has not shown")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] Src[*None*] d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :source '((regexp . "hoge"))
                           :sources '(((shown . "hoge")) ((regexp . "fuga")) ((shown . "bar")))
                           :action-name pophint--default-action-name
                           :not-switch-source t)
                          789))
  (desc "make-prompt not-switch-direction")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] s/S:SwSrc(hoge|fuga|bar) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :source '((shown . "hoge"))
                           :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                           :action-name pophint--default-action-name
                           :not-switch-direction t)
                          789))
  (desc "make-prompt not-switch-window")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] s/S:SwSrc(hoge|fuga|bar) d/D:SwDrct(around|forward|backward) "
    (pophint--make-prompt (make-pophint--condition
                           :source '((shown . "hoge"))
                           :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                           :action-name pophint--default-action-name
                           :not-switch-window t)
                          789))
  (desc "make-prompt sources has selector")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] s/S:SwSrc(A:hoge|b:fuga|9:bar) d/D:SwDrct(around|forward|backward) w:SwWnd "
    (pophint--make-prompt (make-pophint--condition
                           :source '((shown . "hoge"))
                           :sources '(((shown . "hoge") (selector . "A"))
                                      ((shown . "fuga") (selector . "b"))
                                      ((shown . "bar") (selector . "9")))
                           :action-name pophint--default-action-name)
                          789))
  (desc "make-prompt not pophint:switch-source-reverse-char")
  (expect "Select ch. Hints[789] Act[Go/SrcAct] s:SwSrc(hoge|fuga|bar) d/D:SwDrct(around|forward|backward) w:SwWnd "
    (let ((pophint:switch-source-reverse-char nil))
      (pophint--make-prompt (make-pophint--condition
                             :source '((shown . "hoge"))
                             :sources '(((shown . "hoge")) ((shown . "fuga")) ((shown . "bar")))
                             :action-name pophint--default-action-name)
                            789)))
  )

