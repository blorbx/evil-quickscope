;;; evil-quickscope-tests.el
;;; Unit tests for evil-quickscope.el
;;; Use eval-buffer and then M-x ert to run
;;; Michael Chen 2015

;;; Misc Tests

;; Overlay Function unit tests
(defun test-set-face-at-point ()
  (interactive)
  (evil-quickscope-set-overlay 'evil-quickscope-first-face (point)))

(defun test-remove-all-overlays ()
  (interactive)
  (evil-quickscope-remove-overlays))

;; utility function tests
(defun test-create-char-plist ()
  (interactive)
  (evil-quickscope-create-char-plist "aBcZ9"))

;;; Utility tests
(ert-deftest evil-quickscope-create-char-plist-test ()
  :tags '(evil-quickscope)

  ;; Empty
  (should (equal (evil-quickscope-create-char-plist "")
                 '()))
  ;; abc
  (should (equal (evil-quickscope-create-char-plist "abc")
                 '(?a 0 ?b 0 ?c 0)))
  ;; abcABC
  (should (equal (evil-quickscope-create-char-plist "abcABC")
                 '(?a 0 ?b 0 ?c 0 ?A 0 ?B 0 ?C 0)))
  ;; abcABC123
  (should (equal (evil-quickscope-create-char-plist "abcABC123")
                 '(?a 0 ?b 0 ?c 0 ?A 0 ?B 0 ?C 0 ?1 0 ?2 0 ?3 0)))
  )

(ert-deftest evil-quickscope-increment-plist-char-test ()
  :tags '(evil-quickscope)

  (let ((plist (evil-quickscope-create-char-plist "abc")))
    ;; Initial state
    (should (equal plist
                   '(?a 0 ?b 0 ?c 0)))

    ;; Increment a
    (setq plist (evil-quickscope-increment-plist-char plist ?a))
    (should (equal plist
                   '(?a 1 ?b 0 ?c 0)))
    ;; Increment b
    (setq plist (evil-quickscope-increment-plist-char plist ?b))
    (should (equal plist
                   '(?a 1 ?b 1 ?c 0)))
    ;; Increment c
    (setq plist (evil-quickscope-increment-plist-char plist ?c))
    (should (equal plist
                   '(?a 1 ?b 1 ?c 1)))
    ;; Increment b again
    (setq plist (evil-quickscope-increment-plist-char plist ?b))
    (should (equal plist
                   '(?a 1 ?b 2 ?c 1)))
    ))

(ert-deftest evil-quickscope-is-separator-p-test ()
  :tags '(evil-quickscope)
  ;; Space
  (let ((evil-quickscope-word-separator " "))
    (should (eq (evil-quickscope-is-separator-p ?\s) t))
    (should (eq (evil-quickscope-is-separator-p ?.) nil))
    (should (eq (evil-quickscope-is-separator-p ?a) nil))
    (should (eq (evil-quickscope-is-separator-p ?A) nil))
    (should (eq (evil-quickscope-is-separator-p ?1) nil))
    (should (eq (evil-quickscope-is-separator-p ?,) nil))
    )
  ;; Space and period
  (let ((evil-quickscope-word-separator " ."))
    (should (eq (evil-quickscope-is-separator-p ?\s) t))
    (should (eq (evil-quickscope-is-separator-p ?.) t))
    (should (eq (evil-quickscope-is-separator-p ?a) nil))
    (should (eq (evil-quickscope-is-separator-p ?A) nil))
    (should (eq (evil-quickscope-is-separator-p ?1) nil))
    (should (eq (evil-quickscope-is-separator-p ?,) nil))
    )
  ;; Space and period and comma
  (let ((evil-quickscope-word-separator " .,"))
    (should (eq (evil-quickscope-is-separator-p ?\s) t))
    (should (eq (evil-quickscope-is-separator-p ?.) t))
    (should (eq (evil-quickscope-is-separator-p ?a) nil))
    (should (eq (evil-quickscope-is-separator-p ?A) nil))
    (should (eq (evil-quickscope-is-separator-p ?1) nil))
    (should (eq (evil-quickscope-is-separator-p ?,) t))
    )
  )

;;; Character Finding Tests
(ert-deftest evil-quickscope-get-highlighted-chars ()
  :tags '(evil-quickscope)
  (with-temp-buffer
    ;; Empty buffer
    (should (equal (evil-quickscope-get-highlighted-chars 1 1)
                   '()))

    ;; Single word should return empty
    (insert "abc")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '()))

    ;; "abc def" should return (5 0)
    (insert " def")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '((5 0))))

    ;; "abc def ghi" should return "d": (5 0) and "g": (9 0)
    (insert " ghi")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '((9 0) (5 0))))

    ;; From end of buffer, should return "f": (7 0), "c": (3 0)
    (should (equal (evil-quickscope-get-highlighted-chars (buffer-size) 1)
                   '((3 0) (7 0))))

    ;; Forward from middle of "def", should return "g": (9 0)
    (should (equal (evil-quickscope-get-highlighted-chars 6 (buffer-size))
                   '((9 0))))

    ;; Backward from middle of "def", should return "c": (3 0)
    (should (equal (evil-quickscope-get-highlighted-chars 6 1)
                   '((3 0))))

    ;; Forward from space after "def", should return "g": (9 0)
    (should (equal (evil-quickscope-get-highlighted-chars 8 (buffer-size))
                   '((9 0))))

    ;; Backward from space after "def", should return "f": (7 0), "c": (3 0)
    (should (equal (evil-quickscope-get-highlighted-chars 8 1)
                   '((3 0) (7 0))))

    ;; "abc def ghi bc" should return "d": (5 0), "g": (9 0), "b": (0 13)
    (insert " bc")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '((0 13) (9 0) (5 0))))

    ;; "abc def ghi bc" from "c" should return "d": (5 0), "g": (9 0), "b": (13 0)
    (should (equal (evil-quickscope-get-highlighted-chars 3 (buffer-size))
                   '((13 0) (9 0) (5 0))))
    ))

;;; Overlay tests
(ert-deftest evil-quickscope-update-overlays-directional-test ()
  :tags '(evil-quickscope)

  (defun my-filter (condp lst)
    "Filter function"
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

  (defun is-overlay-quickscope-p (overlay)
    "Checks if overlay is a quickscope first face overlay"
    (eql 'evil-quickscope-first-face (overlay-get overlay 'face)))

  (defun get-first-face-overlay-positions ()
    (let* ((overlays (overlays-in 1 (buffer-size)))
           (qs-overlays (my-filter 'is-overlay-quickscope-p overlays)))
      (mapcar 'overlay-start qs-overlays)))

  (with-temp-buffer
    ;; "*abc" one word should have no overlays (*=point)
    (insert "abc")
    (goto-char 1)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '())))

  (with-temp-buffer
    (insert "abc def")

    ;; "*abc def" should have one overlay forward at d:5, none backward
    (goto-char 1)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '(5)))

    (evil-quickscope-update-overlays-directional nil)
    (should (equal (get-first-face-overlay-positions) '()))

    ;; "abc d*ef" should have no overlays forward, c:3 backward
    (goto-char 6)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '()))

    (evil-quickscope-update-overlays-directional nil)
    (should (equal (get-first-face-overlay-positions) '(3)))
    )

  (with-temp-buffer
    (insert "abc def ghi")

    ;; "*abc def ghi" should have two overlays, d:5, g:9, none backward
    (goto-char 1)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '(9 5)))

    (evil-quickscope-update-overlays-directional nil)
    (should (equal (get-first-face-overlay-positions) '()))

    ;; "abc d*ef ghi" should have g:9 forward, c:3 backward
    (goto-char 6)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '(9)))

    (evil-quickscope-update-overlays-directional nil)
    (should (equal (get-first-face-overlay-positions) '(3)))

    ;; "abc d*ef ghi" should have g:9 forward, c:3 backward
    (goto-char 6)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '(9)))

    (evil-quickscope-update-overlays-directional nil)
    (should (equal (get-first-face-overlay-positions) '(3)))

    ;; "abc def gh*i" should have none forward, f:7, c:3 backward
    (goto-char 11)
    (evil-quickscope-update-overlays-directional t)
    (should (equal (get-first-face-overlay-positions) '()))

    (evil-quickscope-update-overlays-directional nil)
    (should (equal (get-first-face-overlay-positions) '(7 3)))
    )
  )

;;; find-char replacement command tests
(defmacro evil-quickscope-test-macro (string pos kbd-macro &rest body)
  "Runs keyboard macro on STRING starting at POS. BODY argument used for (should) testing."
  `(with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert ,string)
      (evil-local-mode 1)
      (evil-quickscope-mode 1)

      (goto-char ,pos)
      (evil-force-normal-state)
      (execute-kbd-macro ,kbd-macro)
      ,@body))

(defmacro evil-quickscope-should-str-pos-vis (str pos vis-start &optional vis-end)
  "Macro which runs (should) for buffer STRing, ending POSition, and VISual overlay start and end (or nil)."
  `(progn
     (should (equal (buffer-string) ,str))
     (should (equal (point) ,pos))
     (if (equal ,vis-start nil) (should (equal evil-visual-overlay nil))
       (should (equal ,vis-start (overlay-start evil-visual-overlay)))
       (when ,vis-end
         (should (equal ,vis-end (overlay-end evil-visual-overlay)))))
     ))

(ert-deftest evil-quickscope-find-char-normal-test ()
  "Test find-char* commands from normal mode."
  :tags '(evil-quickscope)

  ;; "*abc def ghi" with "fe" should leave point at e:6
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?f ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 6 nil))

  ;; "*abc def ghi" with "te" should leave point at d:5
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?t ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 5 nil))

  ;; "abc def g*hi" with "Fe" should leave point at e:6
  (evil-quickscope-test-macro
   "abc def ghi" 10 [?F ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 6 nil))

  ;; "abc def g*hi" with "Te" should leave point at f:7
  (evil-quickscope-test-macro
   "abc def ghi" 10 [?T ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 7 nil))
  )

(ert-deftest evil-quickscope-find-char-delete-test ()
  "Test find-char* commands as motion for evil-delete command ."
  :tags '(evil-quickscope)

  ;; "*abc def ghi" with "dfe" should leave "*f ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?d ?f ?e]
   (evil-quickscope-should-str-pos-vis "f ghi" 1 nil))

  ;; "*abc def ghi" with "dte" should leave "*ef ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?d ?t ?e]
   (evil-quickscope-should-str-pos-vis "ef ghi" 1 nil))

  ;; "abc def g*hi" with "dFe" should leave "abc d*hi"
  (evil-quickscope-test-macro
   "abc def ghi" 10 [?d ?F ?e]
   (evil-quickscope-should-str-pos-vis "abc dhi" 6 nil))

  ;; "abc def g*hi" with "dTe" should leave "abc de*hi"
  (evil-quickscope-test-macro
   "abc def ghi" 10 [?d ?T ?e]
   (evil-quickscope-should-str-pos-vis "abc dehi" 7 nil))
  )

(ert-deftest evil-quickscope-find-char-visual-test ()
  "Test find-char* commands in visual-mode."
  :tags '(evil-quickscope)

  ;; "*abc def ghi" with "vfe" should leave "[abc d*e]f ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?v ?f ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 6 1 7))

  ;; "*abc def ghi" with "vte" should leave "[abc *d]ef ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?v ?t ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 5 1 6))

  ;; "abc def g*hi" with "vFe" should leave "abc d[*ef gh]i"
  (evil-quickscope-test-macro
   "abc def ghi" 10 [?v ?F ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 6 6 11))

  ;; "abc def g*hi" with "dTe" should leave "abc de[*f gh]i"
  (evil-quickscope-test-macro
   "abc def ghi" 10 [?v ?T ?e]
   (evil-quickscope-should-str-pos-vis "abc def ghi" 7 7 11))
  )
  ;; (with-temp-buffer
  ;;   (switch-to-buffer (current-buffer))
  ;;   (insert "abc def ghi")
  ;;   (evil-local-mode 1)
  ;;   (evil-quickscope-mode 1)

  ;;   ;; "*abc def ghi" with "fe" should leave point at e:6
  ;;   (goto-char 1)
  ;;   (evil-force-normal-state)
  ;;   (execute-kbd-macro [?d ?f ?e])
  ;;   (should (equal (buffer-string) "f ghi"))
  ;;   (should (equal (point) 1))
  ;;   (should (equal evil-visual-overlay nil))
  ;;   )

  ;; (with-temp-buffer
  ;;   (switch-to-buffer (current-buffer))
  ;;   (insert "abc def ghi")
  ;;   (evil-local-mode 1)
  ;;   (evil-quickscope-mode 1)

  ;;   ;; "*abc def ghi" with "te" should leave point at d:5
  ;;   (goto-char 1)
  ;;   (evil-force-normal-state)
  ;;   (execute-kbd-macro [?t ?e])
  ;;   (should (equal (buffer-string) "abc def ghi"))
  ;;   (should (equal (point) 5))
  ;;   (should (equal evil-visual-overlay nil))
  ;;   )

  ;; (with-temp-buffer
  ;;   (switch-to-buffer (current-buffer))
  ;;   (insert "abc def ghi")
  ;;   (evil-local-mode 1)
  ;;   (evil-quickscope-mode 1)

  ;;   ;; "abc def g*hi" with "Fe" should leave point at e:6
  ;;   (goto-char 10)
  ;;   (evil-force-normal-state)
  ;;   (execute-kbd-macro [?F ?e])
  ;;   (should (equal (buffer-string) "abc def ghi"))
  ;;   (should (equal (point) 6))
  ;;   (should (equal evil-visual-overlay nil))
  ;;   )

  ;; (with-temp-buffer
  ;;   (switch-to-buffer (current-buffer))
  ;;   (insert "abc def ghi")
  ;;   (evil-local-mode 1)
  ;;   (evil-quickscope-mode 1)

  ;;   ;; "abc def g*hi" with "Te" should leave point at f:7
  ;;   (goto-char 10)
  ;;   (evil-force-normal-state)
  ;;   (execute-kbd-macro [?T ?e])
  ;;   (should (equal (buffer-string) "abc def ghi"))
  ;;   (should (equal (point) 7))
  ;;   (should (equal evil-visual-overlay nil))
  ;;   )

  ;; )

;;; Minor-mode tests
(ert-deftest evil-quickscope-minor-mode-on-test ()
  :tags '(evil-quickscope)

  ;; Turn off
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-mode nil))

  ;; Toggle on
  (call-interactively 'evil-quickscope-mode)
  (should (eq evil-quickscope-mode t))

  ;; Toggle off
  (call-interactively 'evil-quickscope-mode)
  (should (eq evil-quickscope-mode nil))

  ;; Turn on
  (evil-quickscope-mode 1)
  (should (eq evil-quickscope-mode t))

  ;; Turn back off
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-mode nil))
  )

(ert-deftest evil-quickscope-always-minor-mode-on-test ()
  :tags '(evil-quickscope)

  ;; Turn off
  (evil-quickscope-always-mode 0)
  (should (eq evil-quickscope-always-mode nil))

  ;; Toggle on
  (call-interactively 'evil-quickscope-always-mode)
  (should (eq evil-quickscope-always-mode t))

  ;; Toggle off
  (call-interactively 'evil-quickscope-always-mode)
  (should (eq evil-quickscope-always-mode nil))

  ;; Turn on
  (evil-quickscope-always-mode 1)
  (should (eq evil-quickscope-always-mode t))

  ;; Turn back off
  (evil-quickscope-always-mode 0)
  (should (eq evil-quickscope-always-mode nil))
  )

(ert-deftest evil-quickscope-minor-mode-exclusive-test ()
  :tags '(evil-quickscope)

  ;; Both off
  (evil-quickscope-always-mode 0)
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode nil))

  ;; quickscope-mode on
  (evil-quickscope-mode 1)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode t))

  ;; Always-mode on, quickscope-mode should turn off
  (evil-quickscope-always-mode 1)
  (should (eq evil-quickscope-always-mode t))
  (should (eq evil-quickscope-mode nil))

  ;; quickscope-mode back on, always-mode should turn off
  (evil-quickscope-mode 1)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode t))

  ;; quickscope-mode off, both should be off
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode nil))
  )
