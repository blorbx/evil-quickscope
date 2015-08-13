;;; evil-quickscope-tests.el
;;; Unit tests for evil-quickscope.el
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

  ; Empty
  (should (equal (evil-quickscope-create-char-plist "")
              '()))
  ; abc
  (should (equal (evil-quickscope-create-char-plist "abc")
              '(?a 0 ?b 0 ?c 0)))
  ; abcABC
  (should (equal (evil-quickscope-create-char-plist "abcABC")
              '(?a 0 ?b 0 ?c 0 ?A 0 ?B 0 ?C 0)))
  ; abcABC123
  (should (equal (evil-quickscope-create-char-plist "abcABC123")
              '(?a 0 ?b 0 ?c 0 ?A 0 ?B 0 ?C 0 ?1 0 ?2 0 ?3 0)))
  )

(ert-deftest evil-quickscope-increment-plist-char-test ()
  :tags '(evil-quickscope)

  (let ((plist (evil-quickscope-create-char-plist "abc")))
    ; Initial state
    (should (equal plist
                   '(?a 0 ?b 0 ?c 0)))

    ; Increment a
    (setq plist (evil-quickscope-increment-plist-char plist ?a))
    (should (equal plist
                   '(?a 1 ?b 0 ?c 0)))
    ; Increment b
    (setq plist (evil-quickscope-increment-plist-char plist ?b))
    (should (equal plist
                   '(?a 1 ?b 1 ?c 0)))
    ; Increment c
    (setq plist (evil-quickscope-increment-plist-char plist ?c))
    (should (equal plist
                   '(?a 1 ?b 1 ?c 1)))
    ; Increment b again
    (setq plist (evil-quickscope-increment-plist-char plist ?b))
    (should (equal plist
                   '(?a 1 ?b 2 ?c 1)))
    ))

(ert-deftest evil-quickscope-is-separator-p-test ()
  :tags '(evil-quickscope)
  ; Space
  (let ((evil-quickscope-word-separator " "))
    (should (eq (evil-quickscope-is-separator-p ?\s) t))
    (should (eq (evil-quickscope-is-separator-p ?.) nil))
    (should (eq (evil-quickscope-is-separator-p ?a) nil))
    (should (eq (evil-quickscope-is-separator-p ?A) nil))
    (should (eq (evil-quickscope-is-separator-p ?1) nil))
    (should (eq (evil-quickscope-is-separator-p ?,) nil))
    )
  ; Space and period
  (let ((evil-quickscope-word-separator " ."))
    (should (eq (evil-quickscope-is-separator-p ?\s) t))
    (should (eq (evil-quickscope-is-separator-p ?.) t))
    (should (eq (evil-quickscope-is-separator-p ?a) nil))
    (should (eq (evil-quickscope-is-separator-p ?A) nil))
    (should (eq (evil-quickscope-is-separator-p ?1) nil))
    (should (eq (evil-quickscope-is-separator-p ?,) nil))
    )
  ; Space and period and comma
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
    ; Empty buffer
    (should (equal (evil-quickscope-get-highlighted-chars 1 1)
                   '()))

    ; Single word should return empty
    (insert "abc")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '()))

    ; "abc def" should return (5 0)
    (insert " def")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '((5 0))))

    ; "abc def ghi" should return "d": (5 0) and "g": (9 0)
    (insert " ghi")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '((9 0) (5 0))))

    ; From end of buffer, should return "f": (7 0), "c": (3 0)
    (should (equal (evil-quickscope-get-highlighted-chars (buffer-size) 1)
                   '((3 0) (7 0))))

    ; Forward from middle of "def", should return "g": (9 0)
    (should (equal (evil-quickscope-get-highlighted-chars 6 (buffer-size))
                   '((9 0))))

    ; Backward from middle of "def", should return "c": (3 0)
    (should (equal (evil-quickscope-get-highlighted-chars 6 1)
                   '((3 0))))

    ; Forward from space after "def", should return "g": (9 0)
    (should (equal (evil-quickscope-get-highlighted-chars 8 (buffer-size))
                   '((9 0))))

    ; Backward from space after "def", should return "f": (7 0), "c": (3 0)
    (should (equal (evil-quickscope-get-highlighted-chars 8 1)
                   '((3 0) (7 0))))

    ; "abc def ghi bc" should return "d": (5 0), "g": (9 0), "b": (0 13)
    (insert " bc")
    (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                   '((0 13) (9 0) (5 0))))

    ; "abc def ghi bc" from "c" should return "d": (5 0), "g": (9 0), "b": (13 0)
    (should (equal (evil-quickscope-get-highlighted-chars 3 (buffer-size))
                   '((13 0) (9 0) (5 0))))
  ))

;;; Minor-mode tests
(ert-deftest evil-quickscope-minor-mode-on-test ()
  :tags '(evil-quickscope)

  ; Turn off
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-mode nil))

  ; Toggle on
  (call-interactively 'evil-quickscope-mode)
  (should (eq evil-quickscope-mode t))

  ; Toggle off
  (call-interactively 'evil-quickscope-mode)
  (should (eq evil-quickscope-mode nil))

  ; Turn on
  (evil-quickscope-mode 1)
  (should (eq evil-quickscope-mode t))

  ; Turn back off
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-mode nil))
  )

(ert-deftest evil-quickscope-always-minor-mode-on-test ()
  :tags '(evil-quickscope)

  ; Turn off
  (evil-quickscope-always-mode 0)
  (should (eq evil-quickscope-always-mode nil))

  ; Toggle on
  (call-interactively 'evil-quickscope-always-mode)
  (should (eq evil-quickscope-always-mode t))

  ; Toggle off
  (call-interactively 'evil-quickscope-always-mode)
  (should (eq evil-quickscope-always-mode nil))

  ; Turn on
  (evil-quickscope-always-mode 1)
  (should (eq evil-quickscope-always-mode t))

  ; Turn back off
  (evil-quickscope-always-mode 0)
  (should (eq evil-quickscope-always-mode nil))
  )

(ert-deftest evil-quickscope-minor-mode-exclusive-test ()
  :tags '(evil-quickscope)

  ; Both off
  (evil-quickscope-always-mode 0)
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode nil))

  ; quickscope-mode on
  (evil-quickscope-mode 1)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode t))

  ; Always-mode on, quickscope-mode should turn off
  (evil-quickscope-always-mode 1)
  (should (eq evil-quickscope-always-mode t))
  (should (eq evil-quickscope-mode nil))

  ; quickscope-mode back on, always-mode should turn off
  (evil-quickscope-mode 1)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode t))

  ; quickscope-mode off, both should be off
  (evil-quickscope-mode 0)
  (should (eq evil-quickscope-always-mode nil))
  (should (eq evil-quickscope-mode nil))
  )
