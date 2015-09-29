;;; evil-quickscope-tests.el --- evil-quickscope test suite

;; Copyright (C) 2015 Michael Chen

;; Author: Michael Chen <blorbx@gmail.com>
;; Maintainer: Michael Chen <blorbx@gmail.com>
;; Created: 12 Aug 2015

;; Homepage: http://github.com/blorbx/evil-quickscope

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; evil-quickscope test suite. use M-x eval-buffer then M-x ert to run.


;;; Code:
(load-file "evil-quickscope.el")

;;; Utility tests
(ert-deftest evil-quickscope-create-char-plist-test ()
  "Test creation of character plist."
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
  "Test incrementing character plist."
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
  "Test is-separator predicate function."
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
  "Test character finding function."
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

(ert-deftest evil-quickscope-get-highlighted-chars-search-max-test ()
  "Test search-max parameter."
  :tags '(evil-quickscope)

  (with-temp-buffer
    (insert "abc def ghi jkl mno pqr stu vwx yz")
    (let (evil-quickscope-search-max)

      ;; nil parameter shouldn't limit - (d,g,j,m,p,s,v,y)
      (setq evil-quickscope-search-max nil)
      (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                     '((33 0) (29 0) (25 0) (21 0) (17 0) (13 0) (9 0) (5 0))))

      ;; 10 should limit to (d,g)
      (setq evil-quickscope-search-max 10)
      (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                     '((9 0) (5 0))))

      ;; 21 should limit to (d,g,j,m,p)
      (setq evil-quickscope-search-max 21)
      (should (equal (evil-quickscope-get-highlighted-chars 1 (buffer-size))
                     '((21 0) (17 0) (13 0) (9 0) (5 0))))

      ;; 10 backwards should limit to (x,u)
      (setq evil-quickscope-search-max 10)
      (should (equal (evil-quickscope-get-highlighted-chars (buffer-size) 1)
                     '((27 0) (31 0))))
    )))

;;; Overlay tests
(ert-deftest evil-quickscope-update-overlays-directional-test ()
  "Test overlay placement functions."
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

  ;; evil-quickscope-cross-lines tests
  (with-temp-buffer
    (insert "abc def\nghi jkl\nmno pqr")

    ;; evil-quickscope-cross-lines nil
    (let ((evil-quickscope-cross-lines nil))
      ;; "*abc def\nghi jkl\nmno pqr" with cross-lines nil, d:5, none backward
      (goto-char 1)
      (evil-quickscope-update-overlays-directional t)
      (should (equal (get-first-face-overlay-positions) '(5)))

      (evil-quickscope-update-overlays-directional nil)
      (should (equal (get-first-face-overlay-positions) '()))

      ;; "abc def\n*ghi jkl\nmno pqr" with cross-lines nil, j:12, none backward
      (goto-char 9)
      (evil-quickscope-update-overlays-directional t)
      (should (equal (get-first-face-overlay-positions) '(13)))

      (evil-quickscope-update-overlays-directional nil)
      (should (equal (get-first-face-overlay-positions) '()))

      ;; "abc def\nghi jkl\nmno *pqr" with cross-lines nil, none forward, o:19 back
      (goto-char 21)
      (evil-quickscope-update-overlays-directional t)
      (should (equal (get-first-face-overlay-positions) '()))

      (evil-quickscope-update-overlays-directional nil)
      (should (equal (get-first-face-overlay-positions) '(19))))

    ;; evil-quickscope-cross-lines
    (let ((evil-quickscope-cross-lines t))
      ;; "*abc def\nghi jkl\nmno pqr" with cross-lines t,
      ;; d:5, g:9, j:13, m:17, p:21 forward, none back
      (goto-char 1)
      (evil-quickscope-update-overlays-directional t)
      (should (equal (get-first-face-overlay-positions) '(21 17 13 9 5)))

      (evil-quickscope-update-overlays-directional nil)
      (should (equal (get-first-face-overlay-positions) '()))

      ;; "abc def\n*ghi jkl\nmno pqr" with cross-lines t,
      ;; j:13, m:17, p:21 forward, f:7, c:3 back
      (goto-char 9)
      (evil-quickscope-update-overlays-directional t)
      (should (equal (get-first-face-overlay-positions) '(21 17 13)))

      (evil-quickscope-update-overlays-directional nil)
      (should (equal (get-first-face-overlay-positions) '(7 3)))

      ;; "abc def\nghi jkl\nmno *pqr" with cross-lines t,
      ;; none forward, o:19, l:15, i:11, f:7, c:3 back
      (goto-char 21)
      (evil-quickscope-update-overlays-directional t)
      (should (equal (get-first-face-overlay-positions) '()))

      (evil-quickscope-update-overlays-directional nil)
      (should (equal (get-first-face-overlay-positions) '(19 15 11 7 3)))
      ))
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

;;; find-char evil-repeat behavior tests
(ert-deftest evil-quickscope-find-char-repeat-test ()
  "Test find-char* with evil-repeat. Shouldn't affect the repeats."
  :tags '(evil-quickscope)

  ;; "*abc def ghi" with "ijkl <esc>fg0." should leave "jkl* jkl abc def ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?i ?j ?k ?l ?\s escape ?f ?g ?0 ?\.]
   (evil-quickscope-should-str-pos-vis "jkl jkl abc def ghi" 4 nil))

  ;; "*abc def ghi" with "ijkl <esc>tg0." should leave "jkl* jkl abc def ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?i ?j ?k ?l ?\s escape ?t ?g ?0 ?\.]
   (evil-quickscope-should-str-pos-vis "jkl jkl abc def ghi" 4 nil))

  ;; "*abc def ghi" with "ijkl <esc>Fk0." should leave "jkl* jkl abc def ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?i ?j ?k ?l ?\s escape ?F ?k ?0 ?\.]
   (evil-quickscope-should-str-pos-vis "jkl jkl abc def ghi" 4 nil))

  ;; "*abc def ghi" with "ijkl <esc>Tk0." should leave "jkl* jkl abc def ghi"
  (evil-quickscope-test-macro
   "abc def ghi" 1 [?i ?j ?k ?l ?\s escape ?T ?k ?0 ?\.]
   (evil-quickscope-should-str-pos-vis "jkl jkl abc def ghi" 4 nil))
  )

;;; Minor-mode tests
(ert-deftest evil-quickscope-minor-mode-on-test ()
  "Test evil-quickscope-mode turning on."
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
  "Test evil-quickscope-always-mode turning on."
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
  "Test interactions between qs-mode and qs-always-mode - should turn each other off."
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

(provide 'evil-quickscope-tests)

;;; evil-quickscope-tests.el ends here
