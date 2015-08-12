;;; evil-quickscope.el
;;; Highlights first unique character in each word for easy navigation with f,F,t,T
;;; Michael Chen 2015

;; Tests

(defun test-apply-overlay-firstchar ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (overlay-put (make-overlay (point) (1+ (point))) 'face
  'evil-quickscope-first-face))
  (setq evil-quickscope-overlays-exist-p t))

(defun test-remove-overlays ()
  (interactive)
  (when evil-quickscope-overlays-exist-p
    (dolist (face '(
                    evil-quickscope-first-face
                    evil-quickscope-second-face
                    ))
      (remove-overlays nil nil 'face face))
    (setq evil-quickscope-overlays-exist-p nil)))

(defun test-updating-list ()
  (interactive)
  (setq big-list ())
  (setq small-list '(0 0))

  (dolist (i '(1 2 3))
    (setq big-list (cons small-list big-list))
    (setq small-list (list 0 0))
    (setcar small-list i)
    (setcar (cdr small-list) (1+ i)))
  big-list)

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

;;; Faces

;; (defface evil-quickscope-first-face
;;   '((t (:foreground "blue" :underline t)))
;;   "Face for first unique character.")

(defface evil-quickscope-first-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for first unique character.")

;; (defface evil-quickscope-second-face
;;   '((t (:foreground "green" :underline t)))
;;   "Face for second unique character.")

(defface evil-quickscope-second-face
  '((t (:inherit font-lock-keyword-face :underline t)))
  "Face for second unique character.")

;;; Variables
(defvar evil-quickscope-always-on nil
  "Whether the quickscope overlay is always active or nil to only
  apply overlay after f,F,t,T is pressed.")

(defvar evil-quickscope-bidirectional t
  "If not always on, determines whether overlay only shows in
  direction of F/T (nil) or both directions (t).")

(defvar evil-quickscope-rebind-keys t
  "Whether the fFtT keys are automatically rebound on mode activation.")

(defvar evil-quickscope-accepted-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "String containing which characters are acceptable to highlight.")

(defvar evil-quickscope-word-separator " "
  "String which contains all word separating characters.")

;;; Overlay Functions
(defun evil-quickscope-set-overlay (face pos)
  "Sets face overlay at position."
  (overlay-put (make-overlay pos (1+ pos)) 'face face))

(defun evil-quickscope-remove-overlays ()
  "Removes all quickscope overlays from buffer."
    (dolist (face '(
                    evil-quickscope-first-face
                    evil-quickscope-second-face
                    ))
      (remove-overlays nil nil 'face face)))

;;; Utility functions
(defun evil-quickscope-create-char-plist (chars)
  (setq plist ())
  (mapcar (lambda (c) (setq plist (plist-put plist c 0))) chars)
  plist)

;;; Character Finding Functions
(defun evil-quickscope-increment-plist-char (char-plist char)
  (plist-put char-plist char
             (1+ (plist-get char-plist char))))

(defun evil-quickscope-is-separator-p (char)
  (setq is-separator-list (mapcar (lambda (c) (eq char c)) evil-quickscope-word-separator))
  (if (member t is-separator-list)
      t nil))

(defun evil-quickscope-get-highlighted-chars (start end)
  "Gets highlighted chars and returns a list of first chars and second chars."
  (defun update-hl-chars (pos)
    (let ((char (char-after pos)))
      (if (is-separator-or-invalid-char-p char)
          (add-to-hl-chars)
        (update-seen-chars))
      )
    )

  (defun is-separator-or-invalid-char-p (char)
    (or (evil-quickscope-is-separator-p char)
        (not (plist-get seen-chars char)))
    )

  (defun add-to-hl-chars ()
    (when (not first-word)
      (setq hl-chars (cons word-hl-chars hl-chars)))
    (setq word-hl-chars (list 0 0))
    (setq first-word nil))

  (defun update-seen-chars ()
    (setq seen-chars (evil-quickscope-increment-plist-char seen-chars char))
    (let ((occurences (plist-get seen-chars char))
          (hl-p (car word-hl-chars))
          (hl-s (cadr word-hl-chars)))
      (cond
       ((and (= occurences 1) (= hl-p 0))
        (setcar word-hl-chars pos))
       ((and (= occurences 2) (= hl-s 0))
        (setcar (cdr word-hl-chars) pos))))
    )

  (let ((hl-chars ())
        (first-word t)
        (word-hl-chars '(0 0))
        (seen-chars (evil-quickscope-create-char-plist
                     evil-quickscope-accepted-chars))
        (direction (if (> end start) 1 -1))
        (pos start))
    (while (/= pos end)
      (update-hl-chars pos)
      (setq pos (+ pos direction)))
    (add-to-hl-chars)
    hl-chars))

;;; Updating highlights
(defun evil-quickscope-apply-overlays (hl-positions)
  (dolist (hl-pair hl-positions)
    (cond
     ((> (car hl-pair) 0)
      (evil-quickscope-set-overlay 'evil-quickscope-first-face (car hl-pair)))
     ((> (cadr hl-pair) 0)
      (evil-quickscope-set-overlay 'evil-quickscope-second-face (cadr hl-pair))))
    ))

(defun evil-quickscope-apply-overlays-to-current-line ()
  (setq hl-positions (evil-quickscope-get-highlighted-chars
                      (line-beginning-position) (line-end-position)))
  (evil-quickscope-apply-overlays hl-positions))

(defun evil-quickscope-apply-overlays-forward ()
  (setq hl-positions (evil-quickscope-get-highlighted-chars
                      (1+ (point)) (line-end-position)))
  (evil-quickscope-apply-overlays hl-positions))

(defun evil-quickscope-apply-overlays-backward ()
  (setq hl-positions (evil-quickscope-get-highlighted-chars
                      (1- (point)) (line-beginning-position)))
  (evil-quickscope-apply-overlays hl-positions))

(defun evil-quickscope-apply-overlays-bidirectional ()
  (evil-quickscope-apply-overlays-forward)
  (evil-quickscope-apply-overlays-backward))

(defun evil-quickscope-update-overlays-bidirectional ()
  (evil-quickscope-remove-overlays)
  (evil-quickscope-apply-overlays-bidirectional))

(defun evil-quickscope-update-overlays-directional (is-forward)
  (evil-quickscope-remove-overlays)
  (if is-forward
      (evil-quickscope-apply-overlays-forward)
      (evil-quickscope-apply-overlays-backward)))

;;; Interactive functions
(defun evil-quickscope-update-overlays (is-forward)
  (when (not evil-quickscope-always-on)
    (if evil-quickscope-bidirectional
        (evil-quickscope-update-overlays-bidirectional)
      (evil-quickscope-update-overlays-directional is-forward))))

(defun evil-quickscope-call-find (find-function)
  (unwind-protect
      (call-interactively find-function)
    (evil-quickscope-remove-overlays)))

(defun evil-quickscope-find-char ()
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char))

(defun evil-quickscope-find-char-backward ()
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-backward))

(defun evil-quickscope-find-char-to ()
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char-to))

(defun evil-quickscope-find-char-to-backward ()
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-to-backward))

(defun evil-quickscope-bind-fFtT ()
  (interactive)
  (define-key evil-motion-state-local-map (kbd "f") 'evil-quickscope-find-char)
  (define-key evil-motion-state-local-map (kbd "F") 'evil-quickscope-find-char-backward)
  (define-key evil-motion-state-local-map (kbd "t") 'evil-quickscope-find-char-to)
  (define-key evil-motion-state-local-map (kbd "T") 'evil-quickscope-find-char-to-backward))

;;; TODO: bug where df/dt will be offset by one char
(defun evil-quickscope-unbind-fFtT ()
  (interactive)
  (define-key evil-motion-state-local-map (kbd "f") 'evil-find-char)
  (define-key evil-motion-state-local-map (kbd "F") 'evil-find-char-backward)
  (define-key evil-motion-state-local-map (kbd "t") 'evil-find-char-to)
  (define-key evil-motion-state-local-map (kbd "T") 'evil-find-char-to-backward))

;;; Minor-mode declaration
(define-minor-mode evil-quickscope-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim movement commands."
  :init-value nil
  :lighter " qs"
  :keymap nil
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (remove-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional t)
  (when evil-quickscope-rebind-keys (evil-quickscope-unbind-fFtT))

  (when evil-quickscope-mode
    (when evil-quickscope-rebind-keys (evil-quickscope-bind-fFtT))
    (when evil-quickscope-always-on
      (add-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional nil t))))

(define-globalized-minor-mode global-evil-quickscope-mode
  evil-quickscope-mode
  (lambda () (evil-quickscope-mode 1)))

(provide 'evil-quickscope)
