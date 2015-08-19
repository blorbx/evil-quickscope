;;; evil-quickscope.el
;;; Highlights first unique character in each word for easy navigation with f,F,t,T
;;; Michael Chen 2015

;;; Faces
(defface evil-quickscope-first-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for first unique character.")

(defface evil-quickscope-second-face
  '((t (:inherit font-lock-keyword-face :underline t)))
  "Face for second unique character.")

;;; Variables
(defvar evil-quickscope-bidirectional nil
  "Determines whether overlay only shows in
  direction of F/T (nil) or both directions (t).")

(defvar evil-quickscope-accepted-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "String containing which characters are acceptable to highlight.")

(defvar evil-quickscope-word-separator " "
  "String which contains all word separating characters.")

(defvar evil-quickscope-search-max nil
  "Specifies maximum number of characters to search. nil to disable.")

(defvar evil-quickscope-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "f" 'evil-quickscope-find-char)
    (evil-define-key 'motion map "F" 'evil-quickscope-find-char-backward)
    (evil-define-key 'motion map "t" 'evil-quickscope-find-char-to)
    (evil-define-key 'motion map "T" 'evil-quickscope-find-char-to-backward)
    map)
  "Keymap for evil-quickscope-mode.")

;;; Utility functions
(defun evil-quickscope-create-char-plist (chars)
  "Creates initialized plist with accepted characters."
  (let ((plist ()))
    (mapcar (lambda (c) (setq plist (plist-put plist c 0))) chars)
    plist))

(defun evil-quickscope-increment-plist-char (char-plist char)
  "Add count to corresponding char in plist."
  (plist-put char-plist char
             (1+ (plist-get char-plist char))))

(defun evil-quickscope-is-separator-p (char)
  "Determine if character is a separator."
  (let ((is-separator-list
         (mapcar (lambda (c) (eq char c)) evil-quickscope-word-separator)))
    (if (member t is-separator-list)
        t nil)))

;;; Character Finding Functions
(defun evil-quickscope-get-highlighted-chars (start end)
  "Gets highlighted chars and returns a list of first chars and second chars."
  (defun update-hl-chars (pos)
    "Checks if char at pos is separator/invalid, if not update seen-chars list."
    (let ((char (char-after pos)))
      (if (is-separator-or-invalid-char-p char)
          (add-to-hl-chars)
        (update-seen-chars))))

  (defun is-separator-or-invalid-char-p (char)
    "Determine if char is a separator or invalid."
    (or (evil-quickscope-is-separator-p char)
        (not (plist-get seen-chars char))))

  (defun add-to-hl-chars ()
    "Adds current hl-char pair to hl-chars list."
    (when (not first-word)
      (setq hl-chars (cons word-hl-chars hl-chars)))
    (setq word-hl-chars (list 0 0))
    (setq first-word nil))

  (defun update-seen-chars ()
    "Increments current char in seen-chars list and updates hl-char pair."
    (setq seen-chars (evil-quickscope-increment-plist-char seen-chars char))
    (let ((occurences (plist-get seen-chars char))
          (hl-p (car word-hl-chars))
          (hl-s (cadr word-hl-chars)))
      (cond
       ((and (= occurences 1) (= hl-p 0))
        (setcar word-hl-chars pos))
       ((and (= occurences 2) (= hl-s 0))
        (setcar (cdr word-hl-chars) pos)))))

  (let ((hl-chars ())
        (first-word t)
        (word-hl-chars '(0 0))
        (seen-chars (evil-quickscope-create-char-plist
                     evil-quickscope-accepted-chars))
        (direction (if (> end start) 1 -1))
        (pos start)
        (num-searches 0))
    (while (and (/= pos end)
                (or (eq evil-quickscope-search-max nil)
                    (< num-searches evil-quickscope-search-max)))
      (update-hl-chars pos)
      (setq pos (+ pos direction))
      (setq num-searches (1+ num-searches)))
    (add-to-hl-chars)
    hl-chars))

;;; Overlays
(defun evil-quickscope-apply-overlays-forward ()
  "Gets highlighted characters and applies overlays forward."
  (let ((hl-positions (evil-quickscope-get-highlighted-chars
                       (1+ (point)) (line-end-position))))
    (evil-quickscope-apply-overlays hl-positions)))

(defun evil-quickscope-apply-overlays-backward ()
  "Gets highlighted characters and applies overlays backward."
  (let ((hl-positions (evil-quickscope-get-highlighted-chars
                       (1- (point)) (line-beginning-position))))
    (evil-quickscope-apply-overlays hl-positions)))

(defun evil-quickscope-apply-overlays (hl-positions)
  "Applies quickscope overlays at specified positions"
  (dolist (hl-pair hl-positions)
    (cond
     ((> (car hl-pair) 0) ; First occurence of letter
      (evil-quickscope-set-overlay 'evil-quickscope-first-face (car hl-pair)))
     ((> (cadr hl-pair) 0) ; Second occurence of letter
      (evil-quickscope-set-overlay 'evil-quickscope-second-face (cadr hl-pair))))))

(defun evil-quickscope-set-overlay (face pos)
  "Sets face overlay at position."
  (overlay-put (make-overlay pos (1+ pos)) 'face face))

(defun evil-quickscope-remove-overlays ()
  "Removes all quickscope overlays from buffer."
    (dolist (face '(evil-quickscope-first-face
                    evil-quickscope-second-face))
      (remove-overlays nil nil 'face face)))

;;; Display updates
(defun evil-quickscope-update-overlays-bidirectional ()
  "Update overlays in both directions from point."
  (evil-quickscope-remove-overlays)
  (evil-quickscope-apply-overlays-forward)
  (evil-quickscope-apply-overlays-backward))

(defun evil-quickscope-update-overlays-directional (is-forward)
  "Update overlay forward from point. If arg is nil, update backward."
  (evil-quickscope-remove-overlays)
  (if is-forward
      (evil-quickscope-apply-overlays-forward)
      (evil-quickscope-apply-overlays-backward)))

(defun evil-quickscope-update-overlays (is-forward)
  "Update overlays bidirectionally or directionally."
    (if evil-quickscope-bidirectional
        (evil-quickscope-update-overlays-bidirectional)
      (evil-quickscope-update-overlays-directional is-forward)))

(defun evil-quickscope-call-find (find-function)
  "Calls function and undo overlays if cancelled out."
  (unwind-protect
      (call-interactively find-function)
    (evil-quickscope-remove-overlays)
    ))

;;; Replacement evil-find-char* commands
;;;###autoload
(defun evil-quickscope-find-char ()
  "Move to the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char))

;;;###autoload
(defun evil-quickscope-find-char-backward ()
  "Move to the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-backward))

;;;###autoload
(defun evil-quickscope-find-char-to ()
  "Move before the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char-to))

;;;###autoload
(defun evil-quickscope-find-char-to-backward ()
  "Move before the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-to-backward))

;; Set evil properties of replacement commands
(evil-set-command-properties 'evil-quickscope-find-char
                             :type 'inclusive :jump t :keep-visual t)
(evil-set-command-properties 'evil-quickscope-find-char-backward
                             :type 'exclusive :jump t :keep-visual t)
(evil-set-command-properties 'evil-quickscope-find-char-to
                             :type 'inclusive :jump t :keep-visual t)
(evil-set-command-properties 'evil-quickscope-find-char-to-backward
                             :type 'exclusive :jump t :keep-visual t)

;;; Minor modes
;;;###autoload
(define-minor-mode evil-quickscope-always-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highglights always on."
  :init-value nil
  :lighter ""
  :keymap nil
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (remove-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional t)

  (when evil-quickscope-always-mode
    ;; Turn off quickscope-mode if on
    (when evil-quickscope-mode
      (evil-quickscope-mode 0))

    (add-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional nil t)))

;;;###autoload
(defun turn-on-evil-quickscope-always-mode ()
  "Enable evil-quickscope-mode"
  (evil-quickscope-always-mode 1))

;;;###autoload
(defun turn-off-evil-quickscope-always-mode ()
  "Disable evil-quickscope-mode"
  (evil-quickscope-always-mode 0))

;;;###autoload
(define-minor-mode evil-quickscope-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highlights activate when f,F,t,T pressed."
  :init-value nil
  :lighter ""
  :keymap evil-quickscope-mode-map
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (evil-normalize-keymaps)

  (when evil-quickscope-mode
    ;; Turn off quickscope-always-mode if on
    (when evil-quickscope-always-mode
      (evil-quickscope-always-mode 0))))

;;;###autoload
(defun turn-on-evil-quickscope-mode ()
  "Enable evil-quickscope-mode"
  (evil-quickscope-mode 1))

;;;###autoload
(defun turn-off-evil-quickscope-mode ()
  "Disable evil-quickscope-mode"
  (evil-quickscope-mode 0))

(provide 'evil-quickscope)
