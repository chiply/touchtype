;;; touchtype-ui.el --- Buffer rendering and input loop for touchtype -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/touchtype

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Creates and manages the *touchtype* buffer: target-text display,
;; typed-feedback overlays, status line, and the keymap that drives
;; the training loop.

;;; Code:

(require 'cl-lib)
(require 'touchtype-var)
(require 'touchtype-stats)
(require 'touchtype-algo)

(declare-function touchtype-mode "touchtype")

;;;; Keymap

(defvar touchtype-ui--keymap nil
  "Keymap used in the *touchtype* training buffer.
Built dynamically by `touchtype-ui--make-keymap' so that the user's
word-deletion keybindings are discovered at runtime.")

(defvar touchtype-ui--word-delete-commands
  '(backward-kill-word
    evil-delete-backward-word
    subword-backward-kill)
  "Commands that delete a word backward.
Their keybindings are discovered at runtime and bound in the
touchtype keymap to `touchtype-ui--handle-word-backspace'.")

(defun touchtype-ui--make-keymap ()
  "Build and return the keymap for the *touchtype* training buffer.
Uses `where-is-internal' to discover whatever keys the user has
bound to word-deletion commands, and binds them directly.  Also
installs command remaps as a safety net for minor-mode bindings."
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; Bind all printable ASCII characters (space through tilde)
    (dolist (c (number-sequence 32 126))
      (define-key map (vector c) #'touchtype-ui--handle-char))
    (define-key map (kbd "DEL") #'touchtype-ui--handle-backspace)
    ;; Discover the user's word-deletion keys and bind them directly.
    (dolist (cmd touchtype-ui--word-delete-commands)
      (dolist (key (where-is-internal cmd (current-global-map)))
        (define-key map key #'touchtype-ui--handle-word-backspace)))
    ;; Remap as a safety net: catches word-deletion commands invoked
    ;; from minor-mode keymaps or other sources we didn't discover above.
    (dolist (cmd touchtype-ui--word-delete-commands)
      (define-key map (vector 'remap cmd)
                  #'touchtype-ui--handle-word-backspace))
    (define-key map (kbd "TAB") #'touchtype-ui--handle-tab)
    (define-key map (kbd "RET") #'touchtype-ui--ignore-key)
    (define-key map (kbd "C-g") #'touchtype-ui--quit)
    (define-key map (kbd "C-c C-p") #'touchtype-ui--toggle-pause)
    map))

;;;; Buffer setup

(defun touchtype-ui-setup-buffer ()
  "Create or reset the *touchtype* training buffer and display it."
  (let ((buf (get-buffer-create "*touchtype*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (touchtype-mode 1)
        ;; Make all buffer variables local
        (dolist (sym '(touchtype--current-text
                       touchtype--typed-chars
                       touchtype--cursor-pos
                       touchtype--line-start-time
                       touchtype--last-key-time
                       touchtype--session-wpm-samples
                       touchtype--session-errors
                       touchtype--session-total-keys
                       touchtype--session-word-count
                       touchtype--focused-key
                       touchtype--target-start
                       touchtype--status-start
                       touchtype--cursor-overlay
                       touchtype--char-overlays
                       touchtype--unlocked-keys
                       touchtype--session-corrections
                       touchtype--session-start-time
                       touchtype--session-line-wpms
                       touchtype--narrative-passage
                       touchtype--narrative-offset
                       touchtype--custom-passage
                       touchtype--custom-offset
                       touchtype--session-timer
                       touchtype--session-idle-time
                       touchtype--preview-texts
                       touchtype--pace-timer
                       touchtype--pace-pos
                       touchtype--pace-overlay
                       touchtype--completed-lines
                       touchtype--quote-passage
                       touchtype--quote-offset
                       touchtype--domain-selection
                       touchtype--code-language
                       touchtype--code-block-lines
                       touchtype--paused
                       touchtype--pause-start-time
                       touchtype--pause-overlay
                       touchtype--word-streak
                       touchtype--best-word-streak
                       touchtype--perfect-line-achieved))
          (make-local-variable sym))
        ;; Load persisted state
        (touchtype-stats-load)
        (setq touchtype--unlocked-keys (touchtype-stats-get-unlocked-keys))
        (setq touchtype--session-wpm-samples nil
              touchtype--session-errors      0
              touchtype--session-total-keys  0
              touchtype--session-word-count  0
              touchtype--session-corrections 0
              touchtype--session-start-time  nil
              touchtype--session-line-wpms   nil
              touchtype--focused-key         nil
              touchtype--typed-chars         nil
              touchtype--session-timer       nil
              touchtype--session-idle-time   0.0
              touchtype--preview-texts       nil
              touchtype--pace-timer          nil
              touchtype--pace-pos            0
              touchtype--pace-overlay        nil
              touchtype--completed-lines     nil
              touchtype--paused              nil
              touchtype--pause-start-time    nil
              touchtype--pause-overlay       nil
              touchtype--word-streak         0
              touchtype--best-word-streak    0
              touchtype--perfect-line-achieved nil)
        (setq touchtype-ui--keymap (touchtype-ui--make-keymap))
        (use-local-map touchtype-ui--keymap)
        (setq-local cursor-type nil)
        (setq buffer-read-only t)
        ;; Keep point anchored to the typing area after every command.
        ;; This prevents Evil (and anything else) from drifting into the
        ;; status line or acting on buffer text directly.
        (add-hook 'post-command-hook #'touchtype-ui--enforce-point nil t)))
    (switch-to-buffer buf)
    ;; Enter Evil's emacs state AFTER switching to the buffer so that
    ;; Evil's buffer-switch hooks cannot reset the state back to normal.
    (when (and (fboundp 'evil-emacs-state)
               (bound-and-true-p evil-mode))
      (evil-emacs-state))
    (touchtype-ui--render-new-line)))

;;;; Buffer rendering

(defun touchtype-ui--cap-line-words (line max-words)
  "Truncate LINE to at most MAX-WORDS space-separated words.
Return nil if LINE is nil or MAX-WORDS is zero or negative."
  (when (and line (> max-words 0))
    (let ((words (split-string line)))
      (if (<= (length words) max-words)
          line
        (mapconcat #'identity (seq-take words max-words) " ")))))

(defun touchtype-ui--render-new-line ()
  "Generate a new target line and render the full buffer.
On first call, generates current + preview lines.  On subsequent
calls, shifts first preview to current, appends a new preview.
Completed lines are preserved above the active line.
In word-count mode, limits total queued words to the remaining
session budget so the user never sees words they won't type."
  ;; Stop pace caret from previous line
  (touchtype-ui--stop-pace-caret)
  ;; Save the just-completed line (if any) before generating the next,
  ;; preserving per-character correct/wrong faces.
  (when (and touchtype--current-text
             (> (length touchtype--current-text) 0)
             touchtype--typed-chars)
    (let* ((raw (string-trim-right touchtype--current-text))
           (len (length raw))
           (typed (reverse touchtype--typed-chars))
           (result (copy-sequence raw)))
      (dotimes (i len)
        (let* ((record (nth i typed))
               (face (cond
                      ((null record) 'shadow)
                      ((nth 1 record) 'touchtype-face-correct)
                      (t              'touchtype-face-wrong))))
          (put-text-property i (1+ i) 'face face result)))
      (push result touchtype--completed-lines)
      ;; Record per-word stats from typed chars (only for real-word modes)
      (when (memq touchtype-mode-selection
                  '(full-words common-words narrative quote
                    domain-words custom weak-words))
        (let* ((words (split-string raw " " t))
               (char-idx 0))
          (dolist (word words)
            (let ((word-correct t)
                  (word-ms 0))
              (dotimes (ci (length word))
                (let ((rec (nth (+ char-idx ci) typed)))
                  (when rec
                    (unless (nth 1 rec) (setq word-correct nil))
                    (cl-incf word-ms (or (nth 2 rec) 0)))))
              (touchtype-stats-record-word
               (downcase word) word-correct word-ms)
              (setq char-idx (+ char-idx (length word) 1))))))))
  ;; Word budget: each line costs its actual word count.
  ;; nil means unlimited (timed mode).
  (let ((budget (when (and (eq touchtype-session-type 'words)
                         (not (touchtype-algo--quote-in-progress-p)))
                (- touchtype-session-length touchtype--session-word-count))))
    (if (and touchtype--preview-texts (> (length touchtype--preview-texts) 0))
        ;; Shift from preview
        (progn
          (setq touchtype--current-text (pop touchtype--preview-texts))
          ;; Cap active line to budget
          (when budget
            (let ((capped (touchtype-ui--cap-line-words
                           touchtype--current-text (max 1 budget))))
              (setq touchtype--current-text (or capped touchtype--current-text))))
          (let* ((active-words (length (split-string touchtype--current-text)))
                 (budget-left (when budget (- budget active-words))))
            ;; Trim existing previews to fit remaining budget
            (when budget
              (let ((trimmed nil))
                (dolist (preview touchtype--preview-texts)
                  (when (and budget-left (> budget-left 0))
                    (let* ((capped (touchtype-ui--cap-line-words
                                    preview (max 1 budget-left)))
                           (n (length (split-string (or capped "")))))
                      (when capped
                        (push capped trimmed)
                        (setq budget-left (- budget-left n))))))
                (setq touchtype--preview-texts (nreverse trimmed))))
            ;; Generate a new preview only if budget allows
            (when (or (null budget-left) (> budget-left 0))
              (let* ((new-preview (touchtype-algo-generate-line))
                     (capped (if budget-left
                                 (touchtype-ui--cap-line-words
                                  new-preview (max 1 budget-left))
                               new-preview)))
                (when capped
                  (setq touchtype--preview-texts
                        (append touchtype--preview-texts (list capped))))))))
      ;; First call: generate current + previews, respecting budget
      (let ((line (touchtype-algo-generate-line)))
        (when (and budget line)
          (setq line (or (touchtype-ui--cap-line-words line (max 1 budget))
                         line)))
        (setq touchtype--current-text line))
      (let* ((active-words (length (split-string (or touchtype--current-text ""))))
             (budget-left (when budget (- budget active-words))))
        (setq touchtype--preview-texts nil)
        (when (> touchtype-preview-lines 0)
          (dotimes (_ touchtype-preview-lines)
            (when (or (null budget-left) (> budget-left 0))
              (let* ((preview (touchtype-algo-generate-line))
                     (capped (if budget-left
                                 (touchtype-ui--cap-line-words
                                  preview (max 1 budget-left))
                               preview)))
                (when capped
                  (let ((n (length (split-string capped))))
                    (when budget-left
                      (setq budget-left (- budget-left n))))
                  (setq touchtype--preview-texts
                        (append touchtype--preview-texts (list capped)))))))))))
  ;; Append trailing space so the user must type space to advance
  (when touchtype--current-text
    (setq touchtype--current-text (concat touchtype--current-text " ")))
  (setq touchtype--cursor-pos   0
        touchtype--typed-chars  nil
        touchtype--line-start-time nil
        touchtype--last-key-time   nil)
  (touchtype-ui--redraw-buffer))

(defun touchtype-ui--redraw-buffer ()
  "Redraw the entire *touchtype* buffer content.
Completed lines appear above the active line."
  (let* ((inhibit-read-only t)
         (text touchtype--current-text))
    (erase-buffer)
    ;; Completed lines (dim history above the active line)
    (dolist (line (reverse touchtype--completed-lines))
      (insert "  " line "\n"))
    ;; Active line: each character gets an overlay starting as gray.
    (insert "  ")
    (setq touchtype--target-start (point-marker))
    (let ((n (length text)))
      (setq touchtype--char-overlays (make-vector n nil))
      (dotimes (i n)
        (let ((buf-pos (point)))
          (insert (aref text i))
          (let ((ov (make-overlay buf-pos (point))))
            (overlay-put ov 'face 'touchtype-face-untyped)
            (aset touchtype--char-overlays i ov)))))
    (insert "\n")
    ;; Preview lines (gray, no overlays)
    (when (and touchtype--preview-texts (> touchtype-preview-lines 0))
      (dolist (preview touchtype--preview-texts)
        (insert "  " (propertize preview 'face 'touchtype-face-untyped) "\n")))
    (insert "\n")
    ;; Status line
    (setq touchtype--status-start (point-marker))
    (unless touchtype-zen-mode
      (insert (touchtype-ui--status-string))
      (insert "\n"))
    ;; Cursor underline on first character
    (touchtype-ui--update-cursor-overlay)))

(defun touchtype-ui--update-cursor-overlay ()
  "Underline the character at `touchtype--cursor-pos' in the text line.
Also moves buffer point to the typing position so Evil and other
packages render the cursor there rather than at the end of the buffer."
  (when touchtype--cursor-overlay
    (delete-overlay touchtype--cursor-overlay)
    (setq touchtype--cursor-overlay nil))
  (let ((pos touchtype--cursor-pos)
        (n   (length touchtype--current-text)))
    (when (< pos n)
      (let* ((buf-pos (+ (marker-position touchtype--target-start) pos))
             (ov (make-overlay buf-pos (1+ buf-pos))))
        (overlay-put ov 'face 'touchtype-face-cursor)
        (overlay-put ov 'priority 10)
        (setq touchtype--cursor-overlay ov)))
    ;; Keep buffer point inside the typing area so Evil (and any other
    ;; mode that uses point for cursor display) doesn't wander elsewhere.
    (when (and touchtype--target-start (> n 0))
      (goto-char (+ (marker-position touchtype--target-start)
                    (min pos (1- n)))))))

(defun touchtype-ui--update-typed-char (pos _char face)
  "Apply FACE to the overlay at POS in the target text.
The target character is already in the buffer; only its face changes.
_CHAR is accepted for API compatibility but unused."
  (let ((ov (aref touchtype--char-overlays pos)))
    (when ov
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 5))))

(defun touchtype-ui--update-typed-space (pos)
  "Reset the overlay at POS to the untyped (gray) face."
  (let ((ov (aref touchtype--char-overlays pos)))
    (when ov
      (overlay-put ov 'face 'touchtype-face-untyped)
      (overlay-put ov 'priority 5))))

(defun touchtype-ui--update-status ()
  "Rewrite the status area in place."
  (unless touchtype-zen-mode
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (marker-position touchtype--status-start))
        (delete-region (point) (point-max))
        (insert (touchtype-ui--status-string))
        (insert "\n")))))

(defun touchtype-ui--status-string ()
  "Return a formatted status string for the current session state."
  (let* ((total    touchtype--session-total-keys)
         (errors   touchtype--session-errors)
         (corr     touchtype--session-corrections)
         (now      (float-time))
         (raw-elapsed (if touchtype--session-start-time
                         (- now touchtype--session-start-time)
                       0.0))
         (idle     (or touchtype--session-idle-time 0.0))
         (elapsed  (max 0.0 (- raw-elapsed idle)))
         (minutes  (/ elapsed 60.0))
         ;; Gross WPM from total chars and elapsed time
         (gross    (if (> minutes 0)
                       (/ (/ (float total) 5.0) minutes)
                     0.0))
         ;; Net WPM: gross minus uncorrected errors per minute
         (uncorr   (max 0 (- errors corr)))
         (net      (if (> minutes 0)
                       (max 0.0 (/ (- (/ (float total) 5.0) uncorr) minutes))
                     0.0))
         ;; Accuracy
         (acc      (if (> total 0)
                       (* 100.0 (/ (float (- total errors)) total))
                     100.0))
         ;; Consistency
         (line-wpms touchtype--session-line-wpms)
         (cons-str  (if (>= (length line-wpms) 2)
                        (format "%.0f%%" (touchtype-ui--consistency-score line-wpms))
                      "--"))
         ;; Time display
         (time-str  (if (eq touchtype-session-type 'timed)
                        (let* ((remaining (max 0.0 (- touchtype-session-duration raw-elapsed)))
                               (rm (floor (/ remaining 60.0)))
                               (rs (floor (mod remaining 60.0))))
                          (format "%d:%02d left" rm rs))
                      (let ((time-min (floor (/ elapsed 60.0)))
                            (time-sec (floor (mod elapsed 60.0))))
                        (format "%d:%02d" time-min time-sec))))
         ;; Mode / keys / words
         (mode     (symbol-name touchtype-mode-selection))
         (keys     (if (touchtype-algo--progressive-p)
                       (format "  Keys: %s" touchtype--unlocked-keys)
                     ""))
         ;; Live word count: session total + words completed on current line.
         ;; Count spaces typed so far — each space marks a completed word.
         (in-line  (if (and touchtype--current-text
                            (> touchtype--cursor-pos 0))
                       (cl-count ?\s (substring touchtype--current-text
                                                0 touchtype--cursor-pos))
                     0))
         (live-wc  (+ touchtype--session-word-count in-line))
         (words    (if (eq touchtype-session-type 'timed)
                       (format "%d" live-wc)
                     (format "%d/%d" live-wc touchtype-session-length)))
         ;; Format strings
         (net-str   (if (> total 0) (format "%.0f" net) "--"))
         (gross-str (if (> total 0) (format "%.0f" gross) "--"))
         (acc-str   (if (> total 0) (format "%.0f%%" acc) "--"))
         (streak-str (if (and (boundp 'touchtype--word-streak)
                            (> touchtype--word-streak 0))
                        (format "  Streak: %d" touchtype--word-streak)
                      ""))
         (lv-str (format "  Lv%d" (touchtype-stats-get-level)))
         (line1 (format "  Net: %s  Gross: %s  Acc: %s  Consistency: %s%s%s"
                         net-str gross-str acc-str cons-str streak-str lv-str))
         (unlock-str (or (touchtype-ui--unlock-progress-string) ""))
         (diff-str (if (eq touchtype-difficulty 'normal) ""
                     (format " [%s]" touchtype-difficulty)))
         (line2 (format "  Time: %s  Words: %s  Corrections: %d  Mode: %s%s%s%s"
                         time-str words corr mode diff-str keys unlock-str)))
    (propertize (concat line1 "\n" line2) 'face 'touchtype-face-status)))

(defun touchtype-ui--enforce-point ()
  "Buffer-local `post-command-hook': keep point in the typing area.
Prevents Evil and other packages from moving the cursor into the
status line or elsewhere after each command."
  (when (and (bound-and-true-p touchtype--target-start)
             touchtype--current-text
             (> (length touchtype--current-text) 0))
    (let* ((n       (length touchtype--current-text))
           (pos     (min touchtype--cursor-pos (1- n)))
           (target  (+ (marker-position touchtype--target-start) pos)))
      (unless (= (point) target)
        (goto-char target)))))

;;;; Pause/Resume

(defun touchtype-ui--toggle-pause ()
  "Toggle pause/resume for the current session."
  (interactive)
  (if touchtype--paused
      ;; Resume
      (let ((pause-duration (- (float-time) touchtype--pause-start-time)))
        (setq touchtype--paused nil)
        ;; Add pause duration to idle time so WPM isn't affected
        (setq touchtype--session-idle-time
              (+ (or touchtype--session-idle-time 0.0) pause-duration))
        ;; Shift line-start-time forward if it exists
        (when touchtype--line-start-time
          (setq touchtype--line-start-time
                (+ touchtype--line-start-time pause-duration)))
        ;; Restart timed session timer with remaining time
        (when (and (eq touchtype-session-type 'timed)
                   touchtype--session-start-time)
          (touchtype-ui--cancel-session-timer)
          (let* ((raw-elapsed (- (float-time) touchtype--session-start-time))
                 (remaining (max 0.0 (- touchtype-session-duration
                                        (- raw-elapsed (or touchtype--session-idle-time 0.0))))))
            (when (> remaining 0)
              (setq touchtype--session-timer
                    (run-at-time remaining nil
                                 #'touchtype-ui--timed-session-expire
                                 (current-buffer))))))
        ;; Restart pace caret
        (when touchtype-pace-caret
          (touchtype-ui--start-pace-caret))
        ;; Remove pause overlay
        (when (overlayp touchtype--pause-overlay)
          (delete-overlay touchtype--pause-overlay)
          (setq touchtype--pause-overlay nil))
        (touchtype-ui--update-status))
    ;; Pause
    (setq touchtype--paused t
          touchtype--pause-start-time (float-time))
    ;; Cancel timers
    (touchtype-ui--cancel-session-timer)
    (touchtype-ui--stop-pace-caret)
    ;; Show PAUSED overlay
    (when touchtype--target-start
      (let* ((start (marker-position touchtype--target-start))
             (end (+ start (length (or touchtype--current-text ""))))
             (ov (make-overlay start end)))
        (overlay-put ov 'display (propertize "  *** PAUSED (F5 to resume) ***"
                                             'face 'bold))
        (overlay-put ov 'priority 20)
        (setq touchtype--pause-overlay ov)))))

;;;; Difficulty tier: session failure

(defun touchtype-ui--end-session-failed ()
  "End the session due to difficulty tier failure.
Shows SESSION FAILED header and records :failed t in session."
  (touchtype-ui--cancel-session-timer)
  (touchtype-ui--stop-pace-caret)
  (let* ((total-keys touchtype--session-total-keys)
         (total-errs touchtype--session-errors)
         (corrections touchtype--session-corrections)
         (raw-elapsed (if touchtype--session-start-time
                         (- (float-time) touchtype--session-start-time)
                       0.0))
         (idle (or touchtype--session-idle-time 0.0))
         (elapsed-s (max 0.0 (- raw-elapsed idle)))
         (minutes (/ elapsed-s 60.0))
         (gross-wpm (if (> minutes 0)
                        (/ (/ (float total-keys) 5.0) minutes)
                      0.0))
         (uncorrected (max 0 (- total-errs corrections)))
         (net-wpm (if (> minutes 0)
                      (max 0.0 (/ (- (/ (float total-keys) 5.0) uncorrected) minutes))
                    0.0))
         (accuracy (if (> total-keys 0)
                       (* 100.0 (/ (float (- total-keys total-errs)) total-keys))
                     100.0)))
    (touchtype-stats-record-session
     net-wpm accuracy touchtype-mode-selection touchtype--session-word-count
     :gross-wpm gross-wpm :total-time elapsed-s :total-chars total-keys
     :corrections corrections :uncorrected-errors uncorrected :failed t)
    (touchtype-stats-save)
    (setq touchtype--current-text nil)
    (remove-hook 'post-command-hook #'touchtype-ui--enforce-point t)
    (setq-local cursor-type t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n")
      (insert (propertize "  SESSION FAILED!\n\n" 'face '(:foreground "red" :weight bold)))
      (insert (propertize (format "  Difficulty: %s\n\n" touchtype-difficulty) 'face 'bold))
      (insert (format "    Net WPM:    %.1f\n" net-wpm))
      (insert (format "    Accuracy:   %.1f%%\n" accuracy))
      (insert (format "    Words:      %d\n" touchtype--session-word-count))
      (insert (format "    Characters: %d\n\n" total-keys))
      (insert (propertize
               "  r: restart  q: quit\n"
               'face 'touchtype-face-status))
      (goto-char (point-min)))
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "r") #'touchtype-ui--restart-session)
       (define-key map (kbd "q") #'touchtype-ui--quit)
       map))))

;;;; Input handling

(defun touchtype-ui--handle-char ()
  "Handle a printable character keypress."
  (interactive)
  (let ((char last-command-event))
    (when (characterp char)
      (touchtype-ui--process-char char))))

(defun touchtype-ui--process-char (char)
  "Process CHAR typed by the user against the current target text."
  (cl-block touchtype-ui--process-char
  ;; Pause guard
  (when touchtype--paused
    (cl-return-from touchtype-ui--process-char))
  (let* ((pos     touchtype--cursor-pos)
         (text    touchtype--current-text)
         (n       (length text))
         (now     (float-time))
         (elapsed-s (if touchtype--last-key-time
                        (- now touchtype--last-key-time)
                      0.2))
         (elapsed (round (* 1000 elapsed-s)))
         (expected (when (< pos n) (aref text pos)))
         (correct-p (and expected (= char expected))))
    (when (< pos n)
      ;; Difficulty tier: master mode - fail on any wrong key
      (when (and (eq touchtype-difficulty 'master) (not correct-p))
        (cl-incf touchtype--session-total-keys)
        (cl-incf touchtype--session-errors)
        (touchtype-stats-record-keypress expected nil elapsed)
        (touchtype-ui--end-session-failed)
        (cl-return-from touchtype-ui--process-char))
      ;; Difficulty tier: expert mode - fail at word boundary if word has errors
      (when (and (eq touchtype-difficulty 'expert)
                 expected (= expected ?\s))
        (let ((has-error nil)
              (check-pos (1- pos)))
          (while (and (>= check-pos 0)
                      (not (= (aref text check-pos) ?\s)))
            (let ((rec (nth (- pos check-pos 1) touchtype--typed-chars)))
              (when (and rec (not (cadr rec)))
                (setq has-error t)))
            (cl-decf check-pos))
          (when has-error
            (cl-incf touchtype--session-total-keys)
            (cl-incf touchtype--session-errors)
            (touchtype-ui--end-session-failed)
            (cl-return-from touchtype-ui--process-char))))
      ;; Stop-on-error: letter mode blocks on wrong key
      (when (and (eq touchtype-error-mode 'letter) (not correct-p))
        (cl-incf touchtype--session-total-keys)
        (cl-incf touchtype--session-errors)
        (touchtype-stats-record-keypress expected nil elapsed touchtype-mode-selection)
        (touchtype-ui--update-status)
        (cl-return-from touchtype-ui--process-char))
      ;; Stop-on-error: word mode blocks at space if preceding word has errors
      (when (and (eq touchtype-error-mode 'word)
                 expected (= expected ?\s))
        (let ((has-error nil)
              (check-pos (1- pos)))
          (while (and (>= check-pos 0)
                      (not (= (aref text check-pos) ?\s)))
            (let ((rec (nth (- pos check-pos 1) touchtype--typed-chars)))
              (when (and rec (not (cadr rec)))
                (setq has-error t)))
            (cl-decf check-pos))
          (when has-error
            (cl-incf touchtype--session-total-keys)
            (cl-incf touchtype--session-errors)
            (touchtype-ui--update-status)
            (cl-return-from touchtype-ui--process-char))))
      ;; Start timers on first keypress of a line / session
      (unless touchtype--line-start-time
        (setq touchtype--line-start-time now)
        ;; Start pace caret on first keypress of each line
        (when touchtype-pace-caret
          (touchtype-ui--start-pace-caret)))
      (unless touchtype--session-start-time
        (setq touchtype--session-start-time now)
        ;; Start timed session timer on first keypress
        (when (eq touchtype-session-type 'timed)
          (setq touchtype--session-timer
                (run-at-time touchtype-session-duration nil
                             #'touchtype-ui--timed-session-expire
                             (current-buffer)))))
      ;; Idle detection
      (when (and touchtype--last-key-time
                 (> elapsed-s touchtype-idle-threshold))
        (setq touchtype--session-idle-time
              (+ touchtype--session-idle-time elapsed-s))
        (setq touchtype--line-start-time
              (+ touchtype--line-start-time elapsed-s)))
      (setq touchtype--last-key-time now)
      ;; Record the keypress in stats
      (touchtype-stats-record-keypress char correct-p elapsed touchtype-mode-selection)
      ;; Record bigram if we have a previous char
      (when (> pos 0)
        (let ((prev-char (aref text (1- pos))))
          (touchtype-stats-record-bigram
           (string prev-char char) correct-p elapsed touchtype-mode-selection)))
      ;; Record trigram
      (when (> pos 1)
        (touchtype-stats-record-bigram
         (string (aref text (- pos 2)) (aref text (1- pos)) char)
         correct-p elapsed touchtype-mode-selection))
      ;; Record tetragram
      (when (> pos 2)
        (touchtype-stats-record-bigram
         (string (aref text (- pos 3)) (aref text (- pos 2))
                 (aref text (1- pos)) char)
         correct-p elapsed touchtype-mode-selection))
      ;; Visual feedback
      (touchtype-ui--update-typed-char
       pos char
       (if correct-p 'touchtype-face-correct 'touchtype-face-wrong))
      ;; Track for status display
      (push (list char correct-p elapsed) touchtype--typed-chars)
      (cl-incf touchtype--session-total-keys)
      (unless correct-p (cl-incf touchtype--session-errors))
      ;; Word streak: check when space is typed
      (when (and expected (= expected ?\s) correct-p)
        (let ((word-correct t))
          ;; Walk backward through typed-chars to check the word
          ;; typed-chars is reversed (most recent first), skip the space we just pushed
          (let ((chars (cdr touchtype--typed-chars)))
            (while (and chars
                        (let ((rec (car chars)))
                          (not (= (car rec) ?\s))))
              (unless (cadr (car chars))
                (setq word-correct nil))
              (setq chars (cdr chars))))
          (touchtype-ui--update-word-streak word-correct)))
      ;; Advance cursor
      (setq touchtype--cursor-pos (1+ pos))
      (touchtype-ui--update-cursor-overlay)
      (touchtype-ui--update-status)
      ;; Check for line completion
      (when (>= touchtype--cursor-pos n)
        (touchtype-ui--advance-line))))))

(defun touchtype-ui--handle-backspace ()
  "Remove the last typed character and rewind the cursor."
  (interactive)
  (when (and (not touchtype--paused)
             (> touchtype--cursor-pos 0))
    (cl-incf touchtype--session-corrections)
    (cl-decf touchtype--cursor-pos)
    (touchtype-ui--update-typed-space touchtype--cursor-pos)
    (when touchtype--typed-chars
      (setq touchtype--typed-chars (cdr touchtype--typed-chars)))
    (touchtype-ui--update-cursor-overlay)
    (touchtype-ui--update-status)))

(defun touchtype-ui--handle-word-backspace ()
  "Remove typed characters back to the previous word boundary."
  (interactive)
  (cl-incf touchtype--session-corrections)
  (let* ((text touchtype--current-text)
         (pos  touchtype--cursor-pos))
    ;; Skip back past any spaces already passed
    (while (and (> pos 0)
                (= (aref text (1- pos)) ?\s))
      (cl-decf pos)
      (touchtype-ui--update-typed-space pos)
      (when touchtype--typed-chars
        (setq touchtype--typed-chars (cdr touchtype--typed-chars))))
    ;; Skip back through word characters
    (while (and (> pos 0)
                (not (= (aref text (1- pos)) ?\s)))
      (cl-decf pos)
      (touchtype-ui--update-typed-space pos)
      (when touchtype--typed-chars
        (setq touchtype--typed-chars (cdr touchtype--typed-chars))))
    (setq touchtype--cursor-pos pos)
    (touchtype-ui--update-cursor-overlay)
    (touchtype-ui--update-status)))

(defun touchtype-ui--ignore-key ()
  "Silently ignore a key (used for RET)."
  (interactive))

(defun touchtype-ui--quit ()
  "Save stats and quit the touchtype session."
  (interactive)
  (when (yes-or-no-p "Quit touchtype session? ")
    (touchtype-ui--cancel-session-timer)
    (touchtype-ui--stop-pace-caret)

    (touchtype-stats-save)
    (kill-buffer (current-buffer))))

;;;; Line advancement and session management

(defun touchtype-ui--line-wpm ()
  "Compute WPM for the line just completed."
  (let* ((elapsed-s (- (float-time) touchtype--line-start-time))
         (chars     (length touchtype--current-text))
         (words     (/ chars 5.0)))
    (if (> elapsed-s 0)
        (/ (* words 60.0) elapsed-s)
      0.0)))

(defun touchtype-ui--line-word-count ()
  "Return the number of space-separated words in `touchtype--current-text'."
  (length (split-string (string-trim-right touchtype--current-text))))

(defun touchtype-ui--advance-line ()
  "Complete the current line and set up the next one."
  (let* ((wpm        (touchtype-ui--line-wpm))
         (n-correct  (length (cl-remove-if-not #'cadr touchtype--typed-chars)))
         (n-total    (length touchtype--typed-chars))
         (word-count (touchtype-ui--line-word-count)))
    ;; Check last word on line for word streak
    (let ((word-correct t))
      (let ((chars touchtype--typed-chars))
        (while (and chars
                    (let ((rec (car chars)))
                      (not (= (car rec) ?\s))))
          (unless (cadr (car chars))
            (setq word-correct nil))
          (setq chars (cdr chars))))
      (touchtype-ui--update-word-streak word-correct))
    ;; Check for perfect line (0 errors, WPM > 80)
    (when (and (> wpm 80)
               (= n-correct n-total))
      (setq touchtype--perfect-line-achieved t))
    ;; Record WPM sample
    (when (> wpm 0)
      (push wpm touchtype--session-wpm-samples)
      (push wpm touchtype--session-line-wpms))
    ;; Count words toward session total
    (cl-incf touchtype--session-word-count word-count)
    ;; Check for key unlock (progressive mode only)
    (when (and (touchtype-algo--progressive-p)
               (touchtype-algo-should-unlock-p))
      (let ((new-key (touchtype-algo-unlock-next-key)))
        (when new-key
          (message "Unlocked new key: %c  Keep typing to build confidence!"
                   new-key))))
    ;; Update accuracy in status
    (ignore n-correct n-total)
    ;; Check session end (only for word-count mode; timed mode ends via timer)
    (cond
     ;; Word-count target reached
     ((and (eq touchtype-session-type 'words)
           (>= touchtype--session-word-count touchtype-session-length))
      (if (touchtype-algo--quote-in-progress-p)
          (touchtype-ui--render-new-line)
        (touchtype-ui--end-session)))
     ;; Custom passage exhausted: no preview texts and no more text to generate
     ((and (eq touchtype-mode-selection 'custom)
           (null touchtype--preview-texts)
           touchtype--custom-passage
           (>= touchtype--custom-offset (length touchtype--custom-passage)))
      (touchtype-ui--end-session))
     (t
      (touchtype-ui--render-new-line)))))

(defun touchtype-ui--consistency-score (wpms)
  "Return consistency score for WPMS list.
Computed as 100 - (stddev / mean * 100).  Higher is more consistent.
Returns 100.0 when there are fewer than 2 samples."
  (if (< (length wpms) 2)
      100.0
    (let* ((n    (float (length wpms)))
           (mean (/ (cl-reduce #'+ wpms) n))
           (variance (/ (cl-reduce #'+ (mapcar (lambda (v) (expt (- v mean) 2)) wpms)) n))
           (stddev (sqrt variance))
           (cv (if (> mean 0) (* 100.0 (/ stddev mean)) 0.0)))
      (max 0.0 (- 100.0 cv)))))

;;;; End-session interactive sections

(defvar touchtype-ui--expandable-sections nil
  "List of section plists for expandable end-session areas.
Each plist has keys :id, :header, :trunc-items, :full-items,
:formatter, :is-expanded.")

(defvar touchtype-ui--expandable-area-start nil
  "Marker for the start of the expandable sections area.")

(defvar touchtype-ui--expandable-area-end nil
  "Marker for the end of the expandable sections area.")

(defun touchtype-ui--render-expandable-sections ()
  "Re-render all expandable sections between the area markers.
Deletes the old content and re-inserts every section, respecting
each section's :is-expanded flag."
  (let ((inhibit-read-only t))
    (delete-region touchtype-ui--expandable-area-start
                   touchtype-ui--expandable-area-end)
    (goto-char touchtype-ui--expandable-area-start)
    (dolist (section touchtype-ui--expandable-sections)
      (let* ((id         (plist-get section :id))
             (header     (plist-get section :header))
             (trunc-items (plist-get section :trunc-items))
             (full-items  (plist-get section :full-items))
             (formatter   (plist-get section :formatter))
             (is-expanded (plist-get section :is-expanded))
             (items       (if is-expanded full-items trunc-items))
             (trunc-count (length trunc-items))
             (full-count  (length full-items))
             (expandable  (> full-count trunc-count)))
        (when trunc-items
          (let ((header-str (format "  %s" header))
                (header-props (if expandable
                                  (list 'face 'bold 'touchtype-category t
                                        'touchtype-section-id id)
                                (list 'face 'bold 'touchtype-category t))))
            (insert (apply #'propertize header-str header-props))
            (when expandable
              (insert (propertize
                       (if is-expanded
                           (format "  [all %d]" full-count)
                         (format "  [%d of %d]" trunc-count full-count))
                       'face 'shadow)))
            (insert "\n")
            (insert (mapconcat formatter items ""))
            (when expandable
              (insert (propertize
                       (if is-expanded "    ↵ collapse\n" "    ↵ show all\n")
                       'face 'shadow)))
            (insert "\n")))))))

(defun touchtype-ui--end-session-next-category ()
  "Move point to the next category header in the end-session buffer."
  (interactive)
  (let ((orig (point))
        (found nil))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (when (get-text-property (point) 'touchtype-category)
          (setq found (point)))
        (unless found (forward-line 1))))
    (unless found
      (save-excursion
        (goto-char (point-min))
        (while (and (not found) (< (point) orig))
          (when (get-text-property (point) 'touchtype-category)
            (setq found (point)))
          (unless found (forward-line 1)))))
    (when found (goto-char found))))

(defun touchtype-ui--end-session-prev-category ()
  "Move point to the previous category header in the end-session buffer."
  (interactive)
  (let ((orig (point))
        (found nil))
    (save-excursion
      (forward-line -1)
      (while (and (not found) (not (bobp)))
        (when (get-text-property (point) 'touchtype-category)
          (setq found (point)))
        (unless found (forward-line -1))))
    (unless found
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (while (and (not found) (> (point) orig))
          (when (get-text-property (point) 'touchtype-category)
            (setq found (point)))
          (unless found (forward-line -1)))))
    (when found (goto-char found))))

(defun touchtype-ui--end-session-toggle ()
  "Toggle expansion of the section on the current line, if any."
  (interactive)
  (let ((section-id (get-text-property (line-beginning-position)
                                       'touchtype-section-id)))
    (when section-id
      (let ((section (cl-find section-id touchtype-ui--expandable-sections
                              :key (lambda (s) (plist-get s :id))
                              :test #'string=)))
        (when section
          (plist-put section :is-expanded
                     (not (plist-get section :is-expanded)))
          (touchtype-ui--render-expandable-sections)
          ;; Reposition point on the toggled section's header
          (goto-char touchtype-ui--expandable-area-start)
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (if (equal (get-text-property (point) 'touchtype-section-id)
                         section-id)
                  (setq found t)
                (forward-line 1)))))))))

(defun touchtype-ui--progress-bar (pct width)
  "Return a WIDTH-char progress bar showing PCT percent (0-100).
Uses Unicode block characters: filled=█ empty=░."
  (let* ((ratio (min 1.0 (max 0.0 (/ pct 100.0))))
         (filled (round (* ratio width)))
         (empty (- width filled)))
    (concat (make-string filled ?█) (make-string empty ?░))))

(defun touchtype-ui--xp-progress-bar ()
  "Return string showing XP progress to next level.
Format: [████████░░] 73% to Level 8: Skilled"
  (let* ((level (touchtype-stats-get-level))
         (max-level (1- (length touchtype--xp-level-thresholds))))
    (if (>= level max-level)
        "MAX LEVEL"
      (let* ((current-xp (touchtype-stats-get-xp))
             (level-start (aref touchtype--xp-level-thresholds level))
             (level-end (aref touchtype--xp-level-thresholds (1+ level)))
             (progress (- current-xp level-start))
             (needed (- level-end level-start))
             (pct (min 100 (round (* 100.0 (/ (float progress) needed)))))
             (next-title (aref touchtype--level-titles
                               (min (1+ level) (1- (length touchtype--level-titles)))))
             (bar (touchtype-ui--progress-bar pct 10)))
        (format "%s %d%% to Level %d: %s" bar pct (1+ level) next-title)))))

(defun touchtype-ui--unlock-progress-string ()
  "Return string showing progress toward next key unlock, or nil.
Returns nil when not in progressive mode or all keys unlocked."
  (when (touchtype-algo--progressive-p)
    (let* ((order (touchtype-algo--unlock-order))
           (next-char (cl-find-if-not
                       (lambda (ch)
                         (seq-contains-p touchtype--unlocked-keys ch #'=))
                       order)))
      (when next-char
        (let* ((min-conf 1.0)
               (keys (string-to-list touchtype--unlocked-keys))
               ;; Get position of next key in unlock order
               (pos (1+ (length touchtype--unlocked-keys)))
               (threshold (if (and (boundp 'touchtype-graduated-thresholds)
                                   touchtype-graduated-thresholds)
                              (touchtype-algo--graduated-threshold pos)
                            touchtype-unlock-threshold)))
          ;; Find minimum confidence among unlocked keys
          (dolist (ch keys)
            (let ((conf (touchtype-stats-get-confidence ch)))
              (when (< conf min-conf)
                (setq min-conf conf))))
          (let* ((pct (min 100 (round (* 100 (/ min-conf threshold)))))
                 (bar (touchtype-ui--progress-bar pct 8)))
            (format "  Next: '%c' %s %d%%" next-char bar pct)))))))

(defun touchtype-ui--update-word-streak (correct-p)
  "Update word streak counter.
CORRECT-P is non-nil if the word was typed correctly."
  (if correct-p
      (progn
        (cl-incf touchtype--word-streak)
        (when (> touchtype--word-streak touchtype--best-word-streak)
          (setq touchtype--best-word-streak touchtype--word-streak)))
    (setq touchtype--word-streak 0)))

(defun touchtype-ui--near-miss-messages (net-wpm accuracy session-xp total-keys mode is-pb new-achievements)
  "Return list of near-miss message strings for the session.
NET-WPM, ACCURACY, SESSION-XP, TOTAL-KEYS are session metrics.
MODE is the training mode symbol.  IS-PB is non-nil if a personal best.
NEW-ACHIEVEMENTS is the list of newly earned achievement IDs."
  (let ((msgs nil))
    ;; WPM within 5% of PB (only if not already a PB)
    (unless is-pb
      (let ((pb (touchtype-stats-get-personal-best mode :wpm)))
        (when (and pb (> pb 0) (> net-wpm 0))
          (let ((threshold (* pb 0.95)))
            (when (and (>= net-wpm threshold) (< net-wpm pb))
              (push (format "%.1f WPM — only %.1f from your personal best!"
                            net-wpm (- pb net-wpm))
                    msgs))))))
    ;; Accuracy within 2% of unearned accuracy achievement
    (let ((earned (touchtype-stats-get-achievements))
          (thresholds '((accuracy-95 . 95) (accuracy-99 . 99) (accuracy-100 . 100))))
      (dolist (pair thresholds)
        (unless (or (memq (car pair) earned)
                    (memq (car pair) new-achievements))
          (let ((target (cdr pair)))
            (when (and (>= accuracy (- target 2)) (< accuracy target))
              (push (format "%.1f%% accuracy — only %.1f%% from %s achievement!"
                            accuracy (- target accuracy)
                            (symbol-name (car pair)))
                    msgs))))))
    ;; XP within 20% of next level
    (let ((xp-to-next (touchtype-stats-xp-to-next-level)))
      (when (and (> xp-to-next 0) (> session-xp 0))
        (let ((threshold (* xp-to-next 0.2)))
          (when (<= xp-to-next (max threshold session-xp))
            (let* ((level (touchtype-stats-get-level))
                   (next-level (1+ level))
                   (title (aref touchtype--level-titles
                                (min next-level (1- (length touchtype--level-titles))))))
              (push (format "Only %d XP to level %d: %s!"
                            xp-to-next next-level title)
                    msgs))))))
    ;; Speed within 10% of unearned speed achievement
    (let ((earned (touchtype-stats-get-achievements))
          (thresholds '((speed-30 . 30) (speed-50 . 50) (speed-70 . 70)
                        (speed-100 . 100) (speed-40 . 40) (speed-60 . 60)
                        (speed-80 . 80) (speed-120 . 120) (speed-150 . 150))))
      (dolist (pair thresholds)
        (unless (or (memq (car pair) earned)
                    (memq (car pair) new-achievements))
          (let ((target (cdr pair)))
            (when (and (>= net-wpm (* target 0.90)) (< net-wpm target))
              (push (format "%.0f WPM — only %.0f from %s achievement!"
                            net-wpm (- target net-wpm)
                            (symbol-name (car pair)))
                    msgs))))))
    (nreverse msgs)))

(defun touchtype-ui--end-session ()
  "Display the session summary and prompt to continue or quit."
  ;; Cancel any active timers
  (touchtype-ui--cancel-session-timer)
  (touchtype-ui--stop-pace-caret)
  (let* ((total-keys  touchtype--session-total-keys)
         (total-errs  touchtype--session-errors)
         (corrections touchtype--session-corrections)
         (raw-elapsed (if touchtype--session-start-time
                         (- (float-time) touchtype--session-start-time)
                       0.0))
         (idle        (or touchtype--session-idle-time 0.0))
         (elapsed-s   (max 0.0 (- raw-elapsed idle)))
         (minutes     (/ elapsed-s 60.0))
         ;; Gross WPM: (total-chars / 5) / minutes
         (gross-wpm   (if (> minutes 0)
                          (/ (/ (float total-keys) 5.0) minutes)
                        0.0))
         ;; Uncorrected errors: total errors minus corrections (but not below 0)
         (uncorrected (max 0 (- total-errs corrections)))
         ;; Net WPM: ((total-chars / 5) - uncorrected) / minutes
         (net-wpm     (if (> minutes 0)
                          (max 0.0 (/ (- (/ (float total-keys) 5.0) uncorrected)
                                      minutes))
                        0.0))
         ;; Accuracy
         (accuracy    (if (> total-keys 0)
                          (* 100.0 (/ (float (- total-keys total-errs))
                                      total-keys))
                        100.0))
         ;; Raw accuracy (including corrected mistakes)
         (raw-acc     (if (> total-keys 0)
                          (* 100.0 (/ (float (- total-keys total-errs))
                                      (+ total-keys corrections)))
                        100.0))
         ;; Consistency
         (line-wpms   touchtype--session-line-wpms)
         (consistency (touchtype-ui--consistency-score line-wpms))
         ;; Time formatted
         (time-min    (floor (/ elapsed-s 60.0)))
         (time-sec    (round (mod elapsed-s 60.0)))
         ;; Per-mode weak letters and n-grams
         (mode touchtype-mode-selection)
         (all-weak-letters (cl-remove-if
                            (lambda (ch)
                              (<= (touchtype-stats-get-confidence ch mode) 0.0))
                            (touchtype-stats-get-weak-letters mode)))
         (all-weak-ngrams (touchtype-stats-get-weak-ngrams 2 4 50 mode))
         ;; Personal best check (before recording this session)
         (prev-best-wpm (touchtype-stats-get-personal-best
                         touchtype-mode-selection :wpm))
         (is-pb        (and prev-best-wpm (> net-wpm prev-best-wpm))))
    ;; XP and achievements
    (touchtype-stats-record-session
     net-wpm accuracy touchtype-mode-selection touchtype--session-word-count
     :gross-wpm gross-wpm
     :total-time elapsed-s
     :total-chars total-keys
     :corrections corrections
     :uncorrected-errors uncorrected
     :consistency consistency
     :difficulty touchtype-difficulty)
    (touchtype-stats-update-streak-and-time elapsed-s)
    (let* ((streak (touchtype-stats-get-streak))
           (session-xp (touchtype-stats-xp-for-session
                        net-wpm accuracy touchtype--session-word-count
                        :streak streak
                        :is-pb is-pb
                        :difficulty touchtype-difficulty
                        :consistency consistency
                        :accuracy-perfect (>= accuracy 100)))
           (old-level (touchtype-stats-get-level)))
      (touchtype-stats-add-xp session-xp)
      (let ((new-achievements (touchtype-stats-check-achievements
                              net-wpm accuracy touchtype--session-word-count consistency))
            (new-level (touchtype-stats-get-level))
            (level-up-p (> (touchtype-stats-get-level) old-level)))
    (touchtype-stats-save)
    ;; Disable typing-area cursor lock so the end-session buffer is navigable
    (setq touchtype--current-text nil)
    (remove-hook 'post-command-hook #'touchtype-ui--enforce-point t)
    (setq-local cursor-type t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Notifications
      (insert "* Session Complete!\n")
      (when is-pb
        (insert "*New Personal Best!*\n"))
      (when level-up-p
        (let ((msg (format "Level Up! Level %d: %s"
                           new-level
                           (aref touchtype--level-titles
                                 (min new-level (1- (length touchtype--level-titles)))))))
          (insert (propertize (concat "*" msg "*\n")
                              'face '(:foreground "green" :weight bold)))))
      (when new-achievements
        (dolist (id new-achievements)
          (let ((ach (cl-find id touchtype--achievements
                              :key (lambda (a) (plist-get a :id)))))
            (when ach
              (insert (format "*Achievement Unlocked: %s*\n"
                              (plist-get ach :name)))))))
      ;; Near-miss feedback
      (let ((near-misses (touchtype-ui--near-miss-messages
                          net-wpm accuracy session-xp total-keys
                          mode is-pb new-achievements)))
        (when near-misses
          (insert "\n** Almost There!\n")
          (dolist (msg near-misses)
            (insert (format "- %s\n" msg)))))
      ;; Speed
      (insert "\n** Speed\n")
      (insert "| Metric    |  Value |\n")
      (insert "|-----------+--------|\n")
      (insert (format "| Net WPM   | %6.1f |\n" net-wpm))
      (insert (format "| Gross WPM | %6.1f |\n" gross-wpm))
      (insert (format "| Net CPM   | %6d |\n" (round (* net-wpm 5))))
      (insert (format "| Gross CPM | %6d |\n" (round (* gross-wpm 5))))
      ;; Accuracy
      (insert "\n** Accuracy\n")
      (insert "| Metric       |  Value |\n")
      (insert "|--------------+--------|\n")
      (insert (format "| Accuracy     | %5.1f%% |\n" accuracy))
      (insert (format "| Raw Accuracy | %5.1f%% |\n" raw-acc))
      ;; Session details
      (insert "\n** Session\n")
      (insert "| Metric      | Value |\n")
      (insert "|-------------+-------|\n")
      (insert (format "| Time        | %d:%02d |\n" time-min time-sec))
      (insert (format "| Words       | %d |\n" touchtype--session-word-count))
      (insert (format "| Characters  | %d |\n" total-keys))
      (insert (format "| Corrections | %d |\n" corrections))
      (insert (format "| Uncorrected | %d |\n" uncorrected))
      (insert (format "| Consistency | %.0f%% |\n" consistency))
      (when (> touchtype--best-word-streak 0)
        (insert (format "| Best Streak | %d words |\n" touchtype--best-word-streak)))
      ;; WPM sparkline
      (when (>= (length line-wpms) 2)
        (let* ((wpms (reverse line-wpms))
               (sparkline (touchtype-ui--wpm-sparkline wpms)))
          (insert (format "| WPM Graph   | %s (%.0f–%.0f) |\n"
                          sparkline
                          (apply #'min wpms)
                          (apply #'max wpms)))))
      ;; Streak and practice time
      (let ((streak (touchtype-stats-get-streak))
            (total-secs (touchtype-stats-get-total-practice-time))
            (freezes (touchtype-stats-get-streak-freezes))
            (best-streak (touchtype-stats-get-best-streak)))
        (insert (format "| Streak      | %d day%s |\n" streak (if (= streak 1) "" "s")))
        (when (> best-streak streak)
          (insert (format "| Best Streak | %d days |\n" best-streak)))
        (insert (format "| Freezes     | %d |\n" freezes))
        (insert (format "| Total Time  | %s |\n" (touchtype-ui--format-duration total-secs))))
      ;; XP and level display
      (let* ((xp-to-next (touchtype-stats-xp-to-next-level))
             (level-title (aref touchtype--level-titles
                                (min new-level (1- (length touchtype--level-titles)))))
             (breakdown (touchtype-stats-xp-breakdown
                         net-wpm accuracy touchtype--session-word-count
                         :streak streak :is-pb is-pb
                         :difficulty touchtype-difficulty
                         :consistency consistency
                         :accuracy-perfect (>= accuracy 100)))
             (total-mult (plist-get breakdown :total-mult)))
        (if (and touchtype-xp-multipliers-enabled (> total-mult 1.0))
            (insert (format "| XP          | +%d (base %d x %.2f) Level %d: %s |\n"
                            session-xp (plist-get breakdown :base)
                            total-mult new-level level-title))
          (insert (format "| XP          | +%s Level %d: %s |\n"
                          (number-to-string session-xp)
                          new-level level-title)))
        (let ((xp-bar (touchtype-ui--xp-progress-bar)))
          (insert (format "| Progress    | %s |\n" xp-bar))))
      ;; Session delta (vs rolling average)
      (let ((avg-wpm (touchtype-stats-get-rolling-average
                      touchtype-rolling-average-window :wpm))
            (avg-acc (touchtype-stats-get-rolling-average
                      touchtype-rolling-average-window :accuracy)))
        (when avg-wpm
          (let* ((wpm-delta (- net-wpm avg-wpm))
                 (acc-delta (- accuracy avg-acc))
                 (wpm-sign (if (>= wpm-delta 0) "+" ""))
                 (acc-sign (if (>= acc-delta 0) "+" ""))
                 (wpm-face (if (>= wpm-delta 0) 'touchtype-face-correct 'touchtype-face-wrong))
                 (acc-face (if (>= acc-delta 0) 'touchtype-face-correct 'touchtype-face-wrong)))
            (insert "\nvs " (format "%d-session avg: " touchtype-rolling-average-window))
            (insert (propertize (format "%s%.1f WPM" wpm-sign wpm-delta) 'face wpm-face))
            (insert "  ")
            (insert (propertize (format "%s%.1f%% acc" acc-sign acc-delta) 'face acc-face))
            (insert "\n"))))
      ;; Weakest letters as org table
      (let ((mode-label (symbol-name mode)))
        (when all-weak-letters
          (insert (format "\n** Weakest Letters (%s)\n" mode-label))
          (insert "| Letter | Confidence |\n")
          (insert "|--------+------------|\n")
          (dolist (ch all-weak-letters)
            (insert (format "| %c      |       %.2f |\n"
                            ch (touchtype-stats-get-confidence ch mode)))))
        (when all-weak-ngrams
          (insert (format "\n** Weakest N-grams (%s)\n" mode-label))
          (insert "| N-gram | Confidence |\n")
          (insert "|--------+------------|\n")
          (dolist (entry all-weak-ngrams)
            (insert (format "| %-6s |       %.2f |\n"
                            (car entry)
                            (touchtype-stats-get-bigram-confidence
                             (car entry) mode))))))
      ;; Keyboard heatmap
      (insert "\n** Keyboard Heatmap\n")
      (let* ((rows (pcase touchtype-keyboard-layout
                     ('qwerty  touchtype--qwerty-keyboard-rows)
                     ('dvorak  touchtype--dvorak-keyboard-rows)
                     ('colemak touchtype--colemak-keyboard-rows)
                     ('workman touchtype--workman-keyboard-rows)
                     (_        touchtype--qwerty-keyboard-rows)))
             (indents '("  " "   " "    ")))
        (cl-loop for row in rows
                 for indent in indents
                 do (insert indent)
                 do (dotimes (i (length row))
                      (let* ((ch (aref row i))
                             (conf (touchtype-stats-get-confidence ch))
                             (face (touchtype-ui--heatmap-face conf)))
                        (insert (propertize (format "[%c]" ch) 'font-lock-face face))))
                 do (insert "\n"))
        (insert "  Legend: "
                (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-cold) " no data  "
                (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-struggling) " <0.3  "
                (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-developing) " 0.3-0.6  "
                (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-good) " >=0.6\n"))
      (insert "\n~TAB: fold/unfold  r: restart  q: quit~\n"))
    (org-mode)
    (org-table-map-tables #'org-table-align t)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (local-set-key (kbd "r") #'touchtype-ui--restart-session)
    (local-set-key (kbd "q") #'touchtype-ui--quit)))))

(defun touchtype-ui--restart-session ()
  "Reset counters and start a new session."
  (interactive)
  (touchtype-ui--cancel-session-timer)
  (touchtype-ui--stop-pace-caret)
  (setq touchtype--session-wpm-samples nil
        touchtype--session-errors      0
        touchtype--session-total-keys  0
        touchtype--session-word-count  0
        touchtype--session-corrections 0
        touchtype--session-start-time  nil
        touchtype--session-line-wpms   nil
        touchtype--typed-chars         nil
        touchtype--session-idle-time   0.0
        touchtype--preview-texts       nil
        touchtype--completed-lines     nil
        touchtype--paused              nil
        touchtype--pause-start-time    nil
        touchtype--word-streak         0
        touchtype--best-word-streak    0
        touchtype--perfect-line-achieved nil)
  (when (overlayp touchtype--pause-overlay)
    (delete-overlay touchtype--pause-overlay)
    (setq touchtype--pause-overlay nil))
  ;; Re-enable typing-area cursor lock and hide cursor
  (setq-local cursor-type nil)
  (add-hook 'post-command-hook #'touchtype-ui--enforce-point nil t)
  (use-local-map touchtype-ui--keymap)
  (touchtype-ui--render-new-line))

(defun touchtype-ui--at-leading-whitespace-p ()
  "Return non-nil if cursor is in leading whitespace of the current line."
  (let ((pos touchtype--cursor-pos)
        (text touchtype--current-text))
    (and (< pos (length text))
         (= (aref text pos) ?\s)
         (cl-loop for i below pos always (= (aref text i) ?\s)))))

(defun touchtype-ui--fill-indentation ()
  "Simulate space inputs for all remaining leading whitespace."
  (let ((text touchtype--current-text))
    (while (and (< touchtype--cursor-pos (length text))
                (= (aref text touchtype--cursor-pos) ?\s))
      (touchtype-ui--process-char ?\s))))

(defun touchtype-ui--handle-tab ()
  "Handle TAB: fill leading whitespace in code mode, or quick-restart."
  (interactive)
  (if (and (eq touchtype-mode-selection 'code)
           (touchtype-ui--at-leading-whitespace-p))
      (touchtype-ui--fill-indentation)
    (touchtype-ui--quick-restart)))

(defun touchtype-ui--quick-restart ()
  "Instantly restart the session without confirmation."
  (interactive)
  (touchtype-ui--restart-session))

;;;; Time formatting

(defun touchtype-ui--format-duration (seconds)
  "Format SECONDS as a human-readable duration string.
Shows hours, minutes, and seconds (e.g. \"2h 15m 30s\").
Omits zero-valued leading components."
  (let* ((s (truncate seconds))
         (h (/ s 3600))
         (m (/ (% s 3600) 60))
         (sec (% s 60)))
    (cond
     ((> h 0) (format "%dh %dm %ds" h m sec))
     ((> m 0) (format "%dm %ds" m sec))
     (t       (format "%ds" sec)))))

;;;; ASCII bar charts

(defconst touchtype-ui--bar-width 20
  "Default width in characters for ASCII bar charts in stats tables.")

(defun touchtype-ui--ascii-bar (value max-value width)
  "Return an ASCII bar of WIDTH chars representing VALUE out of MAX-VALUE."
  (let* ((ratio (if (> max-value 0)
                    (min 1.0 (max 0.0 (/ (float value) max-value)))
                  0.0))
         (filled (round (* ratio width)))
         (empty (- width filled)))
    (concat (make-string filled ?█) (make-string empty ?░))))

;;;; Timed session support

(defun touchtype-ui--cancel-session-timer ()
  "Cancel the timed session timer if active."
  (when (timerp touchtype--session-timer)
    (cancel-timer touchtype--session-timer)
    (setq touchtype--session-timer nil)))

(defun touchtype-ui--timed-session-expire (buf)
  "Called when a timed session expires in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq touchtype--session-timer nil)
      (touchtype-ui--end-session))))

;;;; WPM sparkline

(defconst touchtype-ui--bar-chars "▁▂▃▄▅▆▇█"
  "Unicode bar characters for sparkline rendering, ascending height.")

(defun touchtype-ui--wpm-sparkline (wpms)
  "Return a sparkline string for WPMS list.
Maps values to bar characters scaled min-to-max."
  (if (null wpms)
      ""
    (let* ((lo (apply #'min wpms))
           (hi (apply #'max wpms))
           (range (- hi lo))
           (n-bars (length touchtype-ui--bar-chars)))
      (apply #'string
             (mapcar (lambda (v)
                       (let ((idx (if (zerop range)
                                      (/ n-bars 2)
                                    (min (1- n-bars)
                                         (floor (* (/ (- v lo) (float range))
                                                   (1- n-bars)))))))
                         (aref touchtype-ui--bar-chars idx)))
                     wpms)))))

;;;; Pace caret

(defun touchtype-ui--start-pace-caret ()
  "Start the pace caret timer for the current line."
  (touchtype-ui--stop-pace-caret)
  (setq touchtype--pace-pos 0)
  (let ((interval (/ 60.0 (* touchtype-target-wpm 5.0))))
    (setq touchtype--pace-timer
          (run-at-time interval interval
                       #'touchtype-ui--advance-pace-caret
                       (current-buffer)))))

(defun touchtype-ui--stop-pace-caret ()
  "Cancel the pace caret timer and remove its overlay."
  (when (timerp touchtype--pace-timer)
    (cancel-timer touchtype--pace-timer)
    (setq touchtype--pace-timer nil))
  (when (overlayp touchtype--pace-overlay)
    (delete-overlay touchtype--pace-overlay)
    (setq touchtype--pace-overlay nil)))

(defun touchtype-ui--advance-pace-caret (buf)
  "Move the pace caret one position forward in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((n (length touchtype--current-text)))
        (when (< touchtype--pace-pos n)
          ;; Remove old overlay
          (when (overlayp touchtype--pace-overlay)
            (delete-overlay touchtype--pace-overlay))
          ;; Place new overlay
          (let* ((buf-pos (+ (marker-position touchtype--target-start)
                             touchtype--pace-pos))
                 (ov (make-overlay buf-pos (1+ buf-pos))))
            (overlay-put ov 'face 'touchtype-face-pace-caret)
            (overlay-put ov 'priority 3)
            (setq touchtype--pace-overlay ov))
          (cl-incf touchtype--pace-pos)
          ;; Stop when past end
          (when (>= touchtype--pace-pos n)
            (touchtype-ui--stop-pace-caret)))))))

;;;; Heatmap and finger stats helpers

(defun touchtype-ui--heatmap-face (confidence)
  "Return the face for CONFIDENCE value in the keyboard heatmap."
  (cond
   ((<= confidence 0.0)  'touchtype-face-heatmap-cold)
   ((< confidence 0.3)   'touchtype-face-heatmap-struggling)
   ((< confidence 0.6)   'touchtype-face-heatmap-developing)
   (t                    'touchtype-face-heatmap-good)))

(defun touchtype-ui--render-keyboard-heatmap ()
  "Insert a keyboard heatmap showing per-key confidence with color coding."
  (let* ((rows (pcase touchtype-keyboard-layout
                 ('qwerty  touchtype--qwerty-keyboard-rows)
                 ('dvorak  touchtype--dvorak-keyboard-rows)
                 ('colemak touchtype--colemak-keyboard-rows)
                 ('workman touchtype--workman-keyboard-rows)
                 (_        touchtype--qwerty-keyboard-rows)))
         (indents '("  " "   " "    ")))
    (insert "* Keyboard Heatmap\n")
    (cl-loop for row in rows
             for indent in indents
             do (insert indent)
             do (dotimes (i (length row))
                  (let* ((ch (aref row i))
                         (conf (touchtype-stats-get-confidence ch))
                         (face (touchtype-ui--heatmap-face conf)))
                    (insert (propertize (format "[%c]" ch) 'font-lock-face face))))
             do (insert "\n"))
    (insert "  Legend: "
            (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-cold) " no data  "
            (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-struggling) " <0.3  "
            (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-developing) " 0.3-0.6  "
            (propertize "[x]" 'font-lock-face 'touchtype-face-heatmap-good) " >=0.6\n\n")))

(defun touchtype-ui--render-finger-stats ()
  "Insert per-finger performance section as an org table."
  (insert "* Per-Finger Performance\n")
  (let ((finger-stats (touchtype-stats-get-finger-stats))
        (finger-order '(left-pinky left-ring left-middle left-index
                        right-index right-middle right-ring right-pinky)))
    (if (cl-every (lambda (f)
                    (let ((entry (assq f finger-stats)))
                      (or (null entry) (= (plist-get (cdr entry) :hits) 0))))
                  finger-order)
        (insert "(not enough data yet)\n")
      (insert "| Finger    | Accuracy | Bar                  | Avg ms | Hits |\n")
      (insert "|-----------+----------+----------------------+--------+------|\n")
      (dolist (finger finger-order)
        (let* ((entry (assq finger finger-stats))
               (name (cdr (assq finger touchtype--finger-names)))
               (hits (if entry (plist-get (cdr entry) :hits) 0))
               (accuracy (if entry (plist-get (cdr entry) :accuracy) 0.0))
               (avg-ms (if entry (plist-get (cdr entry) :avg-ms) 0.0)))
          (when (> hits 0)
            (insert (format "| %-9s | %6.1f%% | %s | %6.0f | %4d |\n"
                            name accuracy
                            (touchtype-ui--ascii-bar accuracy 100.0 touchtype-ui--bar-width)
                            avg-ms hits)))))))
  (insert "\n"))

;;;; Progress charts

(defun touchtype-ui--render-progress-chart (values label unit)
  "Render a multi-row vertical bar chart for VALUES.
LABEL is the chart title (e.g. \"WPM\"), UNIT is the value unit.
Chart uses 8 rows matching the 8 levels of `touchtype-ui--bar-chars'."
  (when (and values (> (length values) 0))
    (let* ((n (length values))
           (lo (apply #'min values))
           (hi (apply #'max values))
           (avg (/ (cl-reduce #'+ values) (float n)))
           (range (- hi lo))
           (height 8)
           (bar-chars touchtype-ui--bar-chars))
      (insert (format "  %s (last %d sessions, avg %.0f%s)\n"
                       label n avg unit))
      ;; Render rows top to bottom
      (dotimes (row height)
        (let* ((inv-row (- height 1 row))
               (threshold (if (zerop range)
                             (if (>= inv-row (/ height 2)) 1.0 0.0)
                           (/ (* inv-row range) (float (1- height))))))
          ;; Y-axis label on first and last rows
          (if (= row 0)
              (insert (format "%4.0f ┤" hi))
            (if (= row (1- height))
                (insert (format "%4.0f ┤" lo))
              (insert "     │")))
          ;; Columns
          (dolist (v values)
            (let* ((normalized (if (zerop range) 0.5
                                (/ (- v lo) (float range))))
                   (level (min (1- height)
                               (floor (* normalized (1- height)))))
                   ;; level is 0..7 from bottom
                   ;; inv-row is 0..7 from bottom (7=top, 0=bottom)
                   )
              (cond
               ((> level inv-row)
                ;; Full block
                (insert (aref bar-chars (1- height))))
               ((= level inv-row)
                ;; Partial block
                (let ((frac (if (zerop range) 4
                              (let* ((step (/ range (float height)))
                                     (cell-bottom (+ lo (* inv-row step)))
                                     (within (- v cell-bottom))
                                     (idx (min (1- height)
                                               (floor (* (/ within step) height)))))
                                (max 0 idx)))))
                  (insert (aref bar-chars frac))))
               (t
                (insert " ")))))
          (insert "\n")))
      (insert "\n"))))

;;;; Stats view

(defun touchtype-ui-show-stats ()
  "Display a summary of all-time typing statistics in org-mode."
  (let ((buf (get-buffer-create "*touchtype-stats*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (touchtype-stats-load)
        (let* ((letter-stats (plist-get touchtype--stats :letter-stats))
               (sessions     (plist-get touchtype--stats :sessions))
               (n-sessions   (length sessions))
               (total-words  (cl-reduce #'+ sessions
                                        :key (lambda (s) (plist-get (cdr s) :words))
                                        :initial-value 0))
               (all-wpm      (mapcar (lambda (s) (plist-get (cdr s) :wpm)) sessions))
               (all-acc      (mapcar (lambda (s) (plist-get (cdr s) :accuracy)) sessions))
               (avg-wpm      (if (> n-sessions 0)
                                 (/ (cl-reduce #'+ all-wpm) (float n-sessions))
                               0.0))
               (avg-acc      (if (> n-sessions 0)
                                 (/ (cl-reduce #'+ all-acc) (float n-sessions))
                               0.0)))
          (insert "#+TITLE: Touchtype Statistics\n\n")
          ;; 1. Overall summary
          (insert "* Overall Summary\n")
          (let ((streak (touchtype-stats-get-streak))
                (total-secs (touchtype-stats-get-total-practice-time)))
            (insert "| Metric         | Value |\n")
            (insert "|----------------+-------|\n")
            (insert (format "| Sessions       | %d |\n" n-sessions))
            (insert (format "| Words          | %d |\n" total-words))
            (insert (format "| Avg WPM        | %.1f |\n" avg-wpm))
            (insert (format "| Avg Accuracy   | %.1f%% |\n" avg-acc))
            (insert (format "| Streak         | %d day%s |\n"
                            streak (if (= streak 1) "" "s")))
            (insert (format "| Total Practice | %s |\n"
                            (touchtype-ui--format-duration total-secs))))
          (insert "\n")
          ;; 1b. Keyboard heatmap
          (touchtype-ui--render-keyboard-heatmap)
          ;; 2. Per-letter confidence
          (insert "* Per-Letter Confidence\n")
          (let ((has-data nil))
            (dolist (ch (touchtype-stats-get-weak-letters))
              (let* ((entry (assq ch letter-stats))
                     (hits  (if entry (touchtype-stats--entry-get entry :hits) 0))
                     (conf  (touchtype-stats-get-confidence ch)))
                (when (> hits 0)
                  (unless has-data
                    (insert "| Letter | Confidence | Bar                  | Hits |\n")
                    (insert "|--------+------------+----------------------+------|\n")
                    (setq has-data t))
                  (insert (format "| %c      | %10.2f | %s | %4d |\n"
                                  ch conf
                                  (touchtype-ui--ascii-bar conf 1.0 touchtype-ui--bar-width)
                                  hits)))))
            (unless has-data
              (insert "(not enough data yet)\n")))
          ;; 2b. Per-Mode Confidence
          (let ((mode-lstats (plist-get touchtype--stats :mode-letter-stats)))
            (when mode-lstats
              (insert "\n* Per-Mode Confidence\n")
              (dolist (mode-entry mode-lstats)
                (let* ((m (car mode-entry))
                       (m-letters (cdr mode-entry)))
                  (when m-letters
                    (insert (format "** %s\n" m))
                    (let ((weak (seq-take
                                 (sort (cl-remove-if
                                        (lambda (ch)
                                          (<= (touchtype-stats-get-confidence ch m) 0.0))
                                        (touchtype-stats-get-weak-letters m))
                                       (lambda (a b)
                                         (< (touchtype-stats-get-confidence a m)
                                            (touchtype-stats-get-confidence b m))))
                                 10))
                          (has-data nil))
                      (dolist (ch weak)
                        (let* ((entry (assq ch m-letters))
                               (hits  (if entry (touchtype-stats--entry-get entry :hits) 0))
                               (conf  (touchtype-stats-get-confidence ch m)))
                          (when (> hits 0)
                            (unless has-data
                              (insert "| Letter | Confidence | Bar                  | Hits |\n")
                              (insert "|--------+------------+----------------------+------|\n")
                              (setq has-data t))
                            (insert (format "| %c      | %10.2f | %s | %4d |\n"
                                            ch conf
                                            (touchtype-ui--ascii-bar conf 1.0 touchtype-ui--bar-width)
                                            hits)))))))))))
          ;; 2c. Per-finger performance
          (insert "\n")
          (touchtype-ui--render-finger-stats)
          ;; 3. Weakest N-grams
          (insert "* Weakest N-grams\n")
          (let ((weak-ngrams (touchtype-stats-get-weak-ngrams 2 4 10)))
            (if (null weak-ngrams)
                (insert "(not enough data yet)\n")
              (insert "| N-gram | Confidence | Bar                  | Hits |\n")
              (insert "|--------+------------+----------------------+------|\n")
              (dolist (entry weak-ngrams)
                (let* ((ng   (car entry))
                       (conf (touchtype-stats-get-bigram-confidence ng))
                       (hits (touchtype-stats--entry-get entry :hits)))
                  (insert (format "| %-6s | %10.2f | %s | %4d |\n"
                                  ng conf
                                  (touchtype-ui--ascii-bar conf 1.0 touchtype-ui--bar-width)
                                  hits))))))
          ;; 4. Trends
          (insert "\n* Trends\n")
          (let* ((wpm-trend (touchtype-stats-get-wpm-trend))
                 (acc-trend (touchtype-stats-get-accuracy-trend))
                 (wpm-dir   (touchtype-stats-get-trend-direction wpm-trend))
                 (acc-dir   (touchtype-stats-get-trend-direction acc-trend))
                 (dir-char  (lambda (d) (pcase d
                                          ('improving "↑")
                                          ('declining "↓")
                                          (_          "→")))))
            (if (null wpm-trend)
                (insert "(no sessions recorded yet)\n")
              (insert (format "- WPM trend (%s): %s\n"
                              (funcall dir-char wpm-dir)
                              (mapconcat (lambda (v) (format "%.0f" v))
                                         wpm-trend " → ")))
              (let ((wpm-max (apply #'max wpm-trend)))
                (insert "#+begin_example\n")
                (cl-loop for v in wpm-trend
                         for i from 1
                         do (insert (format "  %2d. %s %.0f\n"
                                            i (touchtype-ui--ascii-bar v wpm-max 15) v)))
                (insert "#+end_example\n"))
              (insert (format "- Acc trend (%s): %s\n"
                              (funcall dir-char acc-dir)
                              (mapconcat (lambda (v) (format "%.0f%%" v))
                                         acc-trend " → ")))
              (let ((acc-max 100.0))
                (insert "#+begin_example\n")
                (cl-loop for v in acc-trend
                         for i from 1
                         do (insert (format "  %2d. %s %.0f%%\n"
                                            i (touchtype-ui--ascii-bar v acc-max 15) v)))
                (insert "#+end_example\n"))))
          ;; 4b. Progress charts
          (when (> n-sessions 1)
            (insert "\n* Progress\n")
            (let* ((chart-sessions (seq-take sessions touchtype-stats-progress-length))
                   (chart-wpms (nreverse (mapcar (lambda (s) (plist-get (cdr s) :wpm))
                                                  chart-sessions)))
                   (chart-accs (nreverse (mapcar (lambda (s) (plist-get (cdr s) :accuracy))
                                                  chart-sessions))))
              (insert "#+begin_example\n")
              (touchtype-ui--render-progress-chart chart-wpms "WPM" "")
              (touchtype-ui--render-progress-chart chart-accs "Accuracy" "%")
              (insert "#+end_example\n")))
          ;; 5. Session history
          (insert "\n* Session History\n")
          (let ((recent (seq-take sessions touchtype-stats-history-length)))
            (if (null recent)
                (insert "(no sessions recorded yet)\n")
              (let ((max-wpm (apply #'max (mapcar (lambda (s) (plist-get (cdr s) :wpm)) recent))))
                (insert "| Date | WPM | Bar                  | Accuracy | Mode | Words |\n")
                (insert "|------+-----+----------------------+----------+------+-------|\n")
                (dolist (s recent)
                  (let ((wpm (plist-get (cdr s) :wpm)))
                    (insert (format "| %s | %.1f | %s | %.1f%% | %s | %d |\n"
                                    (car s)
                                    wpm
                                    (touchtype-ui--ascii-bar wpm max-wpm touchtype-ui--bar-width)
                                    (plist-get (cdr s) :accuracy)
                                    (plist-get (cdr s) :mode)
                                    (plist-get (cdr s) :words))))))))
          ;; 7. Personal Bests
          (insert "\n* Personal Bests\n")
          (let ((bests (touchtype-stats-get-all-personal-bests)))
            (if (null bests)
                (insert "(no sessions recorded yet)\n")
              (let ((max-wpm (apply #'max (mapcar (lambda (e) (or (plist-get (cdr e) :wpm) 0.0)) bests))))
                (insert "| Mode | WPM | Bar                  | Accuracy |\n")
                (insert "|------+-----+----------------------+----------|\n")
                (dolist (entry bests)
                  (let ((mode (car entry))
                        (best-wpm (or (plist-get (cdr entry) :wpm) 0.0))
                        (best-acc (or (plist-get (cdr entry) :accuracy) 0.0)))
                    (insert (format "| %s | %.1f | %s | %.1f%% |\n"
                                    mode best-wpm
                                    (touchtype-ui--ascii-bar best-wpm max-wpm touchtype-ui--bar-width)
                                    best-acc)))))))
          ;; 8. XP / Level
          (let* ((xp (touchtype-stats-get-xp))
                 (level (touchtype-stats-get-level))
                 (title (aref touchtype--level-titles
                              (min level (1- (length touchtype--level-titles)))))
                 (to-next (touchtype-stats-xp-to-next-level)))
            (insert "\n* Level / XP\n")
            (insert (format "Level %d: %s (%d XP)" level title xp))
            (when (> to-next 0)
              (insert (format " — %d XP to next level" to-next)))
            (insert "\n\n")
            (insert "| # | Status | Title | XP Required |\n")
            (insert "|---+--------+-------+-------------|\n")
            (dotimes (i (length touchtype--xp-level-thresholds))
              (let* ((lvl-title (aref touchtype--level-titles
                                      (min i (1- (length touchtype--level-titles)))))
                     (lvl-xp (aref touchtype--xp-level-thresholds i))
                     (marker (cond ((< i level) "✓")
                                   ((= i level) "►")
                                   (t ""))))
                (insert (format "| %d | %s | %s | %d |\n"
                                (1+ i) marker lvl-title lvl-xp)))))
          ;; 9. Achievements
          (insert "\n* Achievements\n")
          (let ((earned (touchtype-stats-get-achievements)))
            (dolist (ach touchtype--achievements)
              (let* ((id (plist-get ach :id))
                     (name (plist-get ach :name))
                     (desc (plist-get ach :desc))
                     (got (memq id earned)))
                (insert (format "- [%s] %s — %s\n"
                                (if got "X" " ") name desc))))))
        (insert "\n"))
      (org-mode)
      (org-table-map-tables #'org-table-align t)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'kill-current-buffer))
    (switch-to-buffer buf)))

(provide 'touchtype-ui)

;;; touchtype-ui.el ends here
