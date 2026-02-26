;;; touchtype-ui.el --- Buffer rendering and input loop for touchtype -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

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

;;;; Keymap

(defvar touchtype-ui--keymap
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    ;; Bind all printable ASCII characters (space through tilde)
    (dolist (c (number-sequence 32 126))
      (define-key map (vector c) #'touchtype-ui--handle-char))
    (define-key map (kbd "DEL")   #'touchtype-ui--handle-backspace)
    (define-key map (kbd "M-DEL") #'touchtype-ui--handle-word-backspace)
    (define-key map (kbd "RET")   #'touchtype-ui--ignore-key)
    (define-key map (kbd "C-g")   #'touchtype-ui--quit)
    map)
  "Keymap used in the *touchtype* training buffer.
Captures all printable keys and routes them to the typing handler.")

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
                       touchtype--unlocked-keys))
          (make-local-variable sym))
        ;; Load persisted state
        (touchtype-stats-load)
        (setq touchtype--unlocked-keys (touchtype-stats-get-unlocked-keys))
        (setq touchtype--session-wpm-samples nil
              touchtype--session-errors      0
              touchtype--session-total-keys  0
              touchtype--session-word-count  0
              touchtype--focused-key         nil
              touchtype--typed-chars         nil)
        (use-local-map touchtype-ui--keymap)
        (setq-local cursor-type nil)
        (setq buffer-read-only t)))
    (switch-to-buffer buf)
    (touchtype-ui--render-new-line)))

;;;; Buffer rendering

(defun touchtype-ui--render-new-line ()
  "Generate a new target line and render the full buffer."
  (setq touchtype--current-text (touchtype-algo-generate-line))
  (setq touchtype--cursor-pos   0
        touchtype--typed-chars  nil
        touchtype--line-start-time (float-time)
        touchtype--last-key-time   (float-time))
  (touchtype-ui--redraw-buffer))

(defun touchtype-ui--redraw-buffer ()
  "Redraw the entire *touchtype* buffer content."
  (let ((inhibit-read-only t)
        (text touchtype--current-text))
    (erase-buffer)
    ;; Vertical padding
    (insert "\n\n")
    ;; Single text line: each character gets an overlay starting as gray.
    ;; The user types "over" this text; overlays change face as keys are pressed.
    (insert "  ")
    (setq touchtype--target-start (point-marker))
    (let ((n (length text)))
      (setq touchtype--char-overlays (make-vector n nil))
      (dotimes (i n)
        (let* ((buf-pos (point))
               (ov      (make-overlay buf-pos (1+ buf-pos))))
          (insert (aref text i))
          (overlay-put ov 'face 'touchtype-face-untyped)
          (aset touchtype--char-overlays i ov))))
    (insert "\n\n")
    ;; Status line
    (setq touchtype--status-start (point-marker))
    (insert (touchtype-ui--status-string))
    (insert "\n")
    ;; Cursor underline on first character
    (touchtype-ui--update-cursor-overlay)))

(defun touchtype-ui--update-cursor-overlay ()
  "Underline the character at `touchtype--cursor-pos' in the text line."
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
        (setq touchtype--cursor-overlay ov)))))

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
  "Rewrite the status line in place."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position touchtype--status-start))
      (delete-region (point) (line-end-position))
      (insert (touchtype-ui--status-string)))))

(defun touchtype-ui--status-string ()
  "Return a formatted status string for the current session state."
  (let* ((samples touchtype--session-wpm-samples)
         (wpm     (if samples
                      (format "%.0f" (/ (cl-reduce #'+ samples)
                                        (float (length samples))))
                    "--"))
         (total   touchtype--session-total-keys)
         (acc     (if (> total 0)
                      (format "%.0f%%"
                              (* 100.0 (/ (float (- total touchtype--session-errors))
                                          total)))
                    "--"))
         (mode    (symbol-name touchtype-mode-selection))
         (keys    (if (eq touchtype-mode-selection 'progressive)
                      (format "  Keys: %s" touchtype--unlocked-keys)
                    ""))
         (words   (format "  Words: %d/%d"
                          touchtype--session-word-count
                          touchtype-session-length)))
    (propertize
     (format "  WPM: %s  Accuracy: %s  Mode: %s%s%s"
             wpm acc mode keys words)
     'face 'touchtype-face-status)))

;;;; Input handling

(defun touchtype-ui--handle-char ()
  "Handle a printable character keypress."
  (interactive)
  (let ((char last-command-event))
    (when (characterp char)
      (touchtype-ui--process-char char))))

(defun touchtype-ui--process-char (char)
  "Process CHAR typed by the user against the current target text."
  (let* ((pos     touchtype--cursor-pos)
         (text    touchtype--current-text)
         (n       (length text))
         (now     (float-time))
         (elapsed (if touchtype--last-key-time
                      (round (* 1000 (- now touchtype--last-key-time)))
                    200))
         (expected (when (< pos n) (aref text pos)))
         (correct-p (and expected (= char expected))))
    (when (< pos n)
      (setq touchtype--last-key-time now)
      ;; Record the keypress in stats
      (touchtype-stats-record-keypress char correct-p elapsed)
      ;; Record bigram if we have a previous char
      (when (> pos 0)
        (let ((prev-char (aref text (1- pos))))
          (touchtype-stats-record-bigram
           (string prev-char char) correct-p elapsed)))
      ;; Visual feedback
      (touchtype-ui--update-typed-char
       pos char
       (if correct-p 'touchtype-face-correct 'touchtype-face-wrong))
      ;; Track for status display
      (push (list char correct-p elapsed) touchtype--typed-chars)
      (cl-incf touchtype--session-total-keys)
      (unless correct-p (cl-incf touchtype--session-errors))
      ;; Advance cursor
      (setq touchtype--cursor-pos (1+ pos))
      (touchtype-ui--update-cursor-overlay)
      (touchtype-ui--update-status)
      ;; Check for line completion
      (when (>= touchtype--cursor-pos n)
        (touchtype-ui--advance-line)))))

(defun touchtype-ui--handle-backspace ()
  "Remove the last typed character and rewind the cursor."
  (interactive)
  (when (> touchtype--cursor-pos 0)
    (cl-decf touchtype--cursor-pos)
    (touchtype-ui--update-typed-space touchtype--cursor-pos)
    (when touchtype--typed-chars
      (setq touchtype--typed-chars (cdr touchtype--typed-chars)))
    (touchtype-ui--update-cursor-overlay)
    (touchtype-ui--update-status)))

(defun touchtype-ui--handle-word-backspace ()
  "Remove typed characters back to the previous word boundary."
  (interactive)
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
  (1+ (cl-count ?\s touchtype--current-text)))

(defun touchtype-ui--advance-line ()
  "Complete the current line and set up the next one."
  (let* ((wpm        (touchtype-ui--line-wpm))
         (n-correct  (length (cl-remove-if-not #'cadr touchtype--typed-chars)))
         (n-total    (length touchtype--typed-chars))
         (word-count (touchtype-ui--line-word-count)))
    ;; Record WPM sample
    (when (> wpm 0)
      (push wpm touchtype--session-wpm-samples))
    ;; Count words toward session total
    (cl-incf touchtype--session-word-count word-count)
    ;; Check for key unlock (progressive mode only)
    (when (and (eq touchtype-mode-selection 'progressive)
               (touchtype-algo-should-unlock-p))
      (let ((new-key (touchtype-algo-unlock-next-key)))
        (when new-key
          (message "Unlocked new key: %c  Keep typing to build confidence!"
                   new-key))))
    ;; Update accuracy in status
    (ignore n-correct n-total)
    ;; Check session end
    (if (>= touchtype--session-word-count touchtype-session-length)
        (touchtype-ui--end-session)
      (touchtype-ui--render-new-line))))

(defun touchtype-ui--end-session ()
  "Display the session summary and prompt to continue or quit."
  (let* ((samples  touchtype--session-wpm-samples)
         (avg-wpm  (if samples
                       (/ (cl-reduce #'+ samples)
                          (float (length samples)))
                     0.0))
         (total-keys touchtype--session-total-keys)
         (accuracy   (if (> total-keys 0)
                         (* 100.0 (/ (float (- total-keys touchtype--session-errors))
                                     total-keys))
                       100.0))
         (weak       (seq-take (touchtype-stats-get-weak-letters) 3)))
    ;; Record the session
    (touchtype-stats-record-session avg-wpm accuracy
                                    touchtype-mode-selection
                                    touchtype--session-word-count)
    (touchtype-stats-save)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n")
      (insert (propertize "  Session Complete!\n\n" 'face 'bold))
      (insert (format "  WPM:      %.1f\n" avg-wpm))
      (insert (format "  Accuracy: %.1f%%\n" accuracy))
      (insert (format "  Words:    %d\n\n" touchtype--session-word-count))
      (when weak
        (insert "  Focus letters (lowest confidence):\n")
        (dolist (ch weak)
          (insert (format "    %c  confidence: %.2f\n"
                          ch (touchtype-stats-get-confidence ch)))))
      (insert "\n")
      (insert (propertize "  Press RET to start a new session, q to quit.\n"
                          'face 'touchtype-face-status)))
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET") #'touchtype-ui--restart-session)
       (define-key map (kbd "q")   #'touchtype-ui--quit)
       map))))

(defun touchtype-ui--restart-session ()
  "Reset counters and start a new session."
  (interactive)
  (setq touchtype--session-wpm-samples nil
        touchtype--session-errors      0
        touchtype--session-total-keys  0
        touchtype--session-word-count  0
        touchtype--typed-chars         nil)
  (use-local-map touchtype-ui--keymap)
  (touchtype-ui--render-new-line))

;;;; Stats view

(defun touchtype-ui-show-stats ()
  "Display a summary of all-time typing statistics."
  (let ((buf (get-buffer-create "*touchtype-stats*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "\n  Touchtype Statistics\n\n" 'face 'bold))
        (touchtype-stats-load)
        (let* ((letter-stats (plist-get touchtype--stats :letter-stats))
               (sessions     (plist-get touchtype--stats :sessions)))
          ;; Per-letter confidence
          (insert "  Per-letter confidence (sorted by weakness):\n\n")
          (dolist (ch (touchtype-stats-get-weak-letters))
            (let* ((entry  (assq ch letter-stats))
                   (hits   (if entry (touchtype-stats--entry-get entry :hits) 0))
                   (conf   (touchtype-stats-get-confidence ch))
                   (bar    (make-string (round (* 20 conf)) ?|))
                   (pad    (make-string (- 20 (round (* 20 conf))) ?.)))
              (when (> hits 0)
                (insert (format "    %c  [%s%s] %.2f  (%d hits)\n"
                                ch bar pad conf hits)))))
          (insert "\n  Recent sessions:\n\n")
          (dolist (s (seq-take sessions 5))
            (insert (format "    %s  WPM: %.1f  Acc: %.1f%%  Mode: %s\n"
                            (car s)
                            (plist-get (cdr s) :wpm)
                            (plist-get (cdr s) :accuracy)
                            (plist-get (cdr s) :mode)))))
        (insert "\n")
        (setq buffer-read-only t)
        (local-set-key (kbd "q") #'kill-this-buffer)))
    (switch-to-buffer buf)))

(provide 'touchtype-ui)

;;; touchtype-ui.el ends here
