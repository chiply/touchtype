;;; touchtype.el --- Keybr-style progressive touch typing trainer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/touchtype
;; x-release-please-start-version
;; Version: 0.1.0
;; x-release-please-end
;; Package-Requires: ((emacs "29.1"))
;; Keywords: games, education
;;
;; This file is not part of GNU Emacs.
;;
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; touchtype is a keybr-style progressive touch typing trainer for Emacs.
;;
;; It generates pseudo-words from an embedded English bigram frequency table
;; so practice text feels natural from the first session.  Keys are
;; unlocked one at a time as your speed and accuracy improve, starting
;; from the home-row index keys (F and J) and expanding outward.
;;
;; Quick start:
;;   M-x touchtype-progressive   — start progressive training (recommended)
;;   M-x touchtype-full-words    — practice with real English words
;;   M-x touchtype-bigram-drill  — drill common letter pairs
;;   M-x touchtype-stats-view    — show your all-time statistics
;;
;; Training modes:
;;   progressive           — keybr-style; starts with F/J, unlocks more keys
;;                           as confidence (speed × accuracy) reaches the
;;                           configurable threshold.
;;   full-words            — all letters; draws from a built-in word list.
;;   bigram-drill          — repeats common English bigrams: th th he he in …
;;   letters               — all 26 letters, random pseudo-words.
;;   letters+numbers       — letters plus 0–9.
;;   letters+numbers+symbols — letters, numbers, and common punctuation.
;;
;; Typing in the buffer:
;;   Printable keys  — compared to the target character; green = correct,
;;                     red = wrong.
;;   DEL             — delete the last typed character.
;;   M-DEL           — delete back to the previous word boundary.
;;   C-g             — quit and save statistics.
;;
;; Customization:
;;   `touchtype-target-wpm'        WPM target for confidence (default 40).
;;   `touchtype-unlock-threshold'  Confidence needed to unlock a key (0.80).
;;   `touchtype-session-length'    Words per session (default 30).
;;   `touchtype-stats-file'        Where statistics are persisted.

;;; Code:

(require 'touchtype-var)
(require 'touchtype-stats)
(require 'touchtype-algo)
(require 'touchtype-ui)
(require 'touchtype-narrative)

;;;; Minor mode

;;;###autoload
(define-minor-mode touchtype-mode
  "Minor mode active in the *touchtype* training buffer.
Enables the dedicated keymap and display settings.
Do not enable this mode manually; use `touchtype' or one of the
`touchtype-progressive', `touchtype-full-words', etc. commands instead."
  :lighter " TT"
  :group 'touchtype
  (if touchtype-mode
      (progn
        (setq-local truncate-lines t)
        (setq-local line-spacing 4))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'line-spacing)))

;;;; Session length

;;;###autoload
(defun touchtype-set-session-length ()
  "Set `touchtype-session-length' from `touchtype-session-length-presets'."
  (interactive)
  (let* ((names (mapcar (lambda (p)
                          (format "%s (%d words)" (car p) (cdr p)))
                        touchtype-session-length-presets))
         (choice (completing-read "Session length: " names nil t))
         (sym (intern (car (split-string choice " ")))))
    (let ((pair (assq sym touchtype-session-length-presets)))
      (when pair
        (setq touchtype-session-length (cdr pair))
        (message "Session length set to %d words (%s)"
                 (cdr pair) (car pair))))))

(defun touchtype--apply-prefix-arg (arg)
  "When prefix ARG is non-nil, set `touchtype-session-length' to its value."
  (when arg
    (setq touchtype-session-length (prefix-numeric-value arg))))

;;;; Entry point commands

;;;###autoload
(defun touchtype (&optional arg)
  "Start a touchtype session in the mode set by `touchtype-mode-selection'.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-progressive (&optional arg)
  "Start a touchtype session in progressive key-unlock mode.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'progressive)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-full-words (&optional arg)
  "Start a touchtype session using real English words.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'full-words)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-bigram-drill (&optional arg)
  "Start a touchtype session drilling common English bigrams.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'bigram-drill)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-trigram-drill (&optional arg)
  "Start a touchtype session drilling common English trigrams.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'trigram-drill)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-tetragram-drill (&optional arg)
  "Start a touchtype session drilling common English tetragrams.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'tetragram-drill)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-ngram-drill (&optional arg)
  "Start a touchtype session drilling mixed bigrams, trigrams, and tetragrams.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'ngram-drill)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-letters (&optional arg)
  "Start a touchtype session using all 26 letters.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'letters)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-letters+numbers (&optional arg)
  "Start a touchtype session using letters and digits 0–9.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'letters+numbers)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-letters+numbers+symbols (&optional arg)
  "Start a touchtype session using letters, digits, and common punctuation.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'letters+numbers+symbols)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-narrative (&optional arg)
  "Start a touchtype session using narrative text from Project Gutenberg.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'narrative)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-common-words (&optional arg)
  "Start a touchtype session using the top N common words.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'common-words)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-custom (&optional text)
  "Start a touchtype session with custom TEXT.
If region is active, use region text.  Otherwise prompt for TEXT.
Normalizes whitespace (newlines/tabs to spaces, collapses multiples)."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Custom text: "))))
  (when (or (null text) (string-empty-p (string-trim text)))
    (user-error "No text provided"))
  (let ((normalized (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim text))))
    (setq touchtype-session-type 'words)
    (setq touchtype-mode-selection 'custom)
    ;; Set passage vars before setup-buffer which makes them buffer-local
    (let ((buf (get-buffer-create "*touchtype*")))
      (with-current-buffer buf
        (make-local-variable 'touchtype--custom-passage)
        (make-local-variable 'touchtype--custom-offset)
        (setq touchtype--custom-passage normalized
              touchtype--custom-offset 0)))
    (touchtype-ui-setup-buffer)))

;;;###autoload
(defun touchtype-region ()
  "Start a touchtype session using the active region as source text."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (touchtype-custom (buffer-substring-no-properties
                     (region-beginning) (region-end))))

;;;###autoload
(defun touchtype-buffer ()
  "Start a touchtype session using the entire current buffer as source text."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-empty-p (string-trim text))
      (user-error "Buffer is empty"))
    (touchtype-custom text)))

;;;###autoload
(defun touchtype-code (&optional arg)
  "Start a touchtype session with code snippets.
With prefix ARG, set session length to that number of words."
  (interactive "P")
  (touchtype--apply-prefix-arg arg)
  (setq touchtype-session-type 'words)
  (setq touchtype-mode-selection 'code)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-timed (&optional arg)
  "Start a timed touchtype session.
With prefix ARG, set the duration in seconds."
  (interactive "P")
  (when arg
    (setq touchtype-session-duration (prefix-numeric-value arg)))
  (setq touchtype-session-type 'timed)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-stats-view ()
  "Display a summary of all-time touchtype statistics."
  (interactive)
  (touchtype-ui-show-stats))

;;;###autoload
(defalias 'touchtype-export #'touchtype-stats-export
  "Export typing statistics to a file.")

(provide 'touchtype)

;;; touchtype.el ends here
