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

(declare-function touchtype-ui-setup-buffer  "touchtype-ui")
(declare-function touchtype-ui-show-stats    "touchtype-ui")
(declare-function touchtype-stats-load       "touchtype-stats")
(declare-function touchtype-stats-save       "touchtype-stats")

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

;;;; Entry point commands

;;;###autoload
(defun touchtype ()
  "Start a touchtype session in the mode set by `touchtype-mode-selection'."
  (interactive)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-progressive ()
  "Start a touchtype session in progressive key-unlock mode."
  (interactive)
  (setq touchtype-mode-selection 'progressive)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-full-words ()
  "Start a touchtype session using real English words."
  (interactive)
  (setq touchtype-mode-selection 'full-words)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-bigram-drill ()
  "Start a touchtype session drilling common English bigrams."
  (interactive)
  (setq touchtype-mode-selection 'bigram-drill)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-letters ()
  "Start a touchtype session using all 26 letters."
  (interactive)
  (setq touchtype-mode-selection 'letters)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-letters+numbers ()
  "Start a touchtype session using letters and digits 0–9."
  (interactive)
  (setq touchtype-mode-selection 'letters+numbers)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-letters+numbers+symbols ()
  "Start a touchtype session using letters, digits, and common punctuation."
  (interactive)
  (setq touchtype-mode-selection 'letters+numbers+symbols)
  (touchtype-ui-setup-buffer))

;;;###autoload
(defun touchtype-stats-view ()
  "Display a summary of all-time touchtype statistics."
  (interactive)
  (touchtype-ui-show-stats))

(provide 'touchtype)

;;; touchtype.el ends here
