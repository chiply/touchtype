;;; touchtype-stats.el --- Statistics tracking for touchtype -*- lexical-binding: t; -*-

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

;; Per-letter and per-bigram stats tracking, file persistence, and
;; the confidence-score formula used to decide when to unlock new keys.
;;
;; Data model (s-expression written to `touchtype-stats-file'):
;;
;;   (touchtype-stats
;;    :version 1
;;    :letter-stats ((?a :hits 150 :misses 3 :total-ms 45000 :best-ms 180) ...)
;;    :bigram-stats (("th" :hits 80 :misses 2 :total-ms 16000) ...)
;;    :sessions     ((2026-02-25 :wpm 42.3 :accuracy 97.2
;;                               :mode progressive :words 30) ...)
;;    :unlocked-keys "fjdksl"
;;    :confidence    ((?f . 0.91) (?s . 0.85) ...))

;;; Code:

(require 'cl-lib)
(require 'touchtype-var)

;;;; In-memory stats store

(defvar touchtype--stats nil
  "In-memory representation of the stats plist.
Loaded from `touchtype-stats-file' by `touchtype-stats-load' and
written back by `touchtype-stats-save'.")

;;;; Helpers for the plist-of-plists structure

(defun touchtype-stats--letter-entry (char)
  "Return the stats plist for CHAR from `touchtype--stats', creating it if absent."
  (let* ((lstats (plist-get touchtype--stats :letter-stats))
         (entry (assq char lstats)))
    (unless entry
      (setq entry (list char :hits 0 :misses 0 :total-ms 0 :best-ms nil))
      (plist-put touchtype--stats :letter-stats (cons entry lstats)))
    entry))

(defun touchtype-stats--bigram-entry (bigram)
  "Return the stats plist for BIGRAM string from `touchtype--stats'."
  (let* ((bstats (plist-get touchtype--stats :bigram-stats))
         (entry (assoc bigram bstats)))
    (unless entry
      (setq entry (list bigram :hits 0 :misses 0 :total-ms 0))
      (plist-put touchtype--stats :bigram-stats (cons entry bstats)))
    entry))

(defun touchtype-stats--entry-get (entry key)
  "Get KEY from stats ENTRY (a list whose car is the key, cdr is a plist)."
  (plist-get (cdr entry) key))

(defun touchtype-stats--entry-put (entry key value)
  "Set KEY to VALUE in stats ENTRY, mutating it in place."
  (setcdr entry (plist-put (cdr entry) key value)))

;;;; Load / Save

(defun touchtype-stats-load ()
  "Load stats from `touchtype-stats-file' into `touchtype--stats'.
If the file does not exist, initialise with an empty stats plist."
  (if (file-readable-p touchtype-stats-file)
      (with-temp-buffer
        (insert-file-contents touchtype-stats-file)
        (goto-char (point-min))
        (let ((raw (read (current-buffer))))
          ;; raw is (touchtype-stats :version 1 :letter-stats ... )
          (setq touchtype--stats (cdr raw))))
    (setq touchtype--stats
          (list :version 1
                :letter-stats nil
                :bigram-stats nil
                :sessions nil
                :unlocked-keys "fj"
                :confidence nil))))

(defun touchtype-stats-save ()
  "Write `touchtype--stats' to `touchtype-stats-file'."
  (let ((dir (file-name-directory (expand-file-name touchtype-stats-file))))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  (with-temp-file touchtype-stats-file
    (let ((print-level nil)
          (print-length nil))
      ;; Write as (touchtype-stats :key val ...) so cdr is the raw plist.
      (pp (cons 'touchtype-stats touchtype--stats) (current-buffer)))))

;;;; Recording keypresses

(defun touchtype-stats-record-keypress (char correct-p elapsed-ms)
  "Record a keypress for CHAR.
CORRECT-P is non-nil if the key matched the target; ELAPSED-MS is the
time in milliseconds since the previous keypress."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((entry (touchtype-stats--letter-entry char))
         (hits   (touchtype-stats--entry-get entry :hits))
         (misses (touchtype-stats--entry-get entry :misses))
         (total  (touchtype-stats--entry-get entry :total-ms))
         (best   (touchtype-stats--entry-get entry :best-ms)))
    (if correct-p
        (progn
          (touchtype-stats--entry-put entry :hits (1+ hits))
          (touchtype-stats--entry-put entry :total-ms (+ total elapsed-ms))
          (when (or (null best) (< elapsed-ms best))
            (touchtype-stats--entry-put entry :best-ms elapsed-ms)))
      (touchtype-stats--entry-put entry :misses (1+ misses)))))

(defun touchtype-stats-record-bigram (bigram correct-p elapsed-ms)
  "Record a keypress for BIGRAM string.
CORRECT-P is non-nil if the bigram was typed correctly; ELAPSED-MS is
the elapsed time in milliseconds for the second character of the bigram."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((entry  (touchtype-stats--bigram-entry bigram))
         (hits   (touchtype-stats--entry-get entry :hits))
         (misses (touchtype-stats--entry-get entry :misses))
         (total  (touchtype-stats--entry-get entry :total-ms)))
    (if correct-p
        (progn
          (touchtype-stats--entry-put entry :hits (1+ hits))
          (touchtype-stats--entry-put entry :total-ms (+ total elapsed-ms)))
      (touchtype-stats--entry-put entry :misses (1+ misses)))))

;;;; Confidence scoring

(defun touchtype-stats-get-confidence (char)
  "Compute and return a confidence score 0.0–1.0 for CHAR.

Formula (keybr-derived):
  target-ms         = 60000 / (target-wpm * 5)
  avg-ms            = total-ms / hits          (or a large default)
  speed-confidence  = target-ms / avg-ms       (capped at 1.0)
  accuracy          = hits / (hits + misses)
  confidence        = accuracy * speed-confidence

Returns 0.0 when no data is available."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((entry  (assq char (plist-get touchtype--stats :letter-stats)))
         (hits   (if entry (touchtype-stats--entry-get entry :hits)   0))
         (misses (if entry (touchtype-stats--entry-get entry :misses) 0)))
    (if (zerop hits)
        0.0
      (let* ((total-ms     (touchtype-stats--entry-get entry :total-ms))
             (target-ms    (/ 60000.0 (* touchtype-target-wpm 5)))
             (avg-ms       (/ (float total-ms) hits))
             (speed-conf   (min 1.0 (/ target-ms avg-ms)))
             (accuracy     (/ (float hits) (+ hits misses))))
        (* accuracy speed-conf)))))

(defun touchtype-stats-get-weak-letters ()
  "Return a list of letters sorted by confidence ascending.
Letters with no data appear first."
  (unless touchtype--stats (touchtype-stats-load))
  (let ((letters (string-to-list "abcdefghijklmnopqrstuvwxyz")))
    (sort letters
          (lambda (a b)
            (< (touchtype-stats-get-confidence a)
               (touchtype-stats-get-confidence b))))))

;;;; Session summary

(defun touchtype-stats-record-session (wpm accuracy mode words)
  "Append a session record to `touchtype--stats'.
WPM is the session words-per-minute float, ACCURACY is a percentage
float (0–100), MODE is a symbol, and WORDS is the word count."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((sessions (plist-get touchtype--stats :sessions))
         (date (format-time-string "%Y-%m-%d")))
    (plist-put touchtype--stats :sessions
               (cons (list (intern date)
                           :wpm wpm :accuracy accuracy
                           :mode mode :words words)
                     sessions))))

(defun touchtype-stats-session-summary ()
  "Return a plist of metrics for the most recent session.
Keys: :wpm, :accuracy, :mode, :words."
  (let ((sessions (plist-get touchtype--stats :sessions)))
    (when sessions
      (cdr (car sessions)))))

;;;; Unlock-key persistence

(defun touchtype-stats-get-unlocked-keys ()
  "Return the persisted unlocked-keys string, defaulting to \"fj\"."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :unlocked-keys) "fj"))

(defun touchtype-stats-set-unlocked-keys (keys)
  "Persist KEYS string as the current unlocked-keys value."
  (unless touchtype--stats (touchtype-stats-load))
  (plist-put touchtype--stats :unlocked-keys keys))

(provide 'touchtype-stats)

;;; touchtype-stats.el ends here
