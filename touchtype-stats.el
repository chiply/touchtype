;;; touchtype-stats.el --- Statistics tracking for touchtype -*- lexical-binding: t; -*-

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
(require 'time-date)
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

(defun touchtype-stats-get-bigram-confidence (bigram)
  "Compute confidence score 0.0-1.0 for BIGRAM string.
Uses the same formula as `touchtype-stats-get-confidence' but
applied to bigram-stats entries."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((entry  (assoc bigram (plist-get touchtype--stats :bigram-stats)))
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

(defalias 'touchtype-stats-get-ngram-confidence #'touchtype-stats-get-bigram-confidence
  "Alias for `touchtype-stats-get-bigram-confidence'.
Works on any string length since bigram-stats uses `assoc'.")

(defun touchtype-stats-get-weak-bigrams (&optional n)
  "Return the N weakest bigrams sorted by confidence ascending.
Only includes bigrams with at least 5 hits.  N defaults to 10."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n 10))
         (bstats (plist-get touchtype--stats :bigram-stats))
         (qualified
          (cl-remove-if-not
           (lambda (entry)
             (>= (touchtype-stats--entry-get entry :hits) 5))
           bstats))
         (sorted
          (sort (copy-sequence qualified)
                (lambda (a b)
                  (< (touchtype-stats-get-bigram-confidence (car a))
                     (touchtype-stats-get-bigram-confidence (car b)))))))
    (seq-take sorted n)))

(defun touchtype-stats-get-weak-ngrams (min-len max-len &optional n)
  "Return the N weakest n-grams with string length between MIN-LEN and MAX-LEN.
Only includes entries with at least 5 hits.  N defaults to 10.
Sorted by confidence ascending."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n 10))
         (bstats (plist-get touchtype--stats :bigram-stats))
         (qualified
          (cl-remove-if-not
           (lambda (entry)
             (let ((len (length (car entry))))
               (and (>= len min-len)
                    (<= len max-len)
                    (>= (touchtype-stats--entry-get entry :hits) 5))))
           bstats))
         (sorted
          (sort (copy-sequence qualified)
                (lambda (a b)
                  (< (touchtype-stats-get-bigram-confidence (car a))
                     (touchtype-stats-get-bigram-confidence (car b)))))))
    (seq-take sorted n)))

(defun touchtype-stats-get-wpm-trend (&optional n)
  "Return the last N session WPM values, oldest first.
N defaults to `touchtype-stats-history-length'."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n touchtype-stats-history-length))
         (sessions (seq-take (plist-get touchtype--stats :sessions) n)))
    (nreverse (mapcar (lambda (s) (plist-get (cdr s) :wpm)) sessions))))

(defun touchtype-stats-get-accuracy-trend (&optional n)
  "Return the last N session accuracy values, oldest first.
N defaults to `touchtype-stats-history-length'."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n touchtype-stats-history-length))
         (sessions (seq-take (plist-get touchtype--stats :sessions) n)))
    (nreverse (mapcar (lambda (s) (plist-get (cdr s) :accuracy)) sessions))))

(defun touchtype-stats-get-trend-direction (values)
  "Compare first-half vs second-half average of VALUES.
Return `improving' if second half > first half by >2%,
`declining' if lower by >2%, or `stable' otherwise."
  (if (< (length values) 2)
      'stable
    (let* ((mid (/ (length values) 2))
           (first-half (seq-take values mid))
           (second-half (seq-drop values mid))
           (avg1 (/ (cl-reduce #'+ first-half) (float (length first-half))))
           (avg2 (/ (cl-reduce #'+ second-half) (float (length second-half))))
           (pct-change (if (> avg1 0) (* 100.0 (/ (- avg2 avg1) avg1)) 0.0)))
      (cond ((> pct-change 2.0)  'improving)
            ((< pct-change -2.0) 'declining)
            (t                   'stable)))))

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

(defun touchtype-stats-record-session (wpm accuracy mode words &rest extra)
  "Append a session record to `touchtype--stats'.
WPM is the net words-per-minute float, ACCURACY is a percentage
float (0–100), MODE is a symbol, and WORDS is the word count.
EXTRA is a plist of additional fields (e.g. :gross-wpm, :total-time,
:total-chars, :corrections, :uncorrected-errors, :consistency)."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((sessions (plist-get touchtype--stats :sessions))
         (date (format-time-string "%Y-%m-%d"))
         (record (append (list (intern date)
                               :wpm wpm :accuracy accuracy
                               :mode mode :words words)
                         extra)))
    (plist-put touchtype--stats :sessions
               (cons record sessions))))

(defun touchtype-stats-session-summary ()
  "Return a plist of metrics for the most recent session.
Keys: :wpm, :accuracy, :mode, :words."
  (let ((sessions (plist-get touchtype--stats :sessions)))
    (when sessions
      (cdr (car sessions)))))

;;;; Personal bests

(defun touchtype-stats-get-personal-best (mode metric)
  "Return the best value of METRIC for MODE across all sessions.
METRIC is a keyword like :wpm or :accuracy.  Returns nil if no sessions
for MODE exist."
  (unless touchtype--stats (touchtype-stats-load))
  (let ((sessions (plist-get touchtype--stats :sessions))
        (best nil))
    (dolist (s sessions)
      (let ((s-mode (plist-get (cdr s) :mode))
            (val    (plist-get (cdr s) metric)))
        (when (and (eq s-mode mode) val
                   (or (null best) (> val best)))
          (setq best val))))
    best))

(defun touchtype-stats-get-all-personal-bests ()
  "Return an alist of ((MODE . (:wpm N :accuracy N)) ...) for all modes."
  (unless touchtype--stats (touchtype-stats-load))
  (let ((sessions (plist-get touchtype--stats :sessions))
        (bests nil))
    (dolist (s sessions)
      (let* ((mode (plist-get (cdr s) :mode))
             (wpm  (plist-get (cdr s) :wpm))
             (acc  (plist-get (cdr s) :accuracy))
             (entry (assq mode bests)))
        (if entry
            (progn
              (when (and wpm (> wpm (or (plist-get (cdr entry) :wpm) 0)))
                (plist-put (cdr entry) :wpm wpm))
              (when (and acc (> acc (or (plist-get (cdr entry) :accuracy) 0)))
                (plist-put (cdr entry) :accuracy acc)))
          (push (cons mode (list :wpm (or wpm 0) :accuracy (or acc 0))) bests))))
    bests))

;;;; Unlock-key persistence

(defun touchtype-stats-get-unlocked-keys ()
  "Return the persisted unlocked-keys string, defaulting to \"fj\"."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :unlocked-keys) "fj"))

(defun touchtype-stats-set-unlocked-keys (keys)
  "Persist KEYS string as the current unlocked-keys value."
  (unless touchtype--stats (touchtype-stats-load))
  (plist-put touchtype--stats :unlocked-keys keys))

;;;; Daily streak and total practice time

(defun touchtype-stats-update-streak-and-time (elapsed-seconds)
  "Update daily streak and total practice time with ELAPSED-SECONDS.
Call at session end.  Same day: add time, streak unchanged.
Consecutive day: streak + 1.  Gap: streak resets to 1."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((today (format-time-string "%Y-%m-%d"))
         (last-date (plist-get touchtype--stats :last-practice-date))
         (old-streak (or (plist-get touchtype--stats :daily-streak) 0))
         (old-time (or (plist-get touchtype--stats :total-practice-time) 0)))
    (plist-put touchtype--stats :total-practice-time
               (+ old-time elapsed-seconds))
    (plist-put touchtype--stats :last-practice-date today)
    (cond
     ((equal today last-date)
      ;; Same day, streak unchanged
      )
     ((and last-date
           (equal today
                  (format-time-string "%Y-%m-%d"
                    (time-add (date-to-time (concat last-date " 00:00:00"))
                              (* 24 60 60)))))
      ;; Consecutive day
      (plist-put touchtype--stats :daily-streak (1+ old-streak)))
     (t
      ;; First day or gap
      (plist-put touchtype--stats :daily-streak 1)))))

(defun touchtype-stats-get-streak ()
  "Return the current daily streak count."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :daily-streak) 0))

(defun touchtype-stats-get-total-practice-time ()
  "Return total practice time in seconds."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :total-practice-time) 0))

;;;; Stats export

(defun touchtype-stats-export-json ()
  "Return stats as a JSON string."
  (require 'json)
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((sessions (plist-get touchtype--stats :sessions))
         (letter-stats (plist-get touchtype--stats :letter-stats))
         (session-list
          (mapcar (lambda (s)
                    (let ((plist (cdr s)))
                      `((date . ,(symbol-name (car s)))
                        (wpm . ,(plist-get plist :wpm))
                        (accuracy . ,(plist-get plist :accuracy))
                        (mode . ,(symbol-name (plist-get plist :mode)))
                        (words . ,(plist-get plist :words)))))
                  sessions))
         (letter-list
          (mapcar (lambda (entry)
                    `((letter . ,(string (car entry)))
                      (hits . ,(plist-get (cdr entry) :hits))
                      (misses . ,(plist-get (cdr entry) :misses))
                      (total_ms . ,(plist-get (cdr entry) :total-ms))
                      (confidence . ,(touchtype-stats-get-confidence (car entry)))))
                  letter-stats)))
    (json-encode `((sessions . ,(vconcat session-list))
                   (letter_stats . ,(vconcat letter-list))
                   (streak . ,(touchtype-stats-get-streak))
                   (total_practice_seconds . ,(touchtype-stats-get-total-practice-time))))))

(defun touchtype-stats-export-csv ()
  "Return session stats as a CSV string with header."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((sessions (plist-get touchtype--stats :sessions))
         (header "date,wpm,accuracy,mode,words")
         (rows (mapcar
                (lambda (s)
                  (let ((plist (cdr s)))
                    (format "%s,%.1f,%.1f,%s,%d"
                            (symbol-name (car s))
                            (plist-get plist :wpm)
                            (plist-get plist :accuracy)
                            (plist-get plist :mode)
                            (plist-get plist :words))))
                sessions)))
    (mapconcat #'identity (cons header rows) "\n")))

(defun touchtype-stats-export (fmt filename)
  "Export stats to FILENAME in FMT (json or csv)."
  (interactive
   (list (intern (completing-read "Format: " '("json" "csv") nil t))
         (read-file-name "Export to: ")))
  (let ((content (pcase fmt
                   ('json (touchtype-stats-export-json))
                   ('csv  (touchtype-stats-export-csv))
                   (_     (error "Unknown format: %s" fmt)))))
    (with-temp-file filename
      (insert content))
    (message "Stats exported to %s" filename)))

(provide 'touchtype-stats)

;;; touchtype-stats.el ends here
