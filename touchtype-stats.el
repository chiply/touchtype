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

(defvar touchtype--confidence-cache nil
  "When non-nil, a hash-table memoizing confidence scores.
Bound dynamically during render passes to avoid recomputing
the same (char . mode) confidence values hundreds of times.")

;;;; Word stats helpers

(defun touchtype-stats--word-entry (word)
  "Return the stats plist for WORD string from `touchtype--stats'."
  (let* ((wstats (plist-get touchtype--stats :word-stats))
         (entry (assoc word wstats)))
    (unless entry
      (setq entry (list word :hits 0 :misses 0 :total-ms 0))
      (plist-put touchtype--stats :word-stats (cons entry wstats)))
    entry))

;;;; Helpers for the plist-of-plists structure

(defun touchtype-stats--get-or-create-entry (key stats-key lookup-fn default-plist)
  "Get or create an entry for KEY under STATS-KEY in `touchtype--stats'.
LOOKUP-FN is `assq' or `assoc'.  DEFAULT-PLIST is the initial plist for new entries."
  (let* ((store (plist-get touchtype--stats stats-key))
         (entry (funcall lookup-fn key store)))
    (unless entry
      (setq entry (cons key (copy-sequence default-plist)))
      (plist-put touchtype--stats stats-key (cons entry store)))
    entry))

(defun touchtype-stats--letter-entry (char)
  "Return the stats plist for CHAR from `touchtype--stats', creating it if absent."
  (touchtype-stats--get-or-create-entry
   char :letter-stats #'assq
   '(:hits 0 :misses 0 :total-ms 0 :best-ms nil :ema-ms nil)))

(defun touchtype-stats--bigram-entry (bigram)
  "Return the stats plist for BIGRAM string from `touchtype--stats'."
  (touchtype-stats--get-or-create-entry
   bigram :bigram-stats #'assoc
   '(:hits 0 :misses 0 :total-ms 0 :ema-ms nil)))

(defun touchtype-stats--entry-get (entry key)
  "Get KEY from stats ENTRY (a list whose car is the key, cdr is a plist)."
  (plist-get (cdr entry) key))

(defun touchtype-stats--entry-put (entry key value)
  "Set KEY to VALUE in stats ENTRY, mutating it in place."
  (setcdr entry (plist-put (cdr entry) key value)))

(defun touchtype-stats--get-or-create-mode-entry (mode key stats-key lookup-fn default-plist)
  "Get or create an entry for KEY within MODE under STATS-KEY.
LOOKUP-FN is `assq' or `assoc'.  DEFAULT-PLIST is the initial plist."
  (let* ((mstats (plist-get touchtype--stats stats-key))
         (mode-alist (assq mode mstats))
         entry)
    (unless mode-alist
      (setq mode-alist (list mode))
      (plist-put touchtype--stats stats-key (cons mode-alist mstats)))
    (setq entry (funcall lookup-fn key (cdr mode-alist)))
    (unless entry
      (setq entry (cons key (copy-sequence default-plist)))
      (setcdr mode-alist (cons entry (cdr mode-alist))))
    entry))

(defun touchtype-stats--mode-letter-entry (mode char)
  "Return the stats plist for CHAR within MODE from `:mode-letter-stats'."
  (touchtype-stats--get-or-create-mode-entry
   mode char :mode-letter-stats #'assq
   '(:hits 0 :misses 0 :total-ms 0 :best-ms nil)))

(defun touchtype-stats--mode-bigram-entry (mode bigram)
  "Return the stats plist for BIGRAM string within MODE from `:mode-bigram-stats'."
  (touchtype-stats--get-or-create-mode-entry
   mode bigram :mode-bigram-stats #'assoc
   '(:hits 0 :misses 0 :total-ms 0)))

;;;; Load / Save

(defun touchtype-stats-load (&optional force)
  "Load stats from `touchtype-stats-file' into `touchtype--stats'.
If the file does not exist, initialise with an empty stats plist.
When FORCE is nil and `touchtype--stats' is already loaded, do nothing."
  (unless (and touchtype--stats (not force))
    (if (file-readable-p touchtype-stats-file)
      (with-temp-buffer
        (insert-file-contents touchtype-stats-file)
        (goto-char (point-min))
        (let ((raw (read (current-buffer))))
          ;; raw is (touchtype-stats :version N :letter-stats ... )
          (setq touchtype--stats (cdr raw))
          ;; Migrate v1 → v2: add per-mode stats keys
          (when (or (not (plist-get touchtype--stats :version))
                    (< (plist-get touchtype--stats :version) 2))
            (unless (plist-member touchtype--stats :mode-letter-stats)
              (plist-put touchtype--stats :mode-letter-stats nil))
            (unless (plist-member touchtype--stats :mode-bigram-stats)
              (plist-put touchtype--stats :mode-bigram-stats nil))
            (plist-put touchtype--stats :version 2))
          ;; Migrate v2 → v3: add word-stats key
          (when (< (plist-get touchtype--stats :version) 3)
            (unless (plist-member touchtype--stats :word-stats)
              (plist-put touchtype--stats :word-stats nil))
            (plist-put touchtype--stats :version 3))
          ;; Migrate v3 → v4: add :ema-ms to letter and bigram stats
          (when (< (plist-get touchtype--stats :version) 4)
            (dolist (stats-key '(:letter-stats :bigram-stats))
              (dolist (entry (plist-get touchtype--stats stats-key))
                (let ((hits (touchtype-stats--entry-get entry :hits))
                      (total-ms (touchtype-stats--entry-get entry :total-ms)))
                  (when (and (> hits 0) (not (touchtype-stats--entry-get entry :ema-ms)))
                    (touchtype-stats--entry-put entry :ema-ms (/ (float total-ms) hits))))))
            (plist-put touchtype--stats :version 4))
          ;; Migrate v4 → v5: add streak freeze fields
          (when (< (plist-get touchtype--stats :version) 5)
            (unless (plist-member touchtype--stats :streak-freezes-available)
              (plist-put touchtype--stats :streak-freezes-available touchtype-streak-freeze-count))
            (unless (plist-member touchtype--stats :streak-best)
              (plist-put touchtype--stats :streak-best
                         (or (plist-get touchtype--stats :daily-streak) 0)))
            (unless (plist-member touchtype--stats :streak-consecutive-days)
              (plist-put touchtype--stats :streak-consecutive-days 0))
            (plist-put touchtype--stats :version 5))))
    (setq touchtype--stats
          (list :version 5
                :letter-stats nil
                :bigram-stats nil
                :mode-letter-stats nil
                :mode-bigram-stats nil
                :word-stats nil
                :sessions nil
                :unlocked-keys "fj"
                :confidence nil
                :streak-freezes-available touchtype-streak-freeze-count
                :streak-best 0
                :streak-consecutive-days 0)))))

(defun touchtype-stats-save ()
  "Write `touchtype--stats' to `touchtype-stats-file'."
  (let ((dir (file-name-directory (expand-file-name touchtype-stats-file))))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  (with-temp-file touchtype-stats-file
    (let ((print-level nil)
          (print-length nil))
      ;; Write as (touchtype-stats :key val ...) so cdr is the raw plist.
      ;; Use prin1 instead of pp for speed (avoids slow pretty-printing).
      (prin1 (cons 'touchtype-stats touchtype--stats) (current-buffer))
      (insert "\n"))))

;;;; Recording keypresses

(defun touchtype-stats--update-entry (entry correct-p elapsed-ms &optional track-best track-ema)
  "Update ENTRY with a keypress result.
CORRECT-P indicates success; ELAPSED-MS is timing.
When TRACK-BEST is non-nil, update :best-ms.
When TRACK-EMA is non-nil, update :ema-ms."
  (let ((hits   (touchtype-stats--entry-get entry :hits))
        (misses (touchtype-stats--entry-get entry :misses))
        (total  (touchtype-stats--entry-get entry :total-ms)))
    (if correct-p
        (progn
          (touchtype-stats--entry-put entry :hits (1+ hits))
          (touchtype-stats--entry-put entry :total-ms (+ total elapsed-ms))
          (when track-best
            (let ((best (touchtype-stats--entry-get entry :best-ms)))
              (when (or (null best) (< elapsed-ms best))
                (touchtype-stats--entry-put entry :best-ms elapsed-ms))))
          (when track-ema
            (let ((old-ema (touchtype-stats--entry-get entry :ema-ms)))
              (touchtype-stats--entry-put
               entry :ema-ms
               (if old-ema
                   (+ (* touchtype-confidence-ema-alpha elapsed-ms)
                      (* (- 1.0 touchtype-confidence-ema-alpha) old-ema))
                 (float elapsed-ms))))))
      (touchtype-stats--entry-put entry :misses (1+ misses)))))

(defun touchtype-stats-record-keypress (char correct-p elapsed-ms &optional mode)
  "Record a keypress for CHAR.
CORRECT-P is non-nil if the key matched the target; ELAPSED-MS is the
time in milliseconds since the previous keypress.
When MODE is non-nil, also record to the per-mode letter stats."
  (unless touchtype--stats (touchtype-stats-load))
  (touchtype-stats--update-entry
   (touchtype-stats--letter-entry char) correct-p elapsed-ms t t)
  (when mode
    (touchtype-stats--update-entry
     (touchtype-stats--mode-letter-entry mode char) correct-p elapsed-ms t nil)))

(defun touchtype-stats-record-bigram (bigram correct-p elapsed-ms &optional mode)
  "Record a keypress for BIGRAM string.
CORRECT-P is non-nil if the bigram was typed correctly; ELAPSED-MS is
the elapsed time in milliseconds for the second character of the bigram.
When MODE is non-nil, also record to the per-mode bigram stats."
  (unless touchtype--stats (touchtype-stats-load))
  (touchtype-stats--update-entry
   (touchtype-stats--bigram-entry bigram) correct-p elapsed-ms nil t)
  (when mode
    (touchtype-stats--update-entry
     (touchtype-stats--mode-bigram-entry mode bigram) correct-p elapsed-ms nil nil)))

;;;; Confidence scoring

(defun touchtype-stats--compute-confidence (entry)
  "Compute a confidence score 0.0-1.0 from stats ENTRY.
Returns 0.0 when ENTRY is nil or has no data."
  (if (null entry)
      0.0
    (let ((hits   (touchtype-stats--entry-get entry :hits))
          (misses (touchtype-stats--entry-get entry :misses)))
      (if (zerop hits)
          0.0
        (let* ((total-ms     (touchtype-stats--entry-get entry :total-ms))
               (ema-ms       (touchtype-stats--entry-get entry :ema-ms))
               (target-ms    (/ 60000.0 (* touchtype-target-wpm 5)))
               (avg-ms       (if (and touchtype-confidence-use-ema ema-ms)
                                 ema-ms
                               (/ (float total-ms) hits)))
               (speed-conf   (min 1.0 (/ target-ms avg-ms)))
               (accuracy     (/ (float hits) (+ hits misses)))
               (sample-scale (min 1.0 (/ (float hits) touchtype-confidence-min-samples))))
          (* accuracy speed-conf sample-scale))))))

(defun touchtype-stats--compute-accuracy (entry)
  "Return accuracy 0.0-1.0 from stats ENTRY, or 0.0 if no data."
  (if (null entry)
      0.0
    (let ((hits   (touchtype-stats--entry-get entry :hits))
          (misses (touchtype-stats--entry-get entry :misses)))
      (if (zerop hits)
          0.0
        (/ (float hits) (+ hits misses))))))

(defun touchtype-stats--lookup-entry (key stats-key lookup-fn &optional mode)
  "Look up entry for KEY under STATS-KEY using LOOKUP-FN.
When MODE is non-nil, look in per-mode stats instead of global."
  (let ((store (if mode
                   (cdr (assq mode (plist-get touchtype--stats stats-key)))
                 (plist-get touchtype--stats (pcase stats-key
                                              (:mode-letter-stats :letter-stats)
                                              (:mode-bigram-stats :bigram-stats)
                                              (_ stats-key))))))
    (funcall lookup-fn key store)))

(defun touchtype-stats--cached-confidence (cache-key entry)
  "Compute confidence for ENTRY, caching under CACHE-KEY if cache is active."
  (if (and cache-key (gethash cache-key touchtype--confidence-cache))
      (gethash cache-key touchtype--confidence-cache)
    (let ((result (touchtype-stats--compute-confidence entry)))
      (when cache-key
        (puthash cache-key result touchtype--confidence-cache))
      result)))

(defun touchtype-stats-get-confidence (char &optional mode)
  "Compute and return a confidence score 0.0–1.0 for CHAR.
When MODE is non-nil, use per-mode letter stats instead of global.

Formula (keybr-derived):
  target-ms         = 60000 / (target-wpm * 5)
  avg-ms            = total-ms / hits          (or a large default)
  speed-confidence  = target-ms / avg-ms       (capped at 1.0)
  accuracy          = hits / (hits + misses)
  confidence        = accuracy * speed-confidence

Returns 0.0 when no data is available."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((cache-key (when touchtype--confidence-cache (cons char mode)))
         (lstats (if mode
                     (cdr (assq mode (plist-get touchtype--stats :mode-letter-stats)))
                   (plist-get touchtype--stats :letter-stats)))
         (entry (assq char lstats)))
    (touchtype-stats--cached-confidence cache-key entry)))

(defun touchtype-stats-get-letter-accuracy (char &optional mode)
  "Return the accuracy (0.0-1.0) for CHAR, or 0.0 if no data.
When MODE is non-nil, use per-mode letter stats."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((lstats (if mode
                     (cdr (assq mode (plist-get touchtype--stats :mode-letter-stats)))
                   (plist-get touchtype--stats :letter-stats)))
         (entry (assq char lstats)))
    (touchtype-stats--compute-accuracy entry)))

(defun touchtype-stats-get-bigram-accuracy (bigram &optional mode)
  "Return the accuracy (0.0-1.0) for BIGRAM string, or 0.0 if no data.
When MODE is non-nil, use per-mode bigram stats."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((bstats (if mode
                     (cdr (assq mode (plist-get touchtype--stats :mode-bigram-stats)))
                   (plist-get touchtype--stats :bigram-stats)))
         (entry (assoc bigram bstats)))
    (touchtype-stats--compute-accuracy entry)))

(defun touchtype-stats-get-bigram-confidence (bigram &optional mode)
  "Compute confidence score 0.0-1.0 for BIGRAM string.
When MODE is non-nil, use per-mode bigram stats instead of global.
Uses the same formula as `touchtype-stats-get-confidence' but
applied to bigram-stats entries."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((cache-key (when touchtype--confidence-cache (cons bigram mode)))
         (bstats (if mode
                     (cdr (assq mode (plist-get touchtype--stats :mode-bigram-stats)))
                   (plist-get touchtype--stats :bigram-stats)))
         (entry (assoc bigram bstats)))
    (touchtype-stats--cached-confidence cache-key entry)))

(defalias 'touchtype-stats-get-ngram-confidence #'touchtype-stats-get-bigram-confidence
  "Alias for `touchtype-stats-get-bigram-confidence'.
Works on any string length since bigram-stats uses `assoc'.
Inherits the optional MODE parameter.")

(defun touchtype-stats-get-weak-bigrams (&optional n mode)
  "Return the N weakest bigrams sorted by confidence ascending.
Only includes bigrams with at least 5 hits.  N defaults to 10.
When MODE is non-nil, use per-mode bigram stats."
  (touchtype-stats-get-weak-ngrams 2 2 n mode))

(defun touchtype-stats-get-weak-ngrams (min-len max-len &optional n mode)
  "Return the N weakest n-grams with string length between MIN-LEN and MAX-LEN.
Only includes entries with at least 5 hits.  N defaults to 10.
Sorted by confidence ascending.
When MODE is non-nil, use per-mode bigram stats.
Uses a partial-sort approach: computes confidence once per entry and
only keeps the N lowest, avoiding a full O(m log m) sort."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n 10))
         (bstats (if mode
                     (cdr (assq mode (plist-get touchtype--stats :mode-bigram-stats)))
                   (plist-get touchtype--stats :bigram-stats)))
         ;; Build (confidence . entry) pairs, filtering and keeping only top N
         (heap nil)
         (heap-max 0.0)
         (heap-len 0))
    (dolist (entry bstats)
      (let ((len (length (car entry))))
        (when (and (>= len min-len)
                   (<= len max-len)
                   (>= (touchtype-stats--entry-get entry :hits) 5))
          (let ((conf (touchtype-stats-get-bigram-confidence (car entry) mode)))
            (cond
             ((< heap-len n)
              (push (cons conf entry) heap)
              (cl-incf heap-len)
              (when (> conf heap-max) (setq heap-max conf)))
             ((< conf heap-max)
              ;; Replace the max element
              (setq heap (cl-delete-if (lambda (x) (= (car x) heap-max)) heap :count 1))
              (push (cons conf entry) heap)
              (setq heap-max (cl-reduce #'max heap :key #'car))))))))
    (mapcar #'cdr (sort heap (lambda (a b) (< (car a) (car b)))))))

(defun touchtype-stats--filter-sessions-by-mode (sessions mode)
  "Filter SESSIONS to those matching MODE.
Handles both symbol and string :mode values in session data."
  (if mode
      (cl-remove-if-not
       (lambda (s)
         (let ((s-mode (plist-get (cdr s) :mode)))
           (eq (if (symbolp s-mode) s-mode (intern s-mode))
               mode)))
       sessions)
    sessions))

(defun touchtype-stats--get-trend (metric &optional n mode)
  "Return the last N session values for METRIC, oldest first.
METRIC is a keyword like :wpm or :accuracy.
N defaults to `touchtype-stats-history-length'.
When MODE is non-nil, only include sessions with matching :mode."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n touchtype-stats-history-length))
         (filtered (touchtype-stats--filter-sessions-by-mode
                    (plist-get touchtype--stats :sessions) mode))
         (sessions (seq-take filtered n)))
    (nreverse (mapcar (lambda (s) (plist-get (cdr s) metric)) sessions))))

(defun touchtype-stats-get-wpm-trend (&optional n mode)
  "Return the last N session WPM values, oldest first.
N defaults to `touchtype-stats-history-length'.
When MODE is non-nil, only include sessions with matching :mode."
  (touchtype-stats--get-trend :wpm n mode))

(defun touchtype-stats-get-accuracy-trend (&optional n mode)
  "Return the last N session accuracy values, oldest first.
N defaults to `touchtype-stats-history-length'.
When MODE is non-nil, only include sessions with matching :mode."
  (touchtype-stats--get-trend :accuracy n mode))

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

(defun touchtype-stats-get-weak-letters (&optional mode)
  "Return a list of characters sorted by confidence ascending.
Uses all characters that have recorded data.
When MODE is non-nil, use per-mode letter stats."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((lstats (if mode
                     (cdr (assq mode (plist-get touchtype--stats :mode-letter-stats)))
                   (plist-get touchtype--stats :letter-stats)))
         (chars (mapcar #'car lstats)))
    (sort chars
          (lambda (a b)
            (< (touchtype-stats-get-confidence a mode)
               (touchtype-stats-get-confidence b mode))))))

;;;; Word-level statistics

(defun touchtype-stats-record-word (word correct-p elapsed-ms)
  "Record a word result for WORD.
CORRECT-P is non-nil if every character was typed correctly.
ELAPSED-MS is the total time for the word."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((entry (touchtype-stats--word-entry word))
         (hits   (touchtype-stats--entry-get entry :hits))
         (misses (touchtype-stats--entry-get entry :misses))
         (total  (touchtype-stats--entry-get entry :total-ms)))
    (if correct-p
        (progn
          (touchtype-stats--entry-put entry :hits (1+ hits))
          (touchtype-stats--entry-put entry :total-ms (+ total elapsed-ms)))
      (touchtype-stats--entry-put entry :misses (1+ misses)))))

(defun touchtype-stats-get-word-confidence (word)
  "Compute confidence score 0.0-1.0 for WORD.
Uses the same accuracy * speed formula as letter/ngram confidence.
Speed is normalized per-character: avg-ms-per-char vs target-ms.
Returns 0.0 if no data."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((wstats (plist-get touchtype--stats :word-stats))
         (entry (assoc word wstats))
         (hits   (if entry (touchtype-stats--entry-get entry :hits) 0))
         (misses (if entry (touchtype-stats--entry-get entry :misses) 0)))
    (if (zerop hits)
        0.0
      (let* ((total-ms   (touchtype-stats--entry-get entry :total-ms))
             (word-len   (max 1 (length word)))
             (target-ms  (/ 60000.0 (* touchtype-target-wpm 5)))
             (avg-ms     (/ (float total-ms) (* hits word-len)))
             (speed-conf (min 1.0 (/ target-ms avg-ms)))
             (accuracy   (/ (float hits) (+ hits misses)))
             (sample-scale (min 1.0 (/ (float hits) touchtype-confidence-min-samples))))
        (* accuracy speed-conf sample-scale)))))

(defun touchtype-stats-get-weak-words (&optional n)
  "Return N weakest words sorted by confidence ascending.
Only includes words with at least 5 hits.  N defaults to 20."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((n (or n 20))
         (wstats (plist-get touchtype--stats :word-stats))
         (qualified
          (cl-remove-if-not
           (lambda (entry)
             (and (>= (length (car entry)) 4)
                  (>= (touchtype-stats--entry-get entry :hits) 2)))
           wstats))
         (sorted
          (sort (copy-sequence qualified)
                (lambda (a b)
                  (< (touchtype-stats-get-word-confidence (car a))
                     (touchtype-stats-get-word-confidence (car b)))))))
    (seq-take sorted n)))

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
Consecutive day: streak + 1, recharge freeze if threshold met.
Gap: consume freeze if available, else reset to 1."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((today (format-time-string "%Y-%m-%d"))
         (last-date (plist-get touchtype--stats :last-practice-date))
         (old-streak (or (plist-get touchtype--stats :daily-streak) 0))
         (old-time (or (plist-get touchtype--stats :total-practice-time) 0))
         (freezes (or (plist-get touchtype--stats :streak-freezes-available) 0))
         (best-streak (or (plist-get touchtype--stats :streak-best) 0))
         (consec-days (or (plist-get touchtype--stats :streak-consecutive-days) 0)))
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
      (plist-put touchtype--stats :daily-streak (1+ old-streak))
      (setq consec-days (1+ consec-days))
      (plist-put touchtype--stats :streak-consecutive-days consec-days)
      ;; Recharge freeze if threshold met
      (when (and (>= consec-days touchtype-streak-freeze-recharge-days)
                 (< freezes touchtype-streak-freeze-count))
        (plist-put touchtype--stats :streak-freezes-available (1+ freezes))
        (plist-put touchtype--stats :streak-consecutive-days 0)))
     (t
      ;; First day or gap
      (if (and (> freezes 0) (> old-streak 0))
          (progn
            ;; Consume a freeze, preserve streak
            (plist-put touchtype--stats :streak-freezes-available (1- freezes))
            ;; Don't change the streak number
            )
        ;; No freeze: reset
        (plist-put touchtype--stats :daily-streak 1))
      (plist-put touchtype--stats :streak-consecutive-days 0)))
    ;; Update best streak
    (let ((current (or (plist-get touchtype--stats :daily-streak) 0)))
      (when (> current best-streak)
        (plist-put touchtype--stats :streak-best current)))))

(defun touchtype-stats-get-streak ()
  "Return the current daily streak count."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :daily-streak) 0))

(defun touchtype-stats-get-total-practice-time ()
  "Return total practice time in seconds."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :total-practice-time) 0))

(defun touchtype-stats-get-streak-freezes ()
  "Return the number of available streak freezes."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :streak-freezes-available) 0))

(defun touchtype-stats-get-best-streak ()
  "Return the all-time best streak."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :streak-best) 0))

;;;; Per-finger statistics

(defun touchtype-stats--finger-map ()
  "Return the finger map for the current keyboard layout."
  (touchtype--layout-get touchtype-keyboard-layout :finger-map))

(defun touchtype-stats-get-finger-stats ()
  "Return alist of per-finger performance stats.
Each entry is (FINGER . plist) where plist has keys :hits,
:misses, :total-ms, :accuracy, and :avg-ms.  Aggregates
letter-stats by finger using the current layout's finger map."
  (unless touchtype--stats (touchtype-stats-load))
  (let ((fmap (touchtype-stats--finger-map))
        (result nil))
    (dolist (mapping fmap)
      (let* ((ch (car mapping))
             (finger (cdr mapping))
             (entry (assq ch (plist-get touchtype--stats :letter-stats)))
             (hits (if entry (touchtype-stats--entry-get entry :hits) 0))
             (misses (if entry (touchtype-stats--entry-get entry :misses) 0))
             (total-ms (if entry (touchtype-stats--entry-get entry :total-ms) 0))
             (existing (assq finger result)))
        (if existing
            (progn
              (plist-put (cdr existing) :hits
                         (+ (plist-get (cdr existing) :hits) hits))
              (plist-put (cdr existing) :misses
                         (+ (plist-get (cdr existing) :misses) misses))
              (plist-put (cdr existing) :total-ms
                         (+ (plist-get (cdr existing) :total-ms) total-ms)))
          (push (cons finger (list :hits hits :misses misses :total-ms total-ms))
                result))))
    ;; Compute derived metrics
    (dolist (entry result)
      (let* ((hits (plist-get (cdr entry) :hits))
             (misses (plist-get (cdr entry) :misses))
             (total-ms (plist-get (cdr entry) :total-ms))
             (accuracy (if (> (+ hits misses) 0)
                           (* 100.0 (/ (float hits) (+ hits misses)))
                         0.0))
             (avg-ms (if (> hits 0) (/ (float total-ms) hits) 0.0)))
        (plist-put (cdr entry) :accuracy accuracy)
        (plist-put (cdr entry) :avg-ms avg-ms)))
    (nreverse result)))

;;;; Achievement system

(defun touchtype-stats-get-achievements ()
  "Return the list of earned achievement ID symbols."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :achievements) nil))

(defun touchtype-stats--award-achievement (id)
  "Award achievement ID if not already earned."
  (unless touchtype--stats (touchtype-stats-load))
  (let ((earned (plist-get touchtype--stats :achievements)))
    (unless (memq id earned)
      (plist-put touchtype--stats :achievements (cons id earned)))))

(defun touchtype-stats--check-threshold-achievements (value thresholds award-fn)
  "Award achievements from THRESHOLDS where VALUE >= threshold.
THRESHOLDS is a list of (THRESHOLD . ACHIEVEMENT-ID) pairs.
AWARD-FN is called with each earned achievement ID."
  (dolist (pair thresholds)
    (when (>= value (car pair))
      (funcall award-fn (cdr pair)))))

(defun touchtype-stats-check-achievements (wpm accuracy &optional word-count consistency)
  "Check all achievement conditions given session WPM and ACCURACY.
WORD-COUNT and CONSISTENCY are optional session metrics.
Returns list of newly earned achievement IDs."
  (unless touchtype--stats (touchtype-stats-load))
  (let ((earned (touchtype-stats-get-achievements))
        (sessions (plist-get touchtype--stats :sessions))
        (streak (touchtype-stats-get-streak))
        (total-secs (touchtype-stats-get-total-practice-time))
        (unlocked (or (plist-get touchtype--stats :unlocked-keys) "fj"))
        (newly nil))
    (cl-flet ((award (id)
                (unless (memq id earned)
                  (touchtype-stats--award-achievement id)
                  (push id newly))))
      (award 'first-session)
      ;; Threshold-based achievements
      (touchtype-stats--check-threshold-achievements
       wpm '((30 . speed-30) (40 . speed-40) (50 . speed-50) (60 . speed-60)
             (70 . speed-70) (80 . speed-80) (100 . speed-100) (120 . speed-120)
             (150 . speed-150))
       #'award)
      (touchtype-stats--check-threshold-achievements
       accuracy '((95 . accuracy-95) (99 . accuracy-99) (100 . accuracy-100))
       #'award)
      (touchtype-stats--check-threshold-achievements
       streak '((7 . streak-7) (14 . streak-14) (30 . streak-30)
                (60 . streak-60) (100 . streak-100))
       #'award)
      (touchtype-stats--check-threshold-achievements
       (length sessions)
       '((10 . sessions-10) (25 . sessions-25) (50 . sessions-50)
         (100 . sessions-100) (200 . sessions-200) (500 . sessions-500))
       #'award)
      (touchtype-stats--check-threshold-achievements
       total-secs '((3600 . practice-1h) (18000 . practice-5h) (36000 . practice-10h)
                    (90000 . practice-25h) (180000 . practice-50h) (360000 . practice-100h))
       #'award)
      ;; All keys unlocked
      (when (>= (length unlocked) 26) (award 'all-keys))
      ;; Per-mode session counts
      (dolist (mode-check '((progressive . progressive-10)
                            (full-words . full-words-10)
                            (code . code-10)))
        (when (>= (cl-count-if (lambda (s) (eq (plist-get (cdr s) :mode) (car mode-check)))
                                sessions)
                  10)
          (award (cdr mode-check))))
      ;; Iron Fingers: 5 expert-difficulty sessions
      (when (>= (cl-count-if
                 (lambda (s) (eq (plist-get (cdr s) :difficulty) 'expert))
                 sessions)
                5)
        (award 'iron-fingers))
      ;; Marathon: 120+ word session
      (when (and word-count (>= word-count 120))
        (award 'marathon))
      ;; Consistency King: last 10 sessions all have consistency >= 90
      (let ((recent (seq-take sessions 10)))
        (when (and (>= (length recent) 10)
                   (cl-every (lambda (s)
                               (let ((c (plist-get (cdr s) :consistency)))
                                 (and c (>= c 90))))
                             recent))
          (award 'consistency-king)))
      ;; Perfect line (checked via buffer-local flag)
      (when (and (boundp 'touchtype--perfect-line-achieved)
                 touchtype--perfect-line-achieved)
        (award 'perfect-line))
      ;; Night owl / Early bird
      (let ((hour (string-to-number (format-time-string "%H"))))
        (when (= hour 0) (award 'night-owl))
        (when (and (>= hour 0) (< hour 6)) (award 'early-bird))))
    newly))

;;;; XP and level system

(defun touchtype-stats-get-xp ()
  "Return the current total XP."
  (unless touchtype--stats (touchtype-stats-load))
  (or (plist-get touchtype--stats :xp) 0))

(defun touchtype-stats-add-xp (amount)
  "Add AMOUNT of XP to the total."
  (unless touchtype--stats (touchtype-stats-load))
  (plist-put touchtype--stats :xp
             (+ (touchtype-stats-get-xp) amount)))

(defun touchtype-stats-get-level ()
  "Return the current level (0-25) based on total XP."
  (let ((xp (touchtype-stats-get-xp))
        (level 0))
    (cl-loop for i from (1- (length touchtype--xp-level-thresholds)) downto 0
             when (>= xp (aref touchtype--xp-level-thresholds i))
             do (setq level i)
             and return nil)
    level))

(defun touchtype-stats--compute-xp-multipliers (accuracy &rest context)
  "Compute XP multipliers from session ACCURACY and CONTEXT plist.
Returns a plist with :streak-mult, :pb-mult, :diff-mult,
:perfect-mult, :cons-mult, and :total-mult."
  (let* ((streak (or (plist-get context :streak) 0))
         (is-pb (plist-get context :is-pb))
         (difficulty (or (plist-get context :difficulty) 'normal))
         (consistency (or (plist-get context :consistency) 0))
         (acc-perfect (or (plist-get context :accuracy-perfect)
                          (>= accuracy 100)))
         (enabled touchtype-xp-multipliers-enabled)
         (streak-mult (if enabled (+ 1.0 (min 0.5 (* streak 0.05))) 1.0))
         (pb-mult (if (and enabled is-pb) 1.5 1.0))
         (diff-mult (if enabled
                        (pcase difficulty
                          ('expert 1.5)
                          ('master 2.0)
                          (_ 1.0))
                      1.0))
         (perfect-mult (if (and enabled acc-perfect) 1.25 1.0))
         (cons-mult (if enabled
                        (+ 1.0 (/ (max 0 (- consistency 80)) 100.0))
                      1.0)))
    (list :streak-mult streak-mult :pb-mult pb-mult
          :diff-mult diff-mult :perfect-mult perfect-mult
          :cons-mult cons-mult
          :total-mult (* streak-mult pb-mult diff-mult perfect-mult cons-mult))))

(defun touchtype-stats-xp-for-session (wpm accuracy word-count &rest context)
  "Compute XP earned for a session with WPM, ACCURACY, and WORD-COUNT.
CONTEXT is an optional plist with keys :streak, :is-pb, :difficulty,
:consistency, :accuracy-perfect for multiplier calculation.
Base formula: round(WPM * ACCURACY/100 * WORD-COUNT * total-multiplier)."
  (let* ((base (round (* wpm (/ accuracy 100.0) word-count)))
         (total-mult (if (and touchtype-xp-multipliers-enabled context)
                         (plist-get (apply #'touchtype-stats--compute-xp-multipliers
                                           accuracy context)
                                    :total-mult)
                       1.0)))
    (round (* base total-mult))))

(defun touchtype-stats-xp-breakdown (wpm accuracy word-count &rest context)
  "Return plist of XP breakdown for a session.
Keys: :base, :total-mult, :streak-mult, :pb-mult, :diff-mult,
:perfect-mult, :cons-mult, :total."
  (let* ((base (round (* wpm (/ accuracy 100.0) word-count)))
         (mults (apply #'touchtype-stats--compute-xp-multipliers accuracy context))
         (total (round (* base (plist-get mults :total-mult)))))
    (append (list :base base) mults (list :total total))))

(defun touchtype-stats-xp-to-next-level ()
  "Return XP needed to reach the next level, or 0 if at max."
  (let* ((level (touchtype-stats-get-level))
         (xp (touchtype-stats-get-xp))
         (max-level (1- (length touchtype--xp-level-thresholds))))
    (if (>= level max-level)
        0
      (- (aref touchtype--xp-level-thresholds (1+ level)) xp))))

;;;; Rolling average

(defun touchtype-stats-get-rolling-average (n metric)
  "Return the average of METRIC over the last N sessions.
METRIC is a keyword like :wpm or :accuracy.  Returns nil if no sessions."
  (unless touchtype--stats (touchtype-stats-load))
  (let* ((sessions (seq-take (plist-get touchtype--stats :sessions) n))
         (values (cl-remove nil (mapcar (lambda (s) (plist-get (cdr s) metric))
                                        sessions))))
    (when values
      (/ (cl-reduce #'+ values) (float (length values))))))

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

;;;; Global percentile estimation

(defun touchtype-stats-wpm-percentile (wpm)
  "Return the approximate percentile ranking for WPM.
Uses linear interpolation between entries in
`touchtype--wpm-percentile-table'.  Returns a float 0-100."
  (let ((table touchtype--wpm-percentile-table)
        (prev-wpm 0)
        (prev-pct 0.0))
    (catch 'done
      (dolist (entry table)
        (let ((entry-wpm (car entry))
              (entry-pct (cdr entry)))
          (when (<= wpm entry-wpm)
            (if (= entry-wpm prev-wpm)
                (throw 'done entry-pct)
              (let* ((ratio (/ (- wpm prev-wpm) (float (- entry-wpm prev-wpm)))))
                (throw 'done (+ prev-pct (* ratio (- entry-pct prev-pct)))))))
          (setq prev-wpm entry-wpm
                prev-pct entry-pct)))
      ;; Beyond the table: return highest percentile
      prev-pct)))

(provide 'touchtype-stats)

;;; touchtype-stats.el ends here
