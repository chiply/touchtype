;;; touchtype-algo.el --- Word generation and key-unlock logic -*- lexical-binding: t; -*-

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

;; Pseudo-word generation from the embedded bigram frequency table,
;; real-word selection for full-words mode, bigram drill line generation,
;; and the key-unlock decision logic.

;;; Code:

(require 'cl-lib)
(require 'touchtype-var)
(require 'touchtype-stats)

(declare-function touchtype-narrative-generate-line "touchtype-narrative")
(declare-function touchtype-narrative--prepare-passage "touchtype-narrative")

;;;; Internal helpers

(defun touchtype-algo--weighted-pick (weighted-list allowed)
  "Pick a random element from WEIGHTED-LIST filtered to ALLOWED chars.
WEIGHTED-LIST is a list of (CHAR . WEIGHT) pairs.
ALLOWED is a string of permitted characters.
Returns a character, or nil if no valid candidate exists."
  (let* ((filtered (cl-remove-if-not
                    (lambda (pair) (seq-contains-p allowed (car pair) #'=))
                    weighted-list))
         (total (cl-reduce #'+ filtered :key #'cdr :initial-value 0)))
    (when (> total 0)
      (let ((r (random total))
            (acc 0)
            result)
        (cl-loop for (ch . w) in filtered
                 do (cl-incf acc w)
                 when (and (null result) (>= acc r))
                 do (setq result ch))
        result))))

(defun touchtype-algo--unigram-weights (allowed)
  "Return an alist of (CHAR . TOTAL-WEIGHT) for each char in ALLOWED.
Total weight is the sum of all outgoing bigram frequencies for CHAR."
  (cl-loop for ch across allowed
           collect
           (cons ch
                 (let ((row (cdr (assq ch touchtype--bigram-table))))
                   (cl-reduce #'+ row :key #'cdr :initial-value 1)))))

;;;; Pseudo-word generation

(defun touchtype-algo--effective-word-limits (n-allowed)
  "Return (MIN-LEN . MAX-LEN) scaled for N-ALLOWED available characters.
With very few keys, shorter words feel more natural and avoid
single-character repetition."
  (cond ((<= n-allowed 3) (cons 2 4))
        ((<= n-allowed 5) (cons 3 5))
        (t (cons touchtype-word-length-min touchtype-word-length-max))))

(defun touchtype-algo--random-pick-word (allowed-chars min-len max-len)
  "Generate a word by randomly picking from ALLOWED-CHARS.
Length is between MIN-LEN and MAX-LEN.  Avoids repeating the same
character more than twice in a row."
  (let* ((len (+ min-len (random (1+ (- max-len min-len)))))
         (chars (string-to-list allowed-chars))
         (word nil)
         (prev nil)
         (prev2 nil))
    (dotimes (_ len)
      (let* ((candidates (if (and prev (eql prev prev2))
                             (remq prev chars)
                           chars))
             (ch (nth (random (length candidates)) candidates)))
        (push ch word)
        (setq prev2 prev
              prev ch)))
    (apply #'string (nreverse word))))

(defun touchtype-algo-generate-word (allowed-chars &optional focused-char)
  "Generate a pseudo-word using only characters in ALLOWED-CHARS string.
If FOCUSED-CHAR is non-nil, approximately 40% of generated words are
guaranteed to contain it.

When ALLOWED-CHARS has 4 or fewer characters the bigram table is too
sparse for natural generation, so a random-pick strategy is used
instead, with word lengths scaled down to avoid repetitive output.

The bigram algorithm:
1. Picks a start character weighted by total outgoing bigram frequency.
2. Iteratively picks the next character from the bigram table, filtered
   to ALLOWED-CHARS.
3. Word-end probability increases with length: p_end proportional to
   1.3^length, giving a natural 4-7 character distribution.
4. Retries up to 5 times if generation fails.

Returns a string, or nil after 5 failed attempts."
  (let* ((n-allowed (length allowed-chars))
         (limits    (touchtype-algo--effective-word-limits n-allowed))
         (wmin      (car limits))
         (wmax      (cdr limits)))
    ;; Small alphabet: random-pick is more reliable than bigram transitions
    (if (<= n-allowed 4)
        (let ((word (touchtype-algo--random-pick-word allowed-chars wmin wmax)))
          (when (and focused-char
                     (not (seq-contains-p word focused-char #'=))
                     (< (random 100) 40))
            (let ((pos (random (length word))))
              (aset word pos focused-char)))
          word)
      ;; Normal bigram-table generation
      (let ((attempt 0)
            result)
        (while (and (null result) (< attempt 5))
          (cl-incf attempt)
          (let* ((start (touchtype-algo--weighted-pick
                         (touchtype-algo--unigram-weights allowed-chars)
                         allowed-chars))
                 (word (when start (list start)))
                 (current start))
            (when start
              (cl-block word-gen
                (while t
                  (let* ((len (length word))
                         (end-prob
                          (if (< len wmin)
                              0
                            (min 80
                                 (round (* 10 (expt 1.3 (- len wmin)))))))
                         (force-end (>= len wmax))
                         (end-roll (random 100)))
                    (if (or force-end (>= end-roll (- 100 end-prob)))
                        (cl-return-from word-gen)
                      (let* ((row (cdr (assq current touchtype--bigram-table)))
                             (next (touchtype-algo--weighted-pick
                                    row allowed-chars)))
                        (if (null next)
                            (cl-return-from word-gen)
                          (push next word)
                          (setq current next)))))))
              (when (>= (length word) wmin)
                (setq word (nreverse word))
                (when (and focused-char
                           (not (memq focused-char word))
                           (< (random 100) 40))
                  (let ((pos (random (length word))))
                    (setf (nth pos word) focused-char)))
                (setq result (apply #'string word))))))
        result))))                                            ; closes: let, defun

;;;; Real-word mode

(defun touchtype-algo-pick-word (allowed-chars)
  "Pick a word for `full-words' mode using ALLOWED-CHARS.
Prefers real words from `touchtype--builtin-words' that use only
ALLOWED-CHARS; falls back to pseudo-word generation when fewer than
15 real words qualify."
  (let* ((len (length touchtype--builtin-words))
         (valid (cl-loop for i below len
                         for w = (aref touchtype--builtin-words i)
                         when (cl-every (lambda (c)
                                          (seq-contains-p allowed-chars c #'=))
                                        w)
                         collect w)))
    (if (>= (length valid) 15)
        (nth (random (length valid)) valid)
      (or (touchtype-algo-generate-word allowed-chars) "hello"))))

;;;; N-gram drill

(defun touchtype-algo-ngram-line (ngrams &optional n-repeats)
  "Return a line of repeated n-grams from NGRAMS list.
N-REPEATS is the number of repetitions; when nil the function fills
to ~75 characters.  N-grams containing chars not in
`touchtype--unlocked-keys' are skipped."
  (let* ((available
          (cl-remove-if-not
           (lambda (ng)
             (cl-every (lambda (c)
                         (seq-contains-p touchtype--unlocked-keys c #'=))
                       ng))
           ngrams)))
    (when (null available)
      (setq available (list (car ngrams))))  ; minimal fallback
    (let* ((chosen (nth (random (length available)) available))
           (n (or n-repeats
                  (max 4 (/ 75 (1+ (length chosen))))))
           (repeated (cl-loop repeat n collect chosen)))
      (mapconcat #'identity repeated " "))))

(defun touchtype-algo-bigram-line (&optional n-bigrams)
  "Return a line of repeated bigrams for bigram-drill mode.
N-BIGRAMS is the repeat count; nil fills to ~75 chars.
Thin wrapper around `touchtype-algo-ngram-line'."
  (touchtype-algo-ngram-line touchtype--common-bigrams n-bigrams))

;;;; Passage-based line generation helper

(defun touchtype-algo--generate-line-from-passage (passage-var offset-var)
  "Generate a line from a passage stored in buffer-local variables.
PASSAGE-VAR and OFFSET-VAR are symbols naming the passage string
and offset integer.  Breaks at ~60 chars on a word boundary.
Returns nil when the passage is exhausted."
  (let ((passage (symbol-value passage-var))
        (offset (symbol-value offset-var)))
    (when (and passage (< offset (length passage)))
      (let* ((remaining (- (length passage) offset))
             (target 60))
        (if (<= remaining target)
            (prog1 (string-trim (substring passage offset))
              (set offset-var (length passage)))
          (let ((break-pos (+ offset target)))
            (while (and (> break-pos offset)
                        (not (= (aref passage break-pos) ?\s)))
              (cl-decf break-pos))
            (when (= break-pos offset)
              (setq break-pos (+ offset target))
              (while (and (< break-pos (length passage))
                          (not (= (aref passage break-pos) ?\s)))
                (cl-incf break-pos)))
            (let ((line (string-trim (substring passage offset break-pos))))
              (set offset-var (min (length passage) (1+ break-pos)))
              line)))))))

;;;; Line generation

(defun touchtype-algo--allowed-for-mode ()
  "Return the allowed character string for the current mode."
  (pcase touchtype-mode-selection
    ('progressive touchtype--unlocked-keys)
    ('full-words  "abcdefghijklmnopqrstuvwxyz")
    ('bigram-drill touchtype--unlocked-keys)
    ('trigram-drill touchtype--unlocked-keys)
    ('tetragram-drill touchtype--unlocked-keys)
    ('ngram-drill touchtype--unlocked-keys)
    ('letters     "abcdefghijklmnopqrstuvwxyz")
    ('letters+numbers
     (concat "abcdefghijklmnopqrstuvwxyz" touchtype--numbers))
    ('letters+numbers+symbols
     (concat "abcdefghijklmnopqrstuvwxyz"
             touchtype--numbers touchtype--symbols))
    ('narrative
     (concat "abcdefghijklmnopqrstuvwxyz"
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
             touchtype--numbers touchtype--symbols " "))
    ('common-words "abcdefghijklmnopqrstuvwxyz")
    ('custom
     (if touchtype--custom-passage
         (let ((chars (delete-dups (string-to-list touchtype--custom-passage))))
           (apply #'string (cl-remove-if (lambda (c) (= c ?\s)) chars)))
       "abcdefghijklmnopqrstuvwxyz"))
    ('code
     (apply #'string (number-sequence 32 126)))
    (_ touchtype--unlocked-keys)))

(defun touchtype-algo-generate-line ()
  "Generate a full line of text for the current mode.
The line targets ~75 characters and is space-separated words."
  (cond
   ((eq touchtype-mode-selection 'bigram-drill)
    (touchtype-algo-bigram-line))
   ((eq touchtype-mode-selection 'trigram-drill)
    (touchtype-algo-ngram-line touchtype--common-trigrams))
   ((eq touchtype-mode-selection 'tetragram-drill)
    (touchtype-algo-ngram-line touchtype--common-tetragrams))
   ((eq touchtype-mode-selection 'ngram-drill)
    (let ((lists (list touchtype--common-bigrams
                       touchtype--common-trigrams
                       touchtype--common-tetragrams)))
      (touchtype-algo-ngram-line (nth (random 3) lists))))
   ((eq touchtype-mode-selection 'narrative)
    (or (touchtype-narrative-generate-line)
        (progn
          (touchtype-narrative--prepare-passage)
          (touchtype-narrative-generate-line))))
   ((eq touchtype-mode-selection 'custom)
    (touchtype-algo--generate-line-from-passage
     'touchtype--custom-passage 'touchtype--custom-offset))
   ((eq touchtype-mode-selection 'common-words)
    (let* ((n (min touchtype-common-words-count
                   (length touchtype--builtin-words)))
           (words '())
           (total-len 0))
      (while (< total-len 70)
        (let ((w (aref touchtype--builtin-words (random n))))
          (push w words)
          (cl-incf total-len (1+ (length w)))))
      (mapconcat #'identity (nreverse words) " ")))
   ((eq touchtype-mode-selection 'code)
    (let* ((n (length touchtype--code-snippets))
           (snippets '())
           (total-len 0))
      (while (< total-len 60)
        (let ((s (aref touchtype--code-snippets (random n))))
          (push s snippets)
          (cl-incf total-len (+ 2 (length s)))))
      (mapconcat #'identity (nreverse snippets) "  ")))
   (t
    (let* ((allowed (touchtype-algo--allowed-for-mode))
           (words '())
           (total-len 0)
           (focus-char (if (eq touchtype-mode-selection 'progressive)
                           (touchtype-algo--pick-focus-char)
                         touchtype--focused-key))
           (pick-fn (if (memq touchtype-mode-selection '(full-words))
                        (lambda () (touchtype-algo-pick-word allowed))
                      (lambda ()
                        (touchtype-algo-generate-word allowed focus-char)))))
      (while (< total-len 70)
        (let ((w (funcall pick-fn)))
          (when w
            (push w words)
            (cl-incf total-len (1+ (length w))))))
      (mapconcat #'identity (nreverse words) " ")))))

;;;; Key unlock logic

(defun touchtype-algo--unlock-order ()
  "Return the unlock order string for the current keyboard layout."
  (pcase touchtype-keyboard-layout
    ('qwerty  touchtype--qwerty-unlock-order)
    ('dvorak  touchtype--dvorak-unlock-order)
    ('colemak touchtype--colemak-unlock-order)
    ('workman touchtype--workman-unlock-order)
    ('custom  (or touchtype-custom-unlock-order touchtype--qwerty-unlock-order))
    (_        touchtype--qwerty-unlock-order)))

(defun touchtype-algo-should-unlock-p ()
  "Return non-nil if all currently unlocked keys meet the confidence threshold.
Checks every character in `touchtype--unlocked-keys' against
`touchtype-unlock-threshold'."
  (cl-every (lambda (ch)
              (>= (touchtype-stats-get-confidence ch)
                  touchtype-unlock-threshold))
            (string-to-list touchtype--unlocked-keys)))

(defun touchtype-algo-unlock-next-key ()
  "Unlock the next key from the current layout's unlock order.
Updates `touchtype--unlocked-keys' and sets `touchtype--focused-key'.
Returns the newly unlocked character, or nil if all keys are already
unlocked."
  (let* ((order (touchtype-algo--unlock-order))
         (next (cl-find-if-not
                (lambda (ch)
                  (seq-contains-p touchtype--unlocked-keys ch #'=))
                order)))
    (when next
      (setq touchtype--unlocked-keys
            (concat touchtype--unlocked-keys (string next)))
      (touchtype-stats-set-unlocked-keys touchtype--unlocked-keys)
      (setq touchtype--focused-key next)
      next)))

;;;; Weak-letter repetition

(defun touchtype-algo--pick-focus-char ()
  "Pick a focus character for progressive mode word generation.
70% chance: return `touchtype--focused-key' (newest unlock).
30% chance: return a random weak letter below threshold from the
unlocked set.  Returns nil if no focus is warranted."
  (if (null touchtype--focused-key)
      nil
    (if (< (random 100) 70)
        touchtype--focused-key
      (let* ((weak (cl-remove-if
                    (lambda (ch)
                      (>= (touchtype-stats-get-confidence ch)
                          touchtype-unlock-threshold))
                    (string-to-list touchtype--unlocked-keys))))
        (if weak
            (nth (random (length weak)) weak)
          touchtype--focused-key)))))

(provide 'touchtype-algo)

;;; touchtype-algo.el ends here
