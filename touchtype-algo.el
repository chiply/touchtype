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

(defun touchtype-algo--random-token (chars)
  "Generate a short random token (1-4 chars) from CHARS string."
  (let* ((len (1+ (random 4)))
         (n (length chars)))
    (apply #'string (cl-loop repeat len collect (aref chars (random n))))))

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

(defun touchtype-algo--filter-words-for-chars (allowed-chars)
  "Return a list of words from `touchtype--builtin-words' using only ALLOWED-CHARS.
Uses a hash-set for O(1) character membership tests."
  (let ((char-set (make-hash-table :test 'eql :size (length allowed-chars))))
    (dotimes (i (length allowed-chars))
      (puthash (aref allowed-chars i) t char-set))
    (cl-loop for i below (length touchtype--builtin-words)
             for w = (aref touchtype--builtin-words i)
             when (cl-every (lambda (c) (gethash c char-set)) w)
             collect w)))

(defun touchtype-algo-pick-word (allowed-chars)
  "Pick a word for `full-words' mode using ALLOWED-CHARS.
Prefers real words from `touchtype--builtin-words' that use only
ALLOWED-CHARS; falls back to pseudo-word generation when fewer than
15 real words qualify.
Note: This filters on every call.  For hot loops, prefer pre-filtering
with `touchtype-algo--filter-words-for-chars' once and picking from the result."
  (let ((valid (touchtype-algo--filter-words-for-chars allowed-chars)))
    (if (>= (length valid) 15)
        (nth (random (length valid)) valid)
      (or (touchtype-algo-generate-word allowed-chars) "hello"))))

;;;; N-gram drill

(defun touchtype-algo--progressive-p ()
  "Return non-nil when progressive key unlock is active."
  (or (eq touchtype-mode-selection 'progressive)
      (and touchtype-progressive-unlock
           (memq touchtype-mode-selection
                 '(bigram-drill trigram-drill tetragram-drill ngram-drill
                   common-words domain-words
                   weak-letters weak-ngrams weak-mixed)))))

(defun touchtype-algo--progressive-filter-words (words)
  "Filter WORDS sequence to those containing only unlocked characters.
Returns a vector.  Falls back to full WORDS if nothing matches."
  (let* ((unlocked touchtype--unlocked-keys)
         (filtered (cl-remove-if-not
                    (lambda (w)
                      (cl-every (lambda (c)
                                  (seq-contains-p unlocked c #'=))
                                w))
                    words)))
    (if (> (length filtered) 0)
        (vconcat filtered)
      (vconcat words))))

(defun touchtype-algo-ngram-line (ngrams &optional n-repeats)
  "Return a line of mixed n-grams from NGRAMS list.
N-REPEATS is the number of n-grams; when nil the function fills
to `touchtype-text-width' characters.  In progressive mode,
n-grams are filtered to those whose characters are all in
`touchtype--unlocked-keys'."
  (let* ((available
          (if (touchtype-algo--progressive-p)
              (or (cl-remove-if-not
                   (lambda (ng)
                     (cl-every (lambda (c)
                                 (seq-contains-p touchtype--unlocked-keys c #'=))
                               ng))
                   ngrams)
                  (list (car ngrams)))
            ngrams)))
    (let* ((pool-len (length available))
           (picks (if n-repeats
                      (cl-loop repeat n-repeats
                               collect (nth (random pool-len) available))
                    ;; Fill to ~75 characters with a mix of n-grams
                    (let ((parts nil)
                          (total 0))
                      (while (< total touchtype-text-width)
                        (let ((ng (nth (random pool-len) available)))
                          (push ng parts)
                          (cl-incf total (1+ (length ng)))))
                      (nreverse parts)))))
      (mapconcat #'identity picks " "))))

(defun touchtype-algo-bigram-line (&optional n-bigrams)
  "Return a line of repeated bigrams for bigram-drill mode.
N-BIGRAMS is the repeat count; nil fills to `touchtype-text-width' chars.
Thin wrapper around `touchtype-algo-ngram-line'."
  (touchtype-algo-ngram-line touchtype--common-bigrams n-bigrams))

;;;; Passage-based line generation helper

(defun touchtype-algo--generate-line-from-passage (passage-var offset-var)
  "Generate a line from a passage stored in buffer-local variables.
PASSAGE-VAR and OFFSET-VAR are symbols naming the passage string
and offset integer.  Breaks at `touchtype-text-width' chars on a word boundary.
Returns nil when the passage is exhausted."
  (let ((passage (symbol-value passage-var))
        (offset (symbol-value offset-var)))
    (when (and passage (< offset (length passage)))
      (let* ((remaining (- (length passage) offset))
             (target touchtype-text-width))
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

;;;; Code block preparation

(defun touchtype-algo--prepare-code-block ()
  "Pick a random code block and split it into lines."
  (let* ((source (if (and touchtype--code-language
                          (assq touchtype--code-language
                                touchtype--code-blocks-by-language))
                     (cdr (assq touchtype--code-language
                                touchtype--code-blocks-by-language))
                   touchtype--code-blocks))
         (block (aref source (random (length source)))))
    (setq touchtype--code-block-lines
          (cl-remove-if #'string-empty-p (split-string block "\n")))))

;;;; Line generation

(defconst touchtype-algo--alphabet "abcdefghijklmnopqrstuvwxyz"
  "All 26 lowercase letters.")

(defconst touchtype-algo--printable-ascii
  (apply #'string (number-sequence 32 126))
  "All printable ASCII characters.")

(defun touchtype-algo--progressive-or-alphabet ()
  "Return unlocked keys if progressive, otherwise full alphabet."
  (if (touchtype-algo--progressive-p)
      touchtype--unlocked-keys
    touchtype-algo--alphabet))

(defun touchtype-algo--allowed-for-mode ()
  "Return the allowed character string for the current mode."
  (pcase touchtype-mode-selection
    ;; Modes that always use unlocked keys
    ((or 'progressive 'bigram-drill 'trigram-drill 'tetragram-drill 'ngram-drill)
     touchtype--unlocked-keys)
    ;; Modes that use progressive-or-alphabet
    ((or 'common-words 'weak-letters 'weak-ngrams 'weak-mixed 'domain-words)
     (touchtype-algo--progressive-or-alphabet))
    ;; Full alphabet modes
    ((or 'full-words 'letters 'weak-words)
     touchtype-algo--alphabet)
    ;; Extended character sets
    ('letters+numbers
     (concat touchtype-algo--alphabet touchtype--numbers))
    ('letters+numbers+symbols
     (concat touchtype-algo--alphabet touchtype--numbers touchtype--symbols))
    ('narrative
     (concat touchtype-algo--alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
             touchtype--numbers touchtype--symbols " "))
    ('quote
     (concat touchtype-algo--alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
             touchtype--numbers touchtype--symbols " \""))
    ;; Special modes
    ('custom
     (if touchtype--custom-passage
         (let ((chars (delete-dups (string-to-list touchtype--custom-passage))))
           (apply #'string (cl-remove-if (lambda (c) (= c ?\s)) chars)))
       touchtype-algo--alphabet))
    ((or 'code 'symbol-drill) touchtype-algo--printable-ascii)
    ('left-hand  (touchtype-algo--hand-keys 'left))
    ('right-hand (touchtype-algo--hand-keys 'right))
    ('finger-drill (touchtype-algo--finger-keys touchtype--finger-selection))
    (_ touchtype--unlocked-keys)))

(defun touchtype-algo--build-word-line (pick-fn)
  "Build a line of words by calling PICK-FN repeatedly.
Fills to `touchtype-text-width' characters, space-separated."
  (let ((words '()) (total-len 0))
    (while (< total-len touchtype-text-width)
      (let ((w (funcall pick-fn)))
        (when w (push w words) (cl-incf total-len (1+ (length w))))))
    (mapconcat #'identity (nreverse words) " ")))

(defun touchtype-algo--build-word-line-from-vec (source)
  "Build a line by picking random words from SOURCE vector."
  (let ((n (length source)))
    (touchtype-algo--build-word-line (lambda () (aref source (random n))))))

(defun touchtype-algo--build-real-or-pseudo-line (allowed)
  "Build a line using real words from ALLOWED chars, or pseudo-words as fallback."
  (let* ((valid-words (touchtype-algo--filter-words-for-chars allowed))
         (use-real (>= (length valid-words) 15)))
    (touchtype-algo--build-word-line
     (if use-real
         (let ((n (length valid-words)))
           (lambda () (nth (random n) valid-words)))
       (lambda () (touchtype-algo-generate-word allowed))))))

(defun touchtype-algo-generate-line ()
  "Generate a full line of text for the current mode.
The line targets `touchtype-text-width' characters and is space-separated words."
  (pcase touchtype-mode-selection
    ('bigram-drill
     (touchtype-algo-bigram-line))
    ('trigram-drill
     (touchtype-algo-ngram-line touchtype--common-trigrams))
    ('tetragram-drill
     (touchtype-algo-ngram-line touchtype--common-tetragrams))
    ('ngram-drill
     (touchtype-algo-ngram-line (append touchtype--common-bigrams
                                        touchtype--common-trigrams
                                        touchtype--common-tetragrams)))
    ('narrative
     (or (touchtype-narrative-generate-line)
         (progn
           (touchtype-narrative--prepare-passage)
           (touchtype-narrative-generate-line))))
    ('custom
     (touchtype-algo--generate-line-from-passage
      'touchtype--custom-passage 'touchtype--custom-offset))
    ('common-words
     (let* ((pool-size (length touchtype--builtin-words))
            (n (if (zerop touchtype-common-words-count)
                   pool-size
                 (min touchtype-common-words-count pool-size)))
            (base-words (cl-subseq touchtype--builtin-words 0 n))
            (source (if (touchtype-algo--progressive-p)
                        (touchtype-algo--progressive-filter-words base-words)
                      base-words)))
       (touchtype-algo--build-word-line-from-vec source)))
    ('code
     (unless touchtype--code-block-lines
       (touchtype-algo--prepare-code-block))
     (pop touchtype--code-block-lines))
    ('weak-letters (touchtype-algo--weak-letters-line))
    ('weak-ngrams  (touchtype-algo--weak-ngrams-line))
    ('weak-mixed   (touchtype-algo--weak-mixed-line))
    ('weak-words   (touchtype-algo--weak-words-line))
    ('quote
     (or (touchtype-algo--generate-line-from-passage
          'touchtype--quote-passage 'touchtype--quote-offset)
         (progn
           (touchtype-algo--prepare-quote)
           (touchtype-algo--generate-line-from-passage
            'touchtype--quote-passage 'touchtype--quote-offset))))
    ('domain-words
     (let* ((domain (or touchtype--domain-selection 'programming))
            (words-vec (cdr (assq domain touchtype--domain-words))))
       (if (and words-vec (> (length words-vec) 0))
           (let ((source (if (touchtype-algo--progressive-p)
                             (touchtype-algo--progressive-filter-words words-vec)
                           words-vec)))
             (touchtype-algo--build-word-line-from-vec source))
         (touchtype-algo-generate-word touchtype-algo--alphabet))))
    ((or 'finger-drill 'left-hand 'right-hand)
     (touchtype-algo--build-real-or-pseudo-line (touchtype-algo--allowed-for-mode)))
    ('symbol-drill
     (touchtype-algo--symbol-drill-line))
    (_
     (let* ((allowed (touchtype-algo--allowed-for-mode))
            (focus-char (if (eq touchtype-mode-selection 'progressive)
                            (touchtype-algo--pick-focus-char)
                          touchtype--focused-key))
            (extra-chars (pcase touchtype-mode-selection
                           ('letters+numbers touchtype--numbers)
                           ('letters+numbers+symbols
                            (concat touchtype--numbers touchtype--symbols))
                           (_ nil)))
            (pick-fn (if (memq touchtype-mode-selection '(full-words))
                         (let ((valid (or (and (boundp 'touchtype--valid-words-cache)
                                               touchtype--valid-words-cache)
                                          (let ((result (touchtype-algo--filter-words-for-chars allowed)))
                                            (when (boundp 'touchtype--valid-words-cache)
                                              (setq touchtype--valid-words-cache result))
                                            result))))
                           (if (>= (length valid) 15)
                               (let ((n (length valid)))
                                 (lambda () (nth (random n) valid)))
                             (lambda () (or (touchtype-algo-generate-word allowed) "hello"))))
                       (lambda ()
                         (touchtype-algo-generate-word allowed focus-char)))))
       (touchtype-algo--build-word-line
        (if extra-chars
            (lambda ()
              (if (< (random 100) 30)
                  (touchtype-algo--random-token extra-chars)
                (funcall pick-fn)))
          pick-fn))))))

;;;; Inverse-confidence weighting for weak modes

(defun touchtype-algo--inverse-confidence-weights (chars)
  "Return alist of (CHAR . WEIGHT) where WEIGHT = max(0.01, 1.0 - confidence).
CHARS is a string of characters to weight."
  (mapcar (lambda (ch)
            (cons ch (max 0.01 (- 1.0 (touchtype-stats-get-confidence ch)))))
          (string-to-list chars)))

(defun touchtype-algo--pick-weighted-float (alist)
  "Pick a random element from ALIST of (KEY . FLOAT-WEIGHT) pairs.
Returns the KEY, or nil if ALIST is empty."
  (when alist
    (let* ((total (cl-reduce #'+ alist :key #'cdr :initial-value 0.0))
           (r (* (/ (random 10000) 10000.0) total))
           (acc 0.0)
           result)
      (cl-loop for (key . w) in alist
               do (setq acc (+ acc w))
               when (and (null result) (>= acc r))
               do (setq result key))
      (or result (car (car alist))))))

(defun touchtype-algo--build-suffix (len start-char allowed)
  "Build a suffix string of LEN characters starting from START-CHAR.
Walk the bigram table forward, filtered to ALLOWED chars string."
  (let ((result nil)
        (current start-char))
    (dotimes (_ len)
      (let* ((row (cdr (assq current touchtype--bigram-table)))
             (next (touchtype-algo--weighted-pick row allowed)))
        (if next
            (progn (push next result) (setq current next))
          (let ((ch (aref allowed (random (length allowed)))))
            (push ch result)
            (setq current ch)))))
    (apply #'string (nreverse result))))

(defun touchtype-algo--build-prefix (len target-char allowed)
  "Build a prefix string of LEN characters ending at TARGET-CHAR.
Walk the bigram table backward, filtered to ALLOWED chars."
  (let ((result nil)
        (current target-char))
    (dotimes (_ len)
      (let* ((candidates
              (cl-loop for (ch . row) in touchtype--bigram-table
                       when (and (seq-contains-p allowed ch #'=)
                                 (assq current row))
                       collect (cons ch (cdr (assq current row)))))
             (prev (if candidates
                       (touchtype-algo--pick-weighted-float
                        (mapcar (lambda (p) (cons (car p) (float (cdr p)))) candidates))
                     (aref allowed (random (length allowed))))))
        (push prev result)
        (setq current prev)))
    (apply #'string result)))

(defun touchtype-algo-generate-word-containing-ngram (ngram allowed-chars)
  "Generate a pseudo-word containing NGRAM embedded at a random position.
ALLOWED-CHARS is a string of characters.  The word length is 5-8.
Prefix is built by walking bigram table backward from NGRAM start,
suffix is built by walking forward from NGRAM end."
  (let* ((ng-len (length ngram))
         (word-len (+ 5 (random 4)))
         (fill-len (max 0 (- word-len ng-len)))
         (prefix-len (if (> fill-len 0) (random (1+ fill-len)) 0))
         (suffix-len (- fill-len prefix-len))
         (prefix (if (> prefix-len 0)
                     (touchtype-algo--build-prefix prefix-len (aref ngram 0) allowed-chars)
                   ""))
         (suffix (if (> suffix-len 0)
                     (touchtype-algo--build-suffix suffix-len (aref ngram (1- ng-len)) allowed-chars)
                   "")))
    (concat prefix ngram suffix)))

;;;; Weak mode line generators

(defun touchtype-algo--weak-letters-line ()
  "Generate a line focusing on the user's weakest letters.
Pick focus chars from the bottom N letters weighted by inverse confidence."
  (let* ((all-letters (touchtype-algo--progressive-or-alphabet))
         (weights (touchtype-algo--inverse-confidence-weights all-letters))
         (sorted (sort (copy-sequence weights)
                       (lambda (a b) (> (cdr a) (cdr b)))))
         (top-n (seq-take sorted touchtype-weak-letter-count))
         (has-stats (cl-some (lambda (p) (> (cdr p) 0.01)) top-n)))
    (if (not has-stats)
        (touchtype-algo--build-word-line
         (lambda () (touchtype-algo-generate-word all-letters)))
      (touchtype-algo--build-word-line
       (lambda ()
         (let ((focus (touchtype-algo--pick-weighted-float top-n)))
           (touchtype-algo-generate-word all-letters focus)))))))

(defun touchtype-algo--weak-ngrams-line ()
  "Generate a line drilling the user's weakest n-grams.
50% embed in pseudo-words, 50% drill raw."
  (let* ((all-letters (touchtype-algo--progressive-or-alphabet))
         (raw-weak (mapcar #'car
                           (or (and (boundp 'touchtype--weak-ngrams-cache)
                                    touchtype--weak-ngrams-cache)
                               (let ((result (touchtype-stats-get-weak-ngrams 2 4 30)))
                                 (when (boundp 'touchtype--weak-ngrams-cache)
                                   (setq touchtype--weak-ngrams-cache result))
                                 result))))
         (all-weak (if (touchtype-algo--progressive-p)
                       (cl-remove-if-not
                        (lambda (ng)
                          (cl-every (lambda (c)
                                      (seq-contains-p touchtype--unlocked-keys c #'=))
                                    ng))
                        raw-weak)
                     raw-weak)))
    (if (null all-weak)
        (touchtype-algo-ngram-line touchtype--common-bigrams)
      (let ((n-weak (length all-weak)))
        (touchtype-algo--build-word-line
         (lambda ()
           (let ((ngram (nth (random n-weak) all-weak)))
             (if (< (random 100) 50)
                 (touchtype-algo-generate-word-containing-ngram ngram all-letters)
               ngram))))))))

(defun touchtype-algo--weak-mixed-line ()
  "Generate a line mixing weak-letters and weak-ngrams approaches.
40% weak-letters, 40% weak-ngrams, 20% blended."
  (let ((roll (random 100)))
    (cond
     ((< roll 40) (touchtype-algo--weak-letters-line))
     ((< roll 80) (touchtype-algo--weak-ngrams-line))
     (t
      ;; Blended: half words from each
      (let ((part1 (split-string (touchtype-algo--weak-letters-line)))
            (part2 (split-string (touchtype-algo--weak-ngrams-line)))
            (words '())
            (total-len 0))
        (while (< total-len touchtype-text-width)
          (let ((src (if (< (random 100) 50) part1 part2)))
            (when src
              (let ((w (nth (random (length src)) src)))
                (push w words)
                (cl-incf total-len (1+ (length w)))))))
        (mapconcat #'identity (nreverse words) " "))))))

;;;; Weak-words line generator

(defun touchtype-algo--weak-words-line ()
  "Generate a line from the user's weakest words.
Falls back to common-words if no word stats exist.
Uses a pool of up to 50 weak words and avoids repeating the same
word consecutively."
  (let* ((weak (touchtype-stats-get-weak-words 50))
         (weak-words (mapcar #'car weak)))
    (if (null weak-words)
        (let ((n (min 100 (length touchtype--builtin-words))))
          (touchtype-algo--build-word-line
           (lambda () (aref touchtype--builtin-words (random n)))))
      (let* ((weighted (mapcar (lambda (w)
                                 (cons w (max 0.01 (- 1.0 (touchtype-stats-get-word-confidence w)))))
                               weak-words))
             (last-word nil))
        (touchtype-algo--build-word-line
         (lambda ()
           (let ((w (touchtype-algo--pick-weighted-float weighted))
                 (tries 0))
             (while (and w (string= w last-word) (< tries 5))
               (setq w (touchtype-algo--pick-weighted-float weighted))
               (cl-incf tries))
             (setq last-word w)
             w)))))))

;;;; Hand and layout helpers

(defun touchtype-algo--hand-keys (hand)
  "Return the key string for HAND (symbol `left' or `right').
Dispatches on `touchtype-keyboard-layout'."
  (let ((key (if (eq hand 'left) :left-hand :right-hand)))
    (touchtype--layout-get touchtype-keyboard-layout key)))

;;;; Finger drill helpers

(defun touchtype-algo--finger-map ()
  "Return the finger map alist for the current keyboard layout."
  (touchtype--layout-get touchtype-keyboard-layout :finger-map))

(defun touchtype-algo--finger-keys (finger)
  "Return a string of keys assigned to FINGER in the current layout."
  (let ((fmap (touchtype-algo--finger-map)))
    (apply #'string
           (mapcar #'car
                   (cl-remove-if-not (lambda (pair) (eq (cdr pair) finger))
                                     fmap)))))

;;;; Quote mode helpers

(defun touchtype-algo--prepare-quote ()
  "Pick a random quote and set passage vars."
  (let* ((entry (aref touchtype--quotes (random (length touchtype--quotes))))
         (text (car entry))
         (author (cdr entry)))
    (setq touchtype--quote-passage (format "\"%s\" - %s" text author)
          touchtype--quote-offset 0)))

(defun touchtype-algo--quote-in-progress-p ()
  "Return non-nil if a quote passage is partially typed."
  (and (eq touchtype-mode-selection 'quote)
       touchtype--quote-passage
       (< touchtype--quote-offset (length touchtype--quote-passage))))

;;;; Symbol drill

(defconst touchtype--programming-symbols "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"
  "Symbol characters for programming drills.")

(defconst touchtype--programming-bigrams
  '("->" "=>" "!=" "==" "<=" ">=" "&&" "||" "++" "--" "::" "{}" "[]"
    "()" "<>" ":=" "+=" "-=" "*=" "/=" "/*" "*/" "//" "<<" ">>" "?:"
    "?." "??" "~>" "<-" "|>" "<|" "#{" "${")
  "Common programming symbol bigrams for symbol-drill mode.")

(defun touchtype-algo--symbol-drill-line ()
  "Generate a line of symbol drill content.
60% programming bigrams, 40% short identifiers."
  (let ((words '()) (total-len 0))
    (while (< total-len touchtype-text-width)
      (let ((w (if (< (random 100) 60)
                   (nth (random (length touchtype--programming-bigrams))
                        touchtype--programming-bigrams)
                 ;; Short identifier: 2-4 random lowercase letters
                 (let* ((len (+ 2 (random 3)))
                        (chars (cl-loop repeat len
                                        collect (+ ?a (random 26)))))
                   (apply #'string chars)))))
        (push w words)
        (cl-incf total-len (1+ (length w)))))
    (mapconcat #'identity (nreverse words) " ")))

;;;; Key unlock logic

(defun touchtype-algo--unlock-order ()
  "Return the unlock order string for the current keyboard layout."
  (if (eq touchtype-keyboard-layout 'custom)
      (or touchtype-custom-unlock-order
          (touchtype--layout-get 'qwerty :unlock-order))
    (touchtype--layout-get touchtype-keyboard-layout :unlock-order)))

(defun touchtype-algo--graduated-threshold (position)
  "Return unlock confidence threshold for key at POSITION (1-indexed).
When `touchtype-graduated-thresholds' is non-nil, looks up the threshold
from `touchtype-graduated-threshold-tiers'.  Otherwise returns
`touchtype-unlock-threshold'."
  (if (not touchtype-graduated-thresholds)
      touchtype-unlock-threshold
    (let ((threshold touchtype-unlock-threshold))
      (cl-loop for (max-pos . thr) in touchtype-graduated-threshold-tiers
               when (<= position max-pos)
               do (setq threshold thr) and return nil)
      threshold)))

(defun touchtype-algo-should-unlock-p ()
  "Return non-nil if all currently unlocked keys meet the confidence threshold.
Checks every character in `touchtype--unlocked-keys' against the
appropriate threshold (graduated or flat)."
  (let ((order (touchtype-algo--unlock-order)))
    (cl-every (lambda (ch)
                (let* ((pos (1+ (cl-position ch order)))
                       (threshold (touchtype-algo--graduated-threshold pos)))
                  (>= (touchtype-stats-get-confidence ch)
                      threshold)))
              (string-to-list touchtype--unlocked-keys))))

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
