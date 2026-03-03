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
  "Return a line of mixed n-grams from NGRAMS list.
N-REPEATS is the number of n-grams; when nil the function fills
to ~75 characters.  In progressive mode, n-grams are filtered to
those whose characters are all in `touchtype--unlocked-keys'."
  (let* ((available
          (if (eq touchtype-mode-selection 'progressive)
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
                      (while (< total 75)
                        (let ((ng (nth (random pool-len) available)))
                          (push ng parts)
                          (cl-incf total (1+ (length ng)))))
                      (nreverse parts)))))
      (mapconcat #'identity picks " "))))

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
    ('weak-letters "abcdefghijklmnopqrstuvwxyz")
    ('weak-bigrams "abcdefghijklmnopqrstuvwxyz")
    ('weak-mixed   "abcdefghijklmnopqrstuvwxyz")
    ('quote
     (concat "abcdefghijklmnopqrstuvwxyz"
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
             touchtype--numbers touchtype--symbols " "))
    ('domain-words "abcdefghijklmnopqrstuvwxyz")
    ('left-hand  (touchtype-algo--hand-keys 'left))
    ('right-hand (touchtype-algo--hand-keys 'right))
    ('symbol-drill
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
    (let* ((source (if (and touchtype--code-language
                            (assq touchtype--code-language
                                  touchtype--code-snippets-by-language))
                       (cdr (assq touchtype--code-language
                                  touchtype--code-snippets-by-language))
                     touchtype--code-snippets))
           (n (length source))
           (snippets '())
           (total-len 0))
      (while (< total-len 60)
        (let ((s (aref source (random n))))
          (push s snippets)
          (cl-incf total-len (+ 2 (length s)))))
      (mapconcat #'identity (nreverse snippets) "  ")))
   ((eq touchtype-mode-selection 'weak-letters)
    (touchtype-algo--weak-letters-line))
   ((eq touchtype-mode-selection 'weak-bigrams)
    (touchtype-algo--weak-bigrams-line))
   ((eq touchtype-mode-selection 'weak-mixed)
    (touchtype-algo--weak-mixed-line))
   ((eq touchtype-mode-selection 'quote)
    (or (touchtype-algo--generate-line-from-passage
         'touchtype--quote-passage 'touchtype--quote-offset)
        (progn
          (touchtype-algo--prepare-quote)
          (touchtype-algo--generate-line-from-passage
           'touchtype--quote-passage 'touchtype--quote-offset))))
   ((eq touchtype-mode-selection 'domain-words)
    (let* ((domain (or touchtype--domain-selection 'programming))
           (words-vec (cdr (assq domain touchtype--domain-words))))
      (if (and words-vec (> (length words-vec) 0))
          (let ((words '()) (total-len 0)
                (n (length words-vec)))
            (while (< total-len 70)
              (push (aref words-vec (random n)) words)
              (cl-incf total-len (1+ (length (car words)))))
            (mapconcat #'identity (nreverse words) " "))
        ;; Fallback
        (touchtype-algo-generate-word "abcdefghijklmnopqrstuvwxyz"))))
   ((memq touchtype-mode-selection '(left-hand right-hand))
    (let* ((allowed (touchtype-algo--allowed-for-mode))
           (valid-words (cl-loop for i below (length touchtype--builtin-words)
                                 for w = (aref touchtype--builtin-words i)
                                 when (cl-every (lambda (c)
                                                  (seq-contains-p allowed c #'=))
                                                w)
                                 collect w))
           (use-real (>= (length valid-words) 15))
           (words '()) (total-len 0))
      (while (< total-len 70)
        (let ((w (if use-real
                     (nth (random (length valid-words)) valid-words)
                   (touchtype-algo-generate-word allowed))))
          (when w (push w words) (cl-incf total-len (1+ (length w))))))
      (mapconcat #'identity (nreverse words) " ")))
   ((eq touchtype-mode-selection 'symbol-drill)
    (touchtype-algo--symbol-drill-line))
   (t
    (let* ((allowed (touchtype-algo--allowed-for-mode))
           (words '())
           (total-len 0)
           (focus-char (if (eq touchtype-mode-selection 'progressive)
                           (touchtype-algo--pick-focus-char)
                         touchtype--focused-key))
           (extra-chars (pcase touchtype-mode-selection
                          ('letters+numbers touchtype--numbers)
                          ('letters+numbers+symbols
                           (concat touchtype--numbers touchtype--symbols))
                          (_ nil)))
           (pick-fn (if (memq touchtype-mode-selection '(full-words))
                        (lambda () (touchtype-algo-pick-word allowed))
                      (lambda ()
                        (touchtype-algo-generate-word allowed focus-char)))))
      (while (< total-len 70)
        (let ((w (if (and extra-chars (< (random 100) 30))
                     (touchtype-algo--random-token extra-chars)
                   (funcall pick-fn))))
          (when w
            (push w words)
            (cl-incf total-len (1+ (length w))))))
      (mapconcat #'identity (nreverse words) " ")))))

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
  (let* ((all-letters "abcdefghijklmnopqrstuvwxyz")
         (weights (touchtype-algo--inverse-confidence-weights all-letters))
         ;; Sort by weight descending (weakest first)
         (sorted (sort (copy-sequence weights)
                       (lambda (a b) (> (cdr a) (cdr b)))))
         (top-n (seq-take sorted touchtype-weak-letter-count))
         (has-stats (cl-some (lambda (p) (> (cdr p) 0.01)) top-n)))
    (if (not has-stats)
        ;; Cold start: fall back to standard pseudo-words
        (let ((words '()) (total-len 0))
          (while (< total-len 70)
            (let ((w (touchtype-algo-generate-word all-letters)))
              (when w (push w words) (cl-incf total-len (1+ (length w))))))
          (mapconcat #'identity (nreverse words) " "))
      ;; Generate words with weak-letter focus
      (let ((words '()) (total-len 0))
        (while (< total-len 70)
          (let* ((focus (touchtype-algo--pick-weighted-float top-n))
                 (w (touchtype-algo-generate-word all-letters focus)))
            (when w (push w words) (cl-incf total-len (1+ (length w))))))
        (mapconcat #'identity (nreverse words) " ")))))

(defun touchtype-algo--weak-bigrams-line ()
  "Generate a line drilling the user's weakest n-grams.
50% embed in pseudo-words, 50% drill raw."
  (let* ((all-letters "abcdefghijklmnopqrstuvwxyz")
         (weak-bi (touchtype-stats-get-weak-bigrams 10))
         (weak-tri (touchtype-stats-get-weak-ngrams 3 3 10))
         (weak-tet (touchtype-stats-get-weak-ngrams 4 4 10))
         (all-weak (append (mapcar #'car weak-bi)
                           (mapcar #'car weak-tri)
                           (mapcar #'car weak-tet))))
    (if (null all-weak)
        ;; Cold start: fall back to common bigram drill
        (touchtype-algo-ngram-line touchtype--common-bigrams)
      (let ((words '()) (total-len 0))
        (while (< total-len 70)
          (let* ((ngram (nth (random (length all-weak)) all-weak))
                 (embed-p (< (random 100) 50))
                 (w (if embed-p
                        (touchtype-algo-generate-word-containing-ngram ngram all-letters)
                      ngram)))
            (push w words)
            (cl-incf total-len (1+ (length w)))))
        (mapconcat #'identity (nreverse words) " ")))))

(defun touchtype-algo--weak-mixed-line ()
  "Generate a line mixing weak-letters and weak-bigrams approaches.
40% weak-letters, 40% weak-bigrams, 20% blended."
  (let ((roll (random 100)))
    (cond
     ((< roll 40) (touchtype-algo--weak-letters-line))
     ((< roll 80) (touchtype-algo--weak-bigrams-line))
     (t
      ;; Blended: half words from each
      (let ((part1 (split-string (touchtype-algo--weak-letters-line)))
            (part2 (split-string (touchtype-algo--weak-bigrams-line)))
            (words '())
            (total-len 0))
        (while (< total-len 70)
          (let ((src (if (< (random 100) 50) part1 part2)))
            (when src
              (let ((w (nth (random (length src)) src)))
                (push w words)
                (cl-incf total-len (1+ (length w)))))))
        (mapconcat #'identity (nreverse words) " "))))))

;;;; Hand and layout helpers

(defun touchtype-algo--hand-keys (hand)
  "Return the key string for HAND (symbol `left' or `right').
Dispatches on `touchtype-keyboard-layout'."
  (pcase (list touchtype-keyboard-layout hand)
    ('(qwerty left)   touchtype--qwerty-left-hand)
    ('(qwerty right)  touchtype--qwerty-right-hand)
    ('(dvorak left)   touchtype--dvorak-left-hand)
    ('(dvorak right)  touchtype--dvorak-right-hand)
    ('(colemak left)  touchtype--colemak-left-hand)
    ('(colemak right) touchtype--colemak-right-hand)
    ('(workman left)  touchtype--workman-left-hand)
    ('(workman right) touchtype--workman-right-hand)
    (_ (if (eq hand 'left) touchtype--qwerty-left-hand touchtype--qwerty-right-hand))))

;;;; Quote mode helpers

(defun touchtype-algo--prepare-quote ()
  "Pick a random quote and set passage vars."
  (let ((quote (aref touchtype--quotes (random (length touchtype--quotes)))))
    (setq touchtype--quote-passage quote
          touchtype--quote-offset 0)))

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
    (while (< total-len 70)
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
