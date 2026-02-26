;;; touchtype-algo.el --- Word generation and key-unlock logic -*- lexical-binding: t; -*-

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

;; Pseudo-word generation from the embedded bigram frequency table,
;; real-word selection for full-words mode, bigram drill line generation,
;; and the key-unlock decision logic.

;;; Code:

(require 'cl-lib)
(require 'touchtype-var)
(require 'touchtype-stats)

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

(defun touchtype-algo-generate-word (allowed-chars &optional focused-char)
  "Generate a pseudo-word using only characters in ALLOWED-CHARS string.
If FOCUSED-CHAR is non-nil, approximately 40% of generated words are
guaranteed to contain it.

The algorithm:
1. Picks a start character weighted by total outgoing bigram frequency.
2. Iteratively picks the next character from the bigram table, filtered
   to ALLOWED-CHARS.
3. Word-end probability increases with length: p_end proportional to
   1.3^length, giving a natural 4-7 character distribution.
4. Retries up to 5 times if generation fails.

Returns a string, or nil after 5 failed attempts."
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
                      (if (< len touchtype-word-length-min)
                          0
                        (min 80
                             (round (* 10 (expt 1.3
                                              (- len
                                                 touchtype-word-length-min)))))))
                     (force-end (>= len touchtype-word-length-max))
                     (end-roll (random 100)))
                (if (or force-end (>= end-roll (- 100 end-prob)))
                    (cl-return-from word-gen)
                  (let* ((row (cdr (assq current touchtype--bigram-table)))
                         (next (touchtype-algo--weighted-pick
                                row allowed-chars)))
                    (if (null next)
                        (cl-return-from word-gen)
                      (push next word)
                      (setq current next)))))))          ; closes: let*, while, cl-block
          (when (>= (length word) touchtype-word-length-min)
            (setq word (nreverse word))
            (when (and focused-char
                       (not (memq focused-char word))
                       (< (random 100) 40))
              (let ((pos (random (length word))))
                (setf (nth pos word) focused-char)))    ; closes: let, when(and)
            (setq result (apply #'string word)))        ; closes: when(>=)
          )                                             ; closes: when(start)
        )                                               ; closes: let*(start)
      )                                                 ; closes: while(attempt)
    result))                                            ; closes: let, defun

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

;;;; Bigram drill

(defun touchtype-algo-bigram-line (&optional n-bigrams)
  "Return a line of repeated bigrams for bigram-drill mode.
N-BIGRAMS defaults to 10.  Bigrams containing letters not in
`touchtype--unlocked-keys' are skipped."
  (let* ((n (or n-bigrams 10))
         (available
          (cl-remove-if-not
           (lambda (bg)
             (cl-every (lambda (c)
                         (seq-contains-p touchtype--unlocked-keys c #'=))
                       bg))
           touchtype--common-bigrams)))
    (when (null available)
      (setq available (list "fs" "jd")))  ; minimal fallback
    (let* ((chosen (nth (random (length available)) available))
           (repeated (cl-loop repeat n collect chosen)))
      (mapconcat #'identity repeated " "))))

;;;; Line generation

(defun touchtype-algo--allowed-for-mode ()
  "Return the allowed character string for the current mode."
  (pcase touchtype-mode-selection
    ('progressive touchtype--unlocked-keys)
    ('full-words  "abcdefghijklmnopqrstuvwxyz")
    ('bigram-drill touchtype--unlocked-keys)
    ('letters     "abcdefghijklmnopqrstuvwxyz")
    ('letters+numbers
     (concat "abcdefghijklmnopqrstuvwxyz" touchtype--numbers))
    ('letters+numbers+symbols
     (concat "abcdefghijklmnopqrstuvwxyz"
             touchtype--numbers touchtype--symbols))
    (_ touchtype--unlocked-keys)))

(defun touchtype-algo-generate-line ()
  "Generate a full line of text for the current mode.
The line targets ~40 characters and is space-separated words."
  (if (eq touchtype-mode-selection 'bigram-drill)
      (touchtype-algo-bigram-line 8)
    (let* ((allowed (touchtype-algo--allowed-for-mode))
           (words '())
           (total-len 0)
           (pick-fn (if (eq touchtype-mode-selection 'full-words)
                        (lambda () (touchtype-algo-pick-word allowed))
                      (lambda ()
                        (touchtype-algo-generate-word
                         allowed touchtype--focused-key)))))
      (while (< total-len 35)
        (let ((w (funcall pick-fn)))
          (when w
            (push w words)
            (cl-incf total-len (1+ (length w))))))
      (mapconcat #'identity (nreverse words) " "))))

;;;; Key unlock logic

(defun touchtype-algo-should-unlock-p ()
  "Return non-nil if all currently unlocked keys meet the confidence threshold.
Checks every character in `touchtype--unlocked-keys' against
`touchtype-unlock-threshold'."
  (cl-every (lambda (ch)
              (>= (touchtype-stats-get-confidence ch)
                  touchtype-unlock-threshold))
            (string-to-list touchtype--unlocked-keys)))

(defun touchtype-algo-unlock-next-key ()
  "Unlock the next key from `touchtype--qwerty-unlock-order'.
Updates `touchtype--unlocked-keys' and sets `touchtype--focused-key'.
Returns the newly unlocked character, or nil if all keys are already
unlocked."
  (let* ((order touchtype--qwerty-unlock-order)
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

(provide 'touchtype-algo)

;;; touchtype-algo.el ends here
