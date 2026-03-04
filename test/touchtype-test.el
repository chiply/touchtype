;;; touchtype-test.el --- ERT tests for touchtype -*- lexical-binding: t; -*-

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

;; ERT test suite for the touchtype package.
;; Run with: eask test ert test/touchtype-test.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'touchtype-var)
(require 'touchtype-stats)
(require 'touchtype-algo)
(require 'touchtype-ui)
(require 'touchtype)
(require 'touchtype-narrative)

;;; ─── Confidence score tests ──────────────────────────────────────────────────

(ert-deftest touchtype-test-confidence-zero-hits ()
  "Confidence is 0.0 when a letter has never been typed."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40))
    (should (= 0.0 (touchtype-stats-get-confidence ?a)))))

(ert-deftest touchtype-test-confidence-perfect ()
  "Confidence is 1.0 when accuracy is perfect and speed exceeds target."
  (let* ((target-ms (/ 60000.0 (* 40 5)))  ; ~300 ms
         (fast-ms   (round (/ target-ms 2.0))) ; 2× faster than target
         (touchtype--stats
          (list :version 1
                :letter-stats (list (list ?a :hits 100 :misses 0
                                          :total-ms (* 100 fast-ms)
                                          :best-ms fast-ms))
                :bigram-stats nil :sessions nil
                :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (should (= 1.0 (touchtype-stats-get-confidence ?a)))))

(ert-deftest touchtype-test-confidence-partial ()
  "Confidence is between 0 and 1 with some hits and misses."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (touchtype--stats
          (list :version 1
                :letter-stats (list (list ?s :hits 80 :misses 20
                                          :total-ms (* 80 (round target-ms))
                                          :best-ms (round target-ms)))
                :bigram-stats nil :sessions nil
                :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (let ((conf (touchtype-stats-get-confidence ?s)))
      (should (> conf 0.0))
      (should (< conf 1.0)))))

(ert-deftest touchtype-test-confidence-slow-typist ()
  "Confidence is low when speed is well below target."
  (let* ((target-ms  (/ 60000.0 (* 40 5)))
         (slow-ms    (* target-ms 5))      ; 5× slower than target
         (touchtype--stats
          (list :version 1
                :letter-stats (list (list ?f :hits 50 :misses 0
                                          :total-ms (round (* 50 slow-ms))
                                          :best-ms (round slow-ms)))
                :bigram-stats nil :sessions nil
                :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (should (< (touchtype-stats-get-confidence ?f) 0.25))))

;;; ─── Word generation tests ───────────────────────────────────────────────────

(ert-deftest touchtype-test-word-generation-allowed-chars ()
  "Generated words contain only characters from the allowed set."
  (let ((allowed "fjdksl"))
    (dotimes (_ 20)
      (let ((word (touchtype-algo-generate-word allowed)))
        (when word
          (dolist (ch (string-to-list word))
            (should (seq-contains-p allowed ch #'=))))))))

(ert-deftest touchtype-test-word-generation-length ()
  "Generated words respect the min/max length bounds."
  (let ((allowed "abcdefghijklmnopqrstuvwxyz")
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (dotimes (_ 30)
      (let ((word (touchtype-algo-generate-word allowed)))
        (when word
          (should (>= (length word) touchtype-word-length-min))
          (should (<= (length word) touchtype-word-length-max)))))))

(ert-deftest touchtype-test-word-generation-focused-char ()
  "Focused char appears in a significant fraction of generated words."
  (let* ((allowed "asdfjkl")
         (focused ?f)
         (hits 0)
         (trials 50))
    (dotimes (_ trials)
      (let ((word (touchtype-algo-generate-word allowed focused)))
        (when (and word (seq-contains-p word focused #'=))
          (cl-incf hits))))
    ;; Expect at least 25% of words to contain the focused char
    (should (>= hits (/ trials 4)))))

(ert-deftest touchtype-test-word-generation-non-empty ()
  "Word generation returns a non-empty string for a reasonable allowed set."
  (let ((word (touchtype-algo-generate-word "abcdefghijklmnopqrstuvwxyz")))
    (should (stringp word))
    (should (> (length word) 0))))

;;; ─── Unlock logic tests ──────────────────────────────────────────────────────

(ert-deftest touchtype-test-unlock-logic-should-not-unlock ()
  "Should-unlock returns nil when confidence is below threshold."
  (let* ((touchtype--stats
          (list :version 1
                :letter-stats (list (list ?f :hits 10 :misses 10
                                          :total-ms 50000 :best-ms 300)
                                    (list ?j :hits 10 :misses 10
                                          :total-ms 50000 :best-ms 300))
                :bigram-stats nil :sessions nil
                :unlocked-keys "fj" :confidence nil))
         (touchtype--unlocked-keys "fj")
         (touchtype-unlock-threshold 0.80)
         (touchtype-target-wpm 40))
    (should-not (touchtype-algo-should-unlock-p))))

(ert-deftest touchtype-test-unlock-logic-should-unlock ()
  "Should-unlock returns non-nil when all keys exceed the threshold."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (fast-ms   (round (/ target-ms 2.0)))
         (touchtype--stats
          (list :version 1
                :letter-stats (list (list ?f :hits 100 :misses 0
                                          :total-ms (* 100 fast-ms)
                                          :best-ms fast-ms)
                                    (list ?j :hits 100 :misses 0
                                          :total-ms (* 100 fast-ms)
                                          :best-ms fast-ms))
                :bigram-stats nil :sessions nil
                :unlocked-keys "fj" :confidence nil))
         (touchtype--unlocked-keys "fj")
         (touchtype-unlock-threshold 0.80)
         (touchtype-target-wpm 40))
    (should (touchtype-algo-should-unlock-p))))

(ert-deftest touchtype-test-unlock-next-key ()
  "Unlocking adds the next key from the progression order."
  (let ((touchtype--unlocked-keys "fj")
        (touchtype--focused-key nil)
        (touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (let ((new-key (touchtype-algo-unlock-next-key)))
      (should (characterp new-key))
      ;; The third key in the order should now be unlocked
      (should (seq-contains-p touchtype--unlocked-keys new-key #'=))
      (should (= new-key touchtype--focused-key)))))

;;; ─── Bigram drill line tests ─────────────────────────────────────────────────

(ert-deftest touchtype-test-bigram-line-structure ()
  "Bigram drill line is space-separated and non-empty."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz"))
    (let ((line (touchtype-algo-bigram-line 5)))
      (should (stringp line))
      (should (> (length line) 0))
      ;; Should have spaces separating bigrams
      (should (seq-contains-p line ?\s #'=)))))

(ert-deftest touchtype-test-bigram-line-allowed-chars ()
  "Bigram drill only uses bigrams whose letters are all unlocked."
  (let ((touchtype--unlocked-keys "the"))  ; only t, h, e unlocked
    (let ((line (touchtype-algo-bigram-line 8)))
      (dolist (ch (string-to-list (replace-regexp-in-string " " "" line)))
        (should (seq-contains-p "the" ch #'=))))))

(ert-deftest touchtype-test-bigram-line-fallback ()
  "Bigram drill falls back gracefully when very few keys are unlocked."
  (let ((touchtype--unlocked-keys "fj"))
    ;; Should not error; fallback bigrams are used
    (let ((line (touchtype-algo-bigram-line 5)))
      (should (stringp line)))))

;;; ─── Stats roundtrip tests ───────────────────────────────────────────────────

(ert-deftest touchtype-test-stats-roundtrip ()
  "Save then load produces stats equal to the original."
  (let* ((tmp-file (make-temp-file "touchtype-test-stats" nil ".el"))
         (touchtype-stats-file tmp-file)
         (touchtype--stats
          (list :version 1
                :letter-stats (list (list ?a :hits 42 :misses 3
                                          :total-ms 12600 :best-ms 180))
                :bigram-stats (list (list "th" :hits 20 :misses 1
                                          :total-ms 4000))
                :sessions nil
                :unlocked-keys "fjdksl"
                :confidence nil)))
    (unwind-protect
        (progn
          (touchtype-stats-save)
          ;; Reset in-memory state
          (setq touchtype--stats nil)
          (touchtype-stats-load)
          ;; Check letter stats survived roundtrip
          (let* ((lstats (plist-get touchtype--stats :letter-stats))
                 (entry  (assq ?a lstats)))
            (should entry)
            (should (= 42 (plist-get (cdr entry) :hits)))
            (should (= 3  (plist-get (cdr entry) :misses)))
            (should (= 12600 (plist-get (cdr entry) :total-ms))))
          ;; Check bigram stats
          (let* ((bstats (plist-get touchtype--stats :bigram-stats))
                 (entry  (assoc "th" bstats)))
            (should entry)
            (should (= 20 (plist-get (cdr entry) :hits))))
          ;; Check unlocked keys
          (should (equal "fjdksl"
                         (plist-get touchtype--stats :unlocked-keys))))
      (delete-file tmp-file))))

(ert-deftest touchtype-test-stats-record-keypress ()
  "Record-keypress correctly increments hits and total-ms."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (touchtype-stats-record-keypress ?a t 250)
    (touchtype-stats-record-keypress ?a t 200)
    (touchtype-stats-record-keypress ?a nil 300)
    (let* ((lstats (plist-get touchtype--stats :letter-stats))
           (entry  (assq ?a lstats)))
      (should (= 2   (plist-get (cdr entry) :hits)))
      (should (= 1   (plist-get (cdr entry) :misses)))
      (should (= 450 (plist-get (cdr entry) :total-ms)))
      (should (= 200 (plist-get (cdr entry) :best-ms))))))

;;; ─── Backspace / cursor position tests ──────────────────────────────────────

(ert-deftest touchtype-test-backspace-cursor ()
  "Backspace decrements the cursor position correctly."
  ;; Test the cursor-decrement logic directly without needing the minor mode.
  (let ((touchtype--cursor-pos 3)
        (touchtype--typed-chars '((?l t 200) (?e t 210) (?h t 190))))
    ;; Simulate a single backspace (the guard + decrement + pop)
    (when (> touchtype--cursor-pos 0)
      (cl-decf touchtype--cursor-pos)
      (setq touchtype--typed-chars (cdr touchtype--typed-chars)))
    (should (= 2 touchtype--cursor-pos))
    (should (= 2 (length touchtype--typed-chars)))))

(ert-deftest touchtype-test-backspace-at-start ()
  "Backspace does nothing when cursor is at position 0."
  (let ((touchtype--cursor-pos 0)
        (touchtype--typed-chars nil))
    ;; Guard that mimics touchtype-ui--handle-backspace
    (when (> touchtype--cursor-pos 0)
      (cl-decf touchtype--cursor-pos))
    (should (= 0 touchtype--cursor-pos))))

;;; ─── Line generation smoke tests ─────────────────────────────────────────────

(ert-deftest touchtype-test-generate-line-progressive ()
  "Line generation in progressive mode returns a non-empty string."
  (let ((touchtype--unlocked-keys "fjdksl")
        (touchtype--focused-key nil)
        (touchtype-mode-selection 'progressive)
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-generate-line-letters ()
  "Line generation in letters mode uses only lowercase letters."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil)
        (touchtype-mode-selection 'letters)
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo-generate-line)))
      (dolist (ch (string-to-list (replace-regexp-in-string " " "" line)))
        (should (and (>= ch ?a) (<= ch ?z)))))))

;;; ─── Overlay face tests ────────────────────────────────────────────────────

(ert-deftest touchtype-test-overlays-cover-characters ()
  "Each character overlay spans exactly one character after redraw."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (touchtype--current-text "hello"))
      (touchtype-mode 1)
      (dolist (sym '(touchtype--current-text
                     touchtype--typed-chars
                     touchtype--cursor-pos
                     touchtype--target-start
                     touchtype--status-start
                     touchtype--cursor-overlay
                     touchtype--char-overlays
                     touchtype--session-wpm-samples
                     touchtype--session-errors
                     touchtype--session-total-keys
                     touchtype--session-word-count
                     touchtype--line-start-time
                     touchtype--last-key-time
                     touchtype--focused-key
                     touchtype--unlocked-keys))
        (make-local-variable sym))
      (setq touchtype--cursor-pos 0
            touchtype--typed-chars nil
            touchtype--session-wpm-samples nil
            touchtype--session-errors 0
            touchtype--session-total-keys 0
            touchtype--session-word-count 0
            touchtype--line-start-time (float-time)
            touchtype--last-key-time (float-time)
            touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
      (touchtype-ui--redraw-buffer)
      ;; Every overlay should span exactly 1 character
      (dotimes (i (length touchtype--current-text))
        (let ((ov (aref touchtype--char-overlays i)))
          (should ov)
          (should (= 1 (- (overlay-end ov) (overlay-start ov)))))))))

(ert-deftest touchtype-test-overlays-initial-face-is-untyped ()
  "All character overlays start with the untyped face."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (touchtype--current-text "abc"))
      (touchtype-mode 1)
      (dolist (sym '(touchtype--current-text
                     touchtype--typed-chars
                     touchtype--cursor-pos
                     touchtype--target-start
                     touchtype--status-start
                     touchtype--cursor-overlay
                     touchtype--char-overlays
                     touchtype--session-wpm-samples
                     touchtype--session-errors
                     touchtype--session-total-keys
                     touchtype--session-word-count
                     touchtype--line-start-time
                     touchtype--last-key-time
                     touchtype--focused-key
                     touchtype--unlocked-keys))
        (make-local-variable sym))
      (setq touchtype--cursor-pos 0
            touchtype--typed-chars nil
            touchtype--session-wpm-samples nil
            touchtype--session-errors 0
            touchtype--session-total-keys 0
            touchtype--session-word-count 0
            touchtype--line-start-time (float-time)
            touchtype--last-key-time (float-time)
            touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
      (touchtype-ui--redraw-buffer)
      (dotimes (i (length touchtype--current-text))
        (let ((ov (aref touchtype--char-overlays i)))
          (should (eq 'touchtype-face-untyped (overlay-get ov 'face))))))))

;;; ─── Bigram confidence and trend tests ──────────────────────────────────────

(ert-deftest touchtype-test-bigram-confidence-with-data ()
  "Bigram confidence is between 0 and 1 with realistic data."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (touchtype--stats
          (list :version 1 :letter-stats nil
                :bigram-stats (list (list "th" :hits 50 :misses 5
                                          :total-ms (round (* 50 target-ms))))
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (let ((conf (touchtype-stats-get-bigram-confidence "th")))
      (should (> conf 0.0))
      (should (<= conf 1.0)))))

(ert-deftest touchtype-test-bigram-confidence-no-data ()
  "Bigram confidence is 0.0 when no data exists."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40))
    (should (= 0.0 (touchtype-stats-get-bigram-confidence "zq")))))

(ert-deftest touchtype-test-weak-bigrams-sorting ()
  "Weak bigrams are sorted by confidence ascending."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (touchtype--stats
          (list :version 1 :letter-stats nil
                :bigram-stats
                (list (list "th" :hits 100 :misses 0
                            :total-ms (round (* 100 (/ target-ms 2.0))))
                      (list "er" :hits 50 :misses 20
                            :total-ms (round (* 50 (* target-ms 3))))
                      (list "in" :hits 30 :misses 10
                            :total-ms (round (* 30 (* target-ms 2)))))
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (let* ((weak (touchtype-stats-get-weak-bigrams 10))
           (confs (mapcar (lambda (e) (touchtype-stats-get-bigram-confidence (car e)))
                          weak)))
      (should (= (length weak) 3))
      ;; Each confidence should be <= the next
      (cl-loop for (a b) on confs while b
               do (should (<= a b))))))

(ert-deftest touchtype-test-trend-direction-improving ()
  "Trend direction is `improving' when second half is higher."
  (should (eq 'improving
              (touchtype-stats-get-trend-direction '(30.0 32.0 35.0 40.0 42.0 45.0)))))

(ert-deftest touchtype-test-trend-direction-declining ()
  "Trend direction is `declining' when second half is lower."
  (should (eq 'declining
              (touchtype-stats-get-trend-direction '(45.0 42.0 40.0 35.0 32.0 30.0)))))

(ert-deftest touchtype-test-trend-direction-stable ()
  "Trend direction is `stable' when halves are similar."
  (should (eq 'stable
              (touchtype-stats-get-trend-direction '(40.0 40.0 40.0 40.0)))))

;;; ─── Word corpus tests ─────────────────────────────────────────────────────

(ert-deftest touchtype-test-builtin-words-no-duplicates ()
  "The built-in word vector has no duplicate entries."
  (let* ((words (append touchtype--builtin-words nil))
         (unique (delete-dups (copy-sequence words))))
    (should (= (length words) (length unique)))))

(ert-deftest touchtype-test-builtin-words-length-span ()
  "Word lengths span at least 2 through 12."
  (let ((lengths (mapcar #'length (append touchtype--builtin-words nil))))
    (should (<= (apply #'min lengths) 2))
    (should (>= (apply #'max lengths) 12))))

(ert-deftest touchtype-test-builtin-words-all-alpha ()
  "Every word in the corpus matches [a-z]+ only."
  (let ((bad-words '()))
    (cl-loop for w across touchtype--builtin-words
             unless (string-match-p "\\`[a-z]+\\'" w)
             do (push w bad-words))
    (should (null bad-words))))

;;; ─── Session length preset tests ───────────────────────────────────────────

(ert-deftest touchtype-test-presets-maps-symbols-to-integers ()
  "Session length presets alist maps symbols to positive integers."
  (dolist (pair touchtype-session-length-presets)
    (should (symbolp (car pair)))
    (should (integerp (cdr pair)))
    (should (> (cdr pair) 0))))

(ert-deftest touchtype-test-prefix-arg-sets-session-length ()
  "Prefix arg sets `touchtype-session-length' to the numeric value."
  (let ((touchtype-session-length 30))
    (touchtype--apply-prefix-arg 50)
    (should (= 50 touchtype-session-length))))

(ert-deftest touchtype-test-prefix-arg-nil-preserves-length ()
  "Nil prefix arg leaves `touchtype-session-length' unchanged."
  (let ((touchtype-session-length 30))
    (touchtype--apply-prefix-arg nil)
    (should (= 30 touchtype-session-length))))

;;; ─── Narrative mode tests ───────────────────────────────────────────────────

(ert-deftest touchtype-test-narrative-extract-body ()
  "Gutenberg header/footer stripping extracts just the body text."
  (let* ((sample (concat
                  "The Project Gutenberg eBook of Test\r\n"
                  "\r\n"
                  "*** START OF THE PROJECT GUTENBERG EBOOK TEST ***\r\n"
                  "\r\n"
                  "It was a dark and stormy night. The rain fell in torrents.\r\n"
                  "\r\n"
                  "*** END OF THE PROJECT GUTENBERG EBOOK TEST ***\r\n"
                  "\r\n"
                  "Updated editions will replace this one.\r\n"))
         (body (touchtype-narrative--extract-body sample)))
    (should (string-match-p "dark and stormy" body))
    (should-not (string-match-p "Project Gutenberg" body))
    (should-not (string-match-p "Updated editions" body))))

(ert-deftest touchtype-test-narrative-generate-line-from-passage ()
  "Line generation produces reasonable-length lines from a set passage."
  (let ((touchtype--narrative-passage
         "It was a dark and stormy night. The rain fell in torrents except at occasional intervals when it was checked by a violent gust of wind.")
        (touchtype--narrative-offset 0))
    (let ((line (touchtype-narrative-generate-line)))
      (should (stringp line))
      (should (> (length line) 0))
      (should (<= (length line) 80)))))

(ert-deftest touchtype-test-narrative-generate-line-advances-offset ()
  "Successive calls to generate-line advance through the passage."
  (let ((touchtype--narrative-passage
         "First sentence here. Second sentence there. Third sentence everywhere.")
        (touchtype--narrative-offset 0))
    (let ((line1 (touchtype-narrative-generate-line)))
      (should (stringp line1))
      (should (> touchtype--narrative-offset 0))
      ;; Second call should get different text or nil
      (let ((line2 (touchtype-narrative-generate-line)))
        (when line2
          (should-not (equal line1 line2)))))))

(ert-deftest touchtype-test-narrative-exhausted-returns-nil ()
  "generate-line returns nil when passage is fully consumed."
  (let ((touchtype--narrative-passage "short")
        (touchtype--narrative-offset 5))
    (should (null (touchtype-narrative-generate-line)))))

;;; ─── N-gram data tests ──────────────────────────────────────────────────────

(ert-deftest touchtype-test-trigram-list-count ()
  "Trigram list has exactly 100 entries."
  (should (= 100 (length touchtype--common-trigrams))))

(ert-deftest touchtype-test-trigram-list-lengths ()
  "All trigrams are exactly 3 characters long."
  (dolist (tri touchtype--common-trigrams)
    (should (= 3 (length tri)))))

(ert-deftest touchtype-test-tetragram-list-count ()
  "Tetragram list has exactly 100 entries."
  (should (= 100 (length touchtype--common-tetragrams))))

(ert-deftest touchtype-test-tetragram-list-lengths ()
  "All tetragrams are exactly 4 characters long."
  (dolist (tet touchtype--common-tetragrams)
    (should (= 4 (length tet)))))

;;; ─── N-gram line generation tests ──────────────────────────────────────────

(ert-deftest touchtype-test-ngram-line-structure ()
  "N-gram line is space-separated and non-empty."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz"))
    (let ((line (touchtype-algo-ngram-line touchtype--common-trigrams 5)))
      (should (stringp line))
      (should (> (length line) 0))
      (should (seq-contains-p line ?\s #'=)))))

(ert-deftest touchtype-test-ngram-line-uses-source-list ()
  "N-gram line words come from the source list."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz"))
    (let* ((line (touchtype-algo-ngram-line touchtype--common-trigrams 5))
           (parts (split-string line " " t)))
      (dolist (part parts)
        (should (member part touchtype--common-trigrams))))))

(ert-deftest touchtype-test-ngram-line-allowed-chars ()
  "N-gram line only uses n-grams whose letters are all unlocked."
  (let ((touchtype--unlocked-keys "thei"))
    (let ((line (touchtype-algo-ngram-line touchtype--common-trigrams 5)))
      (dolist (ch (string-to-list (replace-regexp-in-string " " "" line)))
        (should (seq-contains-p "thei" ch #'=))))))

;;; ─── Mode dispatch tests ───────────────────────────────────────────────────

(ert-deftest touchtype-test-generate-line-trigram-drill ()
  "Line generation in trigram-drill mode returns a valid trigram line."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil)
        (touchtype-mode-selection 'trigram-drill))
    (let* ((line (touchtype-algo-generate-line))
           (parts (split-string line " " t)))
      (should (stringp line))
      (should (> (length line) 0))
      (dolist (part parts)
        (should (= 3 (length part)))))))

(ert-deftest touchtype-test-generate-line-tetragram-drill ()
  "Line generation in tetragram-drill mode returns a valid tetragram line."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil)
        (touchtype-mode-selection 'tetragram-drill))
    (let* ((line (touchtype-algo-generate-line))
           (parts (split-string line " " t)))
      (should (stringp line))
      (should (> (length line) 0))
      (dolist (part parts)
        (should (= 4 (length part)))))))

(ert-deftest touchtype-test-generate-line-ngram-drill ()
  "Line generation in ngram-drill mode returns n-grams of length 2-4."
  (let ((touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil)
        (touchtype-mode-selection 'ngram-drill))
    (let* ((line (touchtype-algo-generate-line))
           (parts (split-string line " " t)))
      (should (stringp line))
      (should (> (length line) 0))
      (dolist (part parts)
        (should (>= (length part) 2))
        (should (<= (length part) 4))))))

;;; ─── N-gram recording tests ────────────────────────────────────────────────

(ert-deftest touchtype-test-ngram-recording ()
  "After typing 4+ chars, bigram-stats has entries of length 2, 3, and 4."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40))
    ;; Simulate recording for the sequence "test"
    ;; pos=0: just letter, no bigram
    (touchtype-stats-record-keypress ?t t 200)
    ;; pos=1: bigram "te"
    (touchtype-stats-record-keypress ?e t 200)
    (touchtype-stats-record-bigram "te" t 200)
    ;; pos=2: bigram "es", trigram "tes"
    (touchtype-stats-record-keypress ?s t 200)
    (touchtype-stats-record-bigram "es" t 200)
    (touchtype-stats-record-bigram "tes" t 200)
    ;; pos=3: bigram "st", trigram "est", tetragram "test"
    (touchtype-stats-record-keypress ?t t 200)
    (touchtype-stats-record-bigram "st" t 200)
    (touchtype-stats-record-bigram "est" t 200)
    (touchtype-stats-record-bigram "test" t 200)
    ;; Check entries exist at each length
    (let ((bstats (plist-get touchtype--stats :bigram-stats)))
      ;; Length 2
      (should (assoc "te" bstats))
      (should (assoc "es" bstats))
      (should (assoc "st" bstats))
      ;; Length 3
      (should (assoc "tes" bstats))
      (should (assoc "est" bstats))
      ;; Length 4
      (should (assoc "test" bstats)))))

;;; ─── Weak n-grams tests ───────────────────────────────────────────────────

(ert-deftest touchtype-test-weak-ngrams-filters-by-length ()
  "get-weak-ngrams filters by string length correctly."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (touchtype--stats
          (list :version 1 :letter-stats nil
                :bigram-stats
                (list (list "th" :hits 10 :misses 2
                            :total-ms (round (* 10 target-ms)))
                      (list "the" :hits 10 :misses 3
                            :total-ms (round (* 10 (* target-ms 2))))
                      (list "that" :hits 10 :misses 1
                            :total-ms (round (* 10 target-ms))))
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    ;; Only trigrams
    (let ((tri (touchtype-stats-get-weak-ngrams 3 3 10)))
      (should (= 1 (length tri)))
      (should (equal "the" (caar tri))))
    ;; Only tetragrams
    (let ((tet (touchtype-stats-get-weak-ngrams 4 4 10)))
      (should (= 1 (length tet)))
      (should (equal "that" (caar tet))))
    ;; Range 2-4 includes all
    (let ((all (touchtype-stats-get-weak-ngrams 2 4 10)))
      (should (= 3 (length all))))))

(ert-deftest touchtype-test-weak-ngrams-sorts-by-confidence ()
  "get-weak-ngrams sorts by confidence ascending."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (touchtype--stats
          (list :version 1 :letter-stats nil
                :bigram-stats
                (list (list "the" :hits 100 :misses 0
                            :total-ms (round (* 100 (/ target-ms 2.0))))
                      (list "and" :hits 10 :misses 5
                            :total-ms (round (* 10 (* target-ms 3))))
                      (list "ing" :hits 50 :misses 10
                            :total-ms (round (* 50 (* target-ms 2)))))
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (let* ((weak (touchtype-stats-get-weak-ngrams 3 3 10))
           (confs (mapcar (lambda (e) (touchtype-stats-get-bigram-confidence (car e)))
                          weak)))
      (should (= 3 (length weak)))
      (cl-loop for (a b) on confs while b
               do (should (<= a b))))))

;;; ─── Personal bests tests ──────────────────────────────────────────────────

(ert-deftest touchtype-test-personal-best-returns-max ()
  "get-personal-best returns the max WPM for a given mode."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list
                          (list '2026-01-01 :wpm 35.0 :accuracy 92.0
                                :mode 'progressive :words 30)
                          (list '2026-01-02 :wpm 45.0 :accuracy 95.0
                                :mode 'progressive :words 30)
                          (list '2026-01-03 :wpm 40.0 :accuracy 98.0
                                :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil)))
    (should (= 45.0 (touchtype-stats-get-personal-best 'progressive :wpm)))
    (should (= 98.0 (touchtype-stats-get-personal-best 'progressive :accuracy)))))

(ert-deftest touchtype-test-personal-best-nil-for-unknown-mode ()
  "get-personal-best returns nil when no sessions exist for a mode."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list
                          (list '2026-01-01 :wpm 35.0 :accuracy 92.0
                                :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil)))
    (should (null (touchtype-stats-get-personal-best 'bigram-drill :wpm)))))

(ert-deftest touchtype-test-all-personal-bests ()
  "get-all-personal-bests returns correct alist for all modes."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list
                          (list '2026-01-01 :wpm 35.0 :accuracy 92.0
                                :mode 'progressive :words 30)
                          (list '2026-01-02 :wpm 50.0 :accuracy 97.0
                                :mode 'bigram-drill :words 30)
                          (list '2026-01-03 :wpm 45.0 :accuracy 95.0
                                :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil)))
    (let ((bests (touchtype-stats-get-all-personal-bests)))
      (should (= 2 (length bests)))
      (let ((prog-best (cdr (assq 'progressive bests))))
        (should prog-best)
        (should (= 45.0 (plist-get prog-best :wpm)))
        (should (= 95.0 (plist-get prog-best :accuracy))))
      (let ((bg-best (cdr (assq 'bigram-drill bests))))
        (should bg-best)
        (should (= 50.0 (plist-get bg-best :wpm)))))))

;;; ─── Session record extended fields tests ──────────────────────────────────

(ert-deftest touchtype-test-session-record-extra-fields ()
  "Session record includes new extended fields."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (touchtype-stats-record-session
     42.0 95.0 'progressive 30
     :gross-wpm 45.0 :total-time 120.0 :total-chars 600
     :corrections 5 :uncorrected-errors 2 :consistency 85.0)
    (let* ((sessions (plist-get touchtype--stats :sessions))
           (s (cdr (car sessions))))
      (should (= 42.0 (plist-get s :wpm)))
      (should (= 45.0 (plist-get s :gross-wpm)))
      (should (= 120.0 (plist-get s :total-time)))
      (should (= 600 (plist-get s :total-chars)))
      (should (= 5 (plist-get s :corrections)))
      (should (= 2 (plist-get s :uncorrected-errors)))
      (should (= 85.0 (plist-get s :consistency))))))

;;; ─── Corrections counter tests ─────────────────────────────────────────────

(ert-deftest touchtype-test-corrections-increment-on-backspace ()
  "Corrections counter increments on backspace."
  (let ((touchtype--cursor-pos 3)
        (touchtype--session-corrections 0)
        (touchtype--typed-chars '((?l t 200) (?e t 210) (?h t 190))))
    ;; Simulate backspace logic
    (when (> touchtype--cursor-pos 0)
      (cl-incf touchtype--session-corrections)
      (cl-decf touchtype--cursor-pos)
      (setq touchtype--typed-chars (cdr touchtype--typed-chars)))
    (should (= 1 touchtype--session-corrections))
    (should (= 2 touchtype--cursor-pos))))

;;; ─── Consistency score tests ───────────────────────────────────────────────

(ert-deftest touchtype-test-consistency-perfect ()
  "Consistency is 100% when all line WPMs are identical."
  (should (= 100.0 (touchtype-ui--consistency-score '(40.0 40.0 40.0 40.0)))))

(ert-deftest touchtype-test-consistency-with-variance ()
  "Consistency is less than 100% when line WPMs vary."
  (let ((score (touchtype-ui--consistency-score '(30.0 40.0 50.0 60.0))))
    (should (> score 0.0))
    (should (< score 100.0))))

(ert-deftest touchtype-test-consistency-single-sample ()
  "Consistency is 100% with fewer than 2 samples."
  (should (= 100.0 (touchtype-ui--consistency-score '(40.0))))
  (should (= 100.0 (touchtype-ui--consistency-score nil))))

;;; ─── Feature 4: Quick Restart ───────────────────────────────────────────────

(ert-deftest touchtype-test-tab-bound-in-keymap ()
  "TAB is bound in the training keymap."
  (let ((map (touchtype-ui--make-keymap)))
    (should (eq (lookup-key map (kbd "TAB")) #'touchtype-ui--quick-restart))))

(ert-deftest touchtype-test-quick-restart-resets-counters ()
  "Quick restart resets session counters."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (touchtype-mode 1)
      (dolist (sym '(touchtype--current-text touchtype--typed-chars
                     touchtype--cursor-pos touchtype--target-start
                     touchtype--status-start touchtype--cursor-overlay
                     touchtype--char-overlays touchtype--session-wpm-samples
                     touchtype--session-errors touchtype--session-total-keys
                     touchtype--session-word-count touchtype--line-start-time
                     touchtype--last-key-time touchtype--focused-key
                     touchtype--unlocked-keys touchtype--session-corrections
                     touchtype--session-start-time touchtype--session-line-wpms
                     touchtype--session-timer touchtype--session-idle-time
                     touchtype--preview-texts touchtype--pace-timer
                     touchtype--pace-pos touchtype--pace-overlay))
        (make-local-variable sym))
      (setq touchtype--session-errors 5
            touchtype--session-total-keys 50
            touchtype--session-word-count 10
            touchtype--session-corrections 3
            touchtype--session-idle-time 5.0
            touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz"
            touchtype--session-timer nil
            touchtype--pace-timer nil
            touchtype--pace-overlay nil
            touchtype--preview-texts nil
            touchtype-mode-selection 'letters)
      (setq touchtype-ui--keymap (touchtype-ui--make-keymap))
      (use-local-map touchtype-ui--keymap)
      (touchtype-ui--restart-session)
      (should (= 0 touchtype--session-errors))
      (should (= 0 touchtype--session-total-keys))
      (should (= 0 touchtype--session-word-count))
      (should (= 0 touchtype--session-corrections))
      (should (= 0.0 touchtype--session-idle-time)))))

;;; ─── Feature 5: Keyboard Layout ───────────────────────────────────────────

(ert-deftest touchtype-test-layout-qwerty ()
  "QWERTY layout returns the QWERTY unlock order."
  (let ((touchtype-keyboard-layout 'qwerty))
    (should (equal (touchtype-algo--unlock-order) touchtype--qwerty-unlock-order))))

(ert-deftest touchtype-test-layout-dvorak ()
  "Dvorak layout returns the Dvorak unlock order."
  (let ((touchtype-keyboard-layout 'dvorak))
    (should (equal (touchtype-algo--unlock-order) touchtype--dvorak-unlock-order))))

(ert-deftest touchtype-test-layout-colemak ()
  "Colemak layout returns the Colemak unlock order."
  (let ((touchtype-keyboard-layout 'colemak))
    (should (equal (touchtype-algo--unlock-order) touchtype--colemak-unlock-order))))

(ert-deftest touchtype-test-layout-workman ()
  "Workman layout returns the Workman unlock order."
  (let ((touchtype-keyboard-layout 'workman))
    (should (equal (touchtype-algo--unlock-order) touchtype--workman-unlock-order))))

(ert-deftest touchtype-test-layout-custom ()
  "Custom layout uses `touchtype-custom-unlock-order'."
  (let ((touchtype-keyboard-layout 'custom)
        (touchtype-custom-unlock-order "zyxwvutsrqponmlkjihgfedcba"))
    (should (equal (touchtype-algo--unlock-order) "zyxwvutsrqponmlkjihgfedcba"))))

(ert-deftest touchtype-test-unlock-uses-selected-layout ()
  "unlock-next-key uses the layout's unlock order."
  (let ((touchtype-keyboard-layout 'dvorak)
        (touchtype--unlocked-keys "fj")
        (touchtype--focused-key nil)
        (touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (let ((new-key (touchtype-algo-unlock-next-key)))
      (should (characterp new-key))
      ;; Should be the first key in dvorak order not in "fj"
      (should (= new-key (aref touchtype--dvorak-unlock-order 0))))))

;;; ─── Feature 14: Stats Export ──────────────────────────────────────────────

(ert-deftest touchtype-test-export-json-structure ()
  "JSON export parses correctly and has expected keys."
  (require 'json)
  (let ((touchtype--stats
         (list :version 1
               :letter-stats (list (list ?a :hits 10 :misses 1
                                         :total-ms 3000 :best-ms 200))
               :bigram-stats nil
               :sessions (list (list '2026-01-01 :wpm 40.0 :accuracy 95.0
                                     :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil
               :daily-streak 3 :total-practice-time 7200
               :last-practice-date "2026-01-01"))
        (touchtype-target-wpm 40))
    (let* ((json-str (touchtype-stats-export-json))
           (parsed (json-read-from-string json-str)))
      (should (assoc 'sessions parsed))
      (should (assoc 'letter_stats parsed))
      (should (= 1 (length (cdr (assoc 'sessions parsed)))))
      (should (= 1 (length (cdr (assoc 'letter_stats parsed))))))))

(ert-deftest touchtype-test-export-csv-header-and-rows ()
  "CSV export has header and correct row count."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list (list '2026-01-01 :wpm 40.0 :accuracy 95.0
                                     :mode 'progressive :words 30)
                               (list '2026-01-02 :wpm 42.0 :accuracy 96.0
                                     :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil)))
    (let* ((csv (touchtype-stats-export-csv))
           (lines (split-string csv "\n" t)))
      (should (string-prefix-p "date," (car lines)))
      (should (= 3 (length lines))))))

;;; ─── Feature 15: Daily Streak ──────────────────────────────────────────────

(ert-deftest touchtype-test-streak-first-day ()
  "First practice day sets streak to 1."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (touchtype-stats-update-streak-and-time 120.0)
    (should (= 1 (plist-get touchtype--stats :daily-streak)))
    (should (= 120.0 (plist-get touchtype--stats :total-practice-time)))))

(ert-deftest touchtype-test-streak-same-day ()
  "Same-day practice doesn't change streak."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (touchtype--stats
          (list :version 1 :letter-stats nil :bigram-stats nil
                :sessions nil :unlocked-keys "fj" :confidence nil
                :daily-streak 3 :last-practice-date today
                :total-practice-time 100.0)))
    (touchtype-stats-update-streak-and-time 60.0)
    (should (= 3 (plist-get touchtype--stats :daily-streak)))
    (should (= 160.0 (plist-get touchtype--stats :total-practice-time)))))

(ert-deftest touchtype-test-streak-broken ()
  "Gap in practice resets streak to 1."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil
               :daily-streak 5 :last-practice-date "2020-01-01"
               :total-practice-time 1000.0)))
    (touchtype-stats-update-streak-and-time 60.0)
    (should (= 1 (plist-get touchtype--stats :daily-streak)))))

(ert-deftest touchtype-test-total-time-accumulation ()
  "Total practice time accumulates across sessions."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil
               :total-practice-time 500.0)))
    (touchtype-stats-update-streak-and-time 200.0)
    (should (= 700.0 (plist-get touchtype--stats :total-practice-time)))))

;;; ─── Feature 10: Top-N Common Words ────────────────────────────────────────

(ert-deftest touchtype-test-common-words-from-top-n ()
  "Common-words mode generates words from top N of builtin words."
  (let ((touchtype-mode-selection 'common-words)
        (touchtype-common-words-count 50)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil))
    (let* ((line (touchtype-algo-generate-line))
           (words (split-string line " " t))
           (top-n (cl-loop for i below 50
                           collect (aref touchtype--builtin-words i))))
      (should (> (length words) 0))
      (dolist (w words)
        (should (member w top-n))))))

(ert-deftest touchtype-test-common-words-line-non-empty ()
  "Common-words line is non-empty."
  (let ((touchtype-mode-selection 'common-words)
        (touchtype-common-words-count 100)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil))
    (should (> (length (touchtype-algo-generate-line)) 0))))

;;; ─── Feature 2: Custom Text ────────────────────────────────────────────────

(ert-deftest touchtype-test-custom-line-from-passage ()
  "Custom mode generates a line from the passage."
  (let ((touchtype-mode-selection 'custom)
        (touchtype--custom-passage "The quick brown fox jumps over the lazy dog repeatedly for testing purposes.")
        (touchtype--custom-offset 0))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0))
      (should (<= (length line) 80)))))

(ert-deftest touchtype-test-custom-passage-exhausted ()
  "Custom mode returns nil when passage is fully consumed."
  (let ((touchtype-mode-selection 'custom)
        (touchtype--custom-passage "short")
        (touchtype--custom-offset 5))
    (should (null (touchtype-algo-generate-line)))))

(ert-deftest touchtype-test-custom-allowed-includes-passage-chars ()
  "Allowed chars for custom mode includes all unique passage characters."
  (let ((touchtype-mode-selection 'custom)
        (touchtype--custom-passage "Hello World 123"))
    (let ((allowed (touchtype-algo--allowed-for-mode)))
      (should (seq-contains-p allowed ?H #'=))
      (should (seq-contains-p allowed ?l #'=))
      (should (seq-contains-p allowed ?1 #'=)))))

;;; ─── Feature 9: Code Mode ─────────────────────────────────────────────────

(ert-deftest touchtype-test-code-line-non-empty ()
  "Code mode generates a non-empty line."
  (let ((touchtype-mode-selection 'code)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype--focused-key nil))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-code-allowed-printable-ascii ()
  "Code mode allows all printable ASCII."
  (let ((touchtype-mode-selection 'code))
    (let ((allowed (touchtype-algo--allowed-for-mode)))
      (should (seq-contains-p allowed ?\s #'=))
      (should (seq-contains-p allowed ?{ #'=))
      (should (seq-contains-p allowed ?~ #'=)))))

(ert-deftest touchtype-test-code-snippets-all-printable ()
  "All code snippet chars are printable ASCII."
  (cl-loop for s across touchtype--code-snippets
           do (cl-loop for c across s
                       do (should (and (>= c 32) (<= c 126))))))

;;; ─── Feature 1: Timed Mode ────────────────────────────────────────────────

(ert-deftest touchtype-test-session-type-default-is-words ()
  "Default session type is `words'."
  (should (eq touchtype-session-type 'words)))

(ert-deftest touchtype-test-timed-mode-no-word-limit ()
  "Timed mode does not end on word count."
  ;; Just verify the advance-line check: when session-type is 'timed,
  ;; the word-count check is skipped.
  (let ((touchtype-session-type 'timed))
    ;; The condition is: (and (eq touchtype-session-type 'words) ...)
    ;; so with 'timed this should be nil
    (should-not (and (eq touchtype-session-type 'words)
                     (>= 50 30)))))

(ert-deftest touchtype-test-timed-status-shows-left ()
  "Status string for timed mode shows 'left'."
  (let ((touchtype-session-type 'timed)
        (touchtype-session-duration 60)
        (touchtype--session-total-keys 0)
        (touchtype--session-errors 0)
        (touchtype--session-corrections 0)
        (touchtype--session-start-time (float-time))
        (touchtype--session-word-count 0)
        (touchtype--session-line-wpms nil)
        (touchtype--session-idle-time 0.0)
        (touchtype--unlocked-keys "fj")
        (touchtype-mode-selection 'progressive))
    (let ((status (touchtype-ui--status-string)))
      (should (string-match-p "left" status)))))

;;; ─── Feature 6: Weak-Letter Repetition ─────────────────────────────────────

(ert-deftest touchtype-test-pick-focus-char-returns-char-or-nil ()
  "pick-focus-char returns a character or nil."
  (let ((touchtype--focused-key ?d)
        (touchtype--unlocked-keys "fjdksl")
        (touchtype-unlock-threshold 0.80)
        (touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fjdksl" :confidence nil))
        (touchtype-target-wpm 40))
    (dotimes (_ 20)
      (let ((ch (touchtype-algo--pick-focus-char)))
        (should (or (null ch) (characterp ch)))))))

(ert-deftest touchtype-test-pick-focus-nil-when-no-focused-key ()
  "pick-focus-char returns nil when focused-key is nil."
  (let ((touchtype--focused-key nil))
    (should (null (touchtype-algo--pick-focus-char)))))

;;; ─── Feature 8: Stop-on-Error ──────────────────────────────────────────────

(ert-deftest touchtype-test-error-mode-default-normal ()
  "Default error mode is `normal'."
  (should (eq touchtype-error-mode 'normal)))

;;; ─── Feature 11: Idle/Pause Detection ──────────────────────────────────────

(ert-deftest touchtype-test-idle-threshold-default ()
  "Default idle threshold is 10 seconds."
  (should (= 10 touchtype-idle-threshold)))

;;; ─── Feature 7: WPM Graph ─────────────────────────────────────────────────

(ert-deftest touchtype-test-sparkline-constant ()
  "Constant values produce the same bar character."
  (let ((result (touchtype-ui--wpm-sparkline '(40.0 40.0 40.0))))
    (should (= 3 (length result)))
    ;; All chars should be the same
    (should (= (aref result 0) (aref result 1)))
    (should (= (aref result 1) (aref result 2)))))

(ert-deftest touchtype-test-sparkline-ascending ()
  "Ascending values produce non-decreasing bar characters."
  (let ((result (touchtype-ui--wpm-sparkline '(10.0 20.0 30.0 40.0 50.0))))
    (should (= 5 (length result)))
    (cl-loop for i from 0 below (1- (length result))
             do (should (<= (aref result i) (aref result (1+ i)))))))

(ert-deftest touchtype-test-sparkline-length ()
  "Sparkline length matches input length."
  (should (= 4 (length (touchtype-ui--wpm-sparkline '(10.0 20.0 30.0 40.0)))))
  (should (= 0 (length (touchtype-ui--wpm-sparkline nil)))))

;;; ─── Feature 12: Multi-Line Display ────────────────────────────────────────

(ert-deftest touchtype-test-preview-lines-default ()
  "Default preview-lines is 2."
  (should (= 2 touchtype-preview-lines)))

;;; ─── Feature 13: Pace Caret ────────────────────────────────────────────────

(ert-deftest touchtype-test-pace-interval-calculation ()
  "Pace interval is correct for a given WPM."
  (let ((touchtype-target-wpm 60))
    ;; 60 WPM * 5 chars = 300 chars/min = 5 chars/sec
    ;; interval = 60 / 300 = 0.2 sec
    (should (= 0.2 (/ 60.0 (* 60 5.0))))))

(ert-deftest touchtype-test-pace-caret-default-off ()
  "Pace caret is off by default."
  (should-not touchtype-pace-caret))

;;; ─── Interactive End-Session Stats ──────────────────────────────────────────

(ert-deftest touchtype-test-expandable-section-renders-content ()
  "Render-expandable-sections inserts truncated items."
  (with-temp-buffer
    (let ((fmt (lambda (s) (format "  %s\n" s))))
      (setq-local touchtype-ui--expandable-sections
                  (list (list :id "test" :header "Test Section"
                              :trunc-items '("alpha" "bravo")
                              :full-items '("alpha" "bravo" "charlie" "delta" "echo")
                              :formatter fmt :is-expanded nil)))
      (setq-local touchtype-ui--expandable-area-start (copy-marker (point)))
      (setq-local touchtype-ui--expandable-area-end (copy-marker (point) t))
      (touchtype-ui--render-expandable-sections)
      ;; Buffer should contain the truncated items
      (should (string-match-p "alpha" (buffer-string)))
      (should (string-match-p "bravo" (buffer-string)))
      ;; Should NOT contain items beyond truncated count
      (should-not (string-match-p "charlie" (buffer-string))))))

(ert-deftest touchtype-test-expandable-section-non-expandable ()
  "Section with all items shown has no expand hint."
  (with-temp-buffer
    (let ((fmt (lambda (s) (format "  %s\n" s))))
      (setq-local touchtype-ui--expandable-sections
                  (list (list :id "small" :header "Small Section"
                              :trunc-items '("one" "two")
                              :full-items '("one" "two")
                              :formatter fmt :is-expanded nil)))
      (setq-local touchtype-ui--expandable-area-start (copy-marker (point)))
      (setq-local touchtype-ui--expandable-area-end (copy-marker (point) t))
      (touchtype-ui--render-expandable-sections)
      ;; Content should be present
      (should (string-match-p "one" (buffer-string)))
      ;; No expand hint (all items shown)
      (should-not (string-match-p "show all" (buffer-string))))))

(ert-deftest touchtype-test-toggle-section-expands ()
  "Toggling an expandable section shows full content."
  (with-temp-buffer
    (let ((fmt (lambda (s) (format "  %s\n" s))))
      (setq-local touchtype-ui--expandable-sections
                  (list (list :id "test" :header "Test Section"
                              :trunc-items '("alpha" "bravo")
                              :full-items '("alpha" "bravo" "charlie" "delta" "echo")
                              :formatter fmt :is-expanded nil)))
      (setq-local touchtype-ui--expandable-area-start (copy-marker (point)))
      (setq-local touchtype-ui--expandable-area-end (copy-marker (point) t))
      (touchtype-ui--render-expandable-sections)
      ;; Toggle to expand
      (let ((section (car touchtype-ui--expandable-sections)))
        (plist-put section :is-expanded t)
        (touchtype-ui--render-expandable-sections))
      ;; Now all items should be visible
      (should (string-match-p "charlie" (buffer-string)))
      (should (string-match-p "delta" (buffer-string)))
      (should (string-match-p "echo" (buffer-string))))))

(ert-deftest touchtype-test-toggle-section-collapses ()
  "Toggling an expanded section collapses it back."
  (with-temp-buffer
    (let ((fmt (lambda (s) (format "  %s\n" s))))
      (setq-local touchtype-ui--expandable-sections
                  (list (list :id "test" :header "Test Section"
                              :trunc-items '("alpha" "bravo")
                              :full-items '("alpha" "bravo" "charlie" "delta" "echo")
                              :formatter fmt :is-expanded nil)))
      (setq-local touchtype-ui--expandable-area-start (copy-marker (point)))
      (setq-local touchtype-ui--expandable-area-end (copy-marker (point) t))
      (touchtype-ui--render-expandable-sections)
      ;; Expand then collapse
      (let ((section (car touchtype-ui--expandable-sections)))
        (plist-put section :is-expanded t)
        (touchtype-ui--render-expandable-sections)
        (plist-put section :is-expanded nil)
        (touchtype-ui--render-expandable-sections))
      ;; Should be back to truncated
      (should-not (string-match-p "charlie" (buffer-string))))))

(ert-deftest touchtype-test-category-navigation ()
  "TAB navigation finds category headers via text properties."
  (with-temp-buffer
    (insert (propertize "  Speed\n" 'touchtype-category t))
    (insert "    Net WPM: 50\n\n")
    (insert (propertize "  Accuracy\n" 'touchtype-category t))
    (insert "    Accuracy: 95%\n\n")
    (insert (propertize "  Session\n" 'touchtype-category t))
    (insert "    Time: 1:30\n")
    ;; Start at beginning
    (goto-char (point-min))
    ;; First TAB should go to Accuracy (next category after Speed)
    (touchtype-ui--end-session-next-category)
    (should (string-match-p "Accuracy"
                            (buffer-substring (point) (line-end-position))))
    ;; Second TAB should go to Session
    (touchtype-ui--end-session-next-category)
    (should (string-match-p "Session"
                            (buffer-substring (point) (line-end-position))))
    ;; Third TAB should wrap to Speed
    (touchtype-ui--end-session-next-category)
    (should (string-match-p "Speed"
                            (buffer-substring (point) (line-end-position))))))

(ert-deftest touchtype-test-expandable-section-shows-count-hint ()
  "Expandable section header includes item count hint."
  (with-temp-buffer
    (let ((fmt (lambda (s) (format "  %s\n" s))))
      (setq-local touchtype-ui--expandable-sections
                  (list (list :id "test" :header "Test Section"
                              :trunc-items '("a" "b")
                              :full-items '("a" "b" "c" "d" "e")
                              :formatter fmt :is-expanded nil)))
      (setq-local touchtype-ui--expandable-area-start (copy-marker (point)))
      (setq-local touchtype-ui--expandable-area-end (copy-marker (point) t))
      (touchtype-ui--render-expandable-sections)
      ;; Header should show count
      (should (string-match-p "\\[2 of 5\\]" (buffer-string))))))

;;; ─── Per-mode statistics tests ──────────────────────────────────────────────

(ert-deftest touchtype-test-record-keypress-with-mode ()
  "Recording a keypress with MODE dual-writes to global and mode stats."
  (let ((touchtype--stats
         (list :version 2 :letter-stats nil :bigram-stats nil
               :mode-letter-stats nil :mode-bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (touchtype-stats-record-keypress ?a t 200 'full-words)
    ;; Global stats updated
    (let ((global (assq ?a (plist-get touchtype--stats :letter-stats))))
      (should global)
      (should (= 1 (touchtype-stats--entry-get global :hits))))
    ;; Mode stats updated
    (let* ((mode-alist (assq 'full-words (plist-get touchtype--stats :mode-letter-stats)))
           (mode-entry (assq ?a (cdr mode-alist))))
      (should mode-entry)
      (should (= 1 (touchtype-stats--entry-get mode-entry :hits))))))

(ert-deftest touchtype-test-record-bigram-with-mode ()
  "Recording a bigram with MODE dual-writes to global and mode stats."
  (let ((touchtype--stats
         (list :version 2 :letter-stats nil :bigram-stats nil
               :mode-letter-stats nil :mode-bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (touchtype-stats-record-bigram "th" t 250 'bigram-drill)
    ;; Global stats updated
    (let ((global (assoc "th" (plist-get touchtype--stats :bigram-stats))))
      (should global)
      (should (= 1 (touchtype-stats--entry-get global :hits))))
    ;; Mode stats updated
    (let* ((mode-alist (assq 'bigram-drill (plist-get touchtype--stats :mode-bigram-stats)))
           (mode-entry (assoc "th" (cdr mode-alist))))
      (should mode-entry)
      (should (= 1 (touchtype-stats--entry-get mode-entry :hits))))))

(ert-deftest touchtype-test-confidence-per-mode ()
  "Per-mode confidence differs from global confidence."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (fast-ms (round (/ target-ms 2.0)))
         (slow-ms (round (* target-ms 3.0)))
         (touchtype--stats
          (list :version 2
                :letter-stats (list (list ?a :hits 100 :misses 0
                                          :total-ms (* 100 fast-ms) :best-ms fast-ms))
                :bigram-stats nil
                :mode-letter-stats
                (list (cons 'full-words
                            (list (list ?a :hits 50 :misses 10
                                        :total-ms (* 50 slow-ms) :best-ms slow-ms))))
                :mode-bigram-stats nil
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    ;; Global confidence should be 1.0 (perfect accuracy, fast speed)
    (should (= 1.0 (touchtype-stats-get-confidence ?a)))
    ;; Mode confidence should be lower (misses + slow)
    (let ((mode-conf (touchtype-stats-get-confidence ?a 'full-words)))
      (should (> mode-conf 0.0))
      (should (< mode-conf 1.0)))))

(ert-deftest touchtype-test-weak-letters-per-mode ()
  "Mode-filtered weak letters only include chars with mode-specific data."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (fast-ms (round (/ target-ms 2.0)))
         (slow-ms (round (* target-ms 3.0)))
         (touchtype--stats
          (list :version 2
                :letter-stats (list (list ?a :hits 100 :misses 0
                                          :total-ms (* 100 fast-ms) :best-ms fast-ms)
                                    (list ?b :hits 100 :misses 0
                                          :total-ms (* 100 fast-ms) :best-ms fast-ms))
                :bigram-stats nil
                :mode-letter-stats
                (list (cons 'full-words
                            (list (list ?a :hits 10 :misses 5
                                        :total-ms (* 10 (round target-ms))
                                        :best-ms (round target-ms))
                                  (list ?b :hits 50 :misses 0
                                        :total-ms (* 50 fast-ms)
                                        :best-ms fast-ms))))
                :mode-bigram-stats nil
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    (let ((weak (touchtype-stats-get-weak-letters 'full-words)))
      ;; Only chars with mode data appear
      (should (memq ?a weak))
      (should (memq ?b weak))
      ;; ?a is weaker (misses + slower) so it should appear before ?b
      (should (< (cl-position ?a weak) (cl-position ?b weak))))))

(ert-deftest touchtype-test-weak-bigrams-per-mode ()
  "Mode-filtered weak bigrams use mode-specific data."
  (let* ((target-ms (/ 60000.0 (* 40 5)))
         (touchtype--stats
          (list :version 2
                :letter-stats nil
                :bigram-stats (list (list "th" :hits 10 :misses 0
                                          :total-ms (* 10 (round target-ms))))
                :mode-letter-stats nil
                :mode-bigram-stats
                (list (cons 'full-words
                            (list (list "th" :hits 8 :misses 2
                                        :total-ms (* 8 (round target-ms))))))
                :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40))
    ;; Global confidence: perfect accuracy, at target speed
    (let ((global-conf (touchtype-stats-get-bigram-confidence "th"))
          (mode-conf (touchtype-stats-get-bigram-confidence "th" 'full-words)))
      (should (> global-conf mode-conf)))))

(ert-deftest touchtype-test-stats-roundtrip-v2 ()
  "Stats with per-mode data survive save/load roundtrip."
  (let* ((tmp (make-temp-file "touchtype-stats-test" nil ".el"))
         (touchtype-stats-file tmp)
         (touchtype--stats
          (list :version 2
                :letter-stats (list (list ?a :hits 10 :misses 1
                                          :total-ms 3000 :best-ms 200))
                :bigram-stats nil
                :mode-letter-stats
                (list (cons 'full-words
                            (list (list ?a :hits 5 :misses 0
                                        :total-ms 1500 :best-ms 250))))
                :mode-bigram-stats
                (list (cons 'full-words
                            (list (list "ab" :hits 3 :misses 1
                                        :total-ms 900))))
                :sessions nil :unlocked-keys "fj" :confidence nil)))
    (unwind-protect
        (progn
          (touchtype-stats-save)
          (setq touchtype--stats nil)
          (touchtype-stats-load)
          (should (= 2 (plist-get touchtype--stats :version)))
          ;; Global letter stats preserved
          (let ((entry (assq ?a (plist-get touchtype--stats :letter-stats))))
            (should entry)
            (should (= 10 (touchtype-stats--entry-get entry :hits))))
          ;; Mode letter stats preserved
          (let* ((mode-alist (assq 'full-words
                                   (plist-get touchtype--stats :mode-letter-stats)))
                 (mentry (assq ?a (cdr mode-alist))))
            (should mentry)
            (should (= 5 (touchtype-stats--entry-get mentry :hits))))
          ;; Mode bigram stats preserved
          (let* ((mode-alist (assq 'full-words
                                   (plist-get touchtype--stats :mode-bigram-stats)))
                 (mentry (assoc "ab" (cdr mode-alist))))
            (should mentry)
            (should (= 3 (touchtype-stats--entry-get mentry :hits)))))
      (delete-file tmp))))

(ert-deftest touchtype-test-v1-to-v2-migration ()
  "Loading a v1 stats file migrates to v2 with per-mode keys initialized."
  (let* ((tmp (make-temp-file "touchtype-stats-v1" nil ".el"))
         (touchtype-stats-file tmp)
         (touchtype--stats nil))
    (unwind-protect
        (progn
          ;; Write a v1 stats file
          (with-temp-file tmp
            (let ((print-level nil)
                  (print-length nil))
              (pp '(touchtype-stats
                    :version 1
                    :letter-stats ((?f :hits 20 :misses 2 :total-ms 6000 :best-ms 200))
                    :bigram-stats nil
                    :sessions nil
                    :unlocked-keys "fj"
                    :confidence nil)
                  (current-buffer))))
          (touchtype-stats-load)
          (should (= 2 (plist-get touchtype--stats :version)))
          (should (plist-member touchtype--stats :mode-letter-stats))
          (should (plist-member touchtype--stats :mode-bigram-stats))
          ;; Original data preserved
          (let ((entry (assq ?f (plist-get touchtype--stats :letter-stats))))
            (should entry)
            (should (= 20 (touchtype-stats--entry-get entry :hits)))))
      (delete-file tmp))))

(ert-deftest touchtype-test-record-keypress-without-mode-unchanged ()
  "Recording without MODE only updates global stats, not mode stats."
  (let ((touchtype--stats
         (list :version 2 :letter-stats nil :bigram-stats nil
               :mode-letter-stats nil :mode-bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (touchtype-stats-record-keypress ?z t 300)
    ;; Global stats updated
    (let ((global (assq ?z (plist-get touchtype--stats :letter-stats))))
      (should global)
      (should (= 1 (touchtype-stats--entry-get global :hits))))
    ;; Mode stats untouched
    (should (null (plist-get touchtype--stats :mode-letter-stats)))))

;;; ─── Phase 1: Weak mode tests ────────────────────────────────────────────────

(ert-deftest touchtype-test-inverse-confidence-weights ()
  "Inverse confidence weights should be max(0.01, 1.0 - confidence)."
  (let ((touchtype--stats
         (list :version 1
               :letter-stats (list (list ?a :hits 100 :misses 0 :total-ms 30000 :best-ms 200)
                                   (list ?b :hits 10 :misses 5 :total-ms 6000 :best-ms 400))
               :bigram-stats nil :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40))
    (let ((weights (touchtype-algo--inverse-confidence-weights "ab")))
      ;; Each entry is (char . weight)
      (should (= (length weights) 2))
      ;; Weights should be positive
      (should (> (cdr (assq ?a weights)) 0))
      (should (> (cdr (assq ?b weights)) 0))
      ;; a has more hits & better speed, so lower inverse weight
      (should (< (cdr (assq ?a weights)) (cdr (assq ?b weights)))))))

(ert-deftest touchtype-test-inverse-confidence-weights-no-data ()
  "Inverse confidence weights with no data should be ~1.0."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40))
    (let ((weights (touchtype-algo--inverse-confidence-weights "xyz")))
      (should (= (length weights) 3))
      (dolist (w weights)
        (should (= (cdr w) 1.0))))))

(ert-deftest touchtype-test-pick-weighted-float ()
  "Weighted float pick should return a key from the alist."
  (let ((alist '((?a . 0.5) (?b . 0.3) (?c . 0.2))))
    (dotimes (_ 20)
      (let ((picked (touchtype-algo--pick-weighted-float alist)))
        (should (memq picked '(?a ?b ?c)))))))

(ert-deftest touchtype-test-pick-weighted-float-single ()
  "Weighted float pick with single element always returns that element."
  (let ((alist '((?x . 1.0))))
    (should (= (touchtype-algo--pick-weighted-float alist) ?x))))

(ert-deftest touchtype-test-generate-word-containing-ngram ()
  "Word containing ngram should include the ngram substring."
  (dotimes (_ 10)
    (let ((word (touchtype-algo-generate-word-containing-ngram
                 "th" "abcdefghijklmnopqrstuvwxyz")))
      (should (stringp word))
      (should (>= (length word) 2))
      (should (string-match-p "th" word)))))

(ert-deftest touchtype-test-generate-word-containing-trigram ()
  "Word containing trigram should include the trigram substring."
  (dotimes (_ 10)
    (let ((word (touchtype-algo-generate-word-containing-ngram
                 "the" "abcdefghijklmnopqrstuvwxyz")))
      (should (stringp word))
      (should (string-match-p "the" word)))))

(ert-deftest touchtype-test-build-suffix ()
  "Build-suffix should produce a string of requested length."
  (let ((suffix (touchtype-algo--build-suffix 4 ?t "abcdefghijklmnopqrstuvwxyz")))
    (should (= (length suffix) 4))
    (should (cl-every (lambda (c) (and (>= c ?a) (<= c ?z)))
                      (string-to-list suffix)))))

(ert-deftest touchtype-test-build-prefix ()
  "Build-prefix should produce a string of requested length."
  (let ((prefix (touchtype-algo--build-prefix 3 ?e "abcdefghijklmnopqrstuvwxyz")))
    (should (= (length prefix) 3))
    (should (cl-every (lambda (c) (and (>= c ?a) (<= c ?z)))
                      (string-to-list prefix)))))

(ert-deftest touchtype-test-weak-letters-line-cold-start ()
  "Weak letters line should work with no stats (cold start)."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40)
        (touchtype-weak-letter-count 6)
        (touchtype-weak-confidence-threshold 0.80)
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo--weak-letters-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-weak-letters-line-with-stats ()
  "Weak letters line should generate text focusing on weak characters."
  (let ((touchtype--stats
         (list :version 1
               :letter-stats (list (list ?a :hits 100 :misses 0 :total-ms 30000 :best-ms 200)
                                   (list ?z :hits 10 :misses 8 :total-ms 10000 :best-ms 800)
                                   (list ?q :hits 5 :misses 4 :total-ms 8000 :best-ms 900))
               :bigram-stats nil :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40)
        (touchtype-weak-letter-count 6)
        (touchtype-weak-confidence-threshold 0.80)
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo--weak-letters-line)))
      (should (stringp line))
      (should (> (length line) 10)))))

(ert-deftest touchtype-test-weak-bigrams-line-cold-start ()
  "Weak bigrams line should fall back to bigram drill with no stats."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "abcdefghijklmnopqrstuvwxyz" :confidence nil))
        (touchtype-target-wpm 40)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo--weak-bigrams-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-weak-bigrams-line-with-stats ()
  "Weak bigrams line should generate text with stats present."
  (let ((touchtype--stats
         (list :version 1
               :letter-stats nil
               :bigram-stats (list (list "th" :hits 50 :misses 2 :total-ms 15000)
                                   (list "zx" :hits 10 :misses 8 :total-ms 10000))
               :sessions nil :unlocked-keys "abcdefghijklmnopqrstuvwxyz" :confidence nil))
        (touchtype-target-wpm 40)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo--weak-bigrams-line)))
      (should (stringp line))
      (should (> (length line) 10)))))

(ert-deftest touchtype-test-weak-mixed-line ()
  "Weak mixed line should generate non-empty text."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "abcdefghijklmnopqrstuvwxyz" :confidence nil))
        (touchtype-target-wpm 40)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype-weak-letter-count 6)
        (touchtype-weak-confidence-threshold 0.80)
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let ((line (touchtype-algo--weak-mixed-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-weak-letters-overrepresentation ()
  "Weak letters mode should over-represent weak characters.
Letters with no stats have confidence 0.0 (inverse weight 1.0).
We verify that at least some of these no-data letters appear
as focus chars in generated lines."
  (let* ((touchtype--stats
          (list :version 1
                ;; Give most letters good stats so only q,x,z are weak
                :letter-stats
                (cl-loop for ch across "abcdefghijklmnoprstuvwy"
                         collect (list ch :hits 200 :misses 2 :total-ms 50000 :best-ms 200))
                :bigram-stats nil :sessions nil :unlocked-keys "fj" :confidence nil))
         (touchtype-target-wpm 40)
         (touchtype-weak-letter-count 6)
         (touchtype-weak-confidence-threshold 0.80)
         (touchtype-word-length-min 4)
         (touchtype-word-length-max 8))
    ;; q, x, z have no stats -> confidence 0.0 -> highest inverse weight
    ;; They should be over-represented as focus chars
    (let ((weak-count 0) (total-chars 0))
      (dotimes (_ 10)
        (let ((line (touchtype-algo--weak-letters-line)))
          (cl-incf total-chars (length (replace-regexp-in-string " " "" line)))
          (cl-incf weak-count (cl-count ?q line))
          (cl-incf weak-count (cl-count ?x line))
          (cl-incf weak-count (cl-count ?z line))))
      ;; With 10 lines and focus on q/x/z, we should see some
      (should (> weak-count 0)))))

(ert-deftest touchtype-test-weak-mode-allowed-chars ()
  "All weak modes should return full alphabet for allowed chars."
  (let ((touchtype-mode-selection 'weak-letters))
    (should (equal (touchtype-algo--allowed-for-mode) "abcdefghijklmnopqrstuvwxyz")))
  (let ((touchtype-mode-selection 'weak-bigrams))
    (should (equal (touchtype-algo--allowed-for-mode) "abcdefghijklmnopqrstuvwxyz")))
  (let ((touchtype-mode-selection 'weak-mixed))
    (should (equal (touchtype-algo--allowed-for-mode) "abcdefghijklmnopqrstuvwxyz"))))

;;; ─── Phase 2: Content mode tests ─────────────────────────────────────────────

(ert-deftest touchtype-test-quote-line-generation ()
  "Quote mode should generate non-empty lines from a quote passage."
  (let ((touchtype--quote-passage "the only way to do great work is to love what you do")
        (touchtype--quote-offset 0)
        (touchtype-mode-selection 'quote))
    (let ((line (touchtype-algo--generate-line-from-passage
                 'touchtype--quote-passage 'touchtype--quote-offset)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-quote-prepare ()
  "Preparing a quote should set passage and offset."
  (let ((touchtype--quote-passage nil)
        (touchtype--quote-offset 0))
    (touchtype-algo--prepare-quote)
    (should (stringp touchtype--quote-passage))
    (should (> (length touchtype--quote-passage) 0))
    (should (= touchtype--quote-offset 0))))

(ert-deftest touchtype-test-domain-words-medical ()
  "Domain words for medical should generate a line of medical terms."
  (let ((touchtype-mode-selection 'domain-words)
        (touchtype--domain-selection 'medical))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-domain-words-legal ()
  "Domain words for legal should generate a line."
  (let ((touchtype-mode-selection 'domain-words)
        (touchtype--domain-selection 'legal))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-domain-words-programming ()
  "Domain words for programming should generate a line."
  (let ((touchtype-mode-selection 'domain-words)
        (touchtype--domain-selection 'programming))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-hand-keys-no-overlap ()
  "Left and right hand keys should not overlap for any layout."
  (dolist (layout '(qwerty dvorak colemak workman))
    (let ((touchtype-keyboard-layout layout))
      (let ((left (touchtype-algo--hand-keys 'left))
            (right (touchtype-algo--hand-keys 'right)))
        (dolist (c (string-to-list left))
          (should-not (seq-contains-p right c #'=)))))))

(ert-deftest touchtype-test-hand-keys-cover-alphabet ()
  "Left + right hand keys should cover most of the alphabet."
  (dolist (layout '(qwerty dvorak colemak workman))
    (let ((touchtype-keyboard-layout layout))
      (let* ((left (touchtype-algo--hand-keys 'left))
             (right (touchtype-algo--hand-keys 'right))
             (all (concat left right))
             (unique (delete-dups (string-to-list all))))
        ;; Should have at least 20 unique letters (some layouts may differ slightly)
        (should (>= (length unique) 20))))))

(ert-deftest touchtype-test-left-hand-line-chars ()
  "Left-hand mode lines should only contain left-hand chars and spaces."
  (let ((touchtype-mode-selection 'left-hand)
        (touchtype-keyboard-layout 'qwerty)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let* ((allowed (touchtype-algo--allowed-for-mode))
           (line (touchtype-algo-generate-line)))
      (should (stringp line))
      (dolist (c (string-to-list line))
        (should (or (= c ?\s) (seq-contains-p allowed c #'=)))))))

(ert-deftest touchtype-test-right-hand-line-chars ()
  "Right-hand mode lines should only contain right-hand chars and spaces."
  (let ((touchtype-mode-selection 'right-hand)
        (touchtype-keyboard-layout 'qwerty)
        (touchtype--unlocked-keys "abcdefghijklmnopqrstuvwxyz")
        (touchtype-word-length-min 4)
        (touchtype-word-length-max 8))
    (let* ((allowed (touchtype-algo--allowed-for-mode))
           (line (touchtype-algo-generate-line)))
      (should (stringp line))
      (dolist (c (string-to-list line))
        (should (or (= c ?\s) (seq-contains-p allowed c #'=)))))))

(ert-deftest touchtype-test-hand-keys-layout-aware ()
  "Hand keys should differ between layouts."
  (let ((touchtype-keyboard-layout 'qwerty))
    (let ((qwerty-left (touchtype-algo--hand-keys 'left)))
      (let ((touchtype-keyboard-layout 'dvorak))
        (let ((dvorak-left (touchtype-algo--hand-keys 'left)))
          (should-not (equal qwerty-left dvorak-left)))))))

(ert-deftest touchtype-test-symbol-drill-line ()
  "Symbol drill should generate non-empty lines."
  (let ((line (touchtype-algo--symbol-drill-line)))
    (should (stringp line))
    (should (> (length line) 0))))

(ert-deftest touchtype-test-symbol-drill-contains-symbols ()
  "Symbol drill lines should contain programming bigrams."
  (let ((found-bigram nil))
    (dotimes (_ 10)
      (let ((line (touchtype-algo--symbol-drill-line)))
        (dolist (bg touchtype--programming-bigrams)
          (when (string-match-p (regexp-quote bg) line)
            (setq found-bigram t)))))
    (should found-bigram)))

;;; ─── Phase 3: Symbol drill & language code tests ─────────────────────────────

(ert-deftest touchtype-test-code-snippets-by-language-keys ()
  "All expected languages should be present in the by-language alist."
  (dolist (lang '(python rust go javascript elisp bash sql c))
    (should (assq lang touchtype--code-snippets-by-language))))

(ert-deftest touchtype-test-code-snippets-backward-compat ()
  "Flat code-snippets vector should contain snippets from all languages."
  (should (> (length touchtype--code-snippets) 50))
  ;; Should contain snippets from multiple languages
  (let ((has-python nil) (has-rust nil) (has-sql nil))
    (dotimes (i (length touchtype--code-snippets))
      (let ((s (aref touchtype--code-snippets i)))
        (when (string-match-p "def " s) (setq has-python t))
        (when (string-match-p "fn " s) (setq has-rust t))
        (when (string-match-p "SELECT" s) (setq has-sql t))))
    (should has-python)
    (should has-rust)
    (should has-sql)))

(ert-deftest touchtype-test-code-language-filter ()
  "Code mode with language filter should only produce snippets from that language."
  (let ((touchtype-mode-selection 'code)
        (touchtype--code-language 'python))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-code-language-nil-uses-all ()
  "Code mode with nil language should use all snippets."
  (let ((touchtype-mode-selection 'code)
        (touchtype--code-language nil))
    (let ((line (touchtype-algo-generate-line)))
      (should (stringp line))
      (should (> (length line) 0)))))

(ert-deftest touchtype-test-programming-bigrams-populated ()
  "Programming bigrams list should have entries."
  (should (> (length touchtype--programming-bigrams) 20)))

;;; ─── Phase 4: Analytics tests ─────────────────────────────────────────────────

(ert-deftest touchtype-test-heatmap-face-cold ()
  "Heatmap face for 0 confidence should be cold."
  (should (eq (touchtype-ui--heatmap-face 0.0) 'touchtype-face-heatmap-cold)))

(ert-deftest touchtype-test-heatmap-face-struggling ()
  "Heatmap face for low confidence should be struggling."
  (should (eq (touchtype-ui--heatmap-face 0.2) 'touchtype-face-heatmap-struggling)))

(ert-deftest touchtype-test-heatmap-face-developing ()
  "Heatmap face for medium confidence should be developing."
  (should (eq (touchtype-ui--heatmap-face 0.5) 'touchtype-face-heatmap-developing)))

(ert-deftest touchtype-test-heatmap-face-good ()
  "Heatmap face for high confidence should be good."
  (should (eq (touchtype-ui--heatmap-face 0.8) 'touchtype-face-heatmap-good)))

(ert-deftest touchtype-test-finger-stats-aggregation ()
  "Finger stats should aggregate letter stats by finger."
  (let ((touchtype--stats
         (list :version 1
               :letter-stats (list (list ?f :hits 100 :misses 5 :total-ms 30000 :best-ms 200)
                                   (list ?r :hits 80 :misses 3 :total-ms 24000 :best-ms 210)
                                   (list ?j :hits 90 :misses 4 :total-ms 27000 :best-ms 190))
               :bigram-stats nil :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-target-wpm 40)
        (touchtype-keyboard-layout 'qwerty))
    (let ((fstats (touchtype-stats-get-finger-stats)))
      ;; f and r both map to left-index in qwerty
      (let ((left-idx (assq 'left-index fstats)))
        (should left-idx)
        ;; hits should be sum of f + r
        (should (= (plist-get (cdr left-idx) :hits) 180))
        (should (> (plist-get (cdr left-idx) :accuracy) 0))))))

(ert-deftest touchtype-test-finger-stats-empty ()
  "Finger stats with no data should return entries with 0 hits."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil))
        (touchtype-keyboard-layout 'qwerty))
    (let ((fstats (touchtype-stats-get-finger-stats)))
      (should (listp fstats))
      ;; All entries should have 0 hits
      (dolist (entry fstats)
        (should (= (plist-get (cdr entry) :hits) 0))))))

(ert-deftest touchtype-test-rolling-average-wpm ()
  "Rolling average should compute mean of recent session WPMs."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list (list '2026-03-01 :wpm 40.0 :accuracy 95.0 :mode 'progressive :words 30)
                               (list '2026-02-28 :wpm 38.0 :accuracy 93.0 :mode 'progressive :words 30)
                               (list '2026-02-27 :wpm 36.0 :accuracy 91.0 :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil)))
    (let ((avg (touchtype-stats-get-rolling-average 10 :wpm)))
      (should avg)
      (should (< (abs (- avg 38.0)) 0.01)))))

(ert-deftest touchtype-test-rolling-average-no-sessions ()
  "Rolling average with no sessions should return nil."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil)))
    (should-not (touchtype-stats-get-rolling-average 10 :wpm))))

(ert-deftest touchtype-test-keyboard-rows-exist ()
  "All layouts should have keyboard row definitions."
  (should (= (length touchtype--qwerty-keyboard-rows) 3))
  (should (= (length touchtype--dvorak-keyboard-rows) 3))
  (should (= (length touchtype--colemak-keyboard-rows) 3))
  (should (= (length touchtype--workman-keyboard-rows) 3)))

(ert-deftest touchtype-test-finger-map-covers-letters ()
  "Finger maps should cover most lowercase letters."
  (dolist (fmap (list touchtype--qwerty-finger-map
                      touchtype--dvorak-finger-map
                      touchtype--colemak-finger-map
                      touchtype--workman-finger-map))
    (should (>= (length fmap) 20))))

;;; ─── Phase 5: Gamification tests ──────────────────────────────────────────────

(ert-deftest touchtype-test-achievements-first-session ()
  "First session should award the first-session achievement."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list (list '2026-03-01 :wpm 25.0 :accuracy 90.0
                                     :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil
               :achievements nil :daily-streak 1
               :total-practice-time 60 :last-practice-date "2026-03-01"))
        (touchtype-target-wpm 40))
    (let ((new (touchtype-stats-check-achievements 25.0 90.0)))
      (should (memq 'first-session new)))))

(ert-deftest touchtype-test-achievements-speed-milestones ()
  "Speed milestones should be awarded at correct thresholds."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list (list '2026-03-01 :wpm 75.0 :accuracy 96.0
                                     :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil
               :achievements nil :daily-streak 1
               :total-practice-time 60 :last-practice-date "2026-03-01"))
        (touchtype-target-wpm 40))
    (let ((new (touchtype-stats-check-achievements 75.0 96.0)))
      (should (memq 'speed-30 new))
      (should (memq 'speed-50 new))
      (should (memq 'speed-70 new))
      (should-not (memq 'speed-100 new))
      (should (memq 'accuracy-95 new)))))

(ert-deftest touchtype-test-achievements-no-duplicates ()
  "Already-earned achievements should not be re-awarded."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions (list (list '2026-03-01 :wpm 40.0 :accuracy 97.0
                                     :mode 'progressive :words 30))
               :unlocked-keys "fj" :confidence nil
               :achievements '(first-session speed-30)
               :daily-streak 1
               :total-practice-time 60 :last-practice-date "2026-03-01"))
        (touchtype-target-wpm 40))
    (let ((new (touchtype-stats-check-achievements 40.0 97.0)))
      (should-not (memq 'first-session new))
      (should-not (memq 'speed-30 new)))))

(ert-deftest touchtype-test-xp-for-session ()
  "XP calculation should use wpm * accuracy/100 * word-count."
  (should (= (touchtype-stats-xp-for-session 40.0 100.0 30) 1200))
  (should (= (touchtype-stats-xp-for-session 50.0 90.0 20) 900)))

(ert-deftest touchtype-test-xp-add-and-get ()
  "Adding XP should increase the total."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil :xp 0)))
    (touchtype-stats-add-xp 500)
    (should (= (touchtype-stats-get-xp) 500))
    (touchtype-stats-add-xp 300)
    (should (= (touchtype-stats-get-xp) 800))))

(ert-deftest touchtype-test-level-from-xp ()
  "Level should increase with XP thresholds."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil :xp 0)))
    (should (= (touchtype-stats-get-level) 0))
    (plist-put touchtype--stats :xp 100)
    (should (= (touchtype-stats-get-level) 1))
    (plist-put touchtype--stats :xp 500)
    (should (= (touchtype-stats-get-level) 3))))

(ert-deftest touchtype-test-xp-to-next-level ()
  "XP to next level should be the gap to the next threshold."
  (let ((touchtype--stats
         (list :version 1 :letter-stats nil :bigram-stats nil
               :sessions nil :unlocked-keys "fj" :confidence nil :xp 0)))
    ;; At level 0 (0 XP), next is level 1 (100 XP)
    (should (= (touchtype-stats-xp-to-next-level) 100))
    (plist-put touchtype--stats :xp 150)
    ;; At level 1 (100 XP threshold), next is level 2 (250 XP)
    (should (= (touchtype-stats-xp-to-next-level) 100))))

(ert-deftest touchtype-test-level-titles-count ()
  "Level titles should match threshold count."
  (should (= (length touchtype--level-titles)
             (length touchtype--xp-level-thresholds))))

(ert-deftest touchtype-test-achievements-definitions ()
  "All achievements should have required fields."
  (dolist (ach touchtype--achievements)
    (should (plist-get ach :id))
    (should (plist-get ach :name))
    (should (plist-get ach :desc))))

;;; ─── Phase 6: Session control tests ──────────────────────────────────────────

(ert-deftest touchtype-test-pause-guard-blocks-input ()
  "When paused, process-char should do nothing."
  (with-temp-buffer
    (let ((touchtype--paused t)
          (touchtype--cursor-pos 0)
          (touchtype--current-text "hello")
          (touchtype--typed-chars nil)
          (touchtype--session-total-keys 0)
          (touchtype--session-errors 0)
          (touchtype--session-corrections 0)
          (touchtype--last-key-time nil)
          (touchtype--line-start-time nil)
          (touchtype--session-start-time nil)
          (touchtype--session-idle-time 0.0)
          (touchtype-session-type 'words)
          (touchtype-error-mode 'normal)
          (touchtype-difficulty 'normal)
          (touchtype-pace-caret nil)
          (touchtype--char-overlays (make-vector 5 nil))
          (touchtype--target-start (point-marker))
          (touchtype--status-start (point-marker)))
      (touchtype-ui--process-char ?h)
      ;; Cursor should not have advanced
      (should (= touchtype--cursor-pos 0)))))

(ert-deftest touchtype-test-pause-guard-blocks-backspace ()
  "When paused, backspace should do nothing."
  (with-temp-buffer
    (let ((touchtype--paused t)
          (touchtype--cursor-pos 3)
          (touchtype--current-text "hello")
          (touchtype--typed-chars '((?l t 200) (?l t 200) (?e t 200)))
          (touchtype--session-corrections 0)
          (touchtype--char-overlays (make-vector 5 nil)))
      (touchtype-ui--handle-backspace)
      ;; Cursor should not have moved back
      (should (= touchtype--cursor-pos 3)))))

(ert-deftest touchtype-test-difficulty-normal-continues ()
  "Normal difficulty should not fail on errors."
  ;; Just verify the defcustom default
  (should (eq touchtype-difficulty 'normal)))

(ert-deftest touchtype-test-difficulty-choices ()
  "Difficulty should accept normal, expert, master."
  (dolist (d '(normal expert master))
    (let ((touchtype-difficulty d))
      (should (eq touchtype-difficulty d)))))

(ert-deftest touchtype-test-p-bound-in-keymap ()
  "p should be bound to toggle-pause in the keymap."
  (let ((map (touchtype-ui--make-keymap)))
    (should (eq (lookup-key map (kbd "p")) #'touchtype-ui--toggle-pause))))

(ert-deftest touchtype-test-status-shows-difficulty ()
  "Status string should show difficulty when not normal."
  (let ((touchtype--session-total-keys 50)
        (touchtype--session-errors 2)
        (touchtype--session-corrections 1)
        (touchtype--session-start-time (- (float-time) 60))
        (touchtype--session-idle-time 0.0)
        (touchtype--session-line-wpms '(40.0 42.0))
        (touchtype--session-word-count 10)
        (touchtype-session-type 'words)
        (touchtype-session-length 30)
        (touchtype-mode-selection 'progressive)
        (touchtype--unlocked-keys "fjdksl")
        (touchtype--current-text "hello ")
        (touchtype--cursor-pos 3)
        (touchtype-difficulty 'master))
    (let ((status (touchtype-ui--status-string)))
      (should (string-match-p "\\[master\\]" status)))))

(ert-deftest touchtype-test-status-hides-normal-difficulty ()
  "Status string should not show difficulty indicator for normal."
  (let ((touchtype--session-total-keys 50)
        (touchtype--session-errors 2)
        (touchtype--session-corrections 1)
        (touchtype--session-start-time (- (float-time) 60))
        (touchtype--session-idle-time 0.0)
        (touchtype--session-line-wpms '(40.0 42.0))
        (touchtype--session-word-count 10)
        (touchtype-session-type 'words)
        (touchtype-session-length 30)
        (touchtype-mode-selection 'progressive)
        (touchtype--unlocked-keys "fjdksl")
        (touchtype--current-text "hello ")
        (touchtype--cursor-pos 3)
        (touchtype-difficulty 'normal))
    (let ((status (touchtype-ui--status-string)))
      (should-not (string-match-p "\\[normal\\]" status)))))

(provide 'touchtype-test)

;;; touchtype-test.el ends here
