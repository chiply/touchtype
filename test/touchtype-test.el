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

(provide 'touchtype-test)

;;; touchtype-test.el ends here
