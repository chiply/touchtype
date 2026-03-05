;;; touchtype-var.el --- Variables and constants for touchtype -*- lexical-binding: t; -*-

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

;; All defcustom, defconst, defface, and defvar declarations for
;; the touchtype package.

;;; Code:

;;;; Customization group

(defgroup touchtype nil
  "Keybr-style progressive touch typing trainer."
  :group 'applications
  :prefix "touchtype-")

;;;; User options

(defcustom touchtype-stats-file
  (expand-file-name "touchtype-stats.el" user-emacs-directory)
  "Path to the file where typing statistics are persisted."
  :type 'file
  :group 'touchtype)

(defcustom touchtype-target-wpm 40
  "Target words per minute used to compute per-key confidence scores."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-unlock-threshold 0.80
  "Confidence score (0.0–1.0) required to unlock the next key."
  :type 'float
  :group 'touchtype)

(defcustom touchtype-progressive-unlock nil
  "When non-nil, additional modes use progressive key unlock.
Applicable modes filter their content to only characters present in
`touchtype--unlocked-keys'.  New keys unlock as confidence grows."
  :type 'boolean
  :group 'touchtype)

(defvaralias 'touchtype-progressive-ngrams 'touchtype-progressive-unlock)

(defcustom touchtype-session-length 30
  "Number of words per training session."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-session-length-presets
  '((short . 15) (medium . 30) (long . 60) (marathon . 120))
  "Alist mapping preset names to word counts for session length.
Used by `touchtype-set-session-length'."
  :type '(alist :key-type symbol :value-type integer)
  :group 'touchtype)

(defcustom touchtype-mode-selection 'progressive
  "Current training mode symbol.
One of: `progressive', `full-words', `bigram-drill', `letters',
`letters+numbers', `letters+numbers+symbols', `common-words',
`custom', or `code'."
  :type '(choice (const :tag "Progressive" progressive)
                 (const :tag "Full Words" full-words)
                 (const :tag "Bigram Drill" bigram-drill)
                 (const :tag "Trigram Drill" trigram-drill)
                 (const :tag "Tetragram Drill" tetragram-drill)
                 (const :tag "N-gram Drill" ngram-drill)
                 (const :tag "Letters" letters)
                 (const :tag "Letters+Numbers" letters+numbers)
                 (const :tag "Letters+Numbers+Symbols" letters+numbers+symbols)
                 (const :tag "Narrative" narrative)
                 (const :tag "Common Words" common-words)
                 (const :tag "Custom Text" custom)
                 (const :tag "Code" code)
                 (const :tag "Weak Letters" weak-letters)
                 (const :tag "Weak N-grams" weak-ngrams)
                 (const :tag "Weak Mixed" weak-mixed)
                 (const :tag "Quote" quote)
                 (const :tag "Domain Words" domain-words)
                 (const :tag "Left Hand" left-hand)
                 (const :tag "Right Hand" right-hand)
                 (const :tag "Symbol Drill" symbol-drill)
                 (const :tag "Weak Words" weak-words)
                 (const :tag "Finger Drill" finger-drill))
  :group 'touchtype)

(defcustom touchtype-text-width 70
  "Target character width for generated practice text.
This controls both the word-wrap width and the centering margins
in the typing buffer."
  :type 'integer
  :group 'touchtype)

(defvaralias 'touchtype-line-length 'touchtype-text-width)

(defcustom touchtype-text-width-step 10
  "Number of columns to grow or shrink per width adjustment."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-stats-history-length 20
  "Number of recent sessions to display in the statistics view."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-word-length-min 4
  "Minimum length of generated pseudo-words."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-word-length-max 8
  "Maximum length of generated pseudo-words."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-keyboard-layout 'qwerty
  "Keyboard layout determining the progressive unlock order."
  :type '(choice (const :tag "QWERTY" qwerty)
                 (const :tag "Dvorak" dvorak)
                 (const :tag "Colemak" colemak)
                 (const :tag "Workman" workman)
                 (const :tag "Custom" custom))
  :group 'touchtype)

(defcustom touchtype-custom-unlock-order nil
  "Custom key unlock order string when `touchtype-keyboard-layout' is `custom'."
  :type '(choice (const nil) string)
  :group 'touchtype)

(defcustom touchtype-common-words-count 0
  "Number of words to sample from in `common-words' mode.
Words are organized by length, so smaller values prefer shorter words.
0 means use the entire word list for a natural mix of lengths."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-session-type 'words
  "Session type: `words' (fixed word count) or `timed' (fixed duration)."
  :type '(choice (const :tag "Words" words)
                 (const :tag "Timed" timed))
  :group 'touchtype)

(defcustom touchtype-session-duration 60
  "Duration in seconds for timed sessions."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-error-mode 'normal
  "Error handling mode.
`normal': errors shown but typing continues.
`letter': must type correct letter before advancing.
`word': must fix errors before crossing word boundaries."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Stop on Letter" letter)
                 (const :tag "Stop on Word" word))
  :group 'touchtype)

(defcustom touchtype-idle-threshold 10
  "Seconds of inactivity before a pause is excluded from WPM calculation."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-preview-lines 2
  "Number of upcoming lines to preview below the active line.
Set to 0 to disable preview."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-pace-caret nil
  "When non-nil, show a pace caret moving at `touchtype-target-wpm'."
  :type 'boolean
  :group 'touchtype)

(defcustom touchtype-weak-letter-count 6
  "Number of weakest letters to focus on in `weak-letters' mode."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-weak-confidence-threshold 0.80
  "Confidence threshold below which a letter/ngram is considered weak."
  :type 'float
  :group 'touchtype)

(defcustom touchtype-confidence-min-samples 20
  "Minimum hits before confidence score is fully trusted.
With fewer hits, confidence is scaled down proportionally."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-xp-multipliers-enabled t
  "When non-nil, apply streak/PB/difficulty/accuracy multipliers to XP."
  :type 'boolean
  :group 'touchtype)

(defcustom touchtype-zen-mode nil
  "When non-nil, always hide the status line during typing sessions.
Stats are still tracked and shown at session end.
For per-session zen mode, use `touchtype-zen' instead."
  :type 'boolean
  :group 'touchtype)

(defcustom touchtype-highlight-mode nil
  "Highlight upcoming text to guide eye movement.
`word': highlight the current word.
`next-word': highlight the next word.
`next-two': highlight the next two words.
nil: no highlighting (default)."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Current word" word)
                 (const :tag "Next word" next-word)
                 (const :tag "Next two words" next-two))
  :group 'touchtype)

(defvar touchtype--zen-active nil
  "Non-nil when the current session was started via `touchtype-zen'.
This is buffer-local and reset each session.")

(defvar touchtype--session-ending nil
  "Non-nil when a timed session has expired but is deferring end.
Used to let quote-mode passages finish before ending the session.
This is buffer-local and reset each session.")

(defvar touchtype--weak-ngrams-cache nil
  "Cached result of `touchtype-stats-get-weak-ngrams' for line generation.
Set once at session start, cleared on session end.  Avoids re-sorting
thousands of bigram entries on every new line.")

(defvar touchtype--highlight-overlays nil
  "List of overlays used for focus/highlight mode.")

(defvar touchtype--session-wpm-timeseries nil
  "List of (ELAPSED-SECONDS . WPM) pairs sampled every second during a session.
Used to render a WPM-over-time graph on the results screen.")

(defvar touchtype--wpm-sample-timer nil
  "Timer that samples WPM once per second during a session.")

(defvar touchtype--valid-words-cache nil
  "Cached list of valid words for the current mode's allowed characters.
Avoids re-filtering `touchtype--builtin-words' on every line generation.")

(defcustom touchtype-stats-progress-length 40
  "Number of recent sessions to display in progress charts."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-confidence-ema-alpha 0.15
  "Smoothing factor for exponential moving average of keystroke times.
Higher values weight recent keystrokes more heavily (0.0-1.0)."
  :type 'float
  :group 'touchtype)

(defcustom touchtype-confidence-use-ema t
  "When non-nil, use EMA instead of all-time average for confidence scoring."
  :type 'boolean
  :group 'touchtype)

(defcustom touchtype-streak-freeze-count 1
  "Maximum number of streak freezes available.
A freeze preserves the streak when a day is missed."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-streak-freeze-recharge-days 7
  "Consecutive practice days needed to recharge one streak freeze."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-graduated-thresholds t
  "When non-nil, use position-based unlock thresholds instead of a flat value."
  :type 'boolean
  :group 'touchtype)

(defcustom touchtype-graduated-threshold-tiers
  '((6 . 0.70) (12 . 0.80) (18 . 0.85) (26 . 0.80))
  "Alist of (MAX-POSITION . THRESHOLD) for graduated unlock.
Keys 1-6: 0.70, 7-12: 0.80, 13-18: 0.85, 19-26: 0.80."
  :type '(alist :key-type integer :value-type float)
  :group 'touchtype)

;;;; Faces

(defface touchtype-face-untyped
  '((t :inherit shadow))
  "Face for characters in the target line not yet typed."
  :group 'touchtype)

(defface touchtype-face-correct
  '((((class color) (background dark))
     :foreground "#4ade80")
    (((class color) (background light))
     :foreground "#16a34a")
    (t :foreground "green"))
  "Face for correctly typed characters."
  :group 'touchtype)

(defface touchtype-face-wrong
  '((((class color) (background dark))
     :foreground "red" :background "#4a1010")
    (((class color) (background light))
     :foreground "red" :background "#ffe0e0")
    (t :foreground "red"))
  "Face for incorrectly typed characters.
Includes a background so mistyped spaces are visible."
  :group 'touchtype)

(defface touchtype-face-cursor
  '((t :underline t :inherit default))
  "Face for the current typing-cursor position."
  :group 'touchtype)

(defface touchtype-face-status
  '((t :inherit mode-line-inactive))
  "Face for the status line at the bottom of the touchtype buffer."
  :group 'touchtype)

(defface touchtype-face-highlight
  '((((class color) (background dark))
     :foreground "#e0e0e0" :weight bold)
    (((class color) (background light))
     :foreground "#1a1a1a" :weight bold)
    (t :weight bold))
  "Face for highlighted upcoming words in focus mode."
  :group 'touchtype)

(defface touchtype-face-pace-caret
  '((((class color) (background dark))
     :background "#333333")
    (((class color) (background light))
     :background "#e0e0e0")
    (t :inverse-video t))
  "Face for the pace caret overlay."
  :group 'touchtype)

;;;; Constants

(defconst touchtype--wpm-percentile-table
  ;; Based on published typing speed distribution data.
  ;; Each entry is (WPM . PERCENTILE).
  ;; Sources: typing speed studies, monkeytype/typeracer aggregated data.
  '((10 . 2) (15 . 5) (20 . 10) (25 . 18) (30 . 28)
    (35 . 38) (40 . 50) (45 . 58) (50 . 66) (55 . 73)
    (60 . 78) (65 . 83) (70 . 87) (75 . 90) (80 . 93)
    (85 . 95) (90 . 96) (95 . 97) (100 . 98) (110 . 99)
    (120 . 99.3) (130 . 99.5) (140 . 99.7) (150 . 99.9))
  "Lookup table mapping WPM to approximate percentile ranking.
Based on aggregated typing speed distribution data from online
typing tests.  Used to show users how they compare to the general
typing population.")

(defconst touchtype--numbers "0123456789"
  "Digit characters available in letters+numbers modes.")

(defconst touchtype--symbols ".,;'!?-"
  "Common punctuation available in letters+numbers+symbols mode.")

(defconst touchtype--bigram-table
  ;; Each entry: (FROM-CHAR . ((TO-CHAR . RELATIVE-FREQUENCY) ...))
  ;; Frequencies are integers 1–100 derived from English corpus analysis.
  '((?a . ((?n . 45) (?t . 35) (?r . 30) (?l . 25) (?s . 22)
           (?c . 18) (?d . 17) (?m . 15) (?y . 12) (?i . 12)
           (?b . 10) (?g . 10) (?k . 8)  (?w . 7)  (?p . 7)
           (?h . 5)  (?v . 5)  (?f . 4)  (?e . 3)  (?u . 3)
           (?o . 3)  (?x . 2)  (?j . 1)  (?z . 1)  (?q . 1)))
   (?b . ((?e . 40) (?l . 28) (?r . 22) (?o . 18) (?u . 15)
           (?a . 13) (?i . 12) (?y . 10) (?s . 5)  (?t . 4)
           (?b . 3)  (?n . 3)  (?d . 2)  (?k . 2)  (?j . 1)))
   (?c . ((?e . 38) (?o . 30) (?a . 22) (?h . 20) (?k . 15)
           (?i . 14) (?r . 12) (?l . 10) (?t . 8)  (?u . 7)
           (?s . 5)  (?y . 3)  (?c . 2)))
   (?d . ((?e . 42) (?i . 30) (?o . 20) (?a . 18) (?u . 12)
           (?s . 10) (?r . 8)  (?l . 7)  (?y . 6)  (?n . 5)
           (?d . 3)  (?w . 3)  (?g . 2)  (?f . 2)))
   (?e . ((?r . 52) (?s . 48) (?n . 42) (?d . 32) (?t . 30)
           (?l . 25) (?a . 22) (?c . 18) (?x . 10) (?m . 10)
           (?p . 9)  (?i . 8)  (?y . 7)  (?w . 6)  (?v . 5)
           (?g . 5)  (?f . 4)  (?b . 3)  (?h . 3)  (?k . 3)
           (?u . 2)  (?o . 2)  (?q . 1)  (?j . 1)  (?z . 1)))
   (?f . ((?o . 38) (?a . 30) (?e . 25) (?i . 20) (?r . 15)
           (?u . 10) (?f . 8)  (?t . 7)  (?l . 5)  (?y . 4)))
   (?g . ((?e . 38) (?h . 30) (?r . 18) (?a . 16) (?o . 14)
           (?i . 12) (?l . 10) (?u . 8)  (?g . 5)  (?s . 5)
           (?n . 4)))
   (?h . ((?e . 58) (?a . 40) (?i . 30) (?o . 20) (?u . 12)
           (?y . 10) (?r . 7)  (?t . 5)  (?s . 3)  (?l . 2)))
   (?i . ((?n . 58) (?s . 42) (?t . 36) (?o . 25) (?r . 20)
           (?c . 16) (?l . 14) (?g . 12) (?d . 10) (?f . 8)
           (?v . 7)  (?e . 5)  (?a . 5)  (?m . 5)  (?k . 3)
           (?z . 1)  (?j . 1)))
   (?j . ((?u . 50) (?o . 28) (?a . 18) (?e . 10) (?i . 8)))
   (?k . ((?e . 45) (?i . 25) (?n . 15) (?s . 12) (?l . 8)
           (?y . 8)  (?a . 5)  (?w . 4)  (?d . 3)  (?b . 2)
           (?c . 2)))
   (?l . ((?e . 45) (?y . 30) (?l . 22) (?a . 20) (?i . 18)
           (?o . 15) (?d . 10) (?s . 8)  (?t . 7)  (?f . 5)
           (?v . 3)  (?k . 3)))
   (?m . ((?e . 40) (?a . 30) (?i . 25) (?o . 20) (?p . 15)
           (?s . 10) (?y . 8)  (?u . 8)  (?b . 5)  (?m . 3)))
   (?n . ((?t . 52) (?g . 40) (?d . 30) (?e . 25) (?s . 20)
           (?i . 16) (?o . 12) (?c . 10) (?a . 8)  (?k . 5)
           (?l . 5)  (?v . 3)))
   (?o . ((?n . 52) (?r . 42) (?f . 38) (?t . 26) (?s . 20)
           (?u . 15) (?m . 14) (?l . 10) (?w . 10) (?d . 10)
           (?e . 7)  (?k . 5)  (?c . 5)  (?v . 5)  (?p . 4)
           (?i . 3)))
   (?p . ((?e . 40) (?r . 28) (?l . 22) (?o . 18) (?a . 15)
           (?t . 12) (?i . 10) (?h . 8)  (?p . 5)  (?s . 5)
           (?u . 4)))
   (?q . ((?u . 95) (?i . 5)))
   (?r . ((?e . 52) (?o . 30) (?i . 25) (?a . 22) (?s . 15)
           (?y . 10) (?d . 10) (?t . 8)  (?n . 8)  (?l . 5)
           (?m . 5)  (?c . 4)))
   (?s . ((?t . 52) (?e . 48) (?s . 25) (?i . 20) (?h . 15)
           (?o . 15) (?a . 12) (?l . 10) (?c . 10) (?w . 7)
           (?p . 5)  (?u . 5)  (?n . 5)  (?k . 3)))
   (?t . ((?h . 62) (?e . 42) (?i . 26) (?o . 24) (?a . 15)
           (?r . 12) (?s . 10) (?y . 10) (?l . 8)  (?u . 7)
           (?w . 5)))
   (?u . ((?s . 40) (?t . 35) (?r . 30) (?l . 25) (?n . 20)
           (?m . 15) (?c . 14) (?e . 10) (?a . 8)  (?g . 8)
           (?p . 5)  (?b . 5)  (?i . 3)  (?d . 3)))
   (?v . ((?e . 72) (?i . 20) (?a . 10) (?o . 6)))
   (?w . ((?a . 42) (?h . 36) (?i . 25) (?e . 20) (?o . 14)
           (?r . 10) (?s . 5)  (?n . 5)))
   (?x . ((?t . 52) (?p . 20) (?a . 15) (?i . 10) (?e . 8)
           (?c . 5)))
   (?y . ((?o . 30) (?s . 26) (?e . 20) (?i . 15) (?n . 10)
           (?a . 10) (?t . 8)  (?l . 7)  (?m . 5)))
   (?z . ((?e . 40) (?i . 22) (?a . 15) (?o . 10) (?z . 5)
           (?y . 5)  (?l . 3))))
  "English bigram transition table.
An alist mapping each character to a weighted list of successor
characters.  Frequencies are relative integers (roughly 1–100)
derived from standard English corpus analysis.  Used by
`touchtype-algo-generate-word' to produce natural-looking
pseudo-words.")

(defconst touchtype--common-bigrams
  '("th" "he" "in" "er" "an" "re" "on" "en" "at" "es"
    "ed" "te" "ti" "or" "st" "ar" "nd" "to" "nt" "is"
    "of" "it" "al" "as" "ha" "hi" "ng" "se" "ou" "le"
    "me" "de" "no" "ne" "ea" "ri" "ro" "li" "co" "ve"
    "el" "ra" "ce" "la" "di" "si" "us" "ta" "lo" "ut"
    "ma" "pe" "ic" "na" "ur" "ni" "ge" "ho" "io" "ac"
    "il" "be" "ca" "ch" "da" "do" "em" "et" "fi" "fo"
    "id" "ig" "im" "ke" "ki" "mo" "oc" "ol" "op" "ow"
    "pa" "pl" "po" "pr" "sa" "sh" "so" "sp" "su" "tr"
    "tu" "un" "up" "wa" "we" "wi" "wo" "ye" "ab" "ad")
  "Ordered list of the 100 most common English bigrams.
Used by `touchtype-algo-bigram-line' for bigram-drill mode.")

(defconst touchtype--common-trigrams
  '("the" "and" "ing" "her" "hat" "his" "tha" "ere" "for" "ent"
    "ion" "ter" "was" "you" "ith" "ver" "all" "wit" "thi" "tio"
    "not" "are" "but" "had" "out" "one" "our" "hou" "rea" "str"
    "ate" "igh" "hen" "ome" "man" "hin" "ons" "ive" "ove" "ine"
    "con" "men" "ght" "est" "sto" "ill" "nte" "ati" "ear" "com"
    "sta" "ted" "res" "int" "pro" "ess" "ave" "per" "nce" "oun"
    "ste" "ort" "ect" "lin" "tur" "eve" "whe" "rom" "ant" "ard"
    "rin" "nal" "ble" "ace" "use" "eri" "ber" "cal" "ore" "din"
    "ran" "nde" "ake" "ide" "ame" "ven" "ree" "min" "kin" "les"
    "ple" "end" "ren" "als" "ity" "age" "ger" "led" "ial" "ous")
  "Ordered list of the 100 most common English trigrams.
Used by `touchtype-algo-ngram-line' for trigram-drill mode.")

(defconst touchtype--common-tetragrams
  '("that" "ther" "with" "tion" "here" "ould" "ight" "have" "hich" "whic"
    "this" "thin" "they" "atio" "ever" "from" "ough" "were" "hing" "ment"
    "them" "ness" "ance" "some" "ally" "over" "ence" "ined" "been" "fore"
    "able" "ated" "ting" "ture" "ical" "ious" "eral" "ving" "lled" "ents"
    "ring" "ling" "ning" "ster" "ered" "ople" "heir" "ount" "east" "irst"
    "nter" "ings" "nder" "ress" "come" "will" "like" "more" "than" "when"
    "each" "know" "just" "only" "take" "into" "year" "back" "also" "work"
    "long" "much" "such" "hand" "high" "keep" "last" "most" "made" "good"
    "give" "very" "need" "tell" "call" "said" "part" "time" "upon" "many"
    "well" "down" "even" "must" "does" "kind" "same" "find" "help" "turn")
  "Ordered list of the 100 most common English tetragrams.
Used by `touchtype-algo-ngram-line' for tetragram-drill mode.")

(defconst touchtype--code-blocks-by-language
  `((python . ["def fibonacci(n):\n    if n <= 1:\n        return n\n    return fibonacci(n - 1) + fibonacci(n - 2)"
               "def greet(name):\n    msg = f\"Hello, {name}!\"\n    print(msg)\n    return msg"
               "class Stack:\n    def __init__(self):\n        self.items = []\n\n    def push(self, x):\n        self.items.append(x)"
               "def read_lines(path):\n    with open(path) as f:\n        for line in f:\n            yield line.strip()"
               "def binary_search(arr, target):\n    lo, hi = 0, len(arr) - 1\n    while lo <= hi:\n        mid = (lo + hi) // 2\n        if arr[mid] == target:\n            return mid\n        elif arr[mid] < target:\n            lo = mid + 1\n        else:\n            hi = mid - 1\n    return -1"
               "try:\n    value = int(input(\"Enter a number: \"))\n    print(f\"You entered {value}\")\nexcept ValueError:\n    print(\"Invalid input\")"])
    (rust . ["fn factorial(n: u64) -> u64 {\n    if n <= 1 {\n        return 1;\n    }\n    n * factorial(n - 1)\n}"
             "struct Point {\n    x: f64,\n    y: f64,\n}\n\nimpl Point {\n    fn distance(&self) -> f64 {\n        (self.x.powi(2) + self.y.powi(2)).sqrt()\n    }\n}"
             "fn main() {\n    let nums = vec![1, 2, 3, 4, 5];\n    let sum: i32 = nums.iter().sum();\n    println!(\"Sum: {}\", sum);\n}"
             "fn parse_int(s: &str) -> Result<i32, String> {\n    s.parse::<i32>()\n        .map_err(|e| format!(\"parse error: {}\", e))\n}"
             "enum Shape {\n    Circle(f64),\n    Rect(f64, f64),\n}\n\nfn area(s: &Shape) -> f64 {\n    match s {\n        Shape::Circle(r) => std::f64::consts::PI * r * r,\n        Shape::Rect(w, h) => w * h,\n    }\n}"
             "use std::collections::HashMap;\n\nfn word_count(text: &str) -> HashMap<&str, usize> {\n    let mut map = HashMap::new();\n    for word in text.split_whitespace() {\n        *map.entry(word).or_insert(0) += 1;\n    }\n    map\n}"])
    (go . ["func sum(nums []int) int {\n    total := 0\n    for _, n := range nums {\n        total += n\n    }\n    return total\n}"
           "type Server struct {\n    Host string\n    Port int\n}\n\nfunc (s *Server) Addr() string {\n    return fmt.Sprintf(\"%s:%d\", s.Host, s.Port)\n}"
           "func readFile(path string) ([]byte, error) {\n    data, err := os.ReadFile(path)\n    if err != nil {\n        return nil, fmt.Errorf(\"read %s: %w\", path, err)\n    }\n    return data, nil\n}"
           "func main() {\n    ch := make(chan string)\n    go func() {\n        ch <- \"hello\"\n    }()\n    msg := <-ch\n    fmt.Println(msg)\n}"
           "func filter(items []string, fn func(string) bool) []string {\n    var result []string\n    for _, item := range items {\n        if fn(item) {\n            result = append(result, item)\n        }\n    }\n    return result\n}"
           "func handleErr(w http.ResponseWriter, err error) {\n    if err != nil {\n        http.Error(w, err.Error(), 500)\n        return\n    }\n}"])
    (javascript . ["function debounce(fn, ms) {\n    let timer;\n    return function (...args) {\n        clearTimeout(timer);\n        timer = setTimeout(() => fn(...args), ms);\n    };\n}"
                   "async function fetchJSON(url) {\n    const res = await fetch(url);\n    if (!res.ok) {\n        throw new Error(res.statusText);\n    }\n    return res.json();\n}"
                   "class EventEmitter {\n    constructor() {\n        this.listeners = {};\n    }\n    on(event, fn) {\n        (this.listeners[event] ||= []).push(fn);\n    }\n}"
                   "const flatten = (arr) => {\n    return arr.reduce((acc, val) => {\n        return acc.concat(\n            Array.isArray(val) ? flatten(val) : val\n        );\n    }, []);\n};"
                   "function groupBy(arr, key) {\n    return arr.reduce((map, item) => {\n        const k = item[key];\n        (map[k] ||= []).push(item);\n        return map;\n    }, {});\n}"
                   "const memoize = (fn) => {\n    const cache = new Map();\n    return (...args) => {\n        const key = JSON.stringify(args);\n        if (!cache.has(key)) {\n            cache.set(key, fn(...args));\n        }\n        return cache.get(key);\n    };\n};"])
    (elisp . ["(defun my-filter (pred lst)\n  (let ((result nil))\n    (dolist (x lst)\n      (when (funcall pred x)\n        (push x result)))\n    (nreverse result)))"
              "(defun my-assoc (key alist)\n  (cl-loop for pair in alist\n           when (equal (car pair) key)\n           return pair))"
              "(defun count-words (str)\n  (let ((count 0)\n        (start 0))\n    (while (string-match \"\\\\w+\" str start)\n      (setq start (match-end 0))\n      (cl-incf count))\n    count))"
              "(defun safe-read-file (path)\n  (condition-case err\n      (with-temp-buffer\n        (insert-file-contents path)\n        (buffer-string))\n    (file-error\n     (message \"Cannot read %s: %s\" path err)\n     nil)))"
              "(defun join-strings (sep strings)\n  (mapconcat #'identity strings sep))"
              "(defun flatten-list (lst)\n  (cond\n   ((null lst) nil)\n   ((listp (car lst))\n    (append (flatten-list (car lst))\n            (flatten-list (cdr lst))))\n   (t (cons (car lst)\n            (flatten-list (cdr lst))))))"])
    (bash . ["#!/bin/bash\nset -euo pipefail\n\nfor f in \"$@\"; do\n    echo \"Processing $f\"\n    wc -l \"$f\"\ndone"
             "function cleanup() {\n    rm -f \"$TMPFILE\"\n    echo \"Cleaned up\"\n}\ntrap cleanup EXIT\nTMPFILE=$(mktemp)"
             "if [ -f \"$1\" ]; then\n    echo \"File exists: $1\"\n    cat \"$1\"\nelse\n    echo \"Not found: $1\" >&2\n    exit 1\nfi"
             "while IFS= read -r line; do\n    if [[ \"$line\" =~ ^# ]]; then\n        continue\n    fi\n    echo \"$line\"\ndone < \"$1\""
             "count=0\nfor dir in */; do\n    n=$(find \"$dir\" -type f | wc -l)\n    count=$((count + n))\n    echo \"$dir: $n files\"\ndone\necho \"Total: $count\""
             "case \"$1\" in\n    start)\n        echo \"Starting service\"\n        ;;\n    stop)\n        echo \"Stopping service\"\n        ;;\n    *)\n        echo \"Usage: $0 {start|stop}\"\n        exit 1\n        ;;\nesac"])
    (sql . ["SELECT u.name, COUNT(o.id) AS orders\nFROM users u\nJOIN orders o ON u.id = o.user_id\nGROUP BY u.name\nHAVING COUNT(o.id) > 5\nORDER BY orders DESC;"
            "CREATE TABLE products (\n    id SERIAL PRIMARY KEY,\n    name TEXT NOT NULL,\n    price NUMERIC(10, 2) DEFAULT 0,\n    created_at TIMESTAMP DEFAULT NOW()\n);"
            "UPDATE accounts\nSET balance = balance - 100\nWHERE id = 42\n    AND balance >= 100\nRETURNING id, balance;"
            "WITH ranked AS (\n    SELECT name, score,\n        ROW_NUMBER() OVER (ORDER BY score DESC) AS rank\n    FROM players\n)\nSELECT * FROM ranked WHERE rank <= 10;"
            "INSERT INTO logs (level, message, ts)\nVALUES ('ERROR', 'Connection failed', NOW())\nON CONFLICT (ts)\nDO UPDATE SET message = EXCLUDED.message;"
            "DELETE FROM sessions\nWHERE last_active < NOW() - INTERVAL '30 days'\nRETURNING user_id, last_active;"])
    (c . ["int factorial(int n) {\n    if (n <= 1) {\n        return 1;\n    }\n    return n * factorial(n - 1);\n}"
          "#include <stdio.h>\n#include <stdlib.h>\n\nint main(int argc, char *argv[]) {\n    if (argc < 2) {\n        fprintf(stderr, \"Usage: %s <n>\\n\", argv[0]);\n        return 1;\n    }\n    printf(\"%d\\n\", atoi(argv[1]));\n    return 0;\n}"
          "struct Node {\n    int value;\n    struct Node *next;\n};\n\nvoid push(struct Node **head, int val) {\n    struct Node *n = malloc(sizeof(*n));\n    n->value = val;\n    n->next = *head;\n    *head = n;\n}"
          "char *strdup_safe(const char *s) {\n    if (s == NULL) {\n        return NULL;\n    }\n    size_t len = strlen(s) + 1;\n    char *copy = malloc(len);\n    if (copy) {\n        memcpy(copy, s, len);\n    }\n    return copy;\n}"
          "void swap(int *a, int *b) {\n    int tmp = *a;\n    *a = *b;\n    *b = tmp;\n}"
          "int binary_search(int *arr, int n, int target) {\n    int lo = 0, hi = n - 1;\n    while (lo <= hi) {\n        int mid = lo + (hi - lo) / 2;\n        if (arr[mid] == target)\n            return mid;\n        if (arr[mid] < target)\n            lo = mid + 1;\n        else\n            hi = mid - 1;\n    }\n    return -1;\n}"]))
  "Alist mapping language symbols to vectors of multi-line code blocks.
Each block is a complete, syntactically valid code snippet with
newlines and indentation using spaces.")

(defconst touchtype--code-blocks
  (apply #'vconcat
         (mapcar #'cdr touchtype--code-blocks-by-language))
  "Vector of all code blocks (computed from per-language collections).")

(defvar touchtype--code-block-lines nil
  "List of remaining lines from the current code block.")

(defvar touchtype--code-language nil
  "Currently selected language for filtered `code' mode, or nil for all.")

(defconst touchtype--builtin-words
  (vconcat
   ;; ~~~ 2-3 letter words (312 words) ~~~
   ["act" "add" "age" "ago" "aid" "aim" "air" "all" "an" "and" "any" "ape"
    "arc" "arm" "art" "as" "ash" "ask" "at" "awe" "axe" "bag" "ban" "bat"
    "bay" "be" "bed" "bee" "bid" "big" "bin" "bit" "bog" "bow" "box" "boy"
    "bud" "bug" "bun" "bus" "but" "by" "cab" "cam" "cap" "cod" "cog" "cow"
    "coy" "cub" "cud" "cup" "cur" "cut" "dab" "dam" "day" "den" "dew" "did"
    "die" "dig" "dim" "din" "dip" "do" "doe" "dog" "don" "dot" "dry" "dub"
    "due" "dug" "duo" "dye" "ear" "eat" "eel" "egg" "ego" "elf" "elm" "end"
    "era" "eve" "eye" "fad" "fan" "far" "fed" "few" "fig" "fin" "fit" "foe"
    "fog" "for" "fox" "fry" "fur" "gag" "gap" "gas" "gel" "gem" "get" "gig"
    "gin" "gnu" "go" "gob" "god" "got" "gum" "gut" "gym" "had" "ham" "has"
    "hay" "he" "hem" "hen" "her" "hew" "hex" "hid" "him" "hip" "his" "hit"
    "hob" "hog" "hop" "hot" "how" "hub" "hue" "hug" "hum" "hut" "ice" "if"
    "in" "ink" "inn" "it" "its" "ivy" "jab" "jam" "jar" "jaw" "jet" "job"
    "joy" "jug" "kit" "lab" "lap" "lay" "led" "let" "lid" "lip" "lit" "log"
    "lot" "low" "mad" "man" "map" "mat" "may" "me" "mix" "mob" "mop" "mud"
    "mug" "my" "nap" "net" "new" "nip" "no" "nod" "nor" "not" "now" "nun"
    "nut" "oak" "oat" "odd" "of" "off" "oil" "old" "on" "one" "or" "ore" "our"
    "out" "owl" "own" "pad" "pan" "pat" "paw" "peg" "pen" "per" "pet" "pig"
    "pin" "pit" "pod" "pop" "pot" "pub" "pun" "pup" "put" "rag" "ram" "ran"
    "rap" "rat" "raw" "red" "rib" "rid" "rig" "rim" "rip" "rob" "rod" "rot"
    "row" "rub" "rug" "run" "rut" "sap" "saw" "say" "sea" "see" "set" "she"
    "sit" "sky" "so" "sob" "sod" "son" "sow" "spy" "sum" "sun" "tab" "tan"
    "tap" "tar" "tax" "ten" "the" "tin" "tip" "to" "too" "top" "tow" "toy"
    "try" "tub" "tug" "two" "up" "urn" "us" "use" "van" "vat" "vet" "via"
    "vow" "wag" "war" "way" "we" "web" "wed" "who" "why" "wig" "win" "wit"
    "woe" "wok" "won" "woo" "yam" "yet" "you" "zap" "zen" "zip" "zoo"]
   ;; ~~~ 4-5 letter words (1256 words) ~~~
   ["abort" "above" "adapt" "admit" "adopt" "adult" "after" "again" "alarm"
    "album" "alien" "align" "alone" "also" "alter" "amid" "ample" "angel"
    "anger" "ankle" "annex" "anvil" "apple" "arch" "area" "arena" "argue"
    "arise" "asset" "atlas" "attic" "avian" "avoid" "award" "away" "back"
    "bacon" "badge" "bald" "bale" "ball" "band" "barb" "barn" "base" "basin"
    "bask" "batch" "bead" "bear" "beef" "been" "began" "begin" "being" "below"
    "bench" "berry" "best" "bind" "bird" "birth" "bite" "black" "blade"
    "blame" "blank" "blast" "blaze" "bleed" "blend" "bless" "blind" "blink"
    "bliss" "block" "bloom" "blot" "blown" "blue" "blunt" "blur" "blush"
    "board" "boast" "boat" "body" "bolt" "bone" "bonus" "book" "booth" "born"
    "bound" "bowl" "brace" "brain" "brand" "brass" "brave" "bread" "break"
    "breed" "brew" "brick" "brief" "brim" "bring" "broad" "brood" "brook"
    "brown" "brush" "buck" "build" "bulk" "bump" "bunch" "burst" "bury"
    "buyer" "cabin" "cable" "call" "calm" "candy" "cane" "cape" "care" "cargo"
    "carry" "cash" "catch" "cause" "cave" "cease" "cell" "chain" "chair"
    "chalk" "chant" "chaos" "charm" "chart" "chase" "cheap" "check" "cheek"
    "cheer" "chess" "chest" "chew" "chief" "child" "chili" "chill" "choir"
    "chop" "chunk" "cigar" "city" "civic" "civil" "claim" "clam" "clan"
    "clash" "clasp" "class" "claw" "clay" "clean" "clear" "clerk" "cliff"
    "climb" "cling" "clip" "clock" "clone" "close" "cloth" "cloud" "clown"
    "cluck" "coach" "coal" "coast" "coil" "cold" "colon" "color" "come"
    "comma" "cool" "coral" "cord" "core" "cost" "could" "count" "court" "cove"
    "cover" "crack" "craft" "cram" "crash" "crawl" "crazy" "cream" "creek"
    "crest" "crime" "crisp" "crop" "cross" "crow" "crowd" "crown" "crude"
    "cruel" "crush" "cubic" "curl" "curry" "curve" "cycle" "daily" "dairy"
    "damp" "dance" "dare" "dark" "data" "dawn" "deal" "dealt" "death" "debt"
    "debug" "decay" "deck" "deep" "demon" "dense" "dent" "depot" "depth"
    "detox" "devil" "dine" "dirt" "dirty" "dive" "dock" "done" "donor" "door"
    "dose" "dote" "doubt" "dough" "draft" "drag" "drain" "drama" "drank"
    "drape" "draw" "drawl" "dream" "dress" "dried" "drift" "drill" "drink"
    "drive" "droit" "drone" "drop" "drops" "drove" "drum" "drunk" "dryer"
    "dull" "dusk" "dust" "duty" "dwarf" "dwell" "dying" "each" "eager" "early"
    "earn" "earth" "edge" "eight" "elect" "elite" "empty" "enemy" "enjoy"
    "enter" "equal" "equip" "erase" "error" "essay" "etch" "ethic" "evade"
    "even" "event" "ever" "every" "exact" "exalt" "exile" "exist" "extra"
    "fable" "face" "facet" "fade" "faint" "fairy" "faith" "false" "fancy"
    "farm" "fault" "favor" "feast" "feel" "fell" "fence" "fern" "ferry"
    "fetch" "fever" "fiber" "field" "fiery" "fifty" "fight" "fill" "filth"
    "final" "find" "fire" "first" "fish" "fist" "five" "flame" "flank" "flare"
    "flash" "flask" "flat" "fleet" "flesh" "flick" "fling" "flint" "flip"
    "float" "flock" "flood" "floor" "flora" "flour" "flow" "flown" "fluid"
    "fluke" "flute" "flux" "foam" "focus" "food" "fool" "foot" "force" "forge"
    "form" "forth" "forum" "foul" "found" "fowl" "frame" "frank" "fraud"
    "free" "fresh" "frog" "from" "front" "frost" "frown" "froze" "fruit"
    "full" "fume" "funny" "gain" "gait" "game" "gang" "gaze" "giant" "gift"
    "girl" "give" "given" "gland" "glare" "glass" "gleam" "glen" "glide"
    "glob" "globe" "gloom" "glory" "gloss" "glove" "glow" "glue" "glum"
    "going" "gold" "gone" "good" "goose" "gorge" "grab" "grace" "grade"
    "grain" "grand" "grant" "graph" "grasp" "grass" "grate" "grave" "gravy"
    "graze" "great" "greed" "green" "greet" "grew" "grid" "grief" "grill"
    "grim" "grind" "grit" "groan" "groom" "gross" "group" "grove" "growl"
    "grown" "guard" "guess" "guest" "guide" "guild" "guilt" "guise" "gust"
    "habit" "half" "halt" "hand" "hard" "harm" "harsh" "haste" "hatch" "haul"
    "haunt" "have" "haven" "haze" "head" "heal" "hear" "heart" "heavy" "hedge"
    "heist" "help" "hemp" "herb" "herd" "here" "heron" "high" "hint" "hold"
    "home" "honor" "hook" "hope" "horse" "hose" "hotel" "house" "hover"
    "human" "humor" "hump" "hurl" "hurry" "hush" "idea" "image" "imply"
    "index" "inner" "input" "into" "iron" "itch" "ivory" "jail" "jest" "jewel"
    "joint" "joker" "jolt" "judge" "juice" "juicy" "just" "keen" "keep" "kind"
    "king" "knee" "knit" "knock" "knot" "know" "known" "label" "labor" "lake"
    "lamb" "lance" "land" "large" "laser" "last" "latch" "later" "laugh"
    "lawn" "layer" "leap" "learn" "least" "leave" "left" "legal" "lemon"
    "less" "level" "lever" "lick" "life" "light" "like" "limit" "line" "linen"
    "list" "live" "liver" "llama" "load" "loan" "lobby" "local" "lodge" "loft"
    "logic" "long" "look" "loom" "loose" "lost" "love" "lover" "lower" "loyal"
    "luck" "lump" "lunar" "lunch" "lunge" "lure" "lyric" "magic" "major"
    "make" "male" "manor" "many" "maple" "march" "mark" "match" "mate" "mayor"
    "meal" "medal" "media" "melt" "mend" "mercy" "mere" "merge" "merit" "mesh"
    "metal" "meter" "might" "mile" "mill" "mind" "mine" "minor" "mist" "moan"
    "model" "money" "month" "mood" "moose" "moral" "more" "most" "mount"
    "mourn" "mouse" "mouth" "move" "movie" "much" "music" "must" "myth" "nail"
    "naive" "name" "nasty" "naval" "near" "need" "nerve" "nest" "never" "next"
    "night" "noble" "noise" "nook" "north" "notch" "note" "novel" "numb"
    "nurse" "nylon" "occur" "ocean" "offer" "often" "once" "onion" "only"
    "open" "opera" "orbit" "order" "organ" "outer" "oval" "over" "oxide"
    "pace" "pact" "page" "pain" "pair" "pale" "palm" "panic" "paper" "part"
    "party" "pass" "patch" "pause" "pave" "peace" "peach" "pearl" "pedal"
    "peel" "penny" "phase" "phone" "photo" "piano" "pick" "piece" "pilot"
    "pinch" "pine" "pipe" "pixel" "pizza" "place" "plain" "plan" "plane"
    "plank" "plant" "plate" "play" "plaza" "plea" "plead" "pleat" "plod"
    "plop" "pluck" "plug" "plum" "plumb" "plume" "plump" "poem" "point" "poke"
    "polar" "pole" "pool" "porch" "pork" "pound" "power" "pray" "press" "prey"
    "price" "pride" "prime" "print" "prior" "prize" "probe" "prone" "proof"
    "prop" "proud" "prove" "proxy" "psalm" "pull" "pulse" "punch" "pupil"
    "pure" "purge" "purse" "quest" "queue" "quick" "quiet" "quiz" "quota"
    "quote" "race" "radar" "radio" "raft" "rage" "rain" "raise" "rally"
    "ranch" "range" "rapid" "ratio" "reach" "react" "read" "ready" "realm"
    "rebel" "reef" "refer" "reign" "relax" "relay" "rely" "repay" "reply"
    "rest" "rich" "ride" "rider" "ridge" "rifle" "right" "rigid" "ring" "ripe"
    "risen" "risky" "rival" "river" "road" "roast" "robe" "robot" "rock"
    "rocky" "roll" "room" "rope" "rose" "rouge" "rough" "round" "route"
    "rover" "royal" "rugby" "rule" "ruler" "rumor" "rung" "rupee" "rural"
    "rush" "safe" "sage" "said" "saint" "sake" "salad" "salon" "same" "sand"
    "sauce" "scale" "scam" "scar" "scare" "scene" "scent" "scope" "score"
    "scout" "scrap" "seed" "seem" "sense" "sent" "serve" "setup" "seven"
    "sewer" "shade" "shaft" "shake" "shall" "shame" "shape" "share" "shark"
    "sharp" "shave" "shed" "shelf" "shell" "shift" "shin" "ship" "shire"
    "shirt" "shock" "shop" "shore" "short" "shout" "show" "shun" "side"
    "siege" "sigh" "sight" "sign" "silk" "since" "sixth" "sixty" "skate"
    "skill" "skim" "skin" "skull" "slab" "slam" "slash" "slate" "slave" "sled"
    "sleep" "slice" "slide" "slit" "slope" "slug" "small" "smart" "smell"
    "smile" "smoke" "snag" "snap" "snip" "snug" "soak" "sock" "soft" "soil"
    "solar" "solve" "some" "song" "soon" "soot" "sore" "sorry" "sort" "soul"
    "sound" "south" "space" "span" "spare" "spark" "spawn" "speak" "spear"
    "speed" "spell" "spend" "spice" "spike" "spine" "spoil" "spoke" "spoon"
    "sport" "spot" "spray" "spur" "squad" "stack" "staff" "stage" "stain"
    "stair" "stake" "stale" "stalk" "stall" "stamp" "stand" "star" "stark"
    "start" "stash" "state" "steal" "steam" "steel" "steep" "steer" "stem"
    "step" "stew" "stick" "stiff" "still" "sting" "stir" "stock" "stomp"
    "stone" "stood" "stool" "stoop" "stop" "store" "storm" "story" "stout"
    "stove" "strap" "straw" "stray" "strip" "stuck" "study" "stuff" "stump"
    "stun" "stung" "stunt" "such" "suds" "sugar" "suit" "super" "sure" "surge"
    "swam" "swamp" "swarm" "swat" "sway" "swear" "sweat" "sweep" "sweet"
    "swell" "swept" "swift" "swing" "swirl" "sword" "swore" "sworn" "swung"
    "syrup" "table" "take" "tale" "talk" "tame" "tang" "tank" "task" "tell"
    "tent" "test" "than" "that" "theft" "them" "theme" "they" "thick" "thief"
    "thin" "thing" "think" "third" "this" "thorn" "those" "three" "threw"
    "throw" "thud" "thumb" "tick" "tier" "tiger" "tight" "tile" "time" "timer"
    "title" "toast" "token" "tone" "tool" "torch" "toss" "total" "tough"
    "towel" "tower" "town" "trace" "track" "trade" "trail" "trait" "trash"
    "tray" "treat" "tree" "trek" "trend" "trial" "tribe" "trick" "tried"
    "trim" "troop" "truck" "true" "truly" "trunk" "trust" "truth" "tube"
    "tuft" "tumor" "tuner" "turn" "twig" "twist" "type" "ultra" "uncle"
    "under" "undue" "union" "unite" "unity" "until" "upper" "upset" "urban"
    "usher" "usual" "utter" "valid" "valor" "value" "valve" "vault" "veil"
    "vein" "vent" "venue" "verb" "verse" "very" "vest" "view" "vigor" "vinyl"
    "viola" "viral" "vivid" "vocal" "vodka" "vogue" "voice" "void" "vote"
    "voter" "vouch" "waist" "wait" "wall" "want" "warm" "warp" "wasp" "waste"
    "watch" "water" "wave" "weak" "weary" "weave" "wedge" "weed" "week" "weld"
    "well" "what" "wheat" "wheel" "when" "where" "whet" "which" "while" "whip"
    "whisk" "white" "whole" "whose" "wide" "widen" "width" "wild" "will"
    "wilt" "wind" "wing" "wipe" "wire" "wired" "wisp" "witch" "with" "woman"
    "wood" "word" "wore" "work" "world" "worm" "worry" "worse" "worst" "worth"
    "wound" "wrap" "wrath" "wreck" "wren" "wrist" "wrote" "yacht" "yard"
    "yawn" "year" "yell" "yield" "young" "youth" "zebra"]
   ;; ~~~ 6-7 letter words (1198 words) ~~~
   ["abandon" "abolish" "absent" "absorb" "absurd" "accent" "accept" "accord"
    "accrue" "accuse" "acidic" "across" "acting" "action" "active" "actual"
    "addict" "adjust" "admire" "advent" "advice" "aerial" "affair" "affirm"
    "afford" "agenda" "amount" "anchor" "animal" "annual" "answer" "anthem"
    "apache" "appeal" "appear" "append" "archer" "arctic" "armour" "around"
    "arrest" "arrive" "artist" "ascend" "aspect" "assert" "assign" "assist"
    "assume" "asylum" "atomic" "attack" "attend" "averse" "awaken" "ballet"
    "bamboo" "banana" "banker" "banner" "barber" "barely" "barrel" "basket"
    "battle" "beacon" "before" "behalf" "behave" "behind" "belong" "benign"
    "betray" "beyond" "bishop" "bitter" "blazer" "blouse" "bonnet" "border"
    "borrow" "bottom" "bounce" "branch" "breach" "breath" "breeze" "bridge"
    "bright" "broken" "broker" "bronze" "bubble" "buckle" "budget" "buffet"
    "bundle" "burden" "bureau" "burner" "butter" "button" "cabinet" "cactus"
    "candle" "cannon" "canvas" "canyon" "capable" "captain" "capture" "carbon"
    "career" "carpet" "carrot" "casino" "castle" "casual" "cattle" "caught"
    "caution" "census" "center" "central" "century" "cereal" "certain"
    "chamber" "change" "channel" "chapel" "chapter" "charge" "charity"
    "cheese" "cherry" "chicken" "choice" "chorus" "chosen" "chrome" "chronic"
    "cinema" "cipher" "circle" "circus" "citizen" "clause" "client" "climate"
    "closet" "clumsy" "cluster" "clutch" "coastal" "cobalt" "coffee" "collar"
    "collect" "colony" "column" "combat" "combine" "comedy" "comfort"
    "command" "comment" "commit" "common" "company" "compare" "compete"
    "complex" "comply" "compose" "compost" "concept" "concern" "conduct"
    "confirm" "connect" "consent" "consist" "consume" "contact" "contain"
    "content" "contest" "context" "control" "convert" "convey" "cooker"
    "cookie" "cooking" "copper" "corner" "correct" "cosmic" "cotton" "council"
    "counter" "country" "county" "courage" "course" "cousin" "cradle" "crafty"
    "crater" "create" "crisis" "crucial" "cruise" "culture" "cumbia" "current"
    "curtain" "curtsy" "custom" "dagger" "damage" "dancer" "danger" "deadly"
    "dealer" "debate" "debris" "decade" "decline" "decree" "defeat" "defect"
    "defence" "defend" "define" "degree" "delist" "deliver" "demand" "denial"
    "density" "depend" "deploy" "deposit" "deputy" "derive" "desert" "design"
    "desire" "detach" "detail" "detect" "device" "devote" "diagram" "dialect"
    "differ" "digest" "digital" "dilute" "dimmer" "dinner" "direct" "disable"
    "display" "dispute" "distant" "distort" "disturb" "diverse" "divert"
    "divide" "divine" "docker" "doctor" "dollar" "dolphin" "domain" "donkey"
    "dosage" "double" "dragon" "drawer" "driven" "drought" "drying" "duress"
    "during" "earthy" "eating" "ecology" "economy" "edition" "editor"
    "educate" "effect" "effort" "either" "elderly" "element" "eleven" "elicit"
    "emerge" "emotion" "empire" "employ" "enable" "encase" "endure" "energy"
    "engage" "engine" "enhance" "enlist" "enough" "enrich" "ensign" "ensure"
    "entail" "entire" "enzyme" "episode" "erosion" "errand" "escape" "essence"
    "esters" "evident" "evolve" "examine" "exceed" "except" "excise" "excite"
    "excuse" "exempt" "exhale" "exhibit" "exotic" "expand" "expect" "expend"
    "exploit" "explore" "export" "expose" "express" "extend" "extent"
    "extract" "extreme" "fabric" "factor" "faculty" "famine" "famous" "fanout"
    "farmer" "fashion" "father" "fatigue" "feature" "fellow" "ferret"
    "fiction" "fiddle" "fierce" "figure" "filter" "finance" "finger" "fiscal"
    "fitter" "flawed" "flight" "flinch" "floppy" "flower" "fluffy" "flying"
    "folder" "follow" "foreign" "forest" "formal" "format" "formula" "fortune"
    "forward" "fossil" "foster" "founder" "freedom" "freight" "frenzy"
    "friend" "fringe" "frisky" "frosty" "frozen" "frugal" "fulfill" "fumble"
    "funeral" "furnace" "further" "future" "gadget" "gained" "galaxy" "gallon"
    "gamble" "garage" "garden" "garlic" "gateway" "gather" "gazebo" "general"
    "gentle" "gently" "genuine" "gesture" "gifted" "ginger" "glider" "glimpse"
    "global" "glossy" "gluten" "goblet" "godson" "golden" "gospel" "gossip"
    "govern" "gradual" "grammar" "granite" "gravel" "gravity" "grocery"
    "groove" "ground" "growth" "grumpy" "guilty" "guitar" "gutter" "gypsum"
    "halfway" "halves" "hamlet" "hammer" "handle" "hangar" "happen" "harass"
    "harbor" "harden" "hardly" "harmony" "harvest" "hazard" "header" "health"
    "healthy" "heater" "heating" "heaven" "height" "helmet" "helpful" "herbal"
    "hidden" "highway" "hiking" "hinder" "history" "holder" "holiday" "hollow"
    "honest" "horizon" "hornet" "horror" "hosted" "hourly" "housing" "huddle"
    "humble" "hunger" "hunter" "hybrid" "iceberg" "ignite" "illegal" "imagine"
    "immense" "immune" "impact" "import" "impose" "impure" "indent" "indoor"
    "infant" "influx" "inform" "initial" "injure" "inmate" "innate" "inquiry"
    "insane" "insect" "insert" "inside" "insist" "inspect" "install" "instant"
    "intact" "intake" "intend" "intent" "interim" "intern" "invade" "invent"
    "invest" "invoke" "inward" "island" "jackal" "jacket" "jargon" "jersey"
    "jockey" "jostle" "journey" "jumble" "jungle" "junior" "justice" "justify"
    "kennel" "kernel" "kettle" "kidney" "killer" "kitten" "knight" "ladder"
    "lagoon" "lament" "lander" "lastly" "lately" "latent" "latest" "latter"
    "launch" "lavish" "lawyer" "layout" "leader" "league" "leather" "legacy"
    "legend" "lender" "length" "lesion" "lesson" "lethal" "letter" "liable"
    "liberty" "license" "likely" "liquid" "listen" "lively" "living" "lizard"
    "locate" "locker" "logical" "lonely" "loosen" "lounge" "loyalty" "luggage"
    "lumber" "maiden" "malice" "mallet" "mammal" "manage" "mangle" "manner"
    "mantle" "marble" "margin" "marker" "market" "marvel" "massive" "master"
    "matter" "mature" "meadow" "measure" "mellow" "memoir" "memory" "mender"
    "mental" "mention" "mentor" "merger" "method" "mineral" "mingle" "minute"
    "mirror" "misery" "missile" "mission" "mister" "mitten" "mixture" "mobile"
    "modern" "modest" "molten" "moment" "monitor" "monkey" "monster" "morals"
    "morning" "mortar" "mosaic" "mostly" "mother" "motion" "muddle" "muffle"
    "murder" "museum" "muster" "mutual" "muzzle" "mystery" "napkin" "narrow"
    "nation" "nature" "nectar" "negate" "nephew" "nettle" "neural" "neutral"
    "nibble" "nickel" "nimble" "nitric" "noodle" "normal" "notable" "notary"
    "nothing" "notice" "nozzle" "nuclear" "nuclei" "nugget" "number" "nursing"
    "nutmeg" "obesity" "object" "observe" "obtain" "obvious" "offend"
    "offense" "office" "oldest" "ongoing" "opaque" "operate" "oppose" "oracle"
    "orange" "orchid" "organic" "orient" "origin" "outage" "outbid" "outcry"
    "outdoor" "outfit" "outlaw" "outlet" "outline" "outlook" "output" "outrun"
    "overlap" "oversee" "oxygen" "oyster" "paddle" "palace" "pander" "parade"
    "parcel" "pardon" "parent" "partly" "patent" "pebble" "pencil" "people"
    "pepper" "period" "permit" "persist" "person" "pickle" "pigeon" "pillar"
    "pillow" "pirate" "pistol" "plague" "planet" "plaque" "plasma" "player"
    "please" "pledge" "plenty" "plough" "plunge" "plural" "pocket" "poison"
    "policy" "polish" "polite" "ponder" "portal" "portion" "poster" "potato"
    "potion" "potter" "poverty" "praise" "prayer" "preach" "predict" "prefer"
    "premium" "prepare" "present" "pretty" "prevent" "primary" "prince"
    "printer" "prison" "privacy" "problem" "proceed" "process" "produce"
    "product" "profile" "profit" "program" "project" "prolong" "promise"
    "promote" "prompt" "propel" "proper" "protect" "protest" "proven"
    "provide" "public" "publish" "puddle" "puppet" "pursue" "puzzle" "quarry"
    "quarter" "quartz" "rabbit" "racket" "radical" "radish" "rafter" "raised"
    "raisin" "random" "ranger" "ransom" "rapids" "rarely" "rattle" "ravine"
    "reacts" "reader" "reading" "reality" "really" "reason" "rebuild" "recall"
    "receipt" "recent" "recess" "reckon" "record" "recover" "redeem" "reduce"
    "refine" "reflect" "reform" "refuge" "refund" "refuse" "regain" "regard"
    "regime" "region" "regret" "reject" "relate" "related" "release" "relief"
    "relish" "reload" "remain" "remedy" "remind" "remote" "removal" "remove"
    "render" "renewal" "rental" "repeal" "repeat" "replace" "report" "request"
    "require" "rescue" "resent" "reside" "resign" "resist" "resolve" "resort"
    "respect" "respond" "restore" "result" "resume" "retail" "retain" "retire"
    "retort" "retreat" "return" "reveal" "revenge" "reverse" "review" "revolt"
    "reward" "ribbon" "riddle" "riffle" "ripple" "ritual" "robust" "rocket"
    "rotate" "rotten" "routine" "rubber" "rubble" "ruffle" "rumble" "runner"
    "rustic" "sacred" "saddle" "safari" "salmon" "sample" "sandal" "satisfy"
    "scenic" "scholar" "school" "scorch" "scratch" "screen" "scroll" "search"
    "season" "second" "secret" "sector" "secure" "segment" "seldom" "select"
    "senior" "sequel" "sermon" "settle" "shadow" "shelter" "shield" "should"
    "shower" "shrewd" "shrink" "shroud" "shrunk" "shudder" "signal" "silence"
    "silent" "silver" "similar" "simmer" "simple" "singer" "single" "sister"
    "sketch" "skilled" "sleeve" "slipper" "slogan" "smaller" "smooth" "smudge"
    "snatch" "social" "society" "socket" "soften" "soldier" "solely" "solemn"
    "somehow" "soothe" "sorbet" "sorrow" "source" "speaker" "speech" "spider"
    "spiral" "spirit" "splash" "splice" "sponge" "spouse" "sprawl" "spread"
    "spring" "sprint" "square" "squash" "squeak" "squeal" "squint" "squire"
    "stable" "staple" "statue" "status" "steady" "stolen" "storage" "storey"
    "strain" "strand" "stream" "street" "strewn" "stride" "strike" "string"
    "stripe" "strive" "stroke" "strong" "struck" "student" "subject" "submit"
    "subtle" "succeed" "sucker" "sudden" "suffix" "suicide" "summer" "summit"
    "sunken" "superb" "supply" "support" "surely" "surplus" "survey" "survive"
    "suspect" "sustain" "switch" "symbol" "system" "tackle" "tactic" "tailor"
    "talent" "tamper" "tangle" "tariff" "teapot" "temper" "temple" "tenant"
    "tender" "terror" "tether" "therapy" "thirst" "thirty" "thorny" "though"
    "thought" "thread" "threat" "thrive" "throne" "throng" "thrust" "ticket"
    "tickle" "tiddly" "timber" "tinder" "tissue" "tobacco" "toddle" "tomato"
    "tongue" "tonight" "torque" "tourism" "toward" "traffic" "tragedy"
    "trance" "trauma" "travel" "treaty" "trench" "tribal" "trigger" "triple"
    "trophy" "trouble" "trudge" "tumble" "tundra" "tunnel" "turban" "turning"
    "turtle" "tussle" "twelve" "tycoon" "typical" "umpire" "unable" "unfair"
    "unfold" "unfurl" "unhook" "unique" "unison" "unjust" "unless" "unload"
    "unlock" "unpack" "unrest" "unruly" "unseen" "untidy" "unveil" "upbeat"
    "update" "uphold" "uproar" "uptake" "upward" "usable" "useful" "utmost"
    "vacant" "vacuum" "vainly" "valley" "vanish" "vanity" "variety" "vehicle"
    "velvet" "vendor" "venture" "verbal" "vermin" "version" "versus" "vessel"
    "veteran" "viable" "victim" "village" "violent" "violin" "virtue"
    "visible" "vision" "volume" "voyage" "waffle" "walnut" "walrus" "wander"
    "warden" "warfare" "warmly" "warmth" "warning" "wealth" "weapon" "wedding"
    "weekly" "weight" "welcome" "welder" "welfare" "western" "whisper"
    "wholly" "wicked" "window" "winery" "winning" "winter" "wisdom" "within"
    "wizard" "wonder" "worker" "workout" "worthy" "wreath" "writing" "yellow"
    "zenith" "zigzag" "zodiac"]
   ;; ~~~ 8-9 letter words (880 words) ~~~
   ["aberrant" "abnormal" "abortion" "abrasion" "abruptly" "absolute"
    "absorber" "abstract" "abundant" "academic" "accident" "accurate"
    "achieved" "acoustic" "acquired" "activate" "actively" "adapting"
    "addicted" "addition" "adequate" "adhesive" "adjacent" "adjusted"
    "admiring" "admitted" "adopting" "adoption" "adorable" "advanced"
    "advisory" "advocate" "affected" "agreeing" "aircraft" "alarming"
    "allergic" "alliance" "allocate" "allowing" "alphabet" "although"
    "ambiance" "ambition" "ambulant" "amending" "amusable" "analysis"
    "ancestor" "animated" "annotate" "announce" "annoying" "anterior"
    "antimony" "anything" "anywhere" "apologue" "apparent" "appendix"
    "appetite" "appraise" "approach" "approval" "argument" "arterial"
    "artifact" "aspiring" "assembly" "assuming" "asteroid" "athletic"
    "attorney" "audience" "autonomy" "aviation" "bachelor" "backbone"
    "backdrop" "backfire" "backhand" "backlash" "backward" "bacteria"
    "balanced" "bankrupt" "baritone" "barnacle" "barnyard" "baseball"
    "basement" "bathroom" "becoming" "befriend" "beginner" "behavior"
    "bellowed" "betrayal" "beverage" "billiard" "birthday" "biweekly"
    "blackout" "blankets" "bleeding" "blending" "blessing" "blizzard"
    "blogging" "blossoms" "borrowed" "botanist" "boundary" "bracelet"
    "breeding" "brighten" "brightly" "broiling" "brothers" "browsing"
    "brunette" "brushing" "building" "bulletin" "bursting" "bushfire"
    "bustling" "calendar" "callback" "campaign" "canister" "capacity"
    "cardinal" "carefree" "carnival" "cassette" "casualty" "catalyst"
    "catching" "category" "catering" "cautious" "cellular" "centered"
    "ceremony" "chairman" "champion" "charcoal" "checkbox" "checkout"
    "checksum" "cheerful" "chemical" "children" "chrysler" "circular"
    "citation" "civilian" "cladding" "claiming" "clanking" "clapping"
    "clasping" "classify" "cleaning" "clearing" "climbing" "clinical"
    "clipping" "clockwork" "clothing" "coaching" "coalesce" "coherent"
    "collapse" "colonial" "colorful" "combated" "combined" "comeback"
    "commerce" "commonly" "communal" "commuter" "compiled" "compiler"
    "complain" "complete" "composed" "compound" "compress" "comprise"
    "computed" "computer" "conceive" "conclude" "concrete" "confetti"
    "confined" "conflict" "confront" "confused" "congress" "conjunct"
    "connects" "conquers" "conquest" "conserve" "consider" "conspire"
    "constant" "consular" "consumer" "contempt" "continue" "contract"
    "contrast" "converge" "convince" "cookbook" "coronary" "corridor"
    "corrosion" "cosmetic" "coupling" "coverage" "cracking" "crafting"
    "cramming" "crashing" "crawling" "creating" "creation" "creature"
    "credited" "criminal" "critical" "critique" "crooning" "crossing"
    "crunched" "cucumber" "cultural" "cupboard" "currency" "customer"
    "cylinder" "database" "daybreak" "deadline" "debating" "deceiver"
    "decisive" "declared" "declined" "decoding" "decorate" "decrease"
    "deducted" "deepened" "defender" "deferral" "defiance" "definite"
    "delicate" "delivers" "delivery" "democrat" "demolish" "departed"
    "depicted" "deployed" "deposing" "describe" "designed" "designer"
    "despatch" "destruct" "detailed" "detected" "detector" "devotion"
    "diabetes" "diagonal" "dialogue" "dinosaur" "diplomat" "directed"
    "directly" "disabled" "disagree" "disaster" "disclose" "discount"
    "discover" "discreet" "dispatch" "disperse" "displace" "disposal"
    "dissolve" "distance" "distinct" "distract" "district" "dividend"
    "dizzying" "doctrine" "document" "doldrums" "domestic" "dominant"
    "donation" "doorbell" "doorstep" "doubtful" "downhill" "download"
    "downtown" "drafting" "drainage" "dramatic" "dreaming" "dressing"
    "drifting" "drilling" "drinking" "droplets" "dropping" "drowning"
    "drumming" "duckweed" "dumbbell" "dumpling" "dumpster" "duration"
    "dutchman" "dwelling" "dynamics" "earnings" "eclectic" "economic"
    "educated" "educator" "effected" "eighteen" "election" "elegance"
    "elephant" "elevated" "elevator" "eligible" "eloquent" "embedded"
    "embezzle" "emerging" "emission" "emoticon" "emphasis" "employed"
    "emporium" "empowered" "enclosed" "encoding" "endorsed" "engaging"
    "engineer" "enormous" "enriched" "enrolled" "ensemble" "entering"
    "entirely" "entitled" "entrance" "envelope" "envision" "epidemic"
    "equality" "equation" "equipped" "erecting" "escalate" "escaping"
    "espresso" "estimate" "eternity" "evaluate" "eventual" "everyday"
    "everyone" "evidence" "evolving" "examined" "examples" "exchange"
    "exciting" "excluded" "exercise" "exertion" "existing" "expanded"
    "expected" "expedite" "expelled" "explicit" "explored" "exponent"
    "exported" "exposure" "extended" "exterior" "external" "extracts"
    "eyesight" "fabulous" "facebook" "facility" "failover" "faithful"
    "fallback" "familiar" "farmland" "farthest" "fastened" "favorite"
    "fearless" "feasible" "featured" "feedback" "feminine" "festival"
    "fiercely" "fighting" "figuring" "filament" "filename" "filmmaker"
    "filtered" "finalist" "finalize" "findings" "finished" "firewall"
    "flagship" "flattery" "flexible" "floating" "flooding" "flourish"
    "folklore" "followed" "follower" "football" "foothold" "footprint"
    "footwear" "forecast" "foremost" "forensic" "formerly" "formwork"
    "fortress" "founding" "fourteen" "fraction" "fragment" "freckled"
    "freeload" "frequent" "friendly" "frontier" "fruitful" "fruition"
    "fullback" "fulltime" "function" "gambling" "gardener" "gasoline"
    "gathered" "generate" "generous" "genetics" "genocide" "geometry"
    "gigantic" "glancing" "glitters" "globally" "glorious" "goodness"
    "goodwill" "gorgeous" "governed" "governor" "graceful" "gracious"
    "graduate" "graphics" "grasping" "grateful" "greeting" "grinding"
    "gripping" "grounded" "growling" "grudging" "guardian" "guidance"
    "gunpoint" "gymnasium" "habitual" "handbook" "handling" "handsome"
    "happened" "hardball" "hardcore" "hardened" "hardship" "hardware"
    "harmless" "harmonic" "harshest" "headband" "headline" "headlong"
    "headroom" "helpless" "heritage" "hermetic" "highland" "hilarity"
    "historic" "holdback" "homeless" "homework" "honestly" "hopeless"
    "horrible" "horrific" "hospital" "hostages" "hotelier" "humanity"
    "humility" "humorous" "hydrogen" "hygienic" "icecream" "idealist"
    "identify" "ideology" "ignorant" "illusion" "imperial" "incident"
    "increase" "indicate" "indirect" "industry" "inferior" "infinite"
    "informed" "inherent" "innocent" "insecure" "interact" "interest"
    "internal" "interval" "intimate" "invasion" "investor" "isolated"
    "judgment" "keyboard" "kindness" "knitting" "landlord" "language"
    "laughter" "lavender" "lawmaker" "learning" "lifetime" "likewise"
    "limiting" "listener" "literacy" "literary" "location" "lonesome"
    "longtime" "magnetic" "maintain" "majority" "makeover" "managing"
    "manifest" "marathon" "marginal" "marriage" "massacre" "material"
    "maturity" "maximize" "measured" "mechanic" "medicine" "medieval"
    "membrane" "merchant" "metaphor" "midnight" "military" "minister"
    "minority" "mischief" "moderate" "molecule" "momentum" "monopoly"
    "mortgage" "mounting" "movement" "multiple" "mumbling" "muscular"
    "mutation" "national" "negative" "neighbor" "networks" "nineteen"
    "nominate" "nonsense" "notebook" "numerous" "obstacle" "occasion"
    "occupied" "offering" "official" "offshore" "opponent" "opposite"
    "optimism" "ordinary" "organism" "organize" "oriented" "original"
    "outbreak" "overcome" "overlook" "overture" "painting" "parallel"
    "parental" "passport" "patience" "peaceful" "peculiar" "pedagogy"
    "pedaling" "pendulum" "perceive" "personal" "persuade" "petition"
    "platform" "pleasant" "pleasure" "plunging" "pointing" "policing"
    "populace" "populate" "portrait" "position" "positive" "possible"
    "powerful" "practice" "precious" "prepared" "presence" "preserve"
    "pressing" "previous" "printing" "priority" "prisoner" "probable"
    "producer" "profound" "progress" "prohibit" "promptly" "properly"
    "property" "proposal" "prospect" "provider" "province" "publicly"
    "purchase" "pursuing" "puzzling" "quantity" "quarters" "question"
    "quotient" "railroad" "rational" "reaching" "reaction" "readable"
    "reasoned" "received" "recorder" "recovery" "referral" "register"
    "regulate" "relevant" "religion" "remember" "renowned" "repeated"
    "reporter" "republic" "research" "resident" "resigned" "resource"
    "response" "restless" "restrain" "restrict" "retailer" "retiring"
    "reversal" "revision" "rhetoric" "rigorous" "romantic" "salesman"
    "sanction" "sandwich" "scenario" "schedule" "scrutiny" "seasonal"
    "security" "sensible" "sentence" "separate" "sequence" "sergeant"
    "settling" "severely" "shipping" "shocking" "shooting" "shortage"
    "shoulder" "sidewalk" "simplify" "simulate" "singular" "skeleton"
    "slightly" "slippery" "smallest" "snapshot" "software" "solution"
    "somebody" "somewhat" "southern" "speaking" "specific" "spectrum"
    "spelling" "spending" "spinning" "splendid" "sporting" "spotless"
    "sprinkle" "squander" "staffing" "standard" "standing" "startled"
    "steadily" "stimulus" "stocking" "stopping" "straight" "stranger"
    "strategy" "strength" "striking" "strongly" "struggle" "stumbled"
    "stunning" "suburban" "suddenly" "suitable" "superman" "supplier"
    "suppress" "surgical" "surprise" "survival" "swimming" "symbolic"
    "sympathy" "tangible" "teaching" "teammate" "terminal" "terrific"
    "thinking" "thirteen" "thorough" "thousand" "thriller" "together"
    "tolerant" "tomorrow" "tracking" "training" "transfer" "treasure"
    "treating" "trillion" "tropical" "troubled" "turnover" "ultimate"
    "uncommon" "underway" "universe" "unlawful" "unlikely" "updating"
    "uprising" "utilized" "vacation" "validity" "valorous" "valuable"
    "variable" "velocity" "vendetta" "ventures" "vertical" "vigorous"
    "violated" "visiting" "vitamins" "volatile" "volcanic" "watching"
    "weakness" "weighing" "wherever" "wildlife" "wireless" "withdraw"
    "woodland" "workshop" "worrying" "yielding"]
   ;; ~~~ 10-11 letter words (457 words) ~~~
   ["abandonment" "abbreviated" "absolutely" "accelerated" "accompanied"
    "accomplish" "accordingly" "accountant" "accumulated" "accurately"
    "accusation" "acknowledge" "acquisition" "adaptation" "additional"
    "adjustment" "admiration" "adolescent" "adventurous" "advertising"
    "affirmative" "affordable" "aggravating" "aggressive" "allegations"
    "alternating" "altogether" "ambassador" "ammunition" "anniversary"
    "anticipated" "apparently" "appearance" "appearances" "application"
    "appointment" "approaching" "appropriate" "approximate" "arrangement"
    "association" "assumption" "atmosphere" "atmospheric" "attachment"
    "attendance" "attraction" "authorities" "automobile" "awkwardness"
    "background" "backlashing" "backpacking" "bankruptcy" "basketball"
    "battlefield" "beforehand" "beneficial" "beneficiary" "birthplace"
    "blacksmith" "blossoming" "bombardment" "bookkeeping" "bottlenecks"
    "boundaries" "brilliance" "broadcaster" "brotherhood" "bureaucracy"
    "butterflies" "calculating" "calibrating" "capitalism" "captivating"
    "celebrated" "centralized" "certificate" "chairperson" "challenging"
    "championing" "chancellor" "chandeliers" "cheerfully" "chromosome"
    "comfortable" "commanding" "commandment" "commentary" "commitment"
    "commonplace" "communicate" "communists" "comparable" "comparisons"
    "compelling" "competence" "competition" "completely" "complexity"
    "compliance" "complicated" "composition" "compromise" "concentrate"
    "conception" "conclusion" "conditional" "confession" "confidence"
    "confidently" "conflicting" "confronted" "conjunction" "connecting"
    "conscience" "consecutive" "consequence" "consequent" "considering"
    "consistency" "conspicuous" "conspiracy" "constituent" "constraint"
    "constraints" "consulting" "contestant" "continental" "contribute"
    "contributor" "controller" "controversy" "convenience" "convention"
    "conventions" "convertible" "conviction" "cooperation" "coordinate"
    "coordinator" "cornerstone" "corporation" "corruption" "counseling"
    "counterfeit" "countryside" "courthouse" "creativity" "credentials"
    "culmination" "curriculum" "customarily" "dangerously" "deceptively"
    "declaration" "decorating" "dedication" "deficiency" "definitely"
    "delegation" "deliberate" "democratic" "demonstrate" "denominator"
    "department" "dependency" "deployment" "depression" "descendant"
    "description" "designated" "designation" "destruction" "detachment"
    "determined" "development" "dictionary" "differently" "dimensional"
    "dimensions" "diplomatist" "directions" "disappeared" "disclaimer"
    "discontinue" "discovering" "discretion" "discussing" "discussions"
    "dismissing" "disruption" "dissolution" "distinctive" "distinguish"
    "distribute" "distributed" "disturbance" "documentary" "domesticate"
    "dominating" "earthquake" "educational" "effectively" "efficiency"
    "elaboration" "elementary" "eliminated" "elimination" "embarrassed"
    "emigration" "emotionally" "employment" "encountered" "encounters"
    "encouraging" "enforcement" "engagement" "engineering" "enlightened"
    "enterprise" "enthusiasm" "environment" "equivalent" "especially"
    "essentially" "established" "evaluation" "everything" "examination"
    "excitement" "exclamation" "exclusively" "exhibition" "expectation"
    "expedition" "expenditure" "experienced" "experiment" "explaining"
    "explanation" "exploration" "expression" "extensively" "extinction"
    "extravagant" "facilitate" "fascinated" "fingertips" "flashlight"
    "fluctuating" "forefathers" "foundation" "fractional" "fragmented"
    "friendship" "frustrated" "fulfilling" "furthermore" "generation"
    "geographic" "government" "graduation" "grasshopper" "gratifying"
    "guaranteed" "guidelines" "handwriting" "harassment" "hereditary"
    "hesitation" "highlighted" "highlights" "homesteader" "horizontal"
    "hospitality" "hypothesis" "illuminate" "illustrate" "imagination"
    "immediately" "immigration" "immobilize" "implication" "importantly"
    "impression" "incomplete" "indication" "industrial" "inequality"
    "inevitable" "infectious" "injunction" "innovation" "inspection"
    "instrument" "integrated" "interested" "introduced" "investment"
    "invitation" "journalism" "justifying" "laboratory" "leadership"
    "legitimate" "likelihood" "limitation" "linguistic" "litigation"
    "locomotive" "mainstream" "management" "manipulate" "manuscript"
    "meaningful" "mechanical" "membership" "metabolism" "microscope"
    "missionary" "motivation" "motorcycle" "mysterious" "nationwide"
    "newsletter" "nomination" "obligation" "occupation" "occurrence"
    "opposition" "outlandish" "outperform" "overcoming" "overlooking"
    "parliament" "passionate" "percentage" "permission" "permitting"
    "persistent" "philosopher" "plantation" "politicking" "popularity"
    "possession" "potentially" "powerfully" "practicable" "predecessor"
    "prediction" "preliminary" "preparation" "presidency" "prestigious"
    "presumably" "prevention" "proceeding" "proceedings" "production"
    "profession" "profitable" "projection" "promotional" "proportion"
    "proposition" "prosperity" "protective" "provisional" "provisions"
    "publication" "punishment" "quarantine" "quarterback" "rebuilding"
    "recognized" "reconstruct" "recreation" "referendum" "reflecting"
    "regulation" "regulations" "relaxation" "remarkable" "remembering"
    "reminiscent" "replacement" "represented" "reputation" "requirement"
    "residential" "resignation" "resistance" "resolution" "responsible"
    "retirement" "retribution" "revelation" "revolution" "ridiculous"
    "scholarship" "settlement" "shareholder" "sharpening" "significant"
    "similarity" "simplicity" "simulation" "sovereignty" "spectacular"
    "stereotype" "stockholder" "strengthen" "structural" "subsequent"
    "substantial" "sufficient" "suggestion" "supplement" "suspension"
    "tablespoon" "technology" "television" "temperature" "themselves"
    "therapeutic" "thoughtful" "threatened" "threatening" "touchstone"
    "tournament" "transition" "transmitted" "transparent" "tremendous"
    "uncertainty" "underground" "underlying" "understand" "undertaking"
    "unemployed" "unfamiliar" "unfortunate" "uniformity" "university"
    "unnecessary" "unofficial" "utilitarian" "validation" "vegetation"
    "volleyball" "voluntarily" "vulnerable" "waterfront" "widespread"
    "withdrawal" "yourselves"]
   ;; ~~~ 12+ letter words (154 words) ~~~
   ["abbreviation" "accidentally" "accomplished" "accomplishing"
    "accountability" "accumulation" "acknowledged" "acknowledgment"
    "administration" "advertisement" "announcement" "appreciation"
    "appropriated" "approximately" "archaeological" "architectural"
    "argumentative" "authenticity" "automatically" "biodiversity"
    "breakthrough" "broadcasting" "characteristic" "chronological"
    "circumstance" "collaboration" "commemorating" "commercialize"
    "communication" "compassionate" "comprehensive" "concentration"
    "confederation" "congregation" "congressional" "conscientious"
    "consciousness" "consequential" "consideration" "consolidated"
    "consolidation" "constellation" "constitutional" "construction"
    "contamination" "contributions" "controversial" "correspondent"
    "corresponding" "counterattack" "deliberately" "demonstration"
    "deterioration" "determination" "developmental" "disadvantaged"
    "disappointed" "disappointment" "discrimination" "disqualified"
    "distinguished" "distribution" "documentation" "dramatically"
    "effectiveness" "entertainment" "entrepreneur" "environmental"
    "establishment" "exaggeration" "experiential" "experimentation"
    "extraordinary" "fortification" "fundamentally" "globalization"
    "grandchildren" "gravitational" "hallucination" "headquarters"
    "heterogeneous" "humanitarian" "identification" "illustrations"
    "implementation" "improvisation" "incorporating" "independently"
    "indispensable" "individualism" "industrialize" "infrastructure"
    "insignificant" "institutional" "instrumentation" "international" "irresponsibly"
    "interpretation" "investigation" "justification" "knowledgeable"
    "manufacturing" "mathematician" "mediterranean" "misconception"
    "misunderstand" "modernization" "multinational" "neighborhood"
    "neighbourhood" "opportunities" "organizational" "organizations"
    "outstandingly" "overwhelming" "overwhelmingly" "parliamentary"
    "participation" "pharmaceutical" "philosophical" "precipitation"
    "predetermined" "predominantly" "privatization" "professionalism"
    "professionals" "pronunciation" "psychological" "qualification"
    "questionnaire" "recommendation" "refrigeration" "refrigerator"
    "relationships" "representative" "restructuring" "revolutionary"
    "semiconductor" "significantly" "simultaneously" "sophisticated"
    "specialization" "specification" "specifications" "strengthening"
    "superintendent" "technological" "traditionally" "transformation"
    "uncomfortable" "understanding" "unfortunately" "unprecedented"
    "vulnerability" "wholehearted"])
  "Vector of ~4258 common English words for `full-words' mode.
Words are selected for variety of letter patterns and length
distribution, and are suitable for typing practice.")

;;;; Quotes

(defconst touchtype--quotes
  [("The only way to do great work is to love what you do." . "Steve Jobs")
   ("In the middle of difficulty lies opportunity." . "Albert Einstein")
   ("It does not matter how slowly you go as long as you do not stop." . "Confucius")
   ("Life is what happens when you are busy making other plans." . "John Lennon")
   ("The unexamined life is not worth living." . "Socrates")
   ("To be or not to be, that is the question." . "William Shakespeare")
   ("I think, therefore I am." . "Rene Descartes")
   ("The only thing we have to fear is fear itself." . "Franklin D. Roosevelt")
   ("Be the change you wish to see in the world." . "Mahatma Gandhi")
   ("Knowledge is power." . "Francis Bacon")
   ("The pen is mightier than the sword." . "Edward Bulwer-Lytton")
   ("Those who cannot remember the past are condemned to repeat it." . "George Santayana")
   ("The greatest glory in living lies not in never falling, but in rising every time we fall." . "Nelson Mandela")
   ("The way to get started is to quit talking and begin doing." . "Walt Disney")
   ("Your time is limited, so do not waste it living someone else's life." . "Steve Jobs")
   ("If life were predictable, it would cease to be life and be without flavor." . "Eleanor Roosevelt")
   ("Spread love everywhere you go. Let no one ever come to you without leaving happier." . "Mother Teresa")
   ("When you reach the end of your rope, tie a knot in it and hang on." . "Franklin D. Roosevelt")
   ("Always remember that you are absolutely unique, just like everyone else." . "Margaret Mead")
   ("The future belongs to those who believe in the beauty of their dreams." . "Eleanor Roosevelt")
   ("Tell me and I forget. Teach me and I remember. Involve me and I learn." . "Benjamin Franklin")
   ("It is during our darkest moments that we must focus to see the light." . "Aristotle")
   ("Whoever is happy will make others happy too." . "Anne Frank")
   ("You will face many defeats in life, but never let yourself be defeated." . "Maya Angelou")
   ("The greatest wealth is to live content with little." . "Plato")
   ("Not all those who wander are lost." . "J.R.R. Tolkien")
   ("The only impossible journey is the one you never begin." . "Tony Robbins")
   ("We are what we repeatedly do. Excellence, then, is not an act, but a habit." . "Aristotle")
   ("Imagination is more important than knowledge." . "Albert Einstein")
   ("Stay hungry, stay foolish." . "Steve Jobs")
   ("Simplicity is the ultimate sophistication." . "Leonardo da Vinci")
   ("The best way to predict the future is to invent it." . "Alan Kay")
   ("Do what you can, with what you have, where you are." . "Theodore Roosevelt")
   ("Success is not final, failure is not fatal; it is the courage to continue that counts." . "Winston Churchill")
   ("A room without books is like a body without a soul." . "Marcus Tullius Cicero")
   ("It always seems impossible until it is done." . "Nelson Mandela")
   ("Nothing is impossible. The word itself says I'm possible!" . "Audrey Hepburn")
   ("The only limit to our realization of tomorrow will be our doubts of today." . "Franklin D. Roosevelt")
   ("Do not go where the path may lead. Go instead where there is no path and leave a trail." . "Ralph Waldo Emerson")
   ("Education is the most powerful weapon which you can use to change the world." . "Nelson Mandela")
   ("In three words I can sum up everything I learned about life: it goes on." . "Robert Frost")
   ("Life is really simple, but we insist on making it complicated." . "Confucius")
   ("The purpose of our lives is to be happy." . "Dalai Lama")
   ("Get busy living or get busy dying." . "Stephen King")
   ("You only live once, but if you do it right, once is enough." . "Mae West")
   ("Many of life's failures are people who did not realize how close they were to success." . "Thomas Edison")
   ("If you look at what you have in life, you will always have more." . "Oprah Winfrey")
   ("If you set your goals ridiculously high and it is a failure, you will fail above everyone else." . "James Cameron")
   ("Life is a succession of lessons which must be lived to be understood." . "Ralph Waldo Emerson")
   ("The mind is everything. What you think, you become." . "Buddha")]
  "Vector of famous quotes for `quote' typing mode.
Each entry is a cons cell (TEXT . AUTHOR) with proper punctuation.")

;;;; Domain word lists

(defconst touchtype--domain-words
  '((medical . ["diagnosis" "prognosis" "symptom" "treatment" "therapy" "chronic"
                "acute" "benign" "malignant" "biopsy" "catheter" "suture"
                "anesthesia" "antibody" "antigen" "pathogen" "syndrome" "lesion"
                "fracture" "hemorrhage" "inflammation" "prescription" "dosage"
                "incision" "transfusion" "ultrasound" "radiology" "oncology"
                "cardiology" "neurology" "orthopedic" "pediatric" "geriatric"
                "obstetric" "dermatology" "endoscopy" "metabolic" "vascular"
                "pulmonary" "hepatic" "renal" "adrenal" "thyroid" "pancreatic"
                "cerebral" "spinal" "arterial" "venous" "platelet"])
    (legal . ["plaintiff" "defendant" "counsel" "verdict" "testimony" "statute"
              "litigation" "arbitration" "deposition" "jurisdiction" "precedent"
              "indictment" "arraignment" "acquittal" "appellant" "adjudicate"
              "allegation" "amendment" "affidavit" "subpoena" "injunction"
              "settlement" "probation" "parole" "felony" "misdemeanor"
              "compliance" "liability" "negligence" "tortious" "fiduciary"
              "contractual" "stipulation" "magistrate" "tribunal" "habeas"
              "mandamus" "certiorari" "jurisprudence" "culpable" "exonerate"
              "prosecute" "mitigate" "adjudicate" "rescind" "ratify"
              "constitutional" "statutory" "regulatory" "judicial" "appellate"])
    (programming . ["algorithm" "variable" "function" "parameter" "argument"
                    "recursion" "iteration" "boolean" "integer" "string"
                    "array" "hashmap" "pointer" "reference" "immutable"
                    "compile" "runtime" "debugger" "exception" "assertion"
                    "callback" "closure" "interface" "abstract" "polymorphism"
                    "inheritance" "encapsulation" "namespace" "dependency"
                    "deployment" "container" "middleware" "endpoint" "serializer"
                    "asynchronous" "concurrent" "deadlock" "semaphore" "mutex"
                    "pipeline" "refactor" "linter" "formatter" "transpiler"
                    "bytecode" "operand" "register" "allocator" "garbage"
                    "collection" "profiler" "benchmark" "throughput" "latency"]))
  "Alist mapping domain names to vectors of domain-specific terms.")

(defvar touchtype--domain-selection nil
  "Currently selected domain for `domain-words' mode.")

;;;; Consolidated keyboard layout data

(defconst touchtype--keyboard-layouts
  '((qwerty . (:unlock-order "fjdkslahetniourgcmpbywvxqz"
               :left-hand "qwertasdfgzxcvb"
               :right-hand "yuiophjklnm"
               :rows ("qwertyuiop" "asdfghjkl" "zxcvbnm")
               :finger-map ((?q . left-pinky) (?a . left-pinky) (?z . left-pinky)
                             (?w . left-ring)  (?s . left-ring)  (?x . left-ring)
                             (?e . left-middle) (?d . left-middle) (?c . left-middle)
                             (?r . left-index) (?f . left-index) (?v . left-index)
                             (?t . left-index) (?g . left-index) (?b . left-index)
                             (?y . right-index) (?h . right-index) (?n . right-index)
                             (?u . right-index) (?j . right-index) (?m . right-index)
                             (?i . right-middle) (?k . right-middle)
                             (?o . right-ring) (?l . right-ring)
                             (?p . right-pinky))))
    (dvorak . (:unlock-order "uhetonasidrljgcmfpbkwvyxqz"
               :left-hand "aoeuidhtns"
               :right-hand "qjkxbmwvzypfgcrl"
               :rows ("pyfgcrl" "aoeuidhtns" "qjkxbmwvz")
               :finger-map ((?p . left-pinky) (?a . left-pinky) (?q . left-pinky)
                             (?y . left-ring) (?o . left-ring) (?j . left-ring)
                             (?f . left-middle) (?e . left-middle) (?k . left-middle)
                             (?g . left-index) (?u . left-index) (?x . left-index)
                             (?c . left-index) (?i . left-index) (?b . left-index)
                             (?r . right-index) (?d . right-index) (?m . right-index)
                             (?l . right-index) (?h . right-index) (?w . right-index)
                             (?t . right-middle) (?n . right-middle) (?v . right-middle)
                             (?s . right-ring) (?z . right-ring)
                             (?\; . right-pinky))))
    (colemak . (:unlock-order "neiostahrdlufywpgmcbkvxjqz"
                :left-hand "qwfpgarstdzxcvb"
                :right-hand "jluyneiohmk"
                :rows ("qwfpgjluy" "arstdhneio" "zxcvbkm")
                :finger-map ((?q . left-pinky) (?a . left-pinky) (?z . left-pinky)
                              (?w . left-ring) (?r . left-ring) (?x . left-ring)
                              (?f . left-middle) (?s . left-middle) (?c . left-middle)
                              (?p . left-index) (?t . left-index) (?v . left-index)
                              (?g . left-index) (?d . left-index) (?b . left-index)
                              (?j . right-index) (?h . right-index) (?k . right-index)
                              (?l . right-index) (?n . right-index) (?m . right-index)
                              (?u . right-middle) (?e . right-middle)
                              (?y . right-ring) (?i . right-ring)
                              (?\; . right-pinky) (?o . right-pinky))))
    (workman . (:unlock-order "nehtosaidruljgcmfpbkwvyxqz"
                :left-hand "qdrwbashtgzxmcv"
                :right-hand "jfupneioylk"
                :rows ("qdrwbjfup" "ashtgyneoi" "zxmcvkl")
                :finger-map ((?q . left-pinky) (?a . left-pinky) (?z . left-pinky)
                              (?d . left-ring) (?s . left-ring) (?x . left-ring)
                              (?r . left-middle) (?h . left-middle) (?m . left-middle)
                              (?w . left-index) (?t . left-index) (?c . left-index)
                              (?b . left-index) (?g . left-index) (?v . left-index)
                              (?j . right-index) (?y . right-index) (?k . right-index)
                              (?f . right-index) (?n . right-index) (?l . right-index)
                              (?u . right-middle) (?e . right-middle)
                              (?p . right-ring) (?o . right-ring) (?i . right-ring)))))
  "Master keyboard layout data.
Each entry is (LAYOUT . plist) with keys :unlock-order, :left-hand,
:right-hand, :rows, and :finger-map.")

(defun touchtype--layout-get (layout key)
  "Get KEY from LAYOUT in `touchtype--keyboard-layouts'.
Falls back to qwerty if LAYOUT is not found."
  (let ((data (cdr (or (assq layout touchtype--keyboard-layouts)
                       (assq 'qwerty touchtype--keyboard-layouts)))))
    (plist-get data key)))

;; Backward-compatible aliases (derived from master data)
(defconst touchtype--qwerty-unlock-order (touchtype--layout-get 'qwerty :unlock-order))
(defconst touchtype--dvorak-unlock-order (touchtype--layout-get 'dvorak :unlock-order))
(defconst touchtype--colemak-unlock-order (touchtype--layout-get 'colemak :unlock-order))
(defconst touchtype--workman-unlock-order (touchtype--layout-get 'workman :unlock-order))
(defconst touchtype--qwerty-left-hand (touchtype--layout-get 'qwerty :left-hand))
(defconst touchtype--qwerty-right-hand (touchtype--layout-get 'qwerty :right-hand))
(defconst touchtype--dvorak-left-hand (touchtype--layout-get 'dvorak :left-hand))
(defconst touchtype--dvorak-right-hand (touchtype--layout-get 'dvorak :right-hand))
(defconst touchtype--colemak-left-hand (touchtype--layout-get 'colemak :left-hand))
(defconst touchtype--colemak-right-hand (touchtype--layout-get 'colemak :right-hand))
(defconst touchtype--workman-left-hand (touchtype--layout-get 'workman :left-hand))
(defconst touchtype--workman-right-hand (touchtype--layout-get 'workman :right-hand))
(defconst touchtype--qwerty-keyboard-rows (touchtype--layout-get 'qwerty :rows))
(defconst touchtype--dvorak-keyboard-rows (touchtype--layout-get 'dvorak :rows))
(defconst touchtype--colemak-keyboard-rows (touchtype--layout-get 'colemak :rows))
(defconst touchtype--workman-keyboard-rows (touchtype--layout-get 'workman :rows))
(defconst touchtype--qwerty-finger-map (touchtype--layout-get 'qwerty :finger-map))
(defconst touchtype--dvorak-finger-map (touchtype--layout-get 'dvorak :finger-map))
(defconst touchtype--colemak-finger-map (touchtype--layout-get 'colemak :finger-map))
(defconst touchtype--workman-finger-map (touchtype--layout-get 'workman :finger-map))

;;;; Heatmap faces

(defface touchtype-face-heatmap-cold
  '((((class color) (background dark))
     :foreground "#6699cc")
    (((class color) (background light))
     :foreground "#336699")
    (t :foreground "blue"))
  "Face for heatmap keys with no data or very low confidence."
  :group 'touchtype)

(defface touchtype-face-heatmap-struggling
  '((((class color) (background dark))
     :foreground "#ff6666")
    (((class color) (background light))
     :foreground "#cc3333")
    (t :foreground "red"))
  "Face for heatmap keys with confidence < 0.3."
  :group 'touchtype)

(defface touchtype-face-heatmap-developing
  '((((class color) (background dark))
     :foreground "#ffcc00")
    (((class color) (background light))
     :foreground "#cc9900")
    (t :foreground "yellow"))
  "Face for heatmap keys with confidence 0.3-0.6."
  :group 'touchtype)

(defface touchtype-face-heatmap-good
  '((((class color) (background dark))
     :foreground "#4ade80")
    (((class color) (background light))
     :foreground "#16a34a")
    (t :foreground "green"))
  "Face for heatmap keys with confidence >= 0.6."
  :group 'touchtype)

;;;; Per-finger maps (derived from master layout data above)

(defconst touchtype--finger-names
  '((left-pinky  . "L Pinky")
    (left-ring   . "L Ring")
    (left-middle . "L Middle")
    (left-index  . "L Index")
    (right-index . "R Index")
    (right-middle . "R Middle")
    (right-ring  . "R Ring")
    (right-pinky . "R Pinky"))
  "Display names for finger symbols.")

(defvar touchtype--finger-selection 'left-index
  "Currently selected finger for `finger-drill' mode.")

;;;; Rolling average window

(defcustom touchtype-rolling-average-window 10
  "Number of recent sessions to use for rolling average in delta display."
  :type 'integer
  :group 'touchtype)

;;;; Achievement definitions

(defconst touchtype--achievements
  '((:id first-session :name "First Steps" :desc "Complete your first typing session")
    (:id speed-30 :name "Warming Up" :desc "Reach 30 WPM in a session")
    (:id speed-40 :name "Steady Pace" :desc "Reach 40 WPM in a session")
    (:id speed-50 :name "Cruising" :desc "Reach 50 WPM in a session")
    (:id speed-60 :name "Quick Fingers" :desc "Reach 60 WPM in a session")
    (:id speed-70 :name "Speed Demon" :desc "Reach 70 WPM in a session")
    (:id speed-80 :name "Velocity" :desc "Reach 80 WPM in a session")
    (:id speed-100 :name "Lightning Fingers" :desc "Reach 100 WPM in a session")
    (:id speed-120 :name "Supersonic" :desc "Reach 120 WPM in a session")
    (:id speed-150 :name "Hyperspeed" :desc "Reach 150 WPM in a session")
    (:id accuracy-95 :name "Sharpshooter" :desc "Achieve 95% accuracy in a session")
    (:id accuracy-99 :name "Precision Master" :desc "Achieve 99% accuracy in a session")
    (:id accuracy-100 :name "Perfectionist" :desc "Achieve 100% accuracy in a session")
    (:id streak-7 :name "Week Warrior" :desc "Maintain a 7-day practice streak")
    (:id streak-14 :name "Fortnight Fighter" :desc "Maintain a 14-day practice streak")
    (:id streak-30 :name "Monthly Master" :desc "Maintain a 30-day practice streak")
    (:id streak-60 :name "Two Month Titan" :desc "Maintain a 60-day practice streak")
    (:id streak-100 :name "Century Streak" :desc "Maintain a 100-day practice streak")
    (:id sessions-10 :name "Getting Started" :desc "Complete 10 sessions")
    (:id sessions-25 :name "Quarter Century" :desc "Complete 25 sessions")
    (:id sessions-50 :name "Dedicated" :desc "Complete 50 sessions")
    (:id sessions-100 :name "Centurion" :desc "Complete 100 sessions")
    (:id sessions-200 :name "Bicentennial" :desc "Complete 200 sessions")
    (:id sessions-500 :name "Half Millennium" :desc "Complete 500 sessions")
    (:id all-keys :name "Full Keyboard" :desc "Unlock all 26 keys in progressive mode")
    (:id practice-1h :name "Hour of Power" :desc "Accumulate 1 hour of practice")
    (:id practice-5h :name "Five Hour Club" :desc "Accumulate 5 hours of practice")
    (:id practice-10h :name "Tenacious" :desc "Accumulate 10 hours of practice")
    (:id practice-25h :name "Day of Typing" :desc "Accumulate 25 hours of practice")
    (:id practice-50h :name "Typing Enthusiast" :desc "Accumulate 50 hours of practice")
    (:id practice-100h :name "Typing Devotee" :desc "Accumulate 100 hours of practice")
    (:id progressive-10 :name "Progressive Pro" :desc "Complete 10 progressive sessions")
    (:id full-words-10 :name "Word Smith" :desc "Complete 10 full-words sessions")
    (:id code-10 :name "Code Monkey" :desc "Complete 10 code sessions")
    (:id iron-fingers :name "Iron Fingers" :desc "Complete 5 expert-difficulty sessions")
    (:id marathon :name "Marathon Runner" :desc "Complete a 120+ word session")
    (:id consistency-king :name "Consistency King" :desc ">90% consistency for 10 sessions")
    (:id perfect-line :name "Flawless" :desc "Type a line at >80 WPM with 0 errors" :hidden t)
    (:id night-owl :name "Night Owl" :desc "Practice after midnight" :hidden t)
    (:id early-bird :name "Early Bird" :desc "Practice before 6 AM" :hidden t))
  "List of achievement plists with :id, :name, and :desc.
Hidden achievements have :hidden t and are not shown until earned.")

;;;; XP and level thresholds

(defconst touchtype--xp-level-thresholds
  [0 100 250 500 850 1300 1900 2600 3500 4600
   5900 7500 9400 11600 14200 17200 20700 24700 29300 34500
   40400 47000 54400 62600 71700 81800]
  "XP thresholds for each level (26 levels, index = level).")

(defconst touchtype--level-titles
  ["Novice" "Beginner" "Apprentice" "Learner" "Student"
   "Practitioner" "Adept" "Skilled" "Proficient" "Expert"
   "Veteran" "Master" "Grandmaster" "Elite" "Champion"
   "Virtuoso" "Prodigy" "Sage" "Legend" "Mythic"
   "Transcendent" "Ascendant" "Paragon" "Titan" "Apex" "Pinnacle"]
  "Title for each level (26 levels).")

;;;; Difficulty tiers

(defcustom touchtype-difficulty 'normal
  "Difficulty tier for typing sessions.
`normal': errors shown but typing continues.
`expert': session fails if a word has errors (checked at word boundary).
`master': session fails on the first incorrect keystroke."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Expert" expert)
                 (const :tag "Master" master))
  :group 'touchtype)

;;;; Pause state

(defvar touchtype--paused nil
  "Non-nil when the session is paused.")

(defvar touchtype--pause-start-time nil
  "Float-time when the current pause began.")

(defvar touchtype--pause-overlay nil
  "Overlay displaying the PAUSED message.")

(defvar touchtype--word-streak 0
  "Consecutive correctly-typed words in current session.")

(defvar touchtype--best-word-streak 0
  "Best word streak this session.")

(defvar touchtype--perfect-line-achieved nil
  "Non-nil if a perfect line was typed this session.")

;;;; Quote passage state

(defvar touchtype--quote-passage nil
  "The current quote passage text being typed.")

(defvar touchtype--quote-offset 0
  "Current read offset into `touchtype--quote-passage'.")

;;;; Buffer-local state variables
;; These are set buffer-local in the *touchtype* buffer.

(defvar touchtype--current-text ""
  "The target text string for the current line.")

(defvar touchtype--typed-chars nil
  "List of (CHAR CORRECT-P ELAPSED-MS) records for the current line.")

(defvar touchtype--cursor-pos 0
  "Zero-based index of the current typing position in `touchtype--current-text'.")

(defvar touchtype--line-start-time nil
  "Float-time timestamp when the current line began.")

(defvar touchtype--last-key-time nil
  "Float-time timestamp of the most recent keypress.")

(defvar touchtype--unlocked-keys "fj"
  "String of letters currently available in progressive mode.")

(defvar touchtype--focused-key nil
  "The most recently unlocked key character, or nil.")

(defvar touchtype--session-wpm-samples nil
  "List of WPM values recorded during the current session.")

(defvar touchtype--session-errors 0
  "Total error count for the current session (never decremented by backspace).")

(defvar touchtype--session-total-keys 0
  "Total keypresses in the current session.
Never decremented by backspace.")

(defvar touchtype--session-word-count 0
  "Number of words typed so far in the current session.")

(defvar touchtype--target-start nil
  "Buffer marker at the start of the target text region.")


(defvar touchtype--status-start nil
  "Buffer marker at the start of the status region.")

(defvar touchtype--cursor-overlay nil
  "Overlay used to highlight the current cursor position.")

(defvar touchtype--char-overlays nil
  "Vector of per-character overlays in the typed feedback region.")

(defvar touchtype--session-corrections 0
  "Total backspace/word-backspace presses in the current session.")

(defvar touchtype--session-start-time nil
  "Float-time timestamp when the current session started.")

(defvar touchtype--session-line-wpms nil
  "List of per-line WPM values for consistency calculation.")

(defvar touchtype--narrative-passage nil
  "The current narrative passage text being typed.")

(defvar touchtype--narrative-offset 0
  "Current read offset into `touchtype--narrative-passage'.")

(defvar touchtype--custom-passage nil
  "The current custom passage text being typed.")

(defvar touchtype--custom-offset 0
  "Current read offset into `touchtype--custom-passage'.")

(defvar touchtype--session-timer nil
  "Timer object for timed sessions.")

(defvar touchtype--session-idle-time 0.0
  "Accumulated idle time in seconds excluded from WPM calculations.")

(defvar touchtype--preview-texts nil
  "List of upcoming line strings for multi-line preview.")

(defvar touchtype--pace-timer nil
  "Timer for the pace caret.")

(defvar touchtype--pace-pos 0
  "Current position of the pace caret.")

(defvar touchtype--pace-overlay nil
  "Overlay for the pace caret.")

(defvar touchtype--completed-lines nil
  "List of propertized strings for previously completed lines.
Displayed above the active typing line, most recent first.")

(provide 'touchtype-var)

;;; touchtype-var.el ends here
