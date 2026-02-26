;;; touchtype-var.el --- Variables and constants for touchtype -*- lexical-binding: t; -*-

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

;; All defcustom, defconst, defface, and defvar declarations for
;; the touchtype package.

;;; Code:

(require 'cl-lib)

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

(defcustom touchtype-session-length 30
  "Number of words per training session."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-mode-selection 'progressive
  "Current training mode symbol.
One of: `progressive', `full-words', `bigram-drill', `letters',
`letters+numbers', or `letters+numbers+symbols'."
  :type '(choice (const :tag "Progressive" progressive)
                 (const :tag "Full Words" full-words)
                 (const :tag "Bigram Drill" bigram-drill)
                 (const :tag "Letters" letters)
                 (const :tag "Letters+Numbers" letters+numbers)
                 (const :tag "Letters+Numbers+Symbols" letters+numbers+symbols))
  :group 'touchtype)

(defcustom touchtype-word-length-min 4
  "Minimum length of generated pseudo-words."
  :type 'integer
  :group 'touchtype)

(defcustom touchtype-word-length-max 8
  "Maximum length of generated pseudo-words."
  :type 'integer
  :group 'touchtype)

;;;; Faces

(defface touchtype-face-untyped
  '((t :inherit shadow))
  "Face for characters in the target line not yet typed."
  :group 'touchtype)

(defface touchtype-face-correct
  '((t :inherit default))
  "Face for correctly typed characters (normal foreground color)."
  :group 'touchtype)

(defface touchtype-face-wrong
  '((t :foreground "red"))
  "Face for incorrectly typed characters."
  :group 'touchtype)

(defface touchtype-face-cursor
  '((t :underline t :inherit default))
  "Face for the current typing-cursor position."
  :group 'touchtype)

(defface touchtype-face-status
  '((t :inherit mode-line-inactive))
  "Face for the status line at the bottom of the touchtype buffer."
  :group 'touchtype)

;;;; Constants

(defconst touchtype--qwerty-unlock-order
  "fjdkslahetniourGcmpbywvxqz"
  "Order in which keys are progressively unlocked.
Home-row index/middle/ring/pinky keys first, then by English
letter frequency.  All 26 lower-case letters are represented.")

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
    "of" "it" "al" "as" "ha" "hi" "ng" "se" "ou" "le")
  "Ordered list of the 30 most common English bigrams.
Used by `touchtype-algo-bigram-line' for bigram-drill mode.")

(defconst touchtype--builtin-words
  (vconcat
   ["the" "be" "to" "of" "and" "in" "that" "have" "it" "for"
    "not" "on" "with" "he" "as" "you" "do" "at" "this" "but"
    "his" "by" "from" "they" "we" "say" "her" "she" "or" "an"
    "will" "my" "one" "all" "would" "there" "their" "what" "so"
    "up" "out" "if" "about" "who" "get" "which" "go" "me" "when"
    "make" "can" "like" "time" "no" "just" "him" "know" "take"
    "people" "into" "year" "your" "good" "some" "could" "them"
    "see" "other" "than" "then" "now" "look" "only" "come" "over"
    "think" "also" "back" "after" "use" "two" "how" "our" "work"
    "first" "well" "way" "even" "new" "want" "any" "these" "give"
    "day" "most" "us" "great" "need" "large" "often" "hand" "high"
    "place" "hold" "turn" "here" "why" "ask" "went" "read" "land"
    "form" "end" "does" "another" "those" "found" "both" "same"
    "tell" "every" "near" "run" "small" "number" "off" "always"
    "move" "point" "play" "away" "call" "few" "got" "part" "big"
    "change" "world" "home" "city" "right" "tree" "start" "plant"
    "night" "feet" "story" "life" "light" "earth" "eyes" "water"
    "name" "boy" "follow" "came" "show" "around" "air" "kind"
    "live" "still" "man" "long" "much" "word" "help" "each"
    "answer" "open" "seem" "learn" "far" "sit" "north" "music"
    "page" "draw" "yet" "while" "did" "many" "until" "side"
    "stop" "food" "keep" "face" "seen" "again" "miss" "hard"
    "river" "car" "care" "second" "book" "carry" "took" "eat"
    "room" "friend" "began" "idea" "fish" "once" "base" "hear"
    "horse" "cut" "sure" "watch" "color" "main" "enough" "girl"
    "ready" "above" "ever" "red" "list" "though" "feel" "talk"
    "bird" "soon" "body" "dog" "family" "leave" "song" "door"
    "black" "short" "class" "wind" "question" "happen" "ship"
    "area" "half" "rock" "order" "fire" "south" "piece" "told"
    "knew" "pass" "since" "top" "whole" "king" "space" "heard"
    "best" "hour" "better" "during" "five" "step" "early" "west"
    "ground" "reach" "fast" "sing" "six" "table" "travel" "less"
    "morning" "ten" "simple" "toward" "war" "lay" "against" "slow"
    "center" "love" "money" "road" "map" "rain" "rule" "pull"
    "cold" "voice" "power" "town" "drive" "dark" "note" "wait"
    "plan" "star" "field" "rest" "done" "front" "teach" "week"
    "gave" "green" "ocean" "warm" "free" "minute" "strong" "mind"
    "clear" "fact" "street" "course" "stay" "wheel" "full" "force"
    "blue" "surface" "deep" "moon" "island" "foot" "system" "test"
    "record" "boat" "common" "gold" "plane" "dry" "wonder" "laugh"
    "thousand" "ago" "ran" "check" "game" "shape" "hot" "brought"
    "heat" "snow" "fill" "east" "paint" "among" "ball" "wave"
    "drop" "heart" "present" "heavy" "dance" "engine" "arm" "wide"
    "sail" "settle" "speak" "weight" "ice" "matter" "circle"
    "pair" "include" "divide" "felt" "pick" "count" "square"
    "reason" "art" "energy" "hunt" "bed" "brother" "egg" "ride"
    "cell" "forest" "window" "store" "summer" "train" "sleep"
    "prove" "leg" "wall" "catch" "wish" "sky" "board" "joy"
    "winter" "sat" "wild" "glass" "grass" "cow" "job" "edge"
    "sign" "visit" "past" "soft" "bright" "gas" "weather" "month"
    "bear" "finish" "flower" "strange" "trade" "trip" "office"
    "row" "mouth" "exact" "symbol" "die" "least" "trouble" "shout"
    "except" "wrote" "seed" "tone" "join" "clean" "break" "lady"
    "yard" "rise" "bad" "blow" "blood" "touch" "grew" "cent" "mix"
    "team" "wire" "cost" "lost" "brown" "wear" "garden" "equal"
    "sent" "choose" "fell" "fit" "flow" "fair" "bank" "collect"
    "save" "control" "gentle" "woman" "captain" "practice" "doctor"
    "please" "protect" "noon" "locate" "ring" "insect" "caught"
    "period" "radio" "spoke" "atom" "human" "history" "expect"
    "crop" "modern" "element" "student" "corner" "party" "supply"
    "bone" "rail" "imagine" "provide" "agree" "chair" "danger"
    "fruit" "rich" "thick" "soldier" "process" "operate" "guess"
    "sharp" "wing" "create" "wash" "bat" "crowd" "corn" "compare"
    "poem" "string" "bell" "depend" "meat" "tube" "famous" "dollar"
    "stream" "fear" "sight" "thin" "planet" "hurry" "clock" "mine"
    "enter" "major" "fresh" "search" "send" "yellow" "allow"
    "print" "dead" "spot" "suit" "current" "lift" "rose" "block"
    "chart" "hat" "sell" "company" "deal" "swim" "term" "wife"
    "shoe" "camp" "cotton" "born" "truck" "level" "chance" "gather"
    "shop" "throw" "shine" "column" "select" "wrong" "repeat"
    "broad" "salt" "nose" "anger" "claim" "value" "result"
    "able" "about" "above" "across" "after" "again" "age" "agree"
    "air" "allow" "also" "always" "among" "area" "arise" "ask"
    "back" "ball" "band" "base" "bear" "beat" "begin" "behind"
    "believe" "below" "best" "better" "between" "big" "black"
    "blue" "board" "body" "book" "born" "break" "bring" "broad"
    "build" "burn" "call" "came" "carry" "case" "cause" "center"
    "change" "charge" "child" "circle" "city" "claim" "class"
    "clear" "close" "cold" "collect" "come" "compare" "complete"
    "contain" "control" "copy" "correct" "cost" "country" "cover"
    "create" "cross" "cut" "dark" "deal" "deep" "describe"
    "develop" "differ" "direct" "discover" "double" "draw" "dream"
    "drive" "drop" "early" "east" "effect" "else" "energy" "enjoy"
    "enter" "equal" "even" "event" "every" "exact" "fall" "fast"
    "fear" "feel" "figure" "fill" "find" "fish" "fit" "flow"
    "flower" "follow" "force" "form" "free" "full" "gain" "general"
    "give" "glass" "gold" "good" "green" "grow" "hand" "head"
    "heart" "high" "hold" "home" "hope" "horse" "idea" "inch"
    "island" "keep" "kind" "land" "large" "last" "learn" "less"
    "letter" "level" "lift" "line" "list" "live" "long" "look"
    "love" "main" "map" "matter" "mean" "meet" "mind" "miss"
    "month" "moon" "more" "most" "mountain" "move" "much" "near"
    "never" "next" "north" "note" "now" "object" "often" "only"
    "open" "order" "other" "over" "own" "paint" "pass" "past"
    "peace" "pick" "place" "plain" "plan" "play" "point" "power"
    "push" "quick" "quiet" "reach" "real" "reason" "rest" "rich"
    "right" "rise" "road" "rule" "safe" "same" "seem" "send"
    "serve" "seven" "show" "sign" "size" "sky" "slow" "small"
    "sound" "south" "speak" "spend" "start" "stay" "stop" "story"
    "strong" "such" "sure" "take" "talk" "teach" "than" "think"
    "together" "too" "town" "travel" "tree" "true" "turn" "under"
    "until" "use" "very" "visit" "voice" "walk" "want" "warm"
    "wave" "well" "west" "wild" "wind" "wish" "wood" "write"
    "young"])
  "Vector of ~500 common English words for `full-words' mode.
Words are selected for variety of letter patterns and are
suitable for typing practice.")

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
  "Total error count for the current session.")

(defvar touchtype--session-word-count 0
  "Number of words typed so far in the current session.")

(defvar touchtype--target-start nil
  "Buffer marker at the start of the target text region.")

(defvar touchtype--typed-start nil
  "Buffer marker at the start of the typed feedback region.")

(defvar touchtype--status-start nil
  "Buffer marker at the start of the status region.")

(defvar touchtype--cursor-overlay nil
  "Overlay used to highlight the current cursor position.")

(defvar touchtype--char-overlays nil
  "Vector of per-character overlays in the typed feedback region.")

(provide 'touchtype-var)

;;; touchtype-var.el ends here
