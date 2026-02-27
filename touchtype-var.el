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
                 (const :tag "Code" code))
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

(defcustom touchtype-common-words-count 100
  "Number of top common words to use in `common-words' mode."
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

;;;; Faces

(defface touchtype-face-untyped
  '((t :inherit shadow))
  "Face for characters in the target line not yet typed."
  :group 'touchtype)

(defface touchtype-face-correct
  '((((class color) (background dark))
     :foreground "#a855f7" :weight bold)
    (((class color) (background light))
     :foreground "#7c3aed" :weight bold)
    (t :foreground "purple" :weight bold))
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

(defface touchtype-face-pace-caret
  '((((class color) (background dark))
     :background "#333333")
    (((class color) (background light))
     :background "#e0e0e0")
    (t :inverse-video t))
  "Face for the pace caret overlay."
  :group 'touchtype)

;;;; Constants

(defconst touchtype--qwerty-unlock-order
  "fjdkslahetniourGcmpbywvxqz"
  "Order in which keys are progressively unlocked.
Home-row index/middle/ring/pinky keys first, then by English
letter frequency.  All 26 lower-case letters are represented.")

(defconst touchtype--dvorak-unlock-order
  "uhetonasidrljgcmfpbkwvyxqz"
  "Dvorak layout progressive unlock order.")

(defconst touchtype--colemak-unlock-order
  "neiostahrdlufywpgmcbkvxjqz"
  "Colemak layout progressive unlock order.")

(defconst touchtype--workman-unlock-order
  "nehtosaidrljgcmfpbkwvyxqz"
  "Workman layout progressive unlock order.")

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

(defconst touchtype--code-snippets
  ["def main():" "return None" "if x > 0:" "for i in range(n):"
   "while True:" "import os" "class Foo:" "self.value = x"
   "fn main() {" "let mut v = Vec::new();" "match x {" "impl Trait for S {"
   "pub fn new() -> Self {" "println!(\"hello\");" "Ok(())"
   "func main() {" "if err != nil {" "fmt.Println(x)" "go func() {"
   "defer f.Close()" "const x = 42;" "let arr = [1, 2, 3];"
   "console.log(x);" "async function f() {" "return new Promise();"
   "export default {" "(defun foo (x)" "(let ((a 1)))" "(setq x 42)"
   "(lambda (x) (* x x))" "#!/bin/bash" "if [ -f $1 ]; then"
   "echo \"$HOME\"" "grep -r 'TODO'" "SELECT * FROM users;"
   "INSERT INTO t (a, b)" "CREATE TABLE t (" "WHERE id = $1"
   "int main() {" "printf(\"%d\\n\", x);" "#include <stdio.h>"
   "struct Node {" "void *ptr = NULL;"]
  "Vector of common code snippets for `code' typing mode.")

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
  "Total number of keypresses in the current session (never decremented by backspace).")

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
