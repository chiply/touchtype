# touchtype

A progressive touch typing trainer for Emacs, inspired by [keybr.com](https://www.keybr.com/).

**Requires**: Emacs 29.1+
**License**: GPL-3.0-or-later

## Overview

touchtype is a self-contained typing tutor that runs inside Emacs. It generates
practice text from an embedded English [bigram frequency table](#bigram-frequency-table),
tracks per-key speed and accuracy across sessions, and progressively unlocks new
keys as your confidence grows. Thirteen training modes cover everything from
home-row drills to real prose from [Project Gutenberg](https://www.gutenberg.org/)
to code snippets in eight languages. Statistics are persisted to disk and can be
exported as JSON or CSV.

### Quick start

```
M-x touchtype-progressive   start progressive training (recommended)
M-x touchtype-full-words    practice with real English words
M-x touchtype-narrative     type real prose from classic literature
M-x touchtype-code          practice code snippets
M-x touchtype-stats-view    show your all-time statistics
```

All word-count entry points accept a prefix argument to override the session
length: `C-u 50 M-x touchtype-progressive` starts a 50-word session.
`M-x touchtype-timed` accepts a prefix argument for the duration in seconds.

---

## Table of Contents

- [Installation](#installation)
- [Training Modes](#training-modes)
  - [Progressive](#progressive)
  - [Full Words](#full-words)
  - [Bigram Drill](#bigram-drill)
  - [Trigram Drill](#trigram-drill)
  - [Tetragram Drill](#tetragram-drill)
  - [N-gram Drill (Mixed)](#n-gram-drill-mixed)
  - [Letters](#letters)
  - [Letters + Numbers](#letters--numbers)
  - [Letters + Numbers + Symbols](#letters--numbers--symbols)
  - [Narrative](#narrative)
  - [Common Words](#common-words)
  - [Custom](#custom)
  - [Code](#code)
- [Session Types](#session-types)
- [Keybindings](#keybindings)
  - [Training Buffer](#training-buffer)
  - [Session End Screen](#session-end-screen)
  - [Statistics View](#statistics-view)
- [Live Session Display](#live-session-display)
  - [Buffer Layout](#buffer-layout)
  - [Status Line Metrics](#status-line-metrics)
  - [Pace Caret](#pace-caret)
- [Session End Summary](#session-end-summary)
- [Statistics View](#statistics-view-1)
- [Algorithms and Data Sources](#algorithms-and-data-sources)
  - [Bigram Frequency Table](#bigram-frequency-table)
  - [Pseudo-word Generation](#pseudo-word-generation)
  - [Confidence Score](#confidence-score)
  - [Key Unlock Logic](#key-unlock-logic)
  - [N-gram Sources](#n-gram-sources)
  - [Word List](#word-list)
  - [Narrative Sources](#narrative-sources)
  - [Code Snippets](#code-snippets)
- [Error Modes](#error-modes)
- [Keyboard Layouts](#keyboard-layouts)
- [Customization](#customization)
- [Data Export](#data-export)
- [File Structure](#file-structure)

---

## Installation

### From MELPA (once available)

```
M-x package-install RET touchtype RET
```

### Manual / straight.el

```elisp
(straight-use-package
 '(touchtype :host github :repo "chiply/touchtype"))
```

### Elpaca

```elisp
(use-package touchtype
  :ensure (:host github :repo "chiply/touchtype"))
```

---

## Training Modes

### Progressive

**Command**: `M-x touchtype-progressive`

The flagship mode. Inspired by [keybr.com](https://www.keybr.com/), it starts
with just two home-row keys (F and J) and unlocks additional keys one at a time
as your speed and accuracy improve. This forces deliberate practice on unfamiliar
keys rather than falling back to comfortable ones.

**How it works**:

1. The session begins with only the keys in your unlocked set (persisted across
   sessions, default `fj`).
2. Practice text is generated as [pseudo-words](#pseudo-word-generation) from the
   [bigram frequency table](#bigram-frequency-table), filtered to your unlocked
   keys. The text feels like English even with a limited alphabet.
3. The most recently unlocked key is the **focused character** -- it appears in
   ~40% of generated words. Additionally, ~30% of the time a random **weak
   letter** (below the confidence threshold) is focused instead, ensuring
   struggling keys get extra practice.
4. After each completed line, the system checks whether **every** unlocked key
   has reached the [confidence threshold](#confidence-score) (default 0.80). If
   so, the next key in the [unlock order](#keyboard-layouts) is added.
5. A message announces each unlock: *"Unlocked new key: s  Keep typing to build
   confidence!"*

The unlock order follows your [keyboard layout](#keyboard-layouts), starting from
home-row index fingers and spiraling outward by English letter frequency.

### Full Words

**Command**: `M-x touchtype-full-words`

Uses all 26 letters. Draws from a [built-in list](#word-list) of ~4,258 real
English words. If fewer than 15 words in the list match the currently allowed
character set (relevant when combined with progressive unlock state), it falls
back to [pseudo-word generation](#pseudo-word-generation).

### Bigram Drill

**Command**: `M-x touchtype-bigram-drill`

Repeats common English [bigrams](#n-gram-sources) (two-letter pairs like `th`,
`he`, `in`). A single bigram is chosen per line and repeated to fill ~75
characters. Bigrams containing characters not yet in your unlocked set are
filtered out, making this mode compatible with progressive unlock state.

Example line: `th th th th th th th th th th th th th`

### Trigram Drill

**Command**: `M-x touchtype-trigram-drill`

Same format as bigram drill but uses common English
[trigrams](#n-gram-sources) (`the`, `and`, `ing`, etc.).

### Tetragram Drill

**Command**: `M-x touchtype-tetragram-drill`

Same format using common English [tetragrams](#n-gram-sources) (`that`, `ther`,
`with`, etc.).

### N-gram Drill (Mixed)

**Command**: `M-x touchtype-ngram-drill`

Each line randomly selects from bigrams, trigrams, or tetragrams, providing
varied n-gram practice within a single session.

### Letters

**Command**: `M-x touchtype-letters`

All 26 lowercase letters. Generates [pseudo-words](#pseudo-word-generation) from
the full alphabet using the bigram table.

### Letters + Numbers

**Command**: `M-x touchtype-letters+numbers`

Letters plus digits 0-9. Pseudo-words may contain digits interspersed with
letters.

### Letters + Numbers + Symbols

**Command**: `M-x touchtype-letters+numbers+symbols`

Letters, digits, and common punctuation: `.,;'!?-`. Useful for practicing the
full range of characters found in everyday writing.

### Narrative

**Command**: `M-x touchtype-narrative`

Type real prose from classic literature. Passages are downloaded from
[Project Gutenberg](https://www.gutenberg.org/) and cached locally. Each session
picks a random book from the [configured list](#narrative-sources) and extracts a
random ~400-character passage, breaking it into lines of ~60 characters at word
boundaries. When a passage is exhausted, a new one is automatically fetched.

Narrative mode includes mixed case, digits, and punctuation, providing realistic
practice with the full range of characters found in published text.

### Common Words

**Command**: `M-x touchtype-common-words`

Draws from the top N most frequently used words in the [built-in word
list](#word-list). N defaults to 100 and is controlled by
`touchtype-common-words-count`. Words are chosen randomly and lines target ~70
characters.

### Custom

**Command**: `M-x touchtype-custom`

Type your own text. Accepts input three ways:

- **Prompt**: `M-x touchtype-custom` prompts for text when no region is active.
- **Region**: `M-x touchtype-region` uses the active region as source text.
- **Buffer**: `M-x touchtype-buffer` uses the entire current buffer.

Whitespace is normalized (newlines, tabs, and multiple spaces collapsed to single
spaces). The text is broken into lines of ~60 characters at word boundaries.

### Code

**Command**: `M-x touchtype-code`

Practice typing [code snippets](#code-snippets) from eight programming languages:
Python, Rust, Go, JavaScript/TypeScript, Emacs Lisp, Shell/Bash, SQL, and C.
Snippets are drawn randomly and separated by double spaces. Includes the full
printable ASCII range (characters 32-126).

---

## Session Types

### Word Count (default)

Set by `(setq touchtype-session-type 'words)`.

The session ends after a fixed number of words (default 30, set by
`touchtype-session-length`). The total words displayed on screen (active line +
preview lines) is capped to the remaining word budget, so you never see words you
won't type.

Presets are available via `M-x touchtype-set-session-length`:

| Preset   | Words |
|----------|-------|
| short    | 15    |
| medium   | 30    |
| long     | 60    |
| marathon | 120   |

### Timed

**Command**: `M-x touchtype-timed`

Set by `(setq touchtype-session-type 'timed)`.

The session runs for a fixed duration (default 60 seconds, set by
`touchtype-session-duration`). The timer starts on the **first keypress**, not
when the buffer opens. The status line shows a countdown. Prefix argument sets
duration: `C-u 120 M-x touchtype-timed` for a 2-minute session.

### Idle Time Handling

Both session types exclude idle time from WPM calculations. If the gap between
two keypresses exceeds `touchtype-idle-threshold` (default 10 seconds), the
entire gap is subtracted from elapsed time. This prevents bathroom breaks from
tanking your WPM.

---

## Keybindings

### Training Buffer

The training buffer uses a custom keymap that suppresses all default bindings.
Only the following keys are active:

| Key                                               | Action                                                                                                                                                                               |
|---------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Any printable character (space through `~`)       | Type the character; compared against the target. Correct = purple, wrong = red.                                                                                                      |
| `DEL` (Backspace)                                 | Delete the last typed character and rewind the cursor.                                                                                                                               |
| `M-DEL` / `C-backspace` / word-backspace variants | Delete back to the previous word boundary. Discovers bindings at runtime via `where-is-internal` for `backward-kill-word`, `evil-delete-backward-word`, and `subword-backward-kill`. |
| `TAB`                                             | Instantly restart the session (same mode and settings).                                                                                                                              |
| `RET`                                             | Silently ignored (prevents accidental line breaks).                                                                                                                                  |
| `C-g`                                             | Quit the session. Prompts for confirmation, saves statistics, and kills the buffer.                                                                                                  |

**Evil mode**: If [evil-mode](https://github.com/emacs-evil/evil) is active,
touchtype enters `evil-emacs-state` after switching to the buffer. A
`post-command-hook` keeps the cursor anchored in the typing area. The built-in
cursor is hidden (`cursor-type nil`) -- touchtype renders its own overlay-based
cursor.

### Session End Screen

| Key                 | Action                                                                       |
|---------------------|------------------------------------------------------------------------------|
| `RET` (Enter)       | Expand or collapse the section under point (weakest letters, bigrams, etc.). |
| `TAB`               | Jump to the next category header.                                            |
| `S-TAB` (Shift-Tab) | Jump to the previous category header.                                        |
| `r`                 | Restart a new session with the same settings.                                |
| `q`                 | Quit and kill the buffer.                                                    |

### Statistics View

| Key | Action                       |
|-----|------------------------------|
| `q` | Close the statistics buffer. |

---

## Live Session Display

### Buffer Layout

During a session, the buffer shows:

```
                          ← blank padding (keeps active line at ~1/3 screen height)

  already typed line      ← completed lines (correct chars in purple, errors in red)
  already typed line

  current target text_    ← active line (gray=untyped, purple=correct, red=wrong, _=cursor)

  upcoming preview text   ← preview lines (gray, no overlays)
  upcoming preview text

  Net: 42  Gross: 45  Acc: 96%  Consistency: 85%       ← status line 1
  Time: 1:23  Words: 15/30  Corrections: 3  Mode: progressive  Keys: fjdksl  ← status line 2
```

Completed lines preserve per-character coloring so you can scroll up and see
exactly where mistakes were made.

### Status Line Metrics

The status line updates after every keypress and shows:

| Metric          | Description                                                                                                                                                        |
|-----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Net**         | Net WPM: `((total_chars / 5) - uncorrected_errors) / minutes`. The standard typing test metric that penalizes uncorrected mistakes.                                |
| **Gross**       | Gross WPM: `(total_chars / 5) / minutes`. Raw speed without error penalty.                                                                                         |
| **Acc**         | Accuracy: `100 * (total_chars - errors) / total_chars`. Percentage of keypresses that matched the target.                                                          |
| **Consistency** | `100 - (stddev(line_wpms) / mean(line_wpms) * 100)`. How uniform your speed is across lines. 100% means perfectly consistent. Requires at least 2 completed lines. |
| **Time**        | Elapsed time (word-count mode: `m:ss`) or countdown (timed mode: `m:ss left`). Excludes idle periods.                                                              |
| **Words**       | Live word count. In word-count mode: `typed/total` (e.g. `15/30`). In timed mode: just the count. Updates word-by-word as you type, not just at line boundaries.   |
| **Corrections** | Number of backspace/word-backspace presses.                                                                                                                        |
| **Mode**        | Current training mode name.                                                                                                                                        |
| **Keys**        | *(Progressive mode only)* The currently unlocked key set.                                                                                                          |

### Pace Caret

When `touchtype-pace-caret` is non-nil, a ghost cursor advances through the
target text at `touchtype-target-wpm`, providing a visual pacing reference. The
pace caret starts on the first keypress of each line and moves at a constant rate
of `60 / (target_wpm * 5)` seconds per character. If you're ahead of the caret,
you're above target speed.

Enable: `(setq touchtype-pace-caret t)`

---

## Session End Summary

When a session completes (word count reached or timer expires), the buffer
displays a detailed summary:

### Speed

| Metric        | Description                                          |
|---------------|------------------------------------------------------|
| **Net WPM**   | `((total_chars / 5) - uncorrected_errors) / minutes` |
| **Gross WPM** | `(total_chars / 5) / minutes`                        |
| **Net CPM**   | Net characters per minute (`net_wpm * 5`)            |
| **Gross CPM** | Gross characters per minute (`gross_wpm * 5`)        |

### Accuracy

| Metric           | Description                                                                                                                                           |
|------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Accuracy**     | `100 * (total_chars - errors) / total_chars`. Does not count corrected mistakes against you.                                                          |
| **Raw Accuracy** | `100 * (total_chars - errors) / (total_chars + corrections)`. Includes corrected mistakes in the denominator, reflecting true first-attempt accuracy. |

### Session

| Metric                 | Description                                                                                    |
|------------------------|------------------------------------------------------------------------------------------------|
| **Time**               | Total elapsed time excluding idle periods (`mm:ss`).                                           |
| **Words**              | Total words typed in the session.                                                              |
| **Characters**         | Total keypresses (including incorrect ones).                                                   |
| **Corrections**        | Total backspace and word-backspace presses.                                                    |
| **Uncorrected errors** | `max(0, total_errors - corrections)`. Errors that were never fixed.                            |
| **Consistency**        | Coefficient of variation of per-line WPM values, subtracted from 100. Higher is better.        |
| **WPM Graph**          | Unicode sparkline (`▁▂▃▄▅▆▇█`) showing per-line WPM values with `(min - max)` range annotated. |
| **Streak**             | Current daily practice streak in days.                                                         |
| **Total Time**         | Cumulative practice time across all sessions, formatted as `Xh Ym Zs`.                         |

If your Net WPM exceeds your previous personal best for the current mode, a
**"New Personal Best!"** banner is displayed.

### Expandable Sections

The end screen includes collapsible drill-down sections (toggle with `Enter`):

| Section                | Default view                 | Expanded view         |
|------------------------|------------------------------|-----------------------|
| **Weakest Letters**    | Top 10 letters by confidence | All letters with data |
| **Weakest Bigrams**    | Top 5                        | Top 50                |
| **Weakest Trigrams**   | Top 5                        | Top 50                |
| **Weakest Tetragrams** | Top 5                        | Top 50                |

Each entry shows the letter/n-gram and its [confidence score](#confidence-score).

---

## Statistics View

**Command**: `M-x touchtype-stats-view`

Displays a comprehensive summary of all-time statistics in a read-only buffer.
Sections:

### Overall Summary

Total sessions played, total words typed, average WPM, average accuracy, current
daily streak, and cumulative practice time.

### Per-Letter Confidence

Every letter a-z sorted from weakest to strongest. Each entry shows:

```
  a  [||||||||..........] 0.42  (150 hits)
```

- A visual bar chart (20 characters wide) representing confidence 0.0-1.0
- The numeric confidence score
- Total hit count

Only letters with at least 1 hit are shown.

### Weakest Bigrams

Top 10 weakest bigrams (minimum 5 hits required) in the same bar chart format.

### Trends

- **WPM Trend**: Last N session WPM values (oldest to newest) with a direction
  indicator: `^` (improving), `v` (declining), or `-` (stable). Direction is
  computed by comparing the average of the first half vs. the second half of
  values; a difference greater than 2% triggers improving/declining.
- **Accuracy Trend**: Same format for accuracy percentages.

N defaults to `touchtype-stats-history-length` (20).

### Session History

Last N sessions in tabular format showing date, WPM, accuracy, mode, and word
count.

### Weakest N-grams

Separate sub-sections for the top 10 weakest trigrams and tetragrams (minimum 5
hits), in bar chart format.

### Personal Bests

Best WPM and best accuracy achieved per mode across all sessions.

---

## Algorithms and Data Sources

### Bigram Frequency Table

The core data structure powering pseudo-word generation is a 26x26 letter
transition table (`touchtype--bigram-table`) derived from English corpus
analysis. Each of the 26 lowercase letters maps to a weighted list of successor
characters with relative frequencies from 1 to 100. For example:

- `t` → `((h . 62) (e . 42) (i . 26) ...)` -- `th` is the strongest transition
- `q` → `((u . 95) (i . 5))` -- `q` nearly always leads to `u`

These frequencies are used for weighted random selection during pseudo-word
generation, producing text that follows English phonotactic patterns even with a
restricted alphabet.

See also: [English letter frequency](https://en.wikipedia.org/wiki/Letter_frequency),
[bigram frequency](https://en.wikipedia.org/wiki/Bigram#Bigram_frequency_in_the_English_language).

### Pseudo-word Generation

The algorithm (`touchtype-algo-generate-word`) works as follows:

**For alphabets > 4 characters (bigram-table method)**:

1. **Start character**: Selected by weighting each allowed character by its total
   outgoing bigram frequency (unigram approximation).
2. **Next character**: At each step, the next character is sampled from the
   bigram table row for the current character, filtered to allowed characters.
3. **Termination**: Word-end probability increases with length following an
   exponential curve: `p_end = min(80, round(10 * 1.3^(length - min_length)))`.
   This naturally produces words of 4-7 characters. Words are forced to end at
   `touchtype-word-length-max` (default 8).
4. **Focused character injection**: If a focused character is specified and the
   generated word doesn't contain it, there is a 40% chance it replaces a random
   position in the word.
5. **Retry**: Up to 5 attempts if generation fails (e.g., dead-end bigram
   transitions).

**For alphabets <= 4 characters (random-pick fallback)**:

With very few keys, the bigram table is too sparse. Instead, characters are
picked randomly with word lengths scaled down (2-4 characters for <= 3 keys, 3-5
for <= 5 keys). Three-in-a-row repetitions of the same character are prevented.

**Line generation**: Words are concatenated with spaces until the line reaches
~70 characters (60 for code mode). A trailing space is appended to each line so
the user must type space to advance to the next line.

### Confidence Score

The confidence score (0.0-1.0) for each character is computed using a formula
derived from [keybr.com](https://www.keybr.com/):

```
target_ms        = 60000 / (target_wpm * 5)
avg_ms           = total_ms / hits
speed_confidence = min(1.0, target_ms / avg_ms)
accuracy         = hits / (hits + misses)
confidence       = accuracy * speed_confidence
```

- **`target_wpm`**: Configurable via `touchtype-target-wpm` (default 40).
- **Speed confidence**: How close your average reaction time is to the target. If
  you're at or below the target time, speed confidence is 1.0.
- **Accuracy**: Simple hit rate for the character.
- **Combined**: The product ensures both speed AND accuracy must be high for a
  character to reach full confidence.

Returns 0.0 when no data exists for a character. The same formula is used for
n-gram confidence scoring.

### Key Unlock Logic

In progressive mode, key unlocks are checked after each completed line:

1. **Should unlock?** (`touchtype-algo-should-unlock-p`): Returns true if
   **every** character in `touchtype--unlocked-keys` has confidence >=
   `touchtype-unlock-threshold` (default 0.80).
2. **Unlock next** (`touchtype-algo-unlock-next-key`): Finds the first character
   in the [layout-specific unlock order](#keyboard-layouts) that is not yet
   unlocked, appends it to `touchtype--unlocked-keys`, and persists the change.
3. **Focus shift**: The newly unlocked key becomes `touchtype--focused-key`,
   ensuring it receives extra practice in generated text.
4. **Weak letter rotation** (`touchtype-algo--pick-focus-char`): During word
   generation, the focused character is chosen with 70% probability. 30% of the
   time, a random letter below the confidence threshold is focused instead. This
   prevents neglecting older keys while prioritizing the newest one.

### N-gram Sources

Three lists of 100 common English n-grams are embedded as constants:

| Type                 | Examples                                                                           | Source                                                                                                  |
|----------------------|------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------|
| **Bigrams** (100)    | `th`, `he`, `in`, `er`, `an`, `re`, `on`, `en`, `at`, `es` ...                     | [Common English bigrams](https://en.wikipedia.org/wiki/Bigram#Bigram_frequency_in_the_English_language) |
| **Trigrams** (100)   | `the`, `and`, `ing`, `her`, `hat`, `his`, `tha`, `ere`, `for`, `ent` ...           | [Common English trigrams](https://en.wikipedia.org/wiki/Trigram#Most_common_trigrams)                   |
| **Tetragrams** (100) | `that`, `ther`, `with`, `tion`, `here`, `ould`, `ight`, `have`, `hich`, `whic` ... | Common English four-letter sequences                                                                    |

N-grams are filtered to the unlocked key set at generation time. If no n-grams
survive filtering, the first entry in the list is used as a fallback.

During typing, n-gram statistics (hits, misses, timing) are recorded for every
bigram, trigram, and tetragram the user types, regardless of mode. These
statistics feed the weakest n-gram sections in the end-of-session summary and
statistics view.

### Word List

The built-in word list (`touchtype--builtin-words`) contains ~4,258 common
English words organized by length, ranging from 2-letter words (`act`, `add`) to
12+ letter words (`abbreviation`, `wholehearted`). Words were selected for
frequency of use in everyday English.

In **full-words** mode, words are filtered to the allowed character set. In
**common-words** mode, only the first `touchtype-common-words-count` (default
100) entries are used, representing the most common words.

### Narrative Sources

Narrative mode downloads and caches full texts from
[Project Gutenberg](https://www.gutenberg.org/). The default book list includes
20 public domain classics:

| ID    | Title                             | Author                   |
|-------|-----------------------------------|--------------------------|
| 1342  | Pride and Prejudice               | Jane Austen              |
| 11    | Alice's Adventures in Wonderland  | Lewis Carroll            |
| 1661  | The Adventures of Sherlock Holmes | Arthur Conan Doyle       |
| 84    | Frankenstein                      | Mary Shelley             |
| 1952  | The Yellow Wallpaper              | Charlotte Perkins Gilman |
| 174   | The Picture of Dorian Gray        | Oscar Wilde              |
| 345   | Dracula                           | Bram Stoker              |
| 1080  | A Modest Proposal                 | Jonathan Swift           |
| 16328 | Beowulf                           | Anonymous                |
| 98    | A Tale of Two Cities              | Charles Dickens          |
| 1260  | Jane Eyre                         | Charlotte Bronte         |
| 219   | Heart of Darkness                 | Joseph Conrad            |
| 2701  | Moby Dick                         | Herman Melville          |
| 74    | The Adventures of Tom Sawyer      | Mark Twain               |
| 76    | Adventures of Huckleberry Finn    | Mark Twain               |
| 120   | Treasure Island                   | Robert Louis Stevenson   |
| 35    | The Time Machine                  | H.G. Wells               |
| 5200  | Metamorphosis                     | Franz Kafka              |
| 1400  | Great Expectations                | Charles Dickens          |
| 55    | The Wonderful Wizard of Oz        | L. Frank Baum            |

Books are downloaded from `https://www.gutenberg.org/cache/epub/ID/pgID.txt` with
a 30-second timeout and cached in `touchtype-narrative-cache-dir` (default
`~/.emacs.d/touchtype/`). The Gutenberg header and footer are stripped
automatically. Each passage is ~400 characters starting from a random sentence
boundary.

The book list is customizable via `touchtype-narrative-book-list` -- add any
valid Gutenberg ID.

### Code Snippets

The code snippet corpus (`touchtype--code-snippets`) contains 42 snippets from
eight languages:

- **Python** (8): `def main():`, `return None`, `if x > 0:`, `for i in range(n):`, `while True:`, `import os`, `class Foo:`, `self.value = x`
- **Rust** (7): `fn main() {`, `let mut v = Vec::new();`, `match x {`, `impl Trait for S {`, `pub fn new() -> Self {`, `println!("hello");`, `Ok(())`
- **Go** (5): `func main() {`, `if err != nil {`, `fmt.Println(x)`, `go func() {`, `defer f.Close()`
- **JavaScript/TypeScript** (6): `const x = 42;`, `let arr = [1, 2, 3];`, `console.log(x);`, `async function f() {`, `return new Promise();`, `export default {`
- **Emacs Lisp** (4): `(defun foo (x)`, `(let ((a 1)))`, `(setq x 42)`, `(lambda (x) (* x x))`
- **Shell** (4): `#!/bin/bash`, `if [ -f $1 ]; then`, `echo "$HOME"`, `grep -r 'TODO'`
- **SQL** (4): `SELECT * FROM users;`, `INSERT INTO t (a, b)`, `CREATE TABLE t (`, `WHERE id = $1`
- **C** (5): `int main() {`, `printf("%d\n", x);`, `#include <stdio.h>`, `struct Node {`, `void *ptr = NULL;`

Snippets are separated by double spaces in generated lines.

---

## Error Modes

Three error handling behaviors are available via `touchtype-error-mode`:

| Mode               | Symbol   | Behavior                                                                                                                                                               |
|--------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Normal**         | `normal` | Errors are shown (red highlight) but the cursor advances. You can continue typing or backspace to correct. This is the default.                                        |
| **Stop on Letter** | `letter` | The cursor does not advance until the correct character is typed. Forces immediate correction.                                                                         |
| **Stop on Word**   | `word`   | Errors are shown and the cursor advances within a word, but you cannot cross a word boundary (space) until all errors in the current word are corrected via backspace. |

---

## Keyboard Layouts

The keyboard layout setting (`touchtype-keyboard-layout`) determines the
progressive unlock order -- the sequence in which keys are introduced during
progressive mode. It does not affect other modes.

| Layout               | Unlock Order                                          |
|----------------------|-------------------------------------------------------|
| **QWERTY** (default) | `f j d k s l a h e t n i o u r g c m p b y w v x q z` |
| **Dvorak**           | `u h e t o n a s i d r l j g c m f p b k w v y x q z` |
| **Colemak**          | `n e i o s t a h r d l u f y w p g m c b k v x j q z` |
| **Workman**          | `n e h t o s a i d r l j g c m f p b k w v y x q z`   |
| **Custom**           | User-provided via `touchtype-custom-unlock-order`     |

Each order starts with home-row keys for that layout, then progresses by English
letter frequency. This ensures the most useful keys are available early.

---

## Customization

All options are in the `touchtype` customize group (`M-x customize-group RET
touchtype`).

```elisp
;; Target WPM for confidence calculation (default 40)
(setq touchtype-target-wpm 50)

;; Confidence threshold to unlock next key (default 0.80, range 0.0-1.0)
(setq touchtype-unlock-threshold 0.80)

;; Words per session (default 30)
(setq touchtype-session-length 60)

;; Session type: 'words or 'timed
(setq touchtype-session-type 'words)

;; Duration for timed sessions in seconds (default 60)
(setq touchtype-session-duration 120)

;; Error handling: 'normal, 'letter, or 'word
(setq touchtype-error-mode 'normal)

;; Keyboard layout for progressive unlock order
(setq touchtype-keyboard-layout 'qwerty)

;; Number of preview lines below the active line (default 2, 0 to disable)
(setq touchtype-preview-lines 2)

;; Enable pace caret at target WPM (default nil)
(setq touchtype-pace-caret t)

;; Idle threshold in seconds before pause is excluded from WPM (default 10)
(setq touchtype-idle-threshold 10)

;; Pseudo-word length range (default 4-8)
(setq touchtype-word-length-min 4)
(setq touchtype-word-length-max 8)

;; Number of top common words for common-words mode (default 100)
(setq touchtype-common-words-count 200)

;; Number of recent sessions in statistics view (default 20)
(setq touchtype-stats-history-length 20)

;; Stats persistence file
(setq touchtype-stats-file "~/.emacs.d/touchtype-stats.el")

;; Narrative mode: cache directory and passage length
(setq touchtype-narrative-cache-dir "~/.emacs.d/touchtype/")
(setq touchtype-narrative-passage-chars 400)

;; Add a custom book to narrative mode
(add-to-list 'touchtype-narrative-book-list 2542)  ; Les Miserables
```

### Faces

Six faces control the visual appearance:

| Face                        | Default                       | Purpose                         |
|-----------------------------|-------------------------------|---------------------------------|
| `touchtype-face-untyped`    | Inherits `shadow`             | Characters not yet typed (gray) |
| `touchtype-face-correct`    | Purple, bold                  | Correctly typed characters      |
| `touchtype-face-wrong`      | Red text, tinted background   | Incorrectly typed characters    |
| `touchtype-face-cursor`     | Underline, inherit default    | Current cursor position         |
| `touchtype-face-status`     | Inherits `mode-line-inactive` | Status line                     |
| `touchtype-face-pace-caret` | Subtle background tint        | Pace caret overlay              |

Customize with `M-x customize-face` or in your init file:

```elisp
(custom-set-faces
 '(touchtype-face-correct ((t :foreground "#00ff00" :weight bold))))
```

---

## Data Export

**Command**: `M-x touchtype-export`

Export your statistics in two formats:

### JSON

Includes sessions (date, WPM, accuracy, mode, words), per-letter stats (hits,
misses, total_ms, confidence), daily streak, and total practice time.

### CSV

Header: `date,wpm,accuracy,mode,words` with one row per session.

---

## File Structure

| File                     | Purpose                                                                              |
|--------------------------|--------------------------------------------------------------------------------------|
| `touchtype.el`           | Entry points, minor mode, session commands                                           |
| `touchtype-var.el`       | All `defcustom`, `defconst`, `defface`, and `defvar` declarations                    |
| `touchtype-ui.el`        | Buffer rendering, keymap, input loop, status line, end-session screen, stats view    |
| `touchtype-algo.el`      | Pseudo-word generation, real-word selection, n-gram drill lines, key-unlock logic    |
| `touchtype-stats.el`     | Statistics tracking, persistence, confidence scoring, trends, personal bests, export |
| `touchtype-narrative.el` | Project Gutenberg book download, caching, passage extraction                         |
| `test/touchtype-test.el` | ERT test suite                                                                       |

---

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
