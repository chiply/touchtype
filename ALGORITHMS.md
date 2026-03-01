# Algorithms and Data Sources

Detailed documentation of the algorithms and data sources used by
[touchtype](README.md).

---

## Bigram Frequency Table

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

## Pseudo-word Generation

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

## Confidence Score

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

## Key Unlock Logic

In progressive mode, key unlocks are checked after each completed line:

1. **Should unlock?** (`touchtype-algo-should-unlock-p`): Returns true if
   **every** character in `touchtype--unlocked-keys` has confidence >=
   `touchtype-unlock-threshold` (default 0.80).
2. **Unlock next** (`touchtype-algo-unlock-next-key`): Finds the first character
   in the [layout-specific unlock order](README.md#keyboard-layouts) that is not yet
   unlocked, appends it to `touchtype--unlocked-keys`, and persists the change.
3. **Focus shift**: The newly unlocked key becomes `touchtype--focused-key`,
   ensuring it receives extra practice in generated text.
4. **Weak letter rotation** (`touchtype-algo--pick-focus-char`): During word
   generation, the focused character is chosen with 70% probability. 30% of the
   time, a random letter below the confidence threshold is focused instead. This
   prevents neglecting older keys while prioritizing the newest one.

## N-gram Sources

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

## Word List

The built-in word list (`touchtype--builtin-words`) contains ~4,258 common
English words organized by length, ranging from 2-letter words (`act`, `add`) to
12+ letter words (`abbreviation`, `wholehearted`). Words were selected for
frequency of use in everyday English.

In **full-words** mode, words are filtered to the allowed character set. In
**common-words** mode, only the first `touchtype-common-words-count` (default
100) entries are used, representing the most common words.

## Narrative Sources

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

## Code Snippets

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
