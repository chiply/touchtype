# touchtype

Keybr-style progressive touch typing trainer for Emacs.

## Features

- **Progressive mode** — starts with a few home-row keys and unlocks more as your confidence grows, just like [keybr.com](https://www.keybr.com/)
- **Bigram-frequency pseudo-words** — words are generated from real English bigram statistics so they feel natural
- **Per-letter statistics** — tracks speed and accuracy for every key; persists across sessions
- **Six training modes** — progressive, full-words, bigram-drill, letters, letters+numbers, letters+numbers+symbols
- **Backspace support** — character backspace and word backspace (M-DEL)
- **Session summary** — WPM, accuracy, best/worst letters after each session

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

## Usage

```
M-x touchtype              start in current/last mode
M-x touchtype-progressive  progressive key-unlock mode
M-x touchtype-full-words   real English words
M-x touchtype-bigram-drill repeat common bigram pairs
M-x touchtype-stats-view   show statistics summary
```

## Customization

```elisp
;; WPM target for confidence calculation (default 40)
(setq touchtype-target-wpm 50)

;; Confidence threshold to unlock next key (default 0.80)
(setq touchtype-unlock-threshold 0.80)

;; Words per session (default 30)
(setq touchtype-session-length 20)

;; Stats persistence file
(setq touchtype-stats-file "~/.emacs.d/touchtype-stats.el")
```

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
