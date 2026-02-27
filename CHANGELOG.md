# Changelog

## [0.1.1](https://github.com/chiply/touchtype/compare/v0.1.0...v0.1.1) (2026-02-27)


### Features

* initial release of touchtype v0.1.0 ([47f0721](https://github.com/chiply/touchtype/commit/47f0721b03cccf041d12bb4acdbcf4b6dfa86a16))
* type-over UI -- single gray text line, overlays only ([a232d59](https://github.com/chiply/touchtype/commit/a232d590d0a4cbf4961ecffc83fd4a1523e15989))


### Bug Fixes

* adding initial features ([b846feb](https://github.com/chiply/touchtype/commit/b846feb51fb6891b5cba0185acd7868ace3fa71f))
* anchor cursor to typing area, enter evil-emacs-state ([4879cd3](https://github.com/chiply/touchtype/commit/4879cd3a0fe121277dad2cea4a27f6e61e065b97))
* MELPA readiness -- headers, obsolete API, byte-compile warnings ([0dd0fc6](https://github.com/chiply/touchtype/commit/0dd0fc63e2aedee9807c830e0e94ac77ed1fbe39))
* remove Package-Requires from sub-files, update test variable names ([fd8768e](https://github.com/chiply/touchtype/commit/fd8768e632a51a47ac2192b4e973f29094b14f81))
* track raw accuracy including corrected mistakes ([ad9ff8d](https://github.com/chiply/touchtype/commit/ad9ff8dab45a91c13a646e122da71a84e4261531))
* use command remaps for word-backspace instead of key bindings ([5336127](https://github.com/chiply/touchtype/commit/533612793fd3d52f6cf0d54911dbef394529e749))
* word-backspace bindings and evil-emacs-state timing ([dcef623](https://github.com/chiply/touchtype/commit/dcef62326eb8cd480828fc270623d47a4e37711f))

## 0.1.0 (2026-02-25)

### Features

* Initial release with progressive, full-words, bigram-drill, letters, letters+numbers, and letters+numbers+symbols modes
* Keybr-style key unlock based on confidence score (speed × accuracy)
* Pseudo-word generation from embedded English bigram frequency table
* Per-letter and per-bigram statistics with file persistence
* Session summary with WPM, accuracy, and best/worst letters
* Backspace (character) and M-DEL (word) support
