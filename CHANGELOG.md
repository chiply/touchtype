# Changelog

## [0.1.5](https://github.com/chiply/touchtype/compare/v0.1.4...v0.1.5) (2026-03-04)


### Features

* add 14 new features across 6 phases ([a86838b](https://github.com/chiply/touchtype/commit/a86838b259af8a75b139774e0485dfde910b014d))


### Bug Fixes

* resolve package-lint errors for CI ([1a690cb](https://github.com/chiply/touchtype/commit/1a690cb511da1715adc02b412150594e20536f27))

## [0.1.4](https://github.com/chiply/touchtype/compare/v0.1.3...v0.1.4) (2026-03-03)


### Features

* add per-mode statistics, paragraph-based narrative passages, and mixed n-gram lines ([772c3bf](https://github.com/chiply/touchtype/commit/772c3bf49a936dca64360cc5504ae2af23f7458f))
* per-mode stats, paragraph narrative passages, mixed n-gram lines ([8243f8f](https://github.com/chiply/touchtype/commit/8243f8f4849a5f0cb4f3365aaa2b9ff4cb4aa8d5))


### Bug Fixes

* filter weakest bigrams to only include 2-character entries ([d65df42](https://github.com/chiply/touchtype/commit/d65df4295a0cfa0f294a78240b8df60c7ce85711))

## [0.1.3](https://github.com/chiply/touchtype/compare/v0.1.2...v0.1.3) (2026-03-01)


### Bug Fixes

* add declare-function for touchtype-mode in touchtype-ui ([3b4e819](https://github.com/chiply/touchtype/commit/3b4e819f99ef967e03648eae85d20a88053e1dc8))

## [0.1.2](https://github.com/chiply/touchtype/compare/v0.1.1...v0.1.2) (2026-02-28)


### Bug Fixes

* change correct-character face from purple to green ([42e0597](https://github.com/chiply/touchtype/commit/42e0597896089c4227ec9758a0c273c78c5bacdf))

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
