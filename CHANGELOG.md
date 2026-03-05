# Changelog

## [0.1.12](https://github.com/chiply/touchtype/compare/v0.1.11...v0.1.12) (2026-03-05)


### Features

* add focus/highlight mode for upcoming words ([7b41379](https://github.com/chiply/touchtype/commit/7b4137973dc366e0a85b659011eef86f82a42bfc))
* add optional typing sound feedback ([4ba900f](https://github.com/chiply/touchtype/commit/4ba900f9fc990455c454eba0351dca3be541dc97))
* add WPM percentile lookup function ([0353721](https://github.com/chiply/touchtype/commit/03537211cc7da5207648796f7eea62c9461a78fd))
* add WPM-over-time graph to session results ([0f5050a](https://github.com/chiply/touchtype/commit/0f5050ad82abfd233ec5b53610bfa9d2fe4dc3d4))
* show global percentile estimate on session results ([c7a8a22](https://github.com/chiply/touchtype/commit/c7a8a22f8ea94a98fa01955702a0c610397d071f))


### Bug Fixes

* eliminate typing sound latency with persistent NSSound daemon ([e19710a](https://github.com/chiply/touchtype/commit/e19710a9231c3084a73eebd6689aa976b2ed0f9b))


### Reverts

* remove typing sound feedback feature ([cd13859](https://github.com/chiply/touchtype/commit/cd1385904565fad14e01611f00e184854757e766))

## [0.1.11](https://github.com/chiply/touchtype/compare/v0.1.10...v0.1.11) (2026-03-05)


### Features

* centered text with word-wrap, flowing text, and jit-lock fix ([91beafa](https://github.com/chiply/touchtype/commit/91beafae640709514ada53b377d1677a7ae648f7))

## [0.1.10](https://github.com/chiply/touchtype/compare/v0.1.9...v0.1.10) (2026-03-05)


### Features

* per-mode stats breakdown, performance optimizations, and multiple UX improvements ([f33921b](https://github.com/chiply/touchtype/commit/f33921bc7388ae0f4eebf5dc8a77d1cda63c3e3d))

## [0.1.9](https://github.com/chiply/touchtype/compare/v0.1.8...v0.1.9) (2026-03-05)


### Bug Fixes

* end-session stats buffer rendering and interaction bugs ([32a199c](https://github.com/chiply/touchtype/commit/32a199c264c882e505a11d384fb4101594a1d4f3))
* use C-c C-r/C-c C-q bindings to satisfy package-lint ([c20c6e4](https://github.com/chiply/touchtype/commit/c20c6e43ad6370bfd6f7b9f8255a6a7b32a031af))

## [0.1.8](https://github.com/chiply/touchtype/compare/v0.1.7...v0.1.8) (2026-03-04)


### Bug Fixes

* install Nix before magic-nix-cache-action in CI ([0fc1f4a](https://github.com/chiply/touchtype/commit/0fc1f4afa3a2a032d46bb1e9d7486946f7cbd16c))

## [0.1.7](https://github.com/chiply/touchtype/compare/v0.1.6...v0.1.7) (2026-03-04)


### Features

* add 10 gamification improvements ([a516320](https://github.com/chiply/touchtype/commit/a51632035ae7c2e9d4aa57202c9553d9921ed029))
* add end-session heatmap, progress charts, and zen mode ([7c4e11e](https://github.com/chiply/touchtype/commit/7c4e11e00121cc009557e1df97bf90bca5e368d1))
* progressive unlock for common-words, domain-words, and weak-* modes ([dea4834](https://github.com/chiply/touchtype/commit/dea483468a8998f4a80d78773647027057e2f424))

## [0.1.6](https://github.com/chiply/touchtype/compare/v0.1.5...v0.1.6) (2026-03-04)


### Bug Fixes

* bind pause to C-c C-p instead of reserved key ([0ebbbc2](https://github.com/chiply/touchtype/commit/0ebbbc2a74c0b05c5a98b2e3c2f0f1de12569ebc))

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
