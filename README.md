# vocoder

This repository contains a collection of Haskell libraries implementing a phase vocoder - an algorithm for processing time-varying signals in frequency domain.
It consists of the following libraries:

* `vocoder` is the core library. It implements the algorithms in the form of pure functions.
  It has minimal dependencies -- it depends only on `vector` and `vector-fftw`.
  It can be used directly, but for most uses it's preferrable to use one of the abstractions provided by other libraries.
* `vocoder-conduit` wraps the algorithms provided by `vocoder` for use with the `conduit` streaming library.
  This library is suitable for both off-line and on-line processing, including time transformations (e.g. time stretching).
* `vocoder-audio` is a convenience library which allows to easily use `vocoder-conduit` with audio streams provided by `conduit-audio`.
* `vocoder-dunai` wraps the algorithms provided by `vocoder` for use with `dunai` and `rhine` FRP libraries.
  This library is suitable for real-time signal processing.
