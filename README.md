# vocoder

This repository contains a collection of Haskell libraries implementing a phase vocoder - an algorithm for processing time-varying signals in frequency domain.
It consists of the following libraries:

* `vocoder` is the core library. It implements the algorithms in the form of pure functions.
  It has minimal dependencies -- it depends only on `vector` and `vector-fftw`.
  It can be used directly, but for most uses it's preferrable to use one of the abstractions provided by other libraries.
* `vocoder-conduit` wraps the algorithms provided by `vocoder` for use with the [conduit](https://github.com/snoyberg/conduit) streaming library.
  This library is suitable for both off-line and on-line processing, including time transformations (e.g. time stretching).
* `vocoder-audio` is a convenience library which allows to easily use `vocoder-conduit` with audio streams provided by `conduit-audio`.
* `vocoder-dunai` wraps the algorithms provided by `vocoder` for use with [dunai](https://github.com/ivanperez-keera/dunai) and [rhine](https://github.com/turion/rhine) FRP libraries.
  This library is suitable for real-time signal processing.

## Example programs

Two example programs are provided to demonstrate the use of the libraries: `vocoder-file` in package `vocoder-audio`, and `vocoder-jack` in package `vocoder-dunai`.

### vocoder-file

This program uses `vocoder-conduit` for off-line audio file processing.
It takes a number of input files and effect specifications for each of the files, and produces an output file which is the result of processing the input files in sequence.
The inputs are joined seamlessly.

Example usage:

* `vocoder-file output.wav input.wav --lowpassBrickwall 1000`

  Low-pass filters the `input.wav` file, leaving only the frequencies below 1000 Hz.

* `vocoder-file output.wav input1.wav --pitchShiftInterpolate 2 input2.wav --playSpeed 2`

  Changes the pitch of `input1.wav` by one octave, and slows down the tempo of `input2.wav` two times.
  
* `vocoder-file output.wav input.wav --playSpeed 10 --randomPhase`

  Slows down the tempo of `input.wav` ten times and randomises the STFT phases, introducing Paulstretch-style effect.

* `vocoder-file output.wav input.wav --envelope 32 --randomPhase`

  Takes spectral envelope of `input.wav` and randomises the STFT phases, which makes speech sound more like a whisper.

### vocoder-jack

This program uses `vocoder-dunai` for processing audio in real-time through the [JACK](https://jackaudio.org/) real-time audio framework.
It allows combining multiple sound inputs using a filter tree, which allows constructing interesting effects, including those known from vocoder pedals.

The filter tree is specified using postfix notation. If a filter argument is missing, input stream 0 is assumed.

Example usage:

* `vocoder-jack --lowpassButterworth 2,1000`

  Low-pass filters the input using a filter with second-order Butterworth-like characteristics and a cut-off frequency of 1000 Hz.

* `vocoder-jack --windowSize 128 --randomPhase`  

  Randomizes STFT phase with small window size, reducing sound clarity and creating a "distorted anonymous voice" effect.

* `vocoder-jack --envelope 32 --randomPhase`  

  Take spectral envelope of the input and randomize the STFT phases, producing a whisper-like effect.

* `vocoder-jack --pitchShiftInterpolate 2 --add`

  Adds the spectrum of the input signal to the spectrum lowered by one octave, creating a harmonizer effect.

* `vocoder-jack --envelope 32 --divide --source 1 --envelope 32 --multiply`

  Creates a talkbox-like vocoder effect. 
  The first input's spectrum is divided by its own envelope, and multiplied by the envelope of the second input's spectrum.

