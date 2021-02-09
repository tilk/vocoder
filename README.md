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

## Short tutorial

The phase vocoder algorithm consists of several stages:

* Splitting the input signal into overlapping frames of certain size, and applying a window function.
* Computing the FFT of the frames.
* Analysing the phases, which involves comparing the phases of the current frame to the previous one, and computing phase increments.
* Processing of the spectrum. This is the part which depends on the application. This phase is also responsible for the correctness of phase increments after the processing.
* Synthesizing the phases, which involves accumulating the phase increments.
* Computing the IFFT.
* Combining the overlapping frames into the output signal.

The analysis and synthesis phases are implemented in the `vocoder` library.
The convenience libraries `vocoder-conduit` and `vocoder-dunai` make it easy to write the application-specific processing stage and to use it to create the full vocoder pipeline.

### vocoder-conduit

The processing function has the type of `Monad m => ConduitT STFTFrame STFTFrame m r`, which means it is a conduit that accepts STFT frames (consisting of amplitudes and phase increments) and produces STFT frames.
To use it, the simplest way is to use the `process` function, of the type `Monad m => VocoderParams -> ConduitT STFTFrame STFTFrame m r -> ConduitT Frame Frame m r`.
The result is a conduit, which accepts a chunked stream of input data, and produces a chunked stream of output data.

For more complex usage, there is the `processFrames` function, of the type `Monad m => VocoderParams -> (Phase, Phase) -> ConduitT STFTFrame STFTFrame m r -> ConduitT Frame Frame m (r, (Phase, Phase))`.
Its arguments are: the vocoder configuration, the initial phases (for analysis and synthesis), and the processing function.
The result is a conduit, which accepts overlapped, windowed signal frames (of arbitrary size), produces overlapped signal frames (of the same size as the vocoder's hop size), and returns the result of the processing function and the final phases (for analysis and synthesis).
Initial phases can be constructed using the function `zeroPhase`.
The returned final phases can be used to seamlessly continue processing using a different processing function.
To use this function with chunked streams, `framesOfE` and `sumFramesE` can be used.

Because the `conduit` abstraction doesn't allow zipping, the library includes additional functions for simulteaneously processing multiple channels (e.g. stereo).
To do that, one should use the `processFrames` function, of the type `(Monad m, Applicative f) => (f Phase, f Phase) -> ConduitT (f STFTFrame) (f STFTFrame) m r -> ConduitT (f Frame) (f Frame) m (r, (f Phase, f Phase))`.
The applicative functor `f` should be a data structure for the channels, with a ,,zipping'' (field-wise) applicative instance.
Therefore using lists as `f` is not correct (but `ZipList` is).

### vocoder-dunai

The processing function has the type of `MSF m [STFTFrame] [STFTFrame]`, which means it accepts sequences of STFT frames and produces sequences of STFT frames (of the same length).
The need for using sequences comes from the fact that processing in `dunai` is synchronous: for every input frame, exactly one output frame has to be produced, and every part of the processing chain works at the same rate.
Therefore, for hop sizes smaller than the input frame size, several STFT frames will be produced for one time-domain input frame.
To use the processing function, the simplest way is to use the `process` function, of the type `MSF m [STFTFrame] [STFTFrame] -> MSF m Frame Frame`.

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

* `vocoder-jack --delay 10 --amplify 0.5 --add`

  Create an echo effect by delaying the input by 10 STFT hops, reducing amplitude and adding to the original signal.

