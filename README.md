# Kairos

live Coding library for music composition and performance using the Csound audio engine and the Haskell interactive compiler GHCi.

This project has been started as my Thesis project for the Bachelor Degree in Electronic Production and Design from Berklee College of Music, advised by Dr. Richard Boulanger.

The main goal of this software is to be able to perform and compose electronic music in a way that makes it more intuitive and immediate to interact with synthesis parameters compared to traditional virtual instruments and analog hardware. Focus of the development has been put in immediacy and easiness of use, while also allowing for a high degree of customization and adaptability. This library is being developed based on the workflow that feels ideal for me, so it might not necessarily be the perfect tool for what you are looking for, but is provided nonetheless as Open Source Software for anyone to enjoy, modify, hack and get inspired by.

To get started you'll need to download:
- [the Haskell platform]
- [Csound]
- this repository

## Getting started


`cd` into the repository folder

`stack build`

run `csound kairos.csd` from within a terminal window or from within CsoundQt

while csound is running, open another terminal window in the same folder and run `ghci`

within ghci run `:script BootKairos.hs`

All the modules should be loaded and ready to play with!

## Tutorial


The library works assigning pattern of times when a note should be performed and patterns of pfields to modify the parameters of synthesis.
This tutorial will serve as an overview of the operational principles of the library. While reading this a quick overview of [Base.hs] is recommended to have some insight on the data types of the arguments of the functions.

#### Basic usage

When run, the script `BootKairos.hs` will instantiate a default performance named `perf`.
The performance is a data structure  that holds informations  about timing (time Signature, bpm, bar length), the instruments and the patterns they use.

To display the names of all the instruments currently loaded, run

`displayIns`

Run `cT bpmvalue` to change the current tempo and

You can run:

`p "instrumentname"`

to start an instrument and

`s  "instrumentname"`

to stop it.  You can also run

`solo "instrumentname"`

to solo a specific instrument or

`silence`

to stop all the currently running instruments.

#### TimePoint Patterns

You can run

`displayTP`

to display the Time Patterns available and their names.

To change the pattern assigned to an instrument use

`cPat "patternName" "instrumentname"`

To add a new pattern, name it and assign it to an instrument use

`addC "instrumentname" "patternName" [timepointlist]`

There are multiple ways to create TP lists and a convenience function `toTP` is provided to convert `[Double]` into `[TimePoint]` and `fromTP` to do the opposite.

Some functions that generate a `[TimePoint]` :

`tupleForBar maxBeats desiredBeats`

`textToTP maxBeats "textString"`

`evolve genN ruleFunction TPPattern`

`=<< patternWithDensity totNumBeats numBeatsToGen density`

Some examples can be seen in the file `Test.hs`

#### Parameter Patterns

Parameter patterns are assigned for every pfield. A generic function to assign a new pattern to an instrument's pfield  is

`addPf "instrumentname" pfnumber list updatefunction`

for simplicity of usage, we want to create partially applied functions based on what we need in every instrument. For example one such function common to all  instruments is

`vol i list fun = addPf i 4 list fun`

that simply assigns the pattern to pfield number 4, which is used as the amplitude scaling value (0-1) common to all the default orchestra instruments.
Check out the file [BootKairos.hs] to see all the functions already implemented in such way.

An alternative to this method is the `params` syntax that allows to set multiple parameters at once.

`params "instrumentname" [(fun1, vol, list1), (fun2, pan, list2)]`

Here is a list of the currently implemented update functions:
- `nextVal` : picks the successive value in the list
- `randomize` : picks a random value
- `percentNext int` : given an int probability (0-100), returns the next value with int % probability, otherwise keeps current value
- `runMarkov csv-file` : given a csv file with a table of probabilities, picks the next value appropriately

#### Adding your own Csound Csound Instruments

To add your own Csound instruments to the `Kairos.csd` file
you need to keep in mind the following criteria:

1. Amplitude is normalized to `0dbfs = 1`

2. Reserved pfields:

   The following pfields are common for all the instruments and should be implemented as such in new instruments for ease of use

   - `p4` = volume (0-1)
   - `p5` = reverb send (0-1)
   - `p6` = delay send (0-1)
   - `p7` = panning  (0-1: Left to Right)

3. Implementing instruments data structures:

   - Each new instrument implemented in the `kairos.csd` file need to be also implemented in the file `Instrument.hs` following the example of the instruments already implemented.
   - After that the new instrument need to be added to the `defaultOrc` function if we want the new instrument to be automatically added to the orchestra on boot.

4. Effect instruments:

   - should use channels instead of pfields to control parameters
   - should have `p3 = -1`(play forever) and be played with the `playFx` function



Leonardo Foletto, 2019

[the Haskell platform]: https://www.haskell.org/downloads/
[Csound]: https://csound.com/download.html
[Base.hs]: https://github.com/Leofltt/Kairos/blob/master/src/kairos/Base.hs
[BootKairos.hs]: https://github.com/Leofltt/Kairos/blob/master/BootKairos.hs
