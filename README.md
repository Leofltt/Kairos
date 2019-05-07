# Kairos

live Coding library for music composition and performance using the Csound audio engine and the Haskell interactive compiler GHCi.

This project has been started as my Thesis project for the Bachelor Degree in Electronic Production and Design from Berklee College of Music, advised by Dr. Richard Boulanger.

The main goal of this software is to be able to perform and compose electronic music in a way that makes it more intuitive and immediate to interact with synthesis parameters compared to traditional virtual instruments and analog hardware. Focus of the development has been put in immediacy and easiness of usage, while also allowing for a high degree of customization and adaptability. This library is being developed based on the workflow that feels ideal for me, so it might not necessarily be the perfect tool for what you are looking for, but is provided nonetheless as Open Source Software for anyone to enjoy, modify, hack and get inspired by.

To get started you'll need to download:
- the Haskell platform
- Csound
- this repository

## Getting started


`cd` into the repository folder

`cabal install`

run `csound kairos.csd` from within a terminal window or from within CsoundQt

while csound is running, open another terminal window in the same folder and run `ghci`

within ghci run `:script BootKairos.hs`

All the modules should be loaded and ready to play with!

## Tutorial


The library works assigning pattern of times when a note should be performed and patterns of pfields to modify the parameters of synthesis.

#### Basic usage

When run, the script `BootKairos.hs` will instantiate a default performance named `perf`.
The performance is a data structure  that holds informations  about timing (time Signature, bpm, bar length), the instruments and the patterns they use.

To display the names of all the instruments currently loaded, run

`displayIns`


`p "instrumentname"`

to start an instrument and

`s  "instrumentname"`

to stop it.  You can also run

`solo "instrumentname"`

to solo a specific instrument or

`silence`

to stop all the currently running instruments.


#### Adding your own Csound Csound Instruments

To add your own Csound instruments to the `Kairos.csd` file
you need to keep in mind the following criteria:

1. Amplitude is normalized to `0dbfs = 1`

2. Reserved pfields:

   The following pfields are common for all the instruments and should be implemented as such in new instruments for ease of use

   - `p4` = volume (0-1)
   - `p5` = reverb send (0-1)
   - `p6` = delay send (0-1)

3. Implementing instrument data structure:

   - Each new instrument implemented in the `kairos.csd` file need to be also implemented in the file `Instrument.hs` following the example of the instruments already implemented.
   - After that the new instrument need to be added to the `defaultOrc` function if we want the new instrument to be automatically added to the orchestra on boot.

Leonardo Foletto, 2019
