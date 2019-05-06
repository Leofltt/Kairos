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

Leonardo Foletto, 2019
