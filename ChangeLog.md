# Changelog for Kairos

0.3.0 Added instrument kind to be able to set up ways of sending to things other than Csound (ex. modular synths or max).

0.3.1 Fixed Markov algorithm, implemented the option to use  tables written as lists of lists instead of only csv files

0.3.2 Implemented euclidean rhythms

0.3.3 Fixed annoying bug with wrong stopping behavior, changed bootkairos.hs and csd (now fxs in csound are run by the csound file)

0.3.4 Initial work for new Pfield data type, updated stack and cabal versions, big refactor of file dependencies, updated MessageTo data type to include UDP port as string

0.3.5 Ability to create patterns for effects parameters, status init will quantize to next bar instead of first available beat

0.3.6 Refactor of PfPat and updaters (phase 1), added simplified shorthand aliases for functions. Some updates to all the necessary files && started revisiting documentation (slowly...)

## Unreleased changes
