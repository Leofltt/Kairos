{--
 -
 - Currently Playing:
 - @leofltt
 -
 -

Updaters & Sequence Helpers:
- nv / k / keep (no update)
- np Int (next pattern)
- rnd / randomize
- genU n updater initial (Generate static sequence from updater)
- withS scale updater (Remap indices to Scale)
- withK kit resolver updater (Remap indices to Kit elements by index)
- withK' kit updater (Shorthand for withK using current performance)

Examples:
-- Generate 8 indices from Markov:

mySeq <- genU 8 (rMkvCSV csvAcid) [0..7]

-- Play indices as notes in D Dorian:

pitch "303" mySeq (withS (withScale 38 dorian) nv)

-- Play indices as samples from a kit (0 maps to 1st sample, 1 to 2nd, etc):

sample "kit" mySeqK (withK' mainKit nv)

-- Real-time Markov remapping (no static list):

pitch "303" [0..3] (withS (withScale 41 aeolian) (rMkvCSV csvAcid))
sample "kit" [0..3] (withK' mainKit (rMkvCSV csvKitT))

TP stuff:
- displayTP / displayIns
- catTP bars tp1 tp2
- seqTP tp1 tp2
- tupleForBar bars Intbeats
- euclid (hit,total) rotation bars
- patternWithDensity bars beatsTotal %Int
- textToTP beats text
- shine maxbeats [Spark]
- toBinToTP beats number

New 16-beat TPats:
- jungle_16, idm_16, techno_long, poly_3_16

Presets:
- 303: classic303, acid303, deep303, drex303, electro303
- Hoover: classicHov, softHov, aggroHov, darkHov
- SuperSaw: brightSaw, cloudSaw, stabbySaw, tranceSaw
- lpFM: bassFM, deepFM, bellFM, harshFM, leadFM, hornFM, stringFM
- Pads: silkPad, darkPad
- Karp: nylonKarp, metalKarp
- DTMF: dialing, alienDial

Markov Tables (CSV):
- Melodic: csvTechno, csvAcid, csvElectro, csvJungle, csvIDM, csvTrance
- Breaks: csvBreakDnB, csvBreakJungle, csvBreakVsnare, csvBreakAe
- Ratchets: csvStutsDnB, csvStutsVsnare, csvStutsAe
- Kits: csvKitT, csvKitJ

Performance Utils:
- displayP "instr" (Show all current p-fields)
- prms "instr" [(updater, param, [values])]

-- common parameters
dur / durTS
vol
rev
del
pan
chorus
phaser
sideComp
sideRing
dist / distPreGain / distPostGain / distChar
ringMod / rmGain / rmSc / rmWt / rmFreq
lpFreq / lpRes / hpFreq / hpRes
compThreshDb / compHard / comp / compSc

-- instrument specific parameters
pitch / cf / res (303, hov, karp, lpFM, sSaw, strPad)
wf02 (303)
cps / sample (sampler, stutter)
divs / pick / stuts (stutter)
openclose / tuning (hihat)
rough / stretch (karp)
detune / sawmix (sSaw)
adRatio (lpFM, hov, sSaw, dtmf)
fmCar / fmIndx / fmDepth (lpFM)
btn / ampX / ampY (dtmf)

-- GLOBAL FX CHANNELS (setChannel)
fbdel / dtdel / voldel (Delay)
fbrev / gkcfrev / volrev (Reverb)
volchorus / delchorus / divchorus (Chorus)
voluzu / uzuwidth / uzuoffset / uzudepth / uzuspeed / uzublur / uzumix / uzuhzmode / uzubass / uzuspread (UzuPhaser)
m_vol (Master)
wl / dropwl / maxwl (Waveloss)

-- FX PFIELD UPDATERS (Pattern-based)
delfb / delt / delvol
revfb / revcf / revvol
chorvol / chordt / chordiv
uzuvol / uzuwd / uzuoff / uzudp / uzusp / uzubl / uzumx / uzuhz / uzubs / uzuspr
mixvol
wlvol / wldrop / wlmax

--}
