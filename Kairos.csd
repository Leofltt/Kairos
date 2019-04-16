;KAIROS.csd

;the audio engine for the kairos live coding library

;Leonardo Foletto, 2018

<CsoundSynthesizer>
<CsOptions>
-odac
--port=10000
-d
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 10
nchnls = 2
0dbfs = 1.0

; common p-fields :
; p4 : amplitude (0 - 1)
; p5 : reverb send (0 - 1)
; p6 : delay send (0 - 1)

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, 0, 2048, 0 ; Square wave

; GLOBAL VARIABLES
garvbL, garvbR init 0
gadelL, gadelR init 0

; INIT CHANNELS FOR FXs
; Delay
chn_k "fbdel", 1, 2, 0.4, 0, 0.99
chn_k "dtdel", 1, 2, 450, 1, 3000
;Reverb
chn_k "fbrev", 1, 2, 0.4, 0, 0.99
chn_k "cfrev", 1, 2, 15000, 0, 20000

;opcode for declicking an audio signal.
;Should only be used in instruments that have positive p3 duration.
;taken from Steven Yi livecode.orc
opcode declick, a, a
  ain xin
  aenv = linseg(0, 0.01, 1, p3 - 0.02, 1, 0.01, 0)
  xout ain * aenv
endop

instr 1 ;Sampler

inchs filenchnls p7

if inchs = 1 then
aLeft diskin2 p7, p8
outs aLeft*p4, aLeft*p4

garvbL = garvbL + p5 * aLeft * p4
garvbR = garvbR + p5 * aLeft * p4

gadelL = gadelL + aLeft  * p4 * p6
gadelR = gadelR + aLeft * p4 * p6

else
aLeft, aRight diskin2 p7, p8
outs aLeft*p4, aRight*p4

garbL = garvbL + p5 * aLeft * p4
garvbR = garvbR + p5 * aRight * p4

gadelL = gadelL + aLeft * p4 * p6
gadelR = gadelR + aRight * p4 * p6
endif

endin

instr 3 ;Bass 303

acut = 200 + expon(1, p3, 0.001) * p8
asig = vco2(1, cpsmidinn(p7))
asig = diode_ladder(asig, acut, p9, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig

outs asig*p4, asig*p4

garvbR = garvbR + p5 * asig * p4
garvbL = garvbL + p5 * asig * p4

gadelL = gadelL + asig * p4 * p6
gadelR = gadelR + asig * p4 * p6

endin

instr 4 ;Hoover Bass

kcf expseg 2, p3/2, 0.1
kr3 unirand 1
kr3 port kr3, 0.01
klf3 lfo 0.5, 1.5*kr3, 0
klf3 limit (klf3+0.5), 0.05, 0.95
a1 = vco2(1, cpsmidinn(p7),4,(klf3*0.01))
a2 = vco2(1, cpsmidinn(p7)*(0.08+(7/12)),4,(klf3*0.01))
a3 = vco2(1, cpsmidinn(p7)*0.52)
af = a1 + a3 * 0.88 + a2 * 0.66
ao diode_ladder af, p8+(kcf * cpsmidinn(p7)), p9
kr1 unirand 1
kr2 unirand 1
kr1 port kr1, 0.01
kr2 port kr2, 0.01
alfo lfo 0.005, kr2 + 0.1
alfo2 lfo 0.005, kr1 + 0.1
adel vdelay3 ao/2, (0.1+alfo)*1000, 1000
adel2 vdelay3 ao/2, (0.1+alfo2)*1000, 1000
adecl declick (ao+adel)
adecr declick  (ao+adel2)
outs adecl*p4,adecr*p4

garvbR = garvbR + p5 * adecl * p4
garvbL = garvbL + p5 * adecr * p4

gadelL = gadelL + adecl * p4 * p6
gadelR = gadelR + adecr * p4 * p6

endin



instr 5 ; HiHats 808

pa        =        (p7 >= 0.5 ? 1 : .15)   ; Select open or closed
ifreq1    =        540                     ; Tune
ifreq2    =        800                     ; Tune

aenv     expsega  .01, .0005, 1, pa - .0005, .01   ; Percussive envelope
asqr1    poscil    1, ifreq1, 2, -1
asqr2    poscil    1, ifreq1*1.342, 2, -1
asqr3    poscil    1, ifreq1*1.2312, 2, -1
asqr4    poscil    1, ifreq1*1.6532, 2, -1
asqr5    poscil    1, ifreq1*1.9523, 2, -1
asqr6    poscil    1, ifreq1*2.1523, 2, -1
a808 = sum(asqr1, asqr2, asqr3, asqr4, asqr5, asqr6)
a808     butterhp a808, 5270
a808     butterhp a808, 5270
outs a808*aenv*p4, a808*aenv*p4

garvbR = garvbR + p5 * a808 * p4 * aenv
garvbL = garvbL + p5 * a808 * p4 * aenv

gadelL = gadelL + a808 * p4 * aenv * p6
gadelR = gadelR + a808 * p4 * aenv * p6

endin

instr 551 ; Delay

kfb chnget "fbdel"
kdt chnget "dtdel"

adelL vdelay3 gadelL, kdt*1.2, 5000
adelR vdelay3 gadelR, kdt*0.8, 5000

adelL = adelL + (gadelR * kfb)
adelR = adelR + (gadelL * kfb)
outs adelL * p4, adelR * p4

clear gadelL, gadelR

endin

instr 550 ; ReverbSC

kfb chnget "fbrev"
kcf chnget "cfrev"

aoutL, aoutR reverbsc garvbL, garvbR, kfb, kcf
outs aoutL * p4,  aoutR * p4

clear garvbL, garvbR

endin

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
