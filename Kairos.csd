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

sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1.0

; common p-fields :
; p4 : amplitude (0 - 1)
; p5 : reverb send (0 - 1)

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, 0, 2048, 0 ; Square wave

; GLOBAL VARIABLES
garvL, garvbR init 0

;opcode for declicking an audio signal. Should only be used in instruments that have positive p3 duration.
;taken from Steven Yi livecode.orc
opcode declick, a, a
  ain xin
  aenv = linseg(0, 0.01, 1, p3 - 0.02, 1, 0.01, 0, 0.01, 0)
  xout ain * aenv
endop

instr 1 ;Sampler

inchs filenchnls p6

if inchs = 1 then
aLeft diskin2 p6, p7
outs aLeft*p4, aLeft*p4
garvL = garvL + p5 * aLeft * p4
garvR = garvR + p5 * aLeft * p4
else
aLeft, aRight diskin2 p6, p7
outs aLeft*p4, aRight*p4
garvL = garvL + p5 * aLeft * p4
garvR = garvR + p5 * aRight * p4
endif

endin

instr 3 ;Bass 303

acut = 200 + expon(1, p3, 0.001) * p7
asig = vco2(1, p6)
asig = diode_ladder(asig, acut, 10, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig

outs asig*p4, asig*p4
garvbR = garvbR + p5 * asig * p4
garvbL = garvbL + p5 * asig * p4
endin

instr 5 ; HiHats 808

pa       =        (p5 >= 0.5 ? 1 : .15)         ; Select open or closed
ifreq1    =        540                     ; Tune
ifreq2    =        800                     ; Tune


aenv     expsega  .1, .0005, 1, pa - .0005, .01   ; Percussive envelope
asqr1    oscil    1, ifreq1, 2, -1
asqr2    oscil    1, ifreq1*1.342, 2, -1
asqr3    oscil    1, ifreq1*1.2312, 2, -1
asqr4    oscil    1, ifreq1*1.6532, 2, -1
asqr5    oscil    1, ifreq1*1.9523, 2, -1
asqr6    oscil    1, ifreq1*2.1523, 2, -1
a808     sum      asqr1, asqr2, asqr3, asqr4, asqr5, asqr6
a808     butterhp a808, 5270
a808     butterhp a808, 5270
outs a808*aenv*p4, a808*aenv*p4
garvbR = garvbR + p5 * a808 * p4 * aenv
garvbL = garvbL + p5 * a808 * p4 * aenv
endin

instr 666 ; ReverbSC

kfb = p5
kcf = p6

aoutL, aoutR reverbsc garvL, garvbR, kfb, kcf
outs aoutL * p4,  aoutR * p4

clear garvL, garvbR

endin


</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
