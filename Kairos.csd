;KAIROS.csd

;the audio engine for the kairos live coding library

;Leonardo Foletto, 2018

<CsoundSynthesizer>
<CsOptions>
-odac
--port=10000
-d
-B 128
-b 64

</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1.0

; common p-fields :
; p4 : amplitude (0 - 1)
; p5 : reverb send (0 - 1)
; p6 : delay send (0 - 1)
; p7 : panning (0 - 1)

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, 0, 2048, 0 ; Square wave


; GLOBAL VARIABLES
garvbL, garvbR init 0
gadelL, gadelR init 0
gacompL, gacompR init 0

; INIT CHANNELS FOR FXs
; Delay

gkfbdel  init 0.9
gkdtdel  init 375
gkvoldel init 1

gkfbdel chnexport "fbdel", 1, 2, 0.9, 0, 0.99
gkdtdel chnexport "dtdel", 1, 2, 375, 1, 5000
gkvoldel chnexport "voldel", 1, 2, 1, 0, 1

; Reverb

gkfbrev init 0.4
gkcfrev init 15000
gkvolrev init 1

gkfbrev chnexport "fbrev", 1, 2, 0.4, 0, 0.99
gkcfrev chnexport "cfrev", 1, 2, 15000, 0, 20000
gkvolrev chnexport "volrev", 1, 2, 1, 0, 1

; Parallel Compressor

gkvolcomp init 1

gkvolcomp chnexport "volcomp", 1, 2, 1, 0, 1

;opcode for declicking an audio signal.
;Should only be used in instruments that have positive p3 duration.
;taken from Steven Yi livecode.orc
opcode declick, a, a
ain xin
aenv = linseg:a(0, 0.01, 1, p3 - 0.02, 1, 0.01, 0)
xout ain * aenv
endop

opcode loadSample, i, S
Sample xin
iNum ftgen 0, 0, 0, -1, Sample, 0, 0, 0
xout iNum
endop

instr 1 ; Sampler

inchs filenchnls p8

if inchs = 1 then
aLeft diskin2 p8, p9
outs aLeft*p4* sqrt(1-p7), aLeft*p4* sqrt(p7)

garvbL +=  p5 * aLeft  * sqrt(1-p7)* p4
garvbR +=  p5 * aLeft  * sqrt(p7)* p4

gadelL +=  aLeft   * p6 * sqrt(1-p7)* p4
gadelR +=  aLeft  * p6 * sqrt(p7)* p4

else
aLeft, aRight diskin2 p8, p9
outs aLeft*p4* sqrt(1-p7), aRight*p4* sqrt(p7)

garvbL +=  p5 * aLeft * sqrt(1-p7)* p4
garvbR +=  p5 * aRight * sqrt(p7)* p4

gadelL +=  aLeft  * p6 * sqrt(1-p7)* p4
gadelR +=  aRight  * p6 * sqrt(p7)* p4
endif

endin

instr 2 ; Karplus - Strong

kpitch = expseg:k(cpsmidinn(p8), p3, 432)

asig = pluck(1, cpsmidinn(p8), 432, 0, 4, p9, (49*p10)+1); p8 = roughness p9 = stretch

outs asig*p4* sqrt(1-p7), asig*p4* sqrt(p7)

garvbR +=  p5 * asig  * sqrt(1-p7)* p4
garvbL +=  p5 * asig  * sqrt(p7)* p4

gadelL += asig  * p6 * sqrt(1-p7)* p4
gadelR +=  asig  * p6 * sqrt(p7)* p4

endin

instr 3 ; Bass 303

;adapted from Steven Yi Livecode.orc

acut = 200 + expon(1, p3, 0.001) * p9
asig = vco2(1, cpsmidinn(p8))
asig = diode_ladder(asig, acut, p10, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig

outs asig*p4* sqrt(1-p7), asig*p4* sqrt(p7)

garvbR +=  p5 * asig  * sqrt(1-p7)* p4
garvbL +=  p5 * asig  * sqrt(p7)* p4

gadelL +=  asig  * p6 * sqrt(1-p7) * p4
gadelR +=  asig  * p6 * sqrt(p7)* p4

endin

instr 4 ; Hoover Bass

iad = p11
aenv = linseg:a(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)
kcf = expseg:k(2, p3/2, 0.1)
kr3 unirand 1
kr3 port kr3, 0.01
klf3 = lfo:k(0.5, 1.5*kr3, 0)
klf3 = limit((klf3+0.5), 0.05, 0.95)
a1 = vco2(1, cpsmidinn(p8),4,(klf3*0.01))
a2 = vco2(1, cpsmidinn(p8)*(0.08+(7/12)),4,(klf3*0.01))
a3 = vco2(1, cpsmidinn(p8)*0.52)
af = a1 + a3 * 0.88 + a2 * 0.66
ao = diode_ladder(af, p9+(kcf * cpsmidinn(p8)), p10)
kr1 unirand 1
kr2 unirand 1
kr1 port kr1, 0.01
kr2 port kr2, 0.01
alfo = lfo(0.005, kr2 + 0.1)
alfo2 = lfo(0.005, kr1 + 0.1)
adel = vdelay3(ao/2, (0.1+alfo)*1000, 1000)
adel2 = vdelay3(ao/2, (0.1+alfo2)*1000, 1000)
adecl = declick(ao+adel*0.8)
adecr = declick(ao+adel2*0.8)
outs adecl*p4* sqrt(1-p7),adecr*p4* sqrt(p7) * aenv

garvbR +=  p5 * adecl * sqrt(1-p7) * aenv * p4
garvbL +=  p5 * adecr * sqrt(p7) * aenv * p4

gadelL +=  adecl  * p6 * sqrt(1-p7) * aenv * p4
gadelR +=  adecr  * p6 * sqrt(p7) * aenv * p4

endin



instr 5 ; HiHats 808

pa        =        (p8 >= 0.5 ? 1 : .15)   ; Select open or closed
ifreq1    =        540*p9                     ; Tune
ifreq2    =        800*p9                     ; Tune

aenv  =   expsega(.01, .0005, 1, pa - .0005, .01)   ; Percussive envelope
asqr1 =   poscil(1, ifreq1, 2, -1)
asqr2 =   poscil(1, ifreq1*1.342, 2, -1)
asqr3 =   poscil(1, ifreq1*1.2312, 2, -1)
asqr4 =   poscil(1, ifreq1*1.6532, 2, -1)
asqr5 =   poscil(1, ifreq1*1.9523, 2, -1)
asqr6 =   poscil(1, ifreq1*2.1523, 2, -1)
a808 = sum(asqr1, asqr2, asqr3, asqr4, asqr5, asqr6)
a808 =    butterhp(a808, 5270)
a808 =    butterhp(a808, 5270)
outs a808*aenv*p4* sqrt(1-p7), a808*aenv*p4* sqrt(p7)

garvbR +=  p5 * a808 * aenv* sqrt(1-p7) * p4
garvbL +=  p5 * a808 * aenv* sqrt(p7) * p4

gadelL +=  a808 * p6 * sqrt(1-p7) * aenv * p4
gadelR +=  a808 * p6 * sqrt(p7) * aenv * p4

endin

instr 6 ; Simple subtractive-FM

kindx = p14
kfilt = p9
kdpth = p13
iad = p11
kres = p10
kdist = p12
aenv = linseg:a(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)


amod = poscil(kdpth, cpsmidinn(p8)* (1/(5*kindx)), gisine)
acar = poscil(1, cpsmidi() + amod, gisine)

audio = diode_ladder(acar, kfilt, kres , 1, kdist)

aL = audio * p4 * sqrt(1-p7) * aenv
aR = audio * p4 * sqrt(p7) * aenv

outs aL, aR

garvbR += p5 * audio * sqrt(1-p7) * aenv * p4
garvbL +=  p5 * audio * sqrt(p7) * aenv * p4

gadelL += audio * p6 * sqrt(1-p7) * aenv * p4
gadelR += audio * p6 * sqrt(p7) * aenv * p4

endin

instr 7 ; SuperSaw

;adapted from Steven Yi Livecode.orc

iad = p11

aenv  = linseg(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)

asig = vco2(1,  cpsmidinn(p8))
asig += vco2(1, cpsmidinn(p8) * cent(9.04234))
asig += vco2(1,  cpsmidinn(p8) * cent(-7.214342))

asig += vco2(1,  cpsmidinn(p8) * cent(1206.294143))
asig += vco2(1,  cpsmidinn(p8) * cent(1193.732))
asig += vco2(1,  cpsmidinn(p8) * cent(1200))

asig += vco2(1,  cpsmidinn(p8) * cent(2406.294143))
asig += vco2(1,  cpsmidinn(p8) * cent(2393.732))
asig += vco2(1,  cpsmidinn(p8) * cent(2400))

asig *= 0.1

asig = zdf_ladder(asig, expseg(800 + p9, p3 - 0.05, p9 + 5000, 0.05, 250), 0.5)
asig = K35_hpf(asig, p9, p10)
asig = declick(asig)

aL = asig * p4 * sqrt(1-p7) * aenv
aR = asig * p4 * sqrt(p7) * aenv

outs aL, aR

garvbR +=  p5 * asig * sqrt(1-p7) * aenv * p4
garvbL +=  p5 * asig * sqrt(p7) * aenv * p4

gadelL +=  asig * p6 * sqrt(1-p7) * aenv * p4
gadelR +=  asig * p6 * sqrt(p7) * aenv  * p4

endin

instr 551 ; Delay

adelL = vdelay3(gadelL, gkdtdel, 5000)
adelR = vdelay3(gadelR, gkdtdel, 5000)

adelL = adelL + (gadelL * gkfbdel)
adelR = adelR + (gadelR * gkfbdel)
outs adelL * gkvoldel, adelR * gkvoldel

clear gadelL, gadelR

endin

instr 550 ; ReverbSC

aoutL, aoutR reverbsc garvbL, garvbR, gkfbrev, gkcfrev
outs aoutL * gkvolrev ,  aoutR * gkvolrev

clear garvbL, garvbR

endin

;instr 660 ; Parallel Compressor
;
;aoutL compress gacompL, gacompL, kthresh, kloknee, khiknee, kratio, katt, krel, ilook
;aoutR compress gacompR, gacompR, kthresh, kloknee, khiknee, kratio, katt, krel, ilook
;outs aoutL * gkvolcomp, aoutR * gkvolcomp
;
;clear gacompL, gacompR
;
;endin


</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>644</x>
 <y>176</y>
 <width>320</width>
 <height>240</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="nobackground">
  <r>255</r>
  <g>255</g>
  <b>255</b>
 </bgcolor>
</bsbPanel>
<bsbPresets>
</bsbPresets>
