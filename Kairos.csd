;KAIROS.csd

;the audio engine for the kairos live coding library

;Leonardo Foletto, 2018

<CsoundSynthesizer>
<CsOptions>
-odac
--port=11000
-d
-B 128
-b 64

</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1.0

zakinit 50,50 
; channels (L,R): 
; 1, 2 : mix
; 3, 4 : rev in
; 5, 6 : rev out
; 7, 8 : del in
; 9, 10 : del out

; common p-fields :
; p4 : amplitude (0 - 1)
; p5 : reverb send (0 - 1)
; p6 : delay send (0 - 1)
; p7 : panning (0 - 1)

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, 0, 2048, 0 ; Square wave
; Waveform for the string-pad
iwave ftgen 3, 0, 16384, 7, 1, 16384, -1




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

; Mixer / Master

gkvolMaster init 1

gkvolMaster chnexport "m_vol", 1, 2, 1, 0, 1

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
aL =  aLeft*p4* sqrt(1-p7)
aR = aLeft*p4* sqrt(p7)

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

else

aLeft, aRight diskin2 p8, p9

aL =  aLeft*p4* sqrt(1-p7)
aR = aRight*p4* sqrt(p7)

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

endif

endin

instr 2 ; Karplus - Strong

kpitch = expseg:k(cpsmidinn(p8), p3, 432)

asig = pluck(1, cpsmidinn(p8), 432, 0, 4, p9, (49*p10)+1); p8 = roughness p9 = stretch

aL = asig*p4* sqrt(1-p7)
aR =  asig*p4* sqrt(p7)

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

endin

instr 3 ; Bass 303

;adapted from Steven Yi Livecode.orc

acut = 200 + expon(1, p3, 0.001) * p9
asig = vco2(1, cpsmidinn(p8))
asig = diode_ladder(asig, acut, p10, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig
aL =  asig*p4* sqrt(1-p7)
aR = asig*p4* sqrt(p7)
zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

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
aL = adecl*p4* sqrt(1-p7) * aenv
aR = adecr*p4* sqrt(p7) * aenv
zawm aL, 1 
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

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
aL = a808*aenv*p4* sqrt(1-p7)
aR = a808*aenv*p4* sqrt(p7)
zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

endin

instr 6 ; Simple subtractive-FM

kindx = p14
kfilt = p9
kdpth = p13
iad = p11
kres = p10
kdist = p12
aenv = linseg:a(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)


amod = poscil(kdpth, cpsmidinn(p8)* kindx, gisine)
acar = poscil(1, cpsmidi() + amod, gisine)

audio = diode_ladder(acar, kfilt, kres , 1, kdist)

aL = audio * p4 * sqrt(1-p7) * aenv
aR = audio * p4 * sqrt(p7) * aenv

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

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

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

endin

instr 8	;String pad

ihz= cpsmidinn(p8)
iamp = p4

  ; Slow attack and release
kctrl = linseg(0, p3*.3, iamp, p3*.3, iamp, p3*.4, 0)  
anoise = rand(kctrl)
anoise = butterlp(anoise, 5*ihz)
  ; Sligth chorus effect
afund = oscil(kctrl, ihz, 3)           ; audio oscillator
acel1 = oscil(kctrl, ihz - .1, 3)        ; audio oscillator - flat
acel2 = oscil(kctrl, ihz + .1, 3)        ; audio oscillator - sharp
asig = afund + acel1 + acel2 
  ; Cut-off high frequencies depending on midi-velocity
  ; (larger velocity implies more brighter sound)
asig = asig + .2*anoise
asig = butterlp(asig, (p4 * 127 - 60)*40+600)

aL = asig*sqrt(p7);
aR = asig*sqrt(1-p7);

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8


endin

instr 551 ; Delay

adelLr zar 7
adelRr zar 8

adelL = vdelay3(adelLr, gkdtdel, 5000)
adelR = vdelay3(adelRr, gkdtdel, 5000)

zawm adelL * gkvoldel, 9
zawm adelR * gkvoldel, 10
zawm adelL * gkfbdel, 7
zawm adelR * gkfbdel, 8

endin

instr 550 ; ReverbSC

arvbL zar 3
arvbR zar 4 

aoutL, aoutR reverbsc arvbL, arvbR, gkfbrev, gkcfrev

zawm aoutL * gkvolrev, 5
zawm aoutR * gkvolrev, 6

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

instr 999 ; Mixer / Master out

aL zar 1
aR zar 2
arvbL zar 5
arvbR zar 6
adelL zar 9
adelR zar 10

aL += arvbL
aR += arvbR
aL += adelL
aR += adelR
aL *= gkvolMaster
aR *= gkvolMaster

outs aL, aR
zacl 0, 50

endin

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>590</x>
 <y>311</y>
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
