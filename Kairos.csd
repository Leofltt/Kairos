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
; 11, 12 chorus in
; 13, 14 chorus out

; common p-fields :
; p4 : amplitude (0 - 1)
; p5 : reverb send (0 - 1)
; p6 : delay send (0 - 1)
; p7 : panning (0 - 1)

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1 ; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, 0, 2048, 0 ; Square wave
iwave ftgen 3, 0, 16384, 7, 1, 16384, -1 ; Waveform for the string-pad




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

; Chorus

gkvolchorus init 1
gkdelchorus init 0
gkdivchorus init 30

gkvolchorus chnexport "volchorus", 1, 2, 1, 0, 1
gkdelchorus chnexport "delchorus", 1, 2, 0, 0, 500
gkdivchorus chnexport "divchorus", 1, 2, 30, 1, 60

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

inchs filenchnls p9

if inchs = 1 then

aLeft diskin2 p9, p10
aL =  aLeft*p4* sqrt(1-p7)
aR = aLeft*p4* sqrt(p7)

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

zawm p8 * aL, 11
zawm p8 * aR, 12

else

aLeft, aRight diskin2 p9, p10

aL =  aLeft*p4* sqrt(1-p7)
aR = aRight*p4* sqrt(p7)

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

zawm p8 * aL, 11
zawm p8 * aR, 12

endif

endin

instr 2 ; Karplus - Strong

kpitch = expseg:k(cpsmidinn(p9), p3, 432)

asig = pluck(1, cpsmidinn(p9), 432, 0, 4, p10, (49*p11)+1); p8 = roughness p9 = stretch

aL = asig*p4* sqrt(1-p7)
aR =  asig*p4* sqrt(p7)

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

zawm p8 * aL, 11
zawm p8 * aR, 12

endin

instr 3 ; Bass 303

;adapted from Steven Yi Livecode.orc

acut = 200 + expon(1, p3, 0.001) * p10
asig = vco2(1, cpsmidinn(p9))
asig = diode_ladder(asig, acut, p11, 1, 4)
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

zawm p8 * aL, 11
zawm p8 * aR, 12

endin

instr 4 ; Hoover Bass

iad = p12
aenv = linseg:a(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)
kcf = expseg:k(2, p3/2, 0.1)
kr3 unirand 1
kr3 port kr3, 0.01
klf3 = lfo:k(0.5, 1.5*kr3, 0)
klf3 = limit((klf3+0.5), 0.05, 0.95)
a1 = vco2(1, cpsmidinn(p9),4,(klf3*0.01))
a2 = vco2(1, cpsmidinn(p9)*(0.08+(7/12)),4,(klf3*0.01))
a3 = vco2(1, cpsmidinn(p9)*0.52)
af = a1 + a3 * 0.88 + a2 * 0.66
ao = diode_ladder(af, p10+(kcf * cpsmidinn(p9)), p11)
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

zawm p8 * aL, 11
zawm p8 * aR, 12

endin



instr 5 ; HiHats 808

pa        =        (p9 >= 0.5 ? 1 : .15)   ; Select open or closed
ifreq1    =        540*p10                     ; Tune
ifreq2    =        800*p10                     ; Tune

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

zawm p8 * aL, 11
zawm p8 * aR, 12

endin

instr 6 ; Simple subtractive-FM

kindx = p15
kfilt = p10
kdpth = p14
iad = p12
kres = p11
kdist = p13
aenv = linseg:a(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)


amod = poscil(kdpth, cpsmidinn(p9)* kindx, gisine)
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

zawm p8 * aL, 11
zawm p8 * aR, 12

endin

instr 7 ; SuperSaw

;adapted from Steven Yi Livecode.orc

iad = p11

aenv  = linseg(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)

asig = vco2(1,  cpsmidinn(p8))
asig += vco2(1, cpsmidinn(p9) * cent(9.04234))
asig += vco2(1,  cpsmidinn(p9) * cent(-7.214342))

asig += vco2(1,  cpsmidinn(p9) * cent(1206.294143))
asig += vco2(1,  cpsmidinn(p9) * cent(1193.732))
asig += vco2(1,  cpsmidinn(p9) * cent(1200))

asig += vco2(1,  cpsmidinn(p9) * cent(2406.294143))
asig += vco2(1,  cpsmidinn(p9) * cent(2393.732))
asig += vco2(1,  cpsmidinn(p9) * cent(2400))

asig *= 0.1

asig = zdf_ladder(asig, expseg(800 + p10, p3 - 0.05, p10 + 5000, 0.05, 250), 0.5)
asig = K35_hpf(asig, p10, p11)
asig = declick(asig)

aL = asig * p4 * sqrt(1-p7) * aenv
aR = asig * p4 * sqrt(p7) * aenv

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

zawm p8 * aL, 11
zawm p8 * aR, 12

endin

instr 8	;String pad from Bay at Night, Diaz

ihz= cpsmidinn(p9)
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

zawm p8 * aL, 11
zawm p8 * aR, 12

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

instr 552	;Chorus

 a1 zar 11
 a2 zar 12

 kdlyml = gkdelchorus ;delay in milliseconds

 k1 = oscili:k(kdlyml/gkdivchorus, 1, 2)
 ar1l = vdelay3(a1, kdlyml/5+k1, 900)
 ar1r = vdelay3(a2, kdlyml/5+k1, 900)
 k2 = oscili:k(kdlyml/gkdivchorus, .995, 2)
 ar2l = vdelay3(a1, kdlyml/5+k2, 700)
 ar2r = vdelay3(a2, kdlyml/5+k2, 700)
 k3 = oscili:k(kdlyml/gkdivchorus, 1.05, 2)
 ar3l = vdelay3(a1, kdlyml/5+k3, 700)
 ar3r = vdelay3(a2, kdlyml/5+k3, 700)
 k4 = oscili:k(kdlyml/gkdivchorus, 1, 2)
 ar4l = vdelay3(a1, kdlyml/5+k4, 900)
 ar4r = vdelay3(a2, kdlyml/5+k4, 900)
 aoutl = (a1+ar1l+ar2l+ar3l+ar4l)*.5 * gkvolchorus
 aoutr = (a2+ar1r+ar2r+ar3r+ar4r)*.5 * gkvolchorus

 zawm aoutl, 13
 zawm aoutr, 14

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
achoL zar 13
achoR zar 14

aL += arvbL
aR += arvbR
aL += adelL
aR += adelR
aL += achoL
aR += achoR
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
 <x>100</x>
 <y>100</y>
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
