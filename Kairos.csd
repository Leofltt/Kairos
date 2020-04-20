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

sr = 44100
ksmps = 32
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
; p8 : chorus

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1                           ; Sine wave
gisquare ftgen 2, 0, 4096, 7, -1, 2048, -1, 0, 1, 2048, 1  ; Square wave
gisaw    ftgen 3, 0, 4096, 7, 0, 2048, -1, 0, 1, 2048, 0   ; Saw wave  



; INIT CHANNELS FOR FXs
; Delay

gkfbdel  init 0.5
gkdtdel  init 375
gkvoldel init 1

gkfbdel chnexport "fbdel", 1, 2, 0.4, 0, 0.99
gkdtdel chnexport "dtdel", 1, 2, 375, 1, 5000
gkvoldel chnexport "voldel", 1, 2, 1, 0, 1

; Reverb

gkfbrev init 0.4
gkcfrev init 15000
gkvolrev init 1

gkfbrev chnexport "fbrev", 1, 2, 0.4, 0, 0.99
gkcfrev chnexport "cfrev", 1, 2, 15000, 0, 20000
gkvolrev chnexport "volrev", 1, 2, 1, 0, 1

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

inchs = filenchnls(p9)
kthreshold = p11
iratio = p12

if inchs == 1 then

aLeft diskin2 p9, p10
aL =  aLeft*p4* sqrt(1-p7)
aR = aLeft*p4* sqrt(p7)

else

aLeft, aRight diskin2 p9, p10

aL =  aLeft*p4* sqrt(1-p7)
aR = aRight*p4* sqrt(p7)

endif

icomp1 = 1/iratio
irtime = 0.01
iftime = 0.05
aL dam aL, kthreshold, icomp1, 1, irtime, iftime
aR dam aR, kthreshold, icomp1, 1, irtime, iftime

zawm aL, 1
zawm aR, 2

zawm p5 * aL, 3
zawm p5 * aR, 4

zawm p6 * aL, 7
zawm p6 * aR, 8

zawm p8 * aL, 11
zawm p8 * aR, 12


endin

instr 2 ; Stutter

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8
Sname = p9
itune = p10  ; to be implemented
kthreshold = p11
iratio = p12

i_divisor = p13
i_pick = p14
i_repeat = (p15 == 0) ? 1 : p15

inchs = filenchnls(Sname)
ilength = filelen(Sname)
isr = filesr(Sname)
isize = ilength * isr
isamdur = i_dur * sr
isamslice =  ceil(isamdur / i_repeat)
ipos = i_pick / i_divisor * isize

andx =  phasor(sr/isamslice) * isamslice + ipos

if inchs == 1 then
isam ftgen 0, 0, isize, -1, Sname, 0, 0, 1
aL table3 andx, isam, 0
aL =  aL*i_vol* sqrt(1-i_pan)
aR =  aL*i_vol* sqrt(i_pan)
else
iLeft ftgen 0, 0, isize, -1, Sname, 0, 0, 1
iRight ftgen 0, 0,isize, -1, Sname, 0, 0, 2
aL table3 andx, iLeft, 0
aR table3 andx, iRight, 0
aL = aL*i_vol* sqrt(1-i_pan)
aR = aR*i_vol* sqrt(i_pan)
endif

aL = declick(aL)
aR = declick(aR)

icomp1 = 1/iratio
irtime = 0.01
iftime = 0.05
aL dam aL, kthreshold, icomp1, 1, irtime, iftime
aR dam aR, kthreshold, icomp1, 1, irtime, iftime

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

imode = p12

acut = 200 + expon(1, p3, 0.001) * p10
asig = vco2(1, cpsmidinn(p9), imode)
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
kcar = p13
kfilt = p10
kdpth = p14
iad = p12
kres = p11
aenv = linseg:a(0, (p3 -0.02)*iad+0.01, 1,   (p3 -0.02)*(1-iad)+0.01, 0)


amod = poscil(1, cpsmidinn(p9) * kindx, gisine)
acar = poscil(1, cpsmidinn(p9) + amod * kdpth * sr/4, gisine)

audio = diode_ladder(acar, kfilt, kres , 1, 1.5)

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

icf = p10
ires = p11
iad = p12
iwidth = p13

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

asig = zdf_ladder(asig, iwidth+icf,1)
asig = K35_hpf(asig, icf, ires)
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


kctrl = linseg(0, p3*.3, iamp, p3*.3, iamp, p3*.4, 0)
anoise = rand(kctrl)
anoise = butterlp(anoise, 5*ihz)
  ; Sligth chorus effect
afund = oscil(kctrl, ihz, 3)           
acel1 = oscil(kctrl, ihz - .1, 3)     
acel2 = oscil(kctrl, ihz + .1, 3)      
asig = afund + acel1 + acel2
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

instr 9 ; Karplus - Strong

kpitch = expseg:k(cpsmidinn(p9), p3, 432)

asig = pluck(1, cpsmidinn(p9), 432, 0, 4, p10, (49*p11)+1) 

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

instr 10 ; phaserSynth thingy

idur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

inote = cpsmidinn(p9)

icf = p10
ires = p11
iad = 12
iadp1 = p13
isim = p14
iadp2 = abs(isim -iad)
itable = p15
itable2 = p16
iwmix = p17
itune = p18
isep = p19
imode = p20
ienv_amp = p21
ifb = p22

aenv = linseg:a(0, (idur -0.02)*iad+0.01, 1,   (idur -0.02)*(1-iad)+0.01, 0)
kep1 = linseg:k(0, idur*iadp1, ienv_amp, idur*(1-iadp1), 0)
kep2 = linseg:k(0, idur*iadp2, ienv_amp, idur*(1-iadp2), 0)

kep1 += (1 -ienv_amp)
kep2 += (1 -ienv_amp)

aosc1 = poscil:a(1, inote, itable)
aosc2 = poscil:a(1, inote, itable2)
 
inote2 = inote * itune
 
aosc3 = poscil:a(1, inote2, itable)
aosc4 = poscil:a(1, inote2, itable2)

a1 = aosc1 * (1 - iwmix) + aosc2 * (iwmix)
a2 = aosc3 * (1 - iwmix) + aosc4 * (iwmix)

a3 = a1 + a2 

ap1 phaser2 a3, kep1 * (icf - 10) + 10, ires, 6, imode, kep1*isep, ifb
ap2 phaser2 a3 + ap1, kep2 * (icf - 10) + 10, ires, 6, imode, kep2*isep, ifb

ap2 *= aenv 
ap1 *= aenv
a3 *= aenv 
ap2 += a3


ap2 = declick(ap2)

ap2 dam ap2, 0.8, 0.8, 2, 0.01, 0.3 

aL = ap2 * sqrt(1- i_pan) * i_vol
aR = ap2 * sqrt(i_pan) * i_vol

zawm aL, 1
zawm aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 551 ; Delay

adelLr zar 7
adelRr zar 8

kporttime	linseg		0, .001, 1, 1, 1
kdlt		portk		(gkdtdel/1000.0), kporttime
adlt		interp		kdlt

abufferL	delayr 	5
adelL 	deltap3	adlt
		delayw	 adelLr + (adelL * gkfbdel)

abufferR	delayr	 5
adelR 	deltap3	adlt
		delayw	 adelRr + (adelR * gkfbdel)

zawm adelL * gkvoldel, 9
zawm adelR * gkvoldel, 10

endin

instr 550 ; ReverbSC

arvbL zar 3
arvbR zar 4

aoutL, aoutR reverbsc arvbL, arvbR, gkfbrev, gkcfrev

zawm aoutL * gkvolrev, 5
zawm aoutR * gkvolrev, 6

endin

instr 552	;Spectral Chorus

 a1 zar 11
 a2 zar 12

 kdlyml = gkdelchorus ;delay in milliseconds
 kblurtime = gkdelchorus / 1000
 
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
 aoutl = (ar1l+ar2l+ar3l+ar4l)*.5
 aoutr = (ar1r+ar2r+ar3r+ar4r)*.5
 
 fsigl pvsanal aoutl, 512, 128, 1024, 0  
 fsiglBlur pvsblur fsigl, kblurtime, 5
 aBlurL pvsynth fsiglBlur
 
 fsigr pvsanal aoutl, 512, 128, 1024, 0  
 fsigrBlur pvsblur fsigr, kblurtime, 5
 aBlurR pvsynth fsigrBlur

aL = aBlurL * gkvolchorus
aR = aBlurR * gkvolchorus

 zawm aL, 13
 zawm aR, 14

endin


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
