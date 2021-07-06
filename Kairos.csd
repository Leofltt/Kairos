;KAIROS.csd

;the audio engine for the kairos live coding library

;Leonardo Foletto, 2018

<CsoundSynthesizer>
<CsOptions>
-odac6
--port=11000
-d
-B 256
-b 128
-Q0

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
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, -1, 2048, -1  ; Square wave
gisaw    ftgen 3, 0, 4096, 7, 0, 2048, 1, 0, -1, 2048, 0   ; Saw wave  



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

opcode panner, aa, aai
aL,aR,i_pan xin
aLeft = aL * sqrt(1-i_pan)
aRight = aR * sqrt(i_pan)
xout aLeft, aRight
endop

opcode loadSample, i, S
Sample xin
iNum ftgen 0, 0, 0, -1, Sample, 0, 0, 0
xout iNum
endop

instr 1 ; Sampler

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

inchs = filenchnls(p9)
kthreshold = p11
iratio = p12

if inchs == 1 then

aLeft diskin2 p9, p10
aL, aR panner aLeft, aLeft, i_pan


else

aLeft, aRight diskin2 p9, p10

aL, aR panner aLeft, aRight, i_pan

endif

icomp1 = 1/iratio
irtime = 0.01
iftime = 0.05
aL dam aL, kthreshold, icomp1, 1, irtime, iftime
aR dam aR, kthreshold, icomp1, 1, irtime, iftime

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12


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
aL, aR panner aL, aL, i_pan
else
iLeft ftgen 0, 0, isize, -1, Sname, 0, 0, 1
iRight ftgen 0, 0,isize, -1, Sname, 0, 0, 2
aL table3 andx, iLeft, 0
aR table3 andx, iRight, 0
aL, aR panner aL, aR, i_pan
endif

aL = declick(aL)
aR = declick(aR)

icomp1 = 1/iratio
irtime = 0.01
iftime = 0.05
aL dam aL, kthreshold, icomp1, 1, irtime, iftime
aR dam aR, kthreshold, icomp1, 1, irtime, iftime

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 3 ; Bass 303

;adapted from Steven Yi Livecode.orc

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

imode = p12

acut = 200 + expon(1, p3, 0.001) * p10
asig = vco2(1, cpsmidinn(p9), imode)
asig = diode_ladder(asig, acut, p11, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig
aL, aR panner asig, asig, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 4 ; Hoover Bass


i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

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

aL, aR panner adecl* aenv, adecl* aenv, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 5 ; HiHats 808

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

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

aL, aR panner a808*aenv, a808*aenv, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 6 ; Simple subtractive-FM

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

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

aL, aR panner audio*aenv, audio*aenv, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 7 ; SuperSaw

;adapted from Steven Yi Livecode.orc

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

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

aL, aR panner asig*aenv, asig*aenv, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 8	;String pad from Bay at Night, Diaz

i_dur = p3
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

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
asig = butterlp(asig, (iamp * 127 - 60)*40+600)

aL, aR panner asig, asig, i_pan

zawm  aL, 1
zawm  aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 9 ; Karplus - Strong

i_dur = p3
i_vol = p4
i_rev = p5
i_del = p6
i_pan = p7
i_chor = p8

kpitch = expseg:k(cpsmidinn(p9), p3, 432)

asig = pluck(1, cpsmidinn(p9), 432, 0, 4, p10, (49*p11)+1) 

aL, aR panner asig, asig, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 10 ; phaserSynth 

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
iadp2 = abs(abs(isim) - abs(iadp1))
itable = p15
itable2 = p16
iwmix = p17
itune = p18
isep = p19
imode = p20
ienv_amp = p21
ifb = p22

aenv = linseg:a(0, (idur -0.02)*iad+0.01, 1,   (idur -0.02)*(1-iad)+0.01, 0)

if (iadp1 >= 0 && isim >= 0) then
kep1 = linseg:k(0, idur*iadp1, ienv_amp, idur*(1-iadp1), 0)
kep2 = linseg:k(0, idur*iadp2, ienv_amp, idur*(1-iadp2), 0)
elseif (iadp1 < 0 && isim >= 0) then
iadp1 = abs(iadp1)
kep1 = linseg:k(ienv_amp, idur*iadp1, 0, idur*(1-iadp1), ienv_amp)
kep2 = linseg:k(ienv_amp, idur*iadp2, 0, idur*(1-iadp2), ienv_amp)
elseif (iadp1 >= 0 && isim < 0) then
kep1 = linseg:k(0, idur*iadp1, ienv_amp, idur*(1-iadp1), 0)
kep2 = linseg:k(ienv_amp, idur*iadp2, 0, idur*(1-iadp2), ienv_amp)
elseif (iadp1 < 0 && isim < 0) then
iadp1 = abs(iadp1)
kep1 = linseg:k(ienv_amp, idur*iadp1, 0, idur*(1-iadp1), ienv_amp)
kep2 = linseg:k(0, idur*iadp2, ienv_amp, idur*(1-iadp2), 0)
endif


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

ap1 phaser2 a3, kep1 * (sr/2 - icf) + icf, ires, 6, imode, kep1*isep, ifb
ap2 phaser2 a3 + ap1, kep2 * (sr/2 - icf) + icf, ires, 6, imode, kep2*isep, ifb

ap2 *= aenv 
ap1 *= aenv
a3 *= aenv 
ap2 += a3


ap2 = declick(ap2)

ap2 dam ap2, 0.8, 0.8, 2, 0.01, 0.3 

aL, aR panner ap2, ap2, i_pan

zawm i_vol * aL, 1
zawm i_vol * aR, 2

zawm i_rev * aL, 3
zawm i_rev * aR, 4

zawm i_del * aL, 7
zawm i_del * aR, 8

zawm i_chor * aL, 11
zawm i_chor * aR, 12

endin

instr 550 ; ReverbSC

arvbL zar 3
arvbR zar 4

aoutL, aoutR reverbsc arvbL, arvbR, gkfbrev, gkcfrev

zawm aoutL * gkvolrev, 5
zawm aoutR * gkvolrev, 6

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

instr 100 ; Model:Cycles MIDI Out

ivol = p4
irev = p5
idel = p6
ipan = p7
ichan = p8
inote = p9
ivel =  p10
ipitch = p11
idecay = p12
icolor = p13
ishape = p14
isweep = p15
icontour = p16
;ilfospeed = p17
;iswing = p18
;ichance = p19
;imachine = p20
;ipunch = p21
;igate = p22

;midiout_i 176, ichan, 7, ivol
;midiout_i 176, ichan, 65, ipitch
;midiout_i 176, ichan, 80, idecay
;midiout_i 176, ichan, 10, ipan
;
;midiout_i 176, ichan, 12, idel
;midiout_i 176, ichan, 13, irev
;
;midiout_i 176, ichan, 16, icolor
;midiout_i 176, ichan, 17, ishape
;midiout_i 176, ichan, 18, isweep
;midiout_i 176, ichan, 19, icontour

midion ichan, inote, ivel

endin 

instr 552	;Spectral Chorus

 a1 zar 11
 a2 zar 12

 kdlyml = gkdelchorus ;delay in milliseconds
 kblurtime = gkdelchorus / 1000
 
 ak1 = oscili:k(kdlyml/gkdivchorus, 1)
 ar1l = vdelay3(a1, kdlyml/5+ak1, 900)
 ar1r = vdelay3(a2, kdlyml/5+ak1, 900)
 ak2 = oscili:k(kdlyml/gkdivchorus, .995)
 ar2l = vdelay3(a1, kdlyml/5+ak2, 700)
 ar2r = vdelay3(a2, kdlyml/5+ak2, 700)
 ak3 = oscili:k(kdlyml/gkdivchorus, 1.05)
 ar3l = vdelay3(a1, kdlyml/5+ak3, 700)
 ar3r = vdelay3(a2, kdlyml/5+ak3, 700)
 ak4 = oscili:k(kdlyml/gkdivchorus, 1)
 ar4l = vdelay3(a1, kdlyml/5+ak4, 900)
 ar4r = vdelay3(a2, kdlyml/5+ak4, 900)
 aoutl = (ar1l+ar2l+ar3l+ar4l)*.5
 aoutr = (ar1r+ar2r+ar3r+ar4r)*.5
 
; fsigl pvsanal aoutl, 512, 128, 1024, 0  
; fsiglBlur pvsblur fsigl, kblurtime, 5
; aBlurL pvsynth fsiglBlur
; 
; fsigr pvsanal aoutl, 512, 128, 1024, 0  
; fsigrBlur pvsblur fsigr, kblurtime, 5
; aBlurR pvsynth fsigrBlur

aL = aoutl * gkvolchorus
aR = aoutr * gkvolchorus

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
