;KAIROS.csd

;the audio engine for the kairos live coding library

;Leonardo Foletto, 2018-2022

<CsoundSynthesizer>
<CsOptions>
-odac1
--port=11000
-d
-B 129
-b 65 
--opcode-lib=~/Users/$USER/Library/csound/7.0/plugins64


</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 16
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
; p8 : chorus (0 - 1)

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

; Waveloss

gkvolWaveloss init 0 
gkdropWaveloss init 0
gkmaxWaveloss init 50

gkvolWaveloss chnexport "wl", 1, 2, 1, 0, 1
gkdropWaveloss chnexport "dropwl", 1, 2, 1, 0, 1
gkmaxWaveloss chnexport "maxwl", 1, 2, 5, 1, 100
gkmodeWaveloss chnexport "modewl", 1, 2, 0, 0, 1

;opcode for declicking an audio signal.
;Should only be used in instruments that have positive p3 duration.
;taken from Steven Yi livecode.orc
opcode declick, a, a
ain xin
aenv = linseg:a(0, 0.01, 1, p3 - 0.02, 1, 0.01, 0)
xout ain * aenv
endop

;opcode to load an audio file into a table.
opcode loadSample, i, S
Sample xin
iNum ftgen 0, 0, 0, -1, Sample, 0, 0, 0
xout iNum
endop
opcode dtmf, a, Skk
    Sbutton, kamp_low, kamp_high xin

    kRowFreqs[] fillarray 697, 770, 852, 941
    kColFreqs[] fillarray 1209, 1336, 1477, 1633

    ; Determine frequencies based on the button input (Sbutton)
    ; DTMF Layout mapping:
    ;           Col0    Col1    Col2    Col3
    ; Row0:     1       2       3       A
    ; Row1:     4       5       6       B
    ; Row2:     7       8       9       C
    ; Row3:     *       0       #       D

    kfreq_low = kRowFreqs[0]
    kfreq_high = kColFreqs[0]

    if strcmpk(Sbutton, "1") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "2") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "3") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "A") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[3]
    elseif strcmpk(Sbutton, "4") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "5") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "6") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "B") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[3]
    elseif strcmpk(Sbutton, "7") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "8") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "9") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "C") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[3]
    elseif strcmpk(Sbutton, "*") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "0") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "#") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "D") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[3]
    endif

    ; Generate the tones

    aosc_low poscil kamp_low, kfreq_low, gisine    ; Assumes gisine is defined globally
    aosc_high poscil kamp_high, kfreq_high, gisine  ; Assumes gisine is defined globally
    xout aosc_low + aosc_high
endop

;opcode to generate dtmf tones
;Should only be used in instruments that have positive p3 duration.
opcode ADEnv, a, i
iAD xin 
if iAD > 1 then 
iADD = iAD % 1
aEnv = expseg:a(0.001, (p3 -0.02)*iADD+0.01, 1, (p3 -0.02)*(1-iADD)+0.01, 0.001)
else 
aEnv = linseg:a(0, (p3 -0.02)*iAD+0.01, 1, (p3 -0.02)*(1-iAD)+0.01, 0)
endif
xout aEnv
endop

instr 1 ; Sampler

p3 = filelen(p9)
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8

inchs = filenchnls(p9)
kthreshold = p11
iratio = p12

if inchs == 1 then

aLeft diskin2 p9, p10
aL, aR pan3 aLeft, aLeft, ipan, 1


else

aLeft, aRight diskin2 p9, p10

aL, aR pan3 aLeft, aRight, ipan, 1

endif

icomp1 = 1/iratio
irtime = 0.01
iftime = 0.05
aL dam aL, kthreshold, icomp1, 1, irtime, iftime
aR dam aR, kthreshold, icomp1, 1, irtime, iftime

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12


endin


instr 2 ; Stutter

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
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
isamdur = idur * sr
isamslice =  ceil(isamdur / i_repeat)
ipos = i_pick / i_divisor * isize

andx =  phasor(sr/isamslice) * isamslice + ipos

if inchs == 1 then
isam ftgen 0, 0, isize, -1, Sname, 0, 0, 1
aL table3 andx, isam, 0
aL, aR pan3 aL, aL, ipan, 1
else
iLeft ftgen 0, 0, isize, -1, Sname, 0, 0, 1
iRight ftgen 0, 0,isize, -1, Sname, 0, 0, 2
aL table3 andx, iLeft, 0
aR table3 andx, iRight, 0
aL, aR pan3 aL, aR, ipan, 1
endif

aL = declick(aL)
aR = declick(aR)

icomp1 = 1/iratio
irtime = 0.01
iftime = 0.05
aL dam aL, kthreshold, icomp1, 1, irtime, iftime
aR dam aR, kthreshold, icomp1, 1, irtime, iftime

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 3 ; Bass 303

;adapted from Steven Yi Livecode.orc

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
ifreq = cpsmidinn(p9)
icf = p10
ires = p11

imode = p12

acut = 200 + expon(1, idur, 0.001) * icf
asig = vco2(1, ifreq, imode)
asig = diode_ladder(asig, acut, ires, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig
aL, aR pan3 asig, asig, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 4 ; Hoover Bass

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
ifreq = cpsmidinn(p9)
icf = p10
ires = p11
iad = p12

ifEnvMax = 1.333*ifreq >= sr/2 ? ifreq : 1.333*ifreq

kcfEnv = expon(ifEnvMax, p3, icf)

aenv = ADEnv(iad)

kr3 unirand 1
kr3 port kr3, 0.01
klf3 = lfo:k(0.5, 1.5*kr3, 0)
klf3 = limit((klf3+0.5), 0.05, 0.95)
a1 = vco2(1, ifreq,4,(klf3*0.01))
a2 = vco2(1, ifreq*(0.08+(7/12)),4,(klf3*0.01))
a3 = vco2(1, ifreq*0.52)
af = a1 + a3 * 0.88 + a2 * 0.66
ao = diode_ladder(af, kcfEnv, ires)
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

aL, aR pan3 adecl* aenv, adecl* aenv, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 5 ; HiHats 808

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
iopen = p9
itune = p10

pa        =        (iopen >= 0.5 ? 1 : .15)   ; Select open or closed
ifreq1    =        540*itune                     ; Tune
ifreq2    =        800*itune                     ; Tune

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

aL, aR pan3 a808*aenv, a808*aenv, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 6 ; Simple subtractive-FM

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8

kindx = p15
kcar = p13
kfilt = p10
kdpth = p14
iad = p12
kres = p11
aenv = ADEnv(iad)

amod = poscil(1, cpsmidinn(p9) * kindx, gisine)
acar = poscil(1, cpsmidinn(p9) + amod * kdpth * sr/4, gisine)

audio = diode_ladder(acar, kfilt, kres , 1, 1.5)

aL, aR pan3 audio*aenv, audio*aenv, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 7 ; SuperSaw

;adapted from Steven Yi Livecode.orc && inspired by the JP8080

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
ifreq = cpsmidinn(p9)
icf = p10
ires = p11
iad = p12
idetune = p13
imix = p14

idetune *= idetune 

aenv  = ADEnv(iad)

kcfEnv  = linseg(ifreq, (p3 -0.02)*iad+0.01, icf,   (p3 -0.02)*(1-iad)+0.01, 200)

; kcfEnv = expon(icf, p3, ifreq/8)

asig1 = vco2(1,  ifreq)

asig = vco2(imix,  ifreq * (1 + idetune * -0.107))
asig += vco2(imix,  ifreq * (1 + idetune * -0.061))
asig += vco2(imix,  ifreq * (1 + idetune * -0.019))
asig += vco2(imix,  ifreq * (1 + idetune * 0.019))
asig += vco2(imix,  ifreq * (1 + idetune * 0.064))
asig += vco2(imix,  ifreq * (1 + idetune * 0.11))

asig *= 0.17

asig += asig1

asig = zdf_ladder(asig, kcfEnv, ires)
asig = K35_hpf(asig, ifreq * 0.5, 2)

asig = declick(asig)

aL, aR pan3 asig*aenv, asig*aenv, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 8	;String pad from Bay at Night, Diaz

idur = p3
irev = p5
idel = p6
ipan = p7
ichor = p8

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

aL, aR pan3 asig, asig, ipan, 1

zawm  aL, 1
zawm  aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 9 ; Karplus - Strong

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8

kpitch = expseg:k(cpsmidinn(p9), p3, 432)

asig = pluck(1, cpsmidinn(p9), 432, 0, 4, p10, (49*p11)+1) 

aL, aR pan3 asig, asig, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 10 ; phaserSynth 

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8

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

aenv = ADEnv(iad)

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

aL, aR pan3 ap2, ap2, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 11 ;DTMF


idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8

SBut = p9
iYAmp = p10
iXAmp = p11
iADRatio = p12

aDTMF = dtmf(SBut, iXAmp, iYAmp)

aenv = ADEnv(iADRatio)

aDTMF = aDTMF * aenv

aL, aR pan3 aDTMF, aDTMF, ipan, 1

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

endin

instr 100 ; Model:Cycles / Model:Samples MIDI Out

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
midiout_i 176, ichan, 10, ipan
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

instr 101 ; Model:Cycles chord control

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
midiout_i 176, ichan, 10, ipan
;
;midiout_i 176, ichan, 12, idel
;midiout_i 176, ichan, 13, irev
;
;midiout_i 176, ichan, 16, icolor
midiout_i 176, ichan, 17, ishape
;midiout_i 176, ichan, 18, isweep
;midiout_i 176, ichan, 19, icontour

midion ichan, inote, ivel

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
aLW waveloss aL * gkvolWaveloss, gkdropWaveloss * gkmaxWaveloss, gkmaxWaveloss, 0
arW waveloss aR * gkvolWaveloss, gkdropWaveloss * gkmaxWaveloss, gkmaxWaveloss, 0
aL *= (1-gkvolWaveloss)
aR *= (1-gkvolWaveloss)
aL += aLW
aR += arW
aL *= gkvolMaster
aR *= gkvolMaster

outs aL, aR
zacl 0, 50

endin

</CsInstruments>
<CsScore>
i 999 0 -1
i 550 0 -1
i 551 0 -1
i 552 0 -1

</CsScore>
</CsoundSynthesizer>

