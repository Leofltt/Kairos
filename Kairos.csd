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

; TABLES
gisine   ftgen 1, 0, 4096, 10, 1; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, 0, 2048, 0 ; Square wave

; GLOBAL VARIABLES
garvL, garvbR init 0

instr 1 ;Sampler

; HOW TO EVALUATE IF A SAMPLE IS STEREO OR MONO AND DECIDE ON THE NUMBER OF OUTS BASED ON THAT?

aLeft diskin p4
outs aLeft, aLeft

endin

instr 5 ; HiHats 808
    
pa       =        (p3 >= 0.5 ? 1 : .15)         ; Select open or closed
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
outs a808*aenv, a808*aenv                  

endin

instr 666 ; ReverbSC

aoutL, aoutR reverbsc garvL, garvbR, 0.5, 12000
outs aoutL, aoutR

clear garvL, garvbR

endin


</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>

