# Revision history for dobutokO-effects

## 0.1.0.0 -- 2020-06-27

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-06-29

* Second version. Added a new module DobutokO.Sound.Effects.Remix to deal with the SoX "remix" effect.

## 0.3.0.0 -- 2020-06-30

* Third version. Added new modules DobutokO.Sound.Effects.Timespec and DobutokO.Sound.Effects.Delay. Some minor documentation improvements.

## 0.3.1.0 -- 2020-06-30

* Third version revised A. Fixed issue for GHC 7.8* series with ambiguous mconcat usage.

## 0.4.0.0 -- 2020-07-02

* Fourth version. Added new modules DobutokO.Sound.Effects.Specs, DobutokO.Sound.Effects.PassReject, DobutokO.Sound.Effects.BassTreble. 

## 0.5.0.0 -- 2020-07-04

* Fifth version. Added new modules DobutokO.Sound.Effects.Trim, DobutokO.Sound.Effects.Repeat, DobutokO.Sound.Effects.Phaser, 
DobutokO.Sound.Effects.Chorus, DobutokO.Sound.Effects.Modulation2, DobutokO.Sound.Effects.Echo, DobutokO.Sound.Effects.Misc, 
DobutokO.Sound.Effects.Channels, DobutokO.Sound.Effects.Bend,Segment, DobutokO.Sound.Effects.Pitch, DobutokO.Sound.Effects.Tempo, 
DobutokO.Sound.Effects.Speed.

## 0.6.0.0 -- 2020-07-06

* Sixth version. Added new modules DobutokO.Sound.ToRange, DobutokO.Sound.Effects.Biquad, DobutokO.Sound.Effects.Contrast, 
DobutokO.Sound.Effects.DCShift, DobutokO.Sound.Effects.Downsample, DobutokO.Sound.Effects.Upsample, DobutokO.Sound.Effects.Hilbert, 
DobutokO.Sound.Effects.Loudness, DobutokO.Sound.Effects.Overdrive, DobutokO.Sound.Effects.Tremolo.

## 0.7.0.0 -- 2020-07-09

* Seventh version. Fixed issue with DobutokO.Sound.Effects.Timespec module with timeSpecC function. Added DobutokO.Sound.One, DobutokO.Sound.Effects.Noise, 
DobutokO.Sound.Effects.Pad, DobutokO.Sound.Effects.MCompand modules. Some minor code improvements.

## 0.7.1.0 -- 2020-07-09

* Seventh version revised A. Fixed issue with DobutokO.Sound.Effects.Pad module with double (ambiguous) import of mconcat. 

## 0.8.0.0 -- 2020-07-11

* Eighth version. Added new modules DobutokO.Sound.Effects.Dither, DobutokO.Sound.Effects.FIR, DobutokO.Sound.Effects.Flanger, DobutokO.Sound.Effects.Gain, 
DobutokO.Sound.Effects.LADSPA, DobutokO.Sound.Effects.Rate, DobutokO.Sound.Effects.Vol. Some minor code improvements.

## 0.9.0.0 -- 2020-07-13

* Ninth version. Fixed issue with being wrongly defined Show instance in the DobutokO.Sound.Effects.Rate module. Added new modules 
DobutokO.Sound.Effects.Silence, DobutokO.Sound.Effects.Sinc, DobutokO.Sound.Effects.Stretch. 

## 0.10.0.0 -- 2020-07-15

* Tenth version. Fixed issue with being out of range application of the toRange function in the modules: DobutokO.Sound.Effects.Dither, 
DobutokO.Sound.Effects.Flanger, DobutokO.Sound.Effects.Rate, DobutokO.Sound.Effects.Reverb, DobutokO.Sound.Effects.Sinc, DobutokO.Sound.Effects.Remix. 
Fixed issue with the empty list in the MscS data in the DobutokO.Sound.Effects.Misc module. Added new modules DobutokO.Sound.Effects.Spectrogram, 
DobutokO.Sound.Effects.Splice, DobutokO.Sound.Effects.Stat, DobutokO.Sound.Effects.Stats, DobutokO.Sound.Effects.Vad.

## 0.10.1.0 -- 2020-07-15

* Tenth version revised. Fixed issue with being not properly defined usage of mconcat for GHC 7.8* series. 

## 0.11.0.0 -- 2020-07-18

* Eleventh version. Added a new module DobutokO.Sound.Effects.Classes with classes for the data types in other modules. Some code improvements. 

## 0.12.0.0 -- 2020-07-21

* Twelfth version. Splitted the module DobutokO.Sound.Effects.Classes into four modules DobutokO.Sound.Effects.Classes.FstParam, 
DobutokO.Sound.Effects.Classes.SndParam, DobutokO.Sound.Effects.Classes.ThdParam, DobutokO.Sound.Effects.Classes.FourthParam. 
Added new modules DobutokO.Sound.Effects.Classes.FstParamSet, DobutokO.Sound.Effects.Classes.SndParamSet, DobutokO.Sound.Effects.Classes.ThdParamSet, 
DobutokO.Sound.Effects.Classes.FourthParamSet and DobutokO.Sound.Effects.Classes.ComplexParamSet. Some code changes and improvements to make 
it more usable with classes.

## 0.13.0.0 -- 2020-07-25

* Thirteenth version. Added README.markdown file. Extended the Set classes (where appropriate) with setG methods to provide unambiguous usage for the instances.
Added functions for conversion to the list of the One2 data. Some code improvements.
