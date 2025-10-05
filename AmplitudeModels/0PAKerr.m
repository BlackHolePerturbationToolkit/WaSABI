(* ::Package:: *)

(* ::Section::Closed:: *)
(*Function on phase space*)


(* ::Subsection::Closed:: *)
(*ISCO radius*)


rISCO[\[Chi]1_]:=3+Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]-Sign[\[Chi]1] \[Sqrt]((2-(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (4+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))+2 Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]));


(* ::Section::Closed:: *)
(*Construct Interpolations*)


(* ::Subsection::Closed:: *)
(*Fetch data*)


directoryamp = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Kerr_Circ"}];


(* ::Subsection::Closed:: *)
(*Real 22 amplitude  1SF (Wishlist - more modes on new grid).*)


RealAmp1SFKerr22=Interpolation[Get[FileNameJoin[{directoryamp, "1SF_RealAmp_KerrCirc_22_NewGrid.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Section::Closed:: *)
(*Mode amplitudes (Wishlist - more modes on new grid).*)


(* ::Subsection::Closed:: *)
(*Mode amplitudes*)


(* ::Input::Initialization:: *)
<|"(2,2)"->(1/(2 (1-\[Chi]1 \[Omega])^(2/3)))M \[Nu] \[Omega]^(2/3) RealAmp1SFKerr22[0.7988880369434854` +0.2901432317650783` Log[1-\[Chi]1],-1+2.` Sqrt[(\[Omega]^(2/3) rISCO[\[Chi]1])/(1-\[Chi]1 \[Omega])^(2/3)]]|>
