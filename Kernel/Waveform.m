(* ::Package:: *)

(* ::Title:: *)
(*Waveform subpackage of WASABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WASABI`Waveform`",{"WASABI`Inspiral`"}];


(* ::Subsection:: *)
(*Usage messages*)


(*Fucntions*)
ModeList::usage = "Returns the list of available amplitude modes";
GetAmplitudes::usage = "Fetches the waveform amplitudes.";
WaveformMode::usage = "Returns a given spin weighted spherical harmonic mode amplitude of the gravitaional wave strain";


(* ::Subsection:: *)
(*Error messages*)


GetAmplitudes::nomodel = "Unknown model";
GetAmplitudes::nomode = "Mode not available";
GetAmplitudes::notlist = "Input should be a list of strings for each (l,m) mode";


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*Get Amplitudes*)


(* ::Text:: *)
(*Pass in a string label of model,  returns association of all available mode amplitudes for the model.*)
(*To do:*)
(*Add relations for negative m modes.*)


GetAmplitudes[model_, modes_:{}]:=Block[{filelocation,amps, selectedamps},

If[ListQ[modes],
filelocation=
Which[model=="DevTest","AmplitudeModels/devtestAmp.m", model=="1PAT1","AmplitudeModelsModels/1PAT1eAmp.m",model=="1PAT1R","AmplitudeModels/1PAT1ReAmp.m",model=="Hybrid","AmplitudeModels/HybrideAmp.m",True, Message[GetAmplitudes::nomodel];Return[]];

amps=Get[StringJoin[$UserBaseDirectory,"/Applications/WASABI/",filelocation]];

selectedamps=If[modes=={}, amps, KeyTake[amps, modes]];
If[Length[selectedamps]==0,Message[GetAmplitudes::nomode];Return[],selectedamps]


, Message[GetAmplitudes::notlist];Return[]]
]


ModeList[model_]:=Block[{filelocation, amps},

filelocation=
Which[model=="DevTest","AmplitudeModels/devtestAmp.m", model=="1PAT1","AmplitudeModelsModels/1PAT1eAmp.m",model=="1PAT1R","AmplitudeModels/1PAT1ReAmp.m",model=="Hybrid","AmplitudeModels/HybrideAmp.m",True, Message[GetAmplitudes::nomodel];Return[]];

(*Assume amplitude files saved as association list with iconised expressions for all the amplitudes available.*)
amps=Get[StringJoin[$UserBaseDirectory,"/Applications/WASABI/",filelocation]];
Keys[amps]
]


(* ::Section:: *)
(*Waveform modes*)


(* ::Text:: *)
(*Trivial function in which you pass in the given inspiral rules and time values, and it returns the timeseries of the waveform mode. (Pass in the inspiral to avoid repeated integration).*)


WaveformMode[Ampmodel_, mode_, inspiral_,tvals_]:= Block[{amp, modevals, l, m},
amp=GetAmplitudes[Ampmodel, {mode}][mode];
modevals=ToExpression@StringReplace[mode, {"(" -> "{", ")" -> "}" }];
l=modevals[[1]];
m=modevals[[2]];

InspiralEvaluate[amp*Exp[-I m \[Phi]], inspiral, tvals]
]


(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
