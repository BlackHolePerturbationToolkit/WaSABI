(* ::Package:: *)

(* ::Title:: *)
(*Waveform subpackage of WASABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WASABI`Waveform`"];


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
GetAmplitudes::notlist = "Input should be a list";


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*Get Amplitudes*)


(* ::Text:: *)
(*Pass in a string label of model,  returns association of all available mode amplitudes for the model.*)
(**)
(*To do:*)
(*Add relations for negative m modes.*)


GetAmplitudes[model_, modes_:{}]:=Block[{filelocation,amps, selectedamps},

filelocation=
Which[model=="DevTest","AmplitudeModels/devtestAmp.m", model=="1PAT1","AmplitudeModelsModels/1PAT1eAmp.m",model=="1PAT1R","AmplitudeModels/1PAT1ReAmp.m",model=="Hybrid","AmplitudeModels/HybrideAmp.m",True, Message[GetAmplitudes::nomodel]];
(*Return error if modes is not a list*)
(*Return error if requested mode is not available*)

(*Amplitude files saved as association list.*)
amps=Get[StringJoin[$UserBaseDirectory,"/Applications/WASABI/",filelocation]];

selectedamps=If[modes=={}, amps, KeyTake[amps, modes]
]]


ModeList[model_]:=Block[{filelocation, amps},

filelocation=
Which[model=="DevTest","AmplitudeModels/devtestAmp.m", model=="1PAT1","AmplitudeModelsModels/1PAT1eAmp.m",model=="1PAT1R","AmplitudeModels/1PAT1ReAmp.m",model=="Hybrid","AmplitudeModels/HybrideAmp.m",True, Message[GetAmplitudes::nomodel]];

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

EvaluateOnInspiral[amp*Exp[-I m \[Phi]], inspiral, tvals]
]
(*Need to think about how best to do this in precessing case? Or do not release precessing case until frame figured out?*)



(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
