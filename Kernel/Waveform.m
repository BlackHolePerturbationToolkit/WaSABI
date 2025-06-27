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
GetAmplitudes::usage = "Fetches the waveform amplitudes.";
WaveformMode::usage = "Returns a given spin weighted spherical harmonic mode amplitude of the gravitaional wave strain";


(* ::Subsection:: *)
(*Error messages*)


GetAmplitudes::nomodel = "Unknown model";


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*Get Amplitudes*)


(* ::Text:: *)
(*Pass in a string label of model,  returns association of all available mode amplitudes for the model.*)
(**)
(*To do:*)
(*Query available modes*)
(*Specify which modes you want*)
(*Add relations for negative m modes etc.*)


GetAmplitudes[model_, evolveprimary_:True]:=Block[{filelocation},

filelocation=If[evolveprimary==True,
Which[model=="DevTest","AmplitudeModels/devtestAmp.m", model=="1PAT1","AmplitudeModelsModels/1PAT1eAmp.m",model=="1PAT1R","AmplitudeModels/1PAT1ReAmp.m",model=="Hybrid","AmplitudeModels/HybrideAmp.m",True, Message[GetAmplitudes::nomodel]],
Which[model=="1PAT1","AmplitudeModels/1PAT1Amp.m",model=="1PAT1R","AmplitudeModels/1PAT1RAmp.m",model=="Hybrid","AmplitudeModels/HybridAmp.m",True, Message[GetAmplitudes::nomodel]]
];

(*Assume amplitude files saved as association list with iconised expressions for all the amplitudes available.*)
Get[StringJoin[$UserBaseDirectory,"/Applications/WASABI/",filelocation]]

]


(* ::Section:: *)
(*Waveform modes*)


(* ::Text:: *)
(*Trivial function in which you pass in the given inspiral rules and amplitude, and it returns the waveform mode. *)


WaveformMode[mode_, amp_, inspiral_]:= Block[{\[ScriptL]=mode[[1]],m=mode[[2]]},
Do[
(*Set variables in amplitude to their time dependent quantities*)
inspiral["Parameters"][[i]][t_]:=inspiral[ToString[inspiral["Parameters"][[i]]]][t],{i,1,Length[inspiral["Parameters"][[i]]]}];

(amp*Exp[-I m \[Phi][t]])
(*Need to think about how best to do this in precessing case? Or do not release precessing case until frame figured out?*)
]


(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
