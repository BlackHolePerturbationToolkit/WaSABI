(* ::Package:: *)

(* ::Title:: *)
(*Waveform subpackage of WaSABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WaSABI`Waveform`"];


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Usage messages*)


ListAmplitudeModels::usage = "Returns the list of available amplitude models"
ModeList::usage = "Returns the list of available amplitude modes";
GetAmplitudes::usage = "Fetches the waveform amplitudes.";
WaveformMode::usage = "Returns a given spin weighted spherical harmonic mode amplitude of the gravitaional wave strain";


(* ::Subsubsection:: *)
(*Error messages*)


GetAmplitudes::nomode = "Mode not available";
GetAmplitudes::notlist = "Input should be a list of strings for each (l,m) mode";


(* ::Section:: *)
(*Get Amplitudes*)


$WaSABIAmplitudeDirectory = FileNameJoin[{FileNameDrop[FindFile["WaSABI`"], -2], "AmplitudeModels"}];


(* ::Text:: *)
(*List amplitude models:*)


ListAmplitudeModels[] := ListAmplitudeModels[] =
 Module[{ampdirctory, ampmodels},
  ampmodels = FileBaseName /@ FileNames["*.m", $WaSABIAmplitudeDirectory];
  ampmodels
]


WaveformModelExistsQ[model_String] :=
  MemberQ[ListAmplitudeModels[], model];


(* ::Text:: *)
(*Pass in a string label of model,  returns association of all available mode amplitudes for the model.*)
(*To do:*)
(*Add relations for negative m modes.*)


GetAmplitudes[model_, modes_:{}] := GetAmplitudes[model, modes] =
 Module[{filelocation, amps, selectedamps},
  If[!ListQ[modes],
    Message[GetAmplitudes::notlist];
    Return[];
  ];

  filelocation = First[FileNames[model<>".m", $WaSABIAmplitudeDirectory]];

  Begin["WaSABI`Inspiral`Model"<>model<>"`"];
  amps = Get[filelocation];
  End[];

  selectedamps = If[modes=={}, amps, KeyTake[amps, modes]];
  If[Length[selectedamps]==0,
    Message[GetAmplitudes::nomode];
    Return[];
  ];

  selectedamps
]


ModeList[model_] :=
  Keys[GetAmplitudes[model]];


(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
