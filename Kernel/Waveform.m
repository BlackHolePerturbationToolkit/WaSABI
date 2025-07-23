(* ::Package:: *)

(* ::Title:: *)
(*Waveform subpackage of WASABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WASABI`Waveform`",{"WASABI`Inspiral`"}];


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


GetAmplitudes::nomodel = "Unknown model";
GetAmplitudes::nomode = "Mode not available";
GetAmplitudes::notlist = "Input should be a list of strings for each (l,m) mode";


(* ::Section:: *)
(*Get Amplitudes*)


$WASABIAmplitudeDirectory = FileNameJoin[{FileNameDrop[FindFile["WASABI`"], -2], "AmplitudeModels"}];


(* ::Text:: *)
(*List amplitude models:*)


ListAmplitudeModels[] :=
 Module[{ampdirctory, ampmodels},
  ampmodels = FileBaseName /@ FileNames["*.m", $WASABIAmplitudeDirectory];
  ampmodels
]


(* ::Text:: *)
(*Pass in a string label of model,  returns association of all available mode amplitudes for the model.*)
(*To do:*)
(*Add relations for negative m modes.*)


GetAmplitudes[model_, modes_:{}] :=
 Module[{filelocation, amps, selectedamps},
  If[!ListQ[modes],
    Message[GetAmplitudes::notlist];
    Return[];
  ];

  If[!MemberQ[ListAmplitudeModels[], model],
    Message[GetAmplitudes::nomodel];
    Return[];
  ];

  filelocation = First[FileNames[model<>".m", $WASABIAmplitudeDirectory]];

  Begin["WASABI`Inspiral`Model"<>model<>"`"];
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
 Module[{filelocation, amps},
  If[!MemberQ[ListAmplitudeModels[], model],
    Message[GetAmplitudes::nomodel];
    Return[];
  ];
  filelocation = First[FileNames[model<>".m", $WASABIAmplitudeDirectory]];

  (*Assume amplitude files saved as association list with iconised expressions for all the amplitudes available.*)
  Begin["WASABI`Inspiral`Model"<>model<>"`"];
  amps = Get[filelocation];
  End[];
  Keys[amps]
]


(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
