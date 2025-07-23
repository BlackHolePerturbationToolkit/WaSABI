(* ::Package:: *)

(* ::Title:: *)
(*BinaryInspiral*)


(* ::Section:: *)
(*Create Package*)


(* ::Subsection:: *)
(*BeginPackage*)


BeginPackage["WASABI`BinaryInspiral`",
  {"WASABI`Inspiral`",
   "WASABI`Waveform`"}
];


(* ::Subsection:: *)
(*Unprotect symbols*)


ClearAttributes[{BinaryInspiral, BinaryInspiralModel}, {Protected, ReadProtected}];


(* ::Subsection:: *)
(*Usage messages*)


BinaryInspiral::usage = "BinaryInspiral[ics] generates a BinaryInspiralModel representing a binary inspiral.";


BinaryInspiralModel::usage = "BinaryInspiralModel[...] represents a binary inspiral.";


(* ::Subsection:: *)
(*Error Messages*)


BinaryInspiral::nomodel = "Unknown model `1`.";


BinaryInspiral::ics = "Invalid initial conditions `1` for model `2`.";


BinaryInspiralModel::nomode = "Mode `1` not available in model `2`.";


(* ::Subsection::Closed:: *)
(*Begin Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*BinaryInspiral*)


Options[BinaryInspiral] = {"Model" -> "1PAT1"};


BinaryInspiral[ics_, opts:OptionsPattern[]] := Module[{model, inspiral, amplitudes, tmax},
  model = OptionValue["Model"];
  If[!WASABI`Inspiral`Private`InspiralModelExistsQ[model] || !WASABI`Waveform`Private`WaveformModelExistsQ[model],
    Message[BinaryInspiral::nomodel, model];
    Return[$Failed];
  ];
  If[Sort[Keys[ics]] != {"M", "r0", "\[Nu]", "\[Phi]"},
      Message[BinaryInspiral::ics, ics, model];
      Return[$Failed];
  ];
  inspiral = WASABI`Inspiral`Private`IntInspiral[model, ics];
  amplitudes = KeyMap[First[StringCases[#,"("~~l_~~","~~m_~~")":>{ToExpression[l],ToExpression[m]}]]&, WASABI`Waveform`Private`GetAmplitudes[model]];
  tmax = Max[inspiral[[1]]["Domain"]];
  BinaryInspiralModel[<|"Model" -> model, "InitialConditions" -> ics, "Inspiral" -> inspiral, "Amplitudes" -> amplitudes, "Duration" -> tmax|>]
];


(* ::Section:: *)
(*BinaryInspiralModel*)


(* ::Subsection:: *)
(*Output format*)


BinaryInspiralModel /:
 MakeBoxes[bim:BinaryInspiralModel[assoc_], form:(StandardForm|TraditionalForm)] :=
 Module[{summary, extended},
  summary = {BoxForm`SummaryItem[{"Model: ", assoc["Model"]}],
             Row[{BoxForm`SummaryItem[{"M: ", assoc["InitialConditions"]["M"]}], "  ",
                  BoxForm`SummaryItem[{"\[Nu]: ", assoc["InitialConditions"]["\[Nu]"]}]}]};
  extended = {BoxForm`SummaryItem[{"Trajectory: ", SymbolName /@ assoc["Inspiral"]["Parameters"]}],
              BoxForm`SummaryItem[{"Duration: ", assoc["Duration"]}]};
  BoxForm`ArrangeSummaryBox[
    BinaryInspiralModel,
    bim,
    None,
    summary,
    extended,
    form
  ]
];


(* ::Subsection:: *)
(*Accessing attributes*)


BinaryInspiralModel[assoc_]["Inspiral"] := Missing["KeyAbsent", "Inspiral"];


BinaryInspiralModel[assoc_]["Amplitudes"] := Missing["KeyAbsent", "Amplitudes"];


BinaryInspiralModel[assoc_][key_String] /; !MemberQ[{"Waveform", "Trajectory"}, key] && KeyExistsQ[assoc, key] := assoc[key];


Keys[m_BinaryInspiralModel] ^:= DeleteCases[Join[Keys[m[[-1]]], {"Waveform", "Trajectory"}], "Inspiral" | "Amplitudes"];


(* ::Subsection:: *)
(*Waveform*)


BinaryInspiralModel[assoc_]["Waveform"]["Modes"] := Keys[assoc["Amplitudes"]];


BinaryInspiralModel[assoc_]["Waveform"][l_, m_][t:(_?NumericQ|{_?NumericQ..})] :=
 Module[{params, paramvals, \[Phi]p},
  If[!MemberQ[BinaryInspiralModel[assoc]["Waveform"]["Modes"], {l,m}],
    Message[BinaryInspiralModel::nomode, {l,m}, assoc["Model"]];
    Return[$Failed];
  ];
  params = assoc["Inspiral"]["Parameters"];
  paramvals = Map[# -> assoc["Inspiral"][SymbolName[#]][t] &, params];
  \[Phi]p = SelectFirst[params, SymbolName[#] == "\[Phi]"&];
  assoc["Amplitudes"][{l,m}] Exp[-I m \[Phi]p] /. paramvals
];


(* ::Subsection:: *)
(*Trajectory*)


BinaryInspiralModel[assoc_]["Trajectory"][param_][t:(_?NumericQ|{_?NumericQ..})] :=
  assoc["Inspiral"][param][t];


(* ::Section:: *)
(*End Package*)


(* ::Subsection:: *)
(*Protect symbols*)


SetAttributes[{BinaryInspiral, BinaryInspiralModel}, {Protected, ReadProtected}];


(* ::Subsection::Closed:: *)
(*End*)


End[];
EndPackage[];
