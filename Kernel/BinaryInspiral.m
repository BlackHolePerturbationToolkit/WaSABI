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


BinaryInspiralModel::usage = "BinaryInspiral[...] represents a binary inspiral.";


(* ::Subsection:: *)
(*Error Messages*)


(* ::Subsection::Closed:: *)
(*Begin Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*BinaryInspiral*)


Options[BinaryInspiral] = {"Model" -> "1PAT1"};


BinaryInspiral[ics_, opts:OptionsPattern[]] := Module[{model, inspiral, amplitudes},
  model = OptionValue["Model"];
  inspiral = WASABI`Inspiral`Private`IntInspiral[model, ics];
  amplitudes = WASABI`Waveform`Private`GetAmplitudes[model];
  BinaryInspiralModel[<|"Model" -> model, "InitialConditions" -> ics, "Inspiral" -> inspiral, "Amplitudes" -> amplitudes|>]
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
  extended = {BoxForm`SummaryItem[{"Trajectory: ", SymbolName /@ assoc["Inspiral"]["Parameters"]}]};
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


BinaryInspiralModel[assoc_]["Waveform"][l_, m_][t:(_?NumericQ|{_?NumericQ..})] :=
 Module[{params, paramvals, \[Phi]p},
  params = assoc["Inspiral"]["Parameters"];
  paramvals = Map[# -> assoc["Inspiral"][SymbolName[#]][t] &, params];
  \[Phi]p = SelectFirst[params, SymbolName[#] == "\[Phi]"&];
  assoc["Amplitudes"]["("<>ToString[l]<>","<>ToString[m]<>")"] Exp[-I m \[Phi]p] /. paramvals
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
