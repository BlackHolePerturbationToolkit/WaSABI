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


BinaryInspiral[ics_, opts:OptionsPattern[]] := Module[{model, inspiral},
  model = OptionValue["Model"];
  inspiral = WASABI`Inspiral`Private`IntInspiral[model, ics];
  BinaryInspiralModel[<|"Model" -> model, "InitialConditions" -> ics, "Inspiral" -> inspiral|>]
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
  extended = {};
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


BinaryInspiralModel[assoc_][key_String] /; !MemberQ[{"Waveform", "Trajectory"}, key] && KeyExistsQ[assoc, key] := assoc[key];


Keys[m_BinaryInspiralModel] ^:= DeleteCases[Join[Keys[m[[-1]]], {"Waveform", "Trajectory"}], "Inspiral"];


(* ::Subsection:: *)
(*Waveform*)


BinaryInspiralModel[assoc_]["Waveform"][t:(_?NumericQ|{_?NumericQ..})] :=
  assoc["Waveform"][t];


(* ::Subsection:: *)
(*Trajectory*)


BinaryInspiralModel[assoc_]["Trajectory"][t:(_?NumericQ|{_?NumericQ..})] :=
  assoc["Trajectory"][t];


(* ::Section:: *)
(*End Package*)


(* ::Subsection:: *)
(*Protect symbols*)


SetAttributes[{BinaryInspiral, BinaryInspiralModel}, {Protected, ReadProtected}];


(* ::Subsection::Closed:: *)
(*End*)


End[];
EndPackage[];
