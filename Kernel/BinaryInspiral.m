(* ::Package:: *)

(* ::Title:: *)
(*BinaryInspiral*)


(* ::Section::Closed:: *)
(*Create Package*)


(* ::Subsection::Closed:: *)
(*BeginPackage*)


BeginPackage["WaSABI`BinaryInspiral`",
  {"WaSABI`Inspiral`",
   "WaSABI`Waveform`"}
];


(* ::Subsection::Closed:: *)
(*Unprotect symbols*)


ClearAttributes[{BinaryInspiral, BinaryInspiralModel}, {Protected, ReadProtected}];


(* ::Subsection::Closed:: *)
(*Usage messages*)


BinaryInspiral::usage = "BinaryInspiral[ics] generates a BinaryInspiralModel representing a binary inspiral.";


BinaryInspiralModel::usage = "BinaryInspiralModel[...] represents a binary inspiral.";


(* ::Subsection:: *)
(*Error Messages*)


BinaryInspiral::nomodel = "Unknown model `1`.";


BinaryInspiral::ics = "Invalid initial conditions `1` for model `2`.";


BinaryInspiral::icsout = "Initial conditions `1`  out of supported parameter space coverage for model `2`.";


BinaryInspiralModel::nomode = "Mode `1` not available in model `2`.";


(* ::Subsection::Closed:: *)
(*Begin Private section*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*BinaryInspiral*)


Options[BinaryInspiral] = {"Model" -> "1PAT1", "Precision" -> 10, "Accuracy" -> 10};


BinaryInspiral[ics_, opts:OptionsPattern[]] := Module[{model,prec,acc, inspiral, amplitudes, tmax},
  model = OptionValue["Model"];
  prec = OptionValue["Precision"];
  acc = OptionValue["Accuracy"];
  
  If[!WaSABI`Inspiral`Private`InspiralModelExistsQ[model] || !WaSABI`Waveform`Private`WaveformModelExistsQ[model],
    Message[BinaryInspiral::nomodel, model];
    Return[$Failed];
  ];
  If[Sort[Keys[ics]] != Sort[WaSABI`Inspiral`Private`GetInspiralEquations[model][["InitialConditionsFormat"]]],
      Message[BinaryInspiral::ics, ics, model];
      Return[$Failed];
  ];
  If[Not@(And@@(WaSABI`Inspiral`Private`GetInspiralEquations[model][["ParameterSpaceCoverage"]]/.ics)),
      Message[BinaryInspiral::icsout, ics, model];
      Return[$Failed];
  ];
  
  inspiral = WaSABI`Inspiral`Private`IntInspiral[model, ics, prec, acc];
  amplitudes = KeyMap[First[StringCases[#,"("~~l_~~","~~m_~~")":>{ToExpression[l],ToExpression[m]}]]&, WaSABI`Waveform`Private`GetAmplitudes[model]];
  tmax = Max[inspiral[[1]]["Domain"]];
  BinaryInspiralModel[<|"Model" -> model, "InitialConditions" -> ics, "Inspiral" -> inspiral, "Amplitudes" -> amplitudes, "Duration" -> tmax|>]
];


(* ::Section::Closed:: *)
(*BinaryInspiralModel*)


(* ::Subsection::Closed:: *)
(*Output format*)


BinaryInspiralModel /:
 MakeBoxes[bim:BinaryInspiralModel[assoc_], form:(StandardForm|TraditionalForm)] :=
 Module[{summary, extended},
  summary = {BoxForm`SummaryItem[{"Model: ", assoc["Model"]}],
             Row[{BoxForm`SummaryItem[{"M: ", Which[KeyExistsQ[assoc["InitialConditions"],"M"],assoc["InitialConditions"]["M"],KeyExistsQ[assoc["InitialConditions"],"m"],assoc["InitialConditions"]["m"]]}], "  ",
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


(* ::Subsection::Closed:: *)
(*Accessing attributes*)


BinaryInspiralModel[assoc_]["Inspiral"] := Missing["KeyAbsent", "Inspiral"];


BinaryInspiralModel[assoc_]["Amplitudes"] := Missing["KeyAbsent", "Amplitudes"];


BinaryInspiralModel[assoc_][key_String] /; !MemberQ[{"Waveform", "Trajectory"}, key] && KeyExistsQ[assoc, key] := assoc[key];


Keys[m_BinaryInspiralModel] ^:= DeleteCases[Join[Keys[m[[-1]]], {"Waveform", "Trajectory"}], "Inspiral" | "Amplitudes"];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Trajectory*)


BinaryInspiralModel[assoc_]["Trajectory"][param_][t:(_?NumericQ|{_?NumericQ..})] :=
  assoc["Inspiral"][param][t];


(* ::Section::Closed:: *)
(*End Package*)


(* ::Subsection::Closed:: *)
(*Protect symbols*)


SetAttributes[{BinaryInspiral, BinaryInspiralModel}, {Protected, ReadProtected}];


(* ::Subsection::Closed:: *)
(*End*)


End[];
EndPackage[];
