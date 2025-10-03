(* ::Package:: *)

(* ::Title:: *)
(*Utilities*)


(* ::Section::Closed:: *)
(*Create Package*)


(* ::Subsection::Closed:: *)
(*BeginPackage*)


BeginPackage["WaSABI`BinaryInspiral`",
  {"WaSABI`Inspiral`"}
];


(* ::Subsection::Closed:: *)
(*Unprotect symbols*)


ClearAttributes[{ForcingTerms,ForcingTermsModel,InitialConditions}, {Protected, ReadProtected}];


(* ::Subsection::Closed:: *)
(*Usage messages*)


ListModels::usage = "Lists available BinaryInspiral models."
ForcingTerms::usage = "ForcingTerms[X] generates the forcing functions of the model X. Evaluate it at a given point on parameter space by parsing ics.";
ForcingTermsModel::usage = "ForcingTermsModel[X] represents the forcing terms/ODE of model X.";
InitialConditions::usage = "InitialConditions[X] returns the list of parameters in model X that require values to evaluate BinaryInspiral or ForcingTerms.";


(* ::Subsection::Closed:: *)
(*Error Messages*)


ForcingTerms::nomodel = "Unknown model `1`.";
InitialConditions::nomodel = "Unknown model `1`.";


ForcingTerms::ics = "Invalid parameters `1` for model `2`.";


(* ::Subsection::Closed:: *)
(*Begin Private section*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*ListModels*)


ListModels[]:=WaSABI`Inspiral`Private`ListInspiralModels[];


(* ::Section::Closed:: *)
(*ForcingTerms*)


Options[ForcingTerms] = {"Precision" -> 10, "Accuracy" -> 10};


ForcingTerms[model_?StringQ,opts:OptionsPattern[]] := Module[{prec,acc,forcingterms,ODElhs,ODErhs,AElhs,AErhs,ODEpos,ODEs,insp,inspeqslhs,inspeqsrhs,params,paramsstr,ics,independentparams},
  prec = OptionValue["Precision"];
  acc = OptionValue["Accuracy"];
  
  If[!WaSABI`Inspiral`Private`InspiralModelExistsQ[model] || !WaSABI`Waveform`Private`WaveformModelExistsQ[model],
    Message[ForcingTerms::nomodel, model];
    Return[$Failed];
  ];

  insp = WaSABI`Inspiral`Private`GetInspiralEquations[model];
  {inspeqslhs,inspeqsrhs} = {insp[["InspiralEquations"]][[;;,1]],insp[["InspiralEquations"]][[;;,2]]};
  ODEpos = Flatten@Position[inspeqslhs,Derivative[1][_Symbol][ToExpression["WaSABI`Inspiral`Model"<>model<>"`t"]]];
  ODElhs = (inspeqslhs[[ODEpos]])/.Derivative[1][s_Symbol][ToExpression["WaSABI`Inspiral`Model"<>model<>"`t"]]:>"d"~~StringReplace[SymbolName[s], Context[s] -> ""]~~"/dt";
  ODErhs = (inspeqsrhs[[ODEpos]])/.s_Symbol[ToExpression["WaSABI`Inspiral`Model"<>model<>"`t"]]:>s;
  AElhs = (Complement[inspeqslhs,inspeqslhs[[ODEpos]]])/.s_Symbol[ToExpression["WaSABI`Inspiral`Model"<>model<>"`t"]]:>s;
  AErhs = (Complement[inspeqsrhs,inspeqsrhs[[ODEpos]]])/.s_Symbol[ToExpression["WaSABI`Inspiral`Model"<>model<>"`t"]]:>s;
  params = insp["Parameters"];
  paramsstr = SymbolName /@ params;
  ics = insp["InitialConditionsFormat"];
  independentparams = Pick[params, paramsstr, Alternatives@@ics];
  
  ForcingTermsModel[<|"Model" -> model, "ODElhs" -> ODElhs,"ODErhs" -> ODErhs, "AElhs" -> AElhs, "AErhs" -> AErhs, "Parameters"-> independentparams|>]
];


InitialConditions[model_?StringQ]:=Module[{icsformat,values},

  If[!WaSABI`Inspiral`Private`InspiralModelExistsQ[model] || !WaSABI`Waveform`Private`WaveformModelExistsQ[model],
    Message[InitialConditions::nomodel, model];
    Return[$Failed];
  ];
  
  icsformat=WaSABI`Inspiral`Private`GetInspiralEquations[model][["InitialConditionsFormat"]];
  values=Table[_,{i,1 Length[icsformat]}];
  
  AssociationThread[icsformat -> values]
];


(*  If[Sort[Keys[ics]] != WaSABI`Inspiral`Private`GetInspiralEquations[model][["InitialConditionsFormat"]],
      Message[BinaryInspiral::ics, ics, model];
      Return[$Failed];
  ];*)


(* ::Section::Closed:: *)
(*ForcingTermsModel*)


(* ::Subsection::Closed:: *)
(*Output format*)


ForcingTermsModel /:
 MakeBoxes[ftm:ForcingTermsModel[assoc_], form:(StandardForm|TraditionalForm)] :=
 Module[{summary, extended},
  summary = {BoxForm`SummaryItem[{"Model: ", assoc["Model"]}],
             Row[{BoxForm`SummaryItem[{"Parameters: ", SymbolName[#]& /@assoc["Parameters"]}], "  "}]};
  extended = {};
  BoxForm`ArrangeSummaryBox[
    ForcingTermsModel,
    ftm,
    None,
    summary,
    extended,
    form
  ]
];


(* ::Subsection:: *)
(*Accessing attributes*)


(* ::Subsection::Closed:: *)
(*Evaluate forcing terms*)


ForcingTermsModel[assoc_][paramvals_] :=Module[{params,paramsstr,paramscond,dependentparams,dependentparamsstr,dependentparamscond,dependentparamsvals},
  params = assoc["Parameters"];
  paramsstr = Transpose@{params,SymbolName[#]& /@ params};
  paramscond = Table[paramstr[[1]] -> paramvals[[paramstr[[2]]]],{paramstr,paramsstr}];
  dependentparams = assoc["AElhs"];
  dependentparamsstr = Transpose@{dependentparams,SymbolName[#]& /@ dependentparams};
  dependentparamsvals = Association@MapThread[(#1->#2)&,{dependentparamsstr[[;;,2]],(assoc["AErhs"]/.paramscond)}];
  dependentparamscond = Table[dependentparamstr[[1]] -> dependentparamsvals[[dependentparamstr[[2]]]],{dependentparamstr,dependentparamsstr}];
  Association@MapThread[(#1->#2)&,{assoc["ODElhs"],(assoc["ODErhs"]/.paramscond/.dependentparamscond//N)}]
]


(* ::Section::Closed:: *)
(*End Package*)


(* ::Subsection:: *)
(*Protect symbols*)


SetAttributes[{ForcingTerms,ForcingTermsModel,InitialConditions}, {Protected, ReadProtected}];


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
