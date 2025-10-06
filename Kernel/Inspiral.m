(* ::Package:: *)

(* ::Title:: *)
(*Inspiral subpackage of WaSABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section::Closed:: *)
(*Create Package*)


BeginPackage["WaSABI`Inspiral`"];


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*Usage messages*)


ListInspiralModels::usage = "Lists available inspiral models";
GetInspiralEquations::usage = "Fetches the coupled equations describing the inspiral evolution.";
IntInspiral::usage = "Determines the inspiral by integrating the inspiral equations.";
InspiralEvaluate::usage = "Evaluates symbolic functions along a given timeseries of a given inspiral";


(* ::Subsubsection:: *)
(*Error messages*)


(* ::Section::Closed:: *)
(*Get Forcing terms*)


$WaSABIInspiralDirectory = FileNameJoin[{FileNameDrop[FindFile["WaSABI`"], -2], "InspiralModels"}];


modelContext[model_String] := "WaSABI`Inspiral`Model"<>StringDelete[model, {"-", ".", "_"}]<>"`";


ListInspiralModels[] := ListInspiralModels[] =
 Module[{inspiralmodels},
  inspiralmodels = FileBaseName /@ FileNames["*.m", $WaSABIInspiralDirectory];
  inspiralmodels
]


InspiralModelExistsQ[model_String] :=
  MemberQ[ListInspiralModels[], model];


GetInspiralEquations[model_String] := GetInspiralEquations[model] =
 Module[{filelocation, equations},
  filelocation = First[FileNames[model<>".m", $WaSABIInspiralDirectory]];

  Begin[modelContext[model]];
  equations = Get[filelocation];
  End[];
  
  equations
]


(* ::Section::Closed:: *)
(*Integrate Inspiral*)


IntInspiral[model_, ics_, prec_, acc_, stopcon_] :=
 Module[{insp, tparam, params, equations, integrations, paramsstr, initparams, initialconds,stopparams, stopcond},

  insp = GetInspiralEquations[model];
  tparam = insp["IntegrationVariable"];
  params = insp["Parameters"];
  equations = insp["InspiralEquations"];
  paramsstr = SymbolName /@ params;
  initparams = Pick[params, paramsstr, Alternatives@@Keys[ics]];
  initialconds = Map[#[0] == SymbolName[#]&, initparams] /. ics;
  stopparams= If[MissingQ[stopcon],Missing[],Pick[params, paramsstr, Alternatives@@Keys[stopcon]]];
  stopcond = If[MissingQ[stopcon],{WhenEvent[Evaluate[insp["StopCondition"]/. ics], "StopIntegration"]},
  {WhenEvent[Evaluate[Map[Between[#[tparam], SymbolName[#]/. stopcon]==False&, stopparams]] , "StopIntegration","LocationMethod" -> "LinearInterpolation"]}];
  integrations = NDSolveValue[Join[equations, initialconds, stopcond], params, {tparam, 0, \[Infinity]}, PrecisionGoal->prec, AccuracyGoal->acc];

  Append[AssociationThread[paramsstr -> integrations], "Parameters" -> params]
];



(* ::Section::Closed:: *)
(*Close the package*)


End[];

EndPackage[];
