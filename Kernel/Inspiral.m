(* ::Package:: *)

(* ::Title:: *)
(*Inspiral subpackage of WaSABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section::Closed:: *)
(*Create Package*)


BeginPackage["WaSABI`Inspiral`"];


(* ::Subsection::Closed:: *)
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

  Begin["WaSABI`Inspiral`Model"<>model<>"`"];
  equations = Get[filelocation];
  End[];
  
  equations
]


(* ::Section::Closed:: *)
(*Integrate Inspiral*)


(* ::Text:: *)
(*To do: *)
(*Add bounds to initial conditions for when data not available.*)


IntInspiral[model_, ics_, prec_, acc_] :=
 Module[{insp, tparam, params, equations, integrations, paramsstr, initparams, initialconds, stopcond},


  insp = GetInspiralEquations[model];
  tparam = insp["IntegrationVariable"];
  params = insp["Parameters"];
  equations = insp["InspiralEquations"];
  paramsstr = SymbolName /@ params;
  initparams = Pick[params, paramsstr, Alternatives@@Keys[ics]];
  initialconds = Map[#[0] == SymbolName[#]&, initparams] /. ics;
  stopcond = {WhenEvent[Evaluate[insp["StopCondition"] /. ics], "StopIntegration"]};
  integrations = NDSolveValue[Join[equations, initialconds, stopcond], params, {tparam, 0, \[Infinity]}, PrecisionGoal->prec, AccuracyGoal->acc];

  Append[AssociationThread[paramsstr -> integrations], "Parameters" -> params]
];



(* ::Section::Closed:: *)
(*Close the package*)


End[];

EndPackage[];
