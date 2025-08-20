(* ::Package:: *)

(* ::Title:: *)
(*Inspiral subpackage of WaSABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WaSABI`Inspiral`"];


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Usage messages*)


ListInspiralModels::usage = "Lists available inspiral models"
GetInspiralEquations::usage = "Fetches the coupled equations describing the inspiral evolution.";
IntInspiral::usage = "Determines the inspiral by integrating the inspiral equations.";
InspiralEvaluate::usage = "Evaluates symbolic functions along a given timeseries of a given inspiral";


(* ::Subsubsection:: *)
(*Error messages*)


(* ::Section:: *)
(*Get Forcing terms*)


(* ::Text:: *)
(*Pass in a string label of model, fetch the inspiral equations which should return: The list of all independent variables joined with the list of dependent variables, the relation of the dependent variables to the independent variables joined with the equations describing the evolution of the independent variables.*)
(**)
(*Can either add evolve primary as an argument here, or name the models differently?*)


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


(* ::Section:: *)
(*Integrate Inspiral*)


(* ::Text:: *)
(*Takes inspiral equations and integrates. Nothing fancy.*)
(**)
(*To do: *)
(*Add bounds to initial conditions - r0 too big -> Data not available.*)
(*Add options for precision and accuracy goal.*)
(*Add default stopping condition? Likely model dependent.*)


IntInspiral[model_, ics_, prec_, acc_] :=
 Module[{insp, tparam, params, equations, integrations, paramsstr, initparams, initialconds, stopcond},
  (*Add options for precision and accuracy goal*)
  (*Add default stopping condition*)

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



(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
