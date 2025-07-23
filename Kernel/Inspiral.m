(* ::Package:: *)

(* ::Title:: *)
(*Inspiral subpackage of WASABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WASABI`Inspiral`"];


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


InspiralEvaluate::tovershoot = "Time series overshoots the inspiral time.";


(* ::Section:: *)
(*Get Forcing terms*)


(* ::Text:: *)
(*Pass in a string label of model, fetch the inspiral equations which should return: The list of all independent variables joined with the list of dependent variables, the relation of the dependent variables to the independent variables joined with the equations describing the evolution of the independent variables.*)
(**)
(*Can either add evolve primary as an argument here, or name the models differently?*)


$WASABIInspiralDirectory = FileNameJoin[{FileNameDrop[FindFile["WASABI`"], -2], "InspiralModels"}];


ListInspiralModels[] := ListInspiralModels[] =
 Module[{inspiralmodels},
  inspiralmodels = FileBaseName /@ FileNames["*.m", $WASABIInspiralDirectory];
  inspiralmodels
]


InspiralModelExistsQ[model_String] :=
  MemberQ[ListInspiralModels[], model];


GetInspiralEquations[model_String] := GetInspiralEquations[model] =
 Module[{filelocation, equations},
  filelocation = First[FileNames[model<>".m", $WASABIInspiralDirectory]];

  Begin["WASABI`Inspiral`Model"<>model<>"`"];
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
(*Move hardcoded stop condition into model.*)


IntInspiral[model_, ics_] :=
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
  stopcond = If[model=="1PAT1",
    {WhenEvent[WASABI`Inspiral`Model1PAT1`r0[WASABI`Inspiral`Model1PAT1`t] <= 7, "StopIntegration"]},
    {WhenEvent[WASABI`Inspiral`Model0PA`r0[WASABI`Inspiral`Model0PA`t] <= 7, "StopIntegration"]}];

  integrations = NDSolveValue[Join[equations, initialconds, stopcond], params, {tparam, 0, \[Infinity]}, PrecisionGoal->10, AccuracyGoal->10];

  Append[AssociationThread[paramsstr -> integrations], "Parameters" -> params]
];



(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
