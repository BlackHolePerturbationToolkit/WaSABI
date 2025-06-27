(* ::Package:: *)

(* ::Title:: *)
(*Inspiral subpackage of WASABI*)


(* ::Chapter:: *)
(*Define usage for public functions*)


(* ::Section:: *)
(*Create Package*)


BeginPackage["WASABI`Inspiral`"];


(* ::Subsection:: *)
(*Usage messages*)


GetInspiralEquations::usage = "Fetches the coupled equations describing the inspiral evolution."
Inspiral::usage = "Determines the inspiral by integrating the inspiral equations."


(* ::Subsection:: *)
(*Error messages*)


GetInspiralEquations::nomodel = "Unknown model"


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*Get Forcing terms*)


(* ::Text:: *)
(*Pass in a string label of model, fetch the inspiral equations which should return: The list of all independent variables joined with the list of dependent variables, the relation of the dependent variables to the independent variables joined with the equations describing the evolution of the independent variables.*)


GetInspiralEquations[model_,evolveprimary_:True]:=Block[{filelocation, params, equations},

filelocation=If[evolveprimary==True,
Which[model=="DevTest","/InspiralModels/devtest.m", model=="1PAT1","/InspiralModels/1PAT1e.m",model=="1PAT1R","/InspiralModels/1PAT1Re.m",model=="Hybrid","/InspiralModels/Hybride.m",True, Message[GetInspiralEquations::nomodel]],
Which[model=="1PAT1","/InspiralModels/1PAT1.m",model=="1PAT1R","/InspiralModels/1PAT1R.m",model=="Hybrid","/InspiralModels/Hybrid.m",True, Message[GetInspiralEquations::nomodel]]
];

{params, equations}=Get[filelocation]

]


(* ::Section:: *)
(*Integrate Inspiral*)


(* ::Text:: *)
(*Takes inspiral equations and integrates. Add option to specify different stop conditions, with default to be xC=1+\[Delta].*)


Inspiral[model_, initialconds_, stopcond_]:=Block[{params, equations, integrations},

{params, equations}=GetInspiralEquations[model];

integrations=NDSolveValue[Join[equations, initialconds, {WhenEvent[stopcond,"StopIntegration"]}],{params},{t,0,\[Infinity]}, PrecisionGoal->10,AccuracyGoal->10]

(*Then return association list of solutions, first test*)

]



(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
