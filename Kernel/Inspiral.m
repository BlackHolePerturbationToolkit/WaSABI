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


GetInspiralEquations::usage = "Fetches the coupled equations describing the inspiral evolution.";
IntInspiral::usage = "Determines the inspiral by integrating the inspiral equations.";
x::usage="Inverse separation variable ((m1+m2)\[CapitalOmega]\!\(\*SuperscriptBox[\()\), \(2/3\)]\)";
\[Nu]::usage="Symmetric mass ratio";
xC::usage="x/\!\(\*SubscriptBox[\(x\), \(Isco\)]\), where \!\(\*SubscriptBox[\(x\), \(Isco\)]\) is the geodesic value of x at the ISCO";
m10::usage="Initial primary's mass";
m2::usage="Secondary's mass";
m1::usage="Primary's mass";
m0::usage="Initial total mass";
m::usage="total mass";
\[Chi]1::usage="Dimensionless spin 1, (normalised by \!\(\*SubscriptBox[\(m\), \(1\)]\))";
\[Chi]0::usage="Initial primary's dimensionless spin (normalised by \!\(\*SubscriptBox[\(m\), \(1\)]\))";
\[Chi]2::usage="Dimensionless spin 2, (normalised by \!\(\*SubscriptBox[\(m\), \(2\)]\))";
\[Chi]t1::usage="\!\(\*FractionBox[\(m1\), \(m\)]\)*\[Chi]1";
\[Chi]t2::usage="\!\(\*FractionBox[\(m2\), \(m\)]\)*\[Chi]2";
\[Epsilon]0::usage="Initial small mass ratio m2/m10";
\[Epsilon]::usage="Small mass ratio m2/m1";
\[Sigma]::usage="...";
s::usage="...";


(* ::Subsection:: *)
(*Error messages*)


GetInspiralEquations::nomodel = "Unknown model";


(* ::Subsection:: *)
(*Being Private section*)


Begin["`Private`"];


(* ::Section:: *)
(*Get Forcing terms*)


(* ::Text:: *)
(*Pass in a string label of model, fetch the inspiral equations which should return: The list of all independent variables joined with the list of dependent variables, the relation of the dependent variables to the independent variables joined with the equations describing the evolution of the independent variables.*)


GetInspiralEquations[model_,evolveprimary_:True]:=Block[{filelocation},

filelocation=If[evolveprimary==True,
Which[model=="DevTest","InspiralModels/devtest.m", model=="1PAT1","InspiralModels/1PAT1e.m",model=="1PAT1R","InspiralModels/1PAT1Re.m",model=="Hybrid","InspiralModels/Hybride.m",True, Message[GetInspiralEquations::nomodel]],
Which[model=="1PAT1","InspiralModels/1PAT1.m",model=="1PAT1R","InspiralModels/1PAT1R.m",model=="Hybrid","InspiralModels/Hybrid.m",True, Message[GetInspiralEquations::nomodel]]
];

Get[StringJoin[$UserBaseDirectory,"/Applications/WASABI/",filelocation]]

]


(* ::Section:: *)
(*Integrate Inspiral*)


(* ::Text:: *)
(*Takes inspiral equations and integrates. Add option to specify different stop conditions, with default to be xC=1+\[Delta].*)


IntInspiral[model_, initialconds_, stopcond_]:=Block[{tparam, params, equations, integrations},

{tparam, params, equations}=GetInspiralEquations[model];

integrations=NDSolveValue[Join[equations, initialconds, {WhenEvent[stopcond,"StopIntegration"]}],params,{tparam,0,\[Infinity]}, PrecisionGoal->10,AccuracyGoal->10]

(*Then return association list of solutions, first test*)


];



(* ::Section:: *)
(*Close the package*)


End[];

EndPackage[];
