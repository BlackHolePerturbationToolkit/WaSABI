(* ::Package:: *)

(* ::Section::Closed:: *)
(*Dependent variables*)


(* ::Subsection::Closed:: *)
(*ISCO radius*)


rISCO[\[Chi]1_]:=3+Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]-Sign[\[Chi]1] \[Sqrt]((2-(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (4+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))+2 Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]));


(* ::Subsection::Closed:: *)
(*WF frequency forcing term*)


F\[Omega][\[Omega]_,\[Nu]_,m_,\[Chi]1_]:=(3 \[Nu] ((-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 \[Omega]^(2/3) (1-\[Chi]1 \[Omega])^(1/3)))^(3/2) (\[Omega]^(5/3) (1-\[Chi]1 \[Omega])^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Chi]1]/(-\[Chi]1+1/\[Omega])^(2/3)]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Chi]1]/(-\[Chi]1+1/\[Omega])^(2/3)]]))/((-\[Chi]1+1/\[Omega])^(11/3) (-1+\[Chi]1 \[Omega]) (-1+6 \[Omega]^(2/3) (1-\[Chi]1 \[Omega])^(1/3)-6 \[Chi]1 (\[Omega]+\[Omega]^(5/3) (1-\[Chi]1 \[Omega])^(1/3))+\[Chi]1^2 (7 \[Omega]^2+3 \[Omega]^(4/3) (1-\[Chi]1 \[Omega])^(2/3))));


(* ::Subsection::Closed:: *)
(*WF phase evolution*)


F\[Phi][\[Omega]_]:=\[Omega];


(* ::Section::Closed:: *)
(*Construct Interpolations*)


(* ::Subsection::Closed:: *)
(*Fetch data*)


directory1SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Kerr_Circ"}];
fluxdata1SF=Get[FileNameJoin[{directory1SF,"SMRfluxdata2025_36x36.data"}]];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalI]*)


\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[fluxdata1SF[[i]]["a"]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^5 fluxdata1SF[[i]]["inf"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalH]*)


\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[fluxdata1SF[[i]]["a"]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^(15/2) fluxdata1SF[[i]]["hor"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Section::Closed:: *)
(*Evolution equations*)


variables={\[Omega],\[Phi],\[Nu],M,\[Chi]1};
evolutionequations={
\[Omega]'[t]==F\[Omega][\[Omega][t],\[Nu][t],M[t],\[Chi]1[t]]/M[t],
\[Phi]'[t]==F\[Phi][\[Omega][t]]/M[t],
\[Nu]'[t]==0,
M'[t]==0, 
\[Chi]1'[t]==0};
InitialConditionFormat={"\[Omega]","\[Phi]","\[Nu]","M","\[Chi]1"};
stopcondition = {\[Omega][t] >= Min[1/((1.05rISCO["\[Chi]1"])^(3/2)+"\[Chi]1"),1/(6.26^(3/2)+"\[Chi]1")]};
parameterspacecoverage = {\[Sqrt]((rISCO["\[Chi]1"] ("\[Omega]")^(2/3))/(1-"\[Chi]1" ("\[Omega]"))^(2/3))<.998,
1/(30^(3/2)+"\[Chi]1")<"\[Omega]"<Min[1/(6.06^(3/2)+"\[Chi]1")],
Abs["\[Chi]1"]>.000001};


ReleaseHold[<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat,"StopCondition" -> stopcondition, "ParameterSpaceCoverage"->parameterspacecoverage|>]
