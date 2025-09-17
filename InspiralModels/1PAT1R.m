(* ::Package:: *)

(* ::Section::Closed:: *)
(*Construct Interpolations (FIX ME - stop condition)*)


(* ::Subsection::Closed:: *)
(*Fetch data (FIX ME - stop condition)*)


directory1SFSch = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Schwarz_Circ"}];
directory2SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/2SF_Flux/Schwarz_Circ"}];
directory1SFLocalInvar = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Local_Invariants/Schwarz_Circ"}];
directorySecSpin = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/Leading_S2_Flux/Schwarz_Circ"}];
directorystop=FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/stop_conditions"}];
directoryPrimarySpin = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Linear_Kerr_Circ"}];

fluxdata2SF=Get[FileNameJoin[{directory2SF, "2SFCircShwarzDotEInf.m"}]];

fluxdata1SFSchwInf=Get[FileNameJoin[{directory1SFSch,"1SFCircShwarzDotEInf.m"}]];
fluxdata1SFSchwHor=Get[FileNameJoin[{directory1SFSch,"1SFCircShwarzDotEHorizon.m"}]];

spinfluxdataHor=Get[FileNameJoin[{directorySecSpin,"chi2_2SFCircShwarzDotEHorizon.m"}]];
spinfluxdataInf=Get[FileNameJoin[{directorySecSpin,"chi2_2SFCircShwarzDotEInf.m"}]];

slowspin1fluxdataHor=Get[FileNameJoin[{directoryPrimarySpin,"Slow_chi1_2SFCircShwarzDotEHorizon.m"}]];
slowspin1fluxdataInf=Get[FileNameJoin[{directoryPrimarySpin,"Slow_chi1_2SFCircShwarzDotEInf.m"}]];

invardata=Get[FileNameJoin[{directory1SFLocalInvar,"1SFCircShwarzRedshfit.m"}]];
(*\[CapitalOmega]critdata=Get[FileNameJoin[{directorystop,"Stop1PAT1R.m"}]];*)


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalI]*)


\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI] = Interpolation[fluxdata1SFSchwInf,InterpolationOrder->8];
\[ScriptCapitalF]2\[Sigma]\[ScriptCapitalE]\[ScriptCapitalI] = Interpolation[spinfluxdataInf,InterpolationOrder->8];
\[ScriptCapitalF]a\[ScriptCapitalE]\[ScriptCapitalI] = Interpolation[slowspin1fluxdataInf,InterpolationOrder->8];
\[ScriptCapitalF]2\[ScriptCapitalE]\[ScriptCapitalI] := Function[{r0},Re[Exp[Interpolation[fluxdata2SF,InterpolationOrder->8][Log[r0]]]]];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalH]*)


\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH] = Interpolation[fluxdata1SFSchwHor,InterpolationOrder->8];
\[ScriptCapitalF]2\[Sigma]\[ScriptCapitalE]\[ScriptCapitalH] = Interpolation[spinfluxdataHor,InterpolationOrder->8];
\[ScriptCapitalF]a\[ScriptCapitalE]\[ScriptCapitalH] = Interpolation[slowspin1fluxdataHor,InterpolationOrder->8];


(* ::Subsection::Closed:: *)
(*Eb*)


z = Interpolation[invardata, InterpolationOrder->8];


(* ::Subsection::Closed:: *)
(*Stop Condition (FIX ME - stop condition)*)


(*\[CapitalOmega]crit=Interpolation[\[CapitalOmega]critdata,InterpolationOrder->All];*)


(* ::Section:: *)
(*Evolution Equations (FIX ME - stop condition)*)


(* ::Subsubsection::Closed:: *)
(*Necessary functions*)


(* ::Input::Initialization:: *)
\[ScriptCapitalF]1[r0_]:=\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI][r0]+\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH][r0];
\[ScriptCapitalF]\[ScriptCapitalL]\[ScriptCapitalH][r0_]:=r0^(3/2) \[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH][r0];
\[ScriptCapitalF]\[ScriptCapitalL]\[ScriptCapitalI][r0_]:=r0^(3/2) \[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI][r0];
\[ScriptCapitalL]1[r0_]:=\[ScriptCapitalF]\[ScriptCapitalL]\[ScriptCapitalH][r0]+\[ScriptCapitalF]\[ScriptCapitalL]\[ScriptCapitalI][r0];

\[ScriptCapitalF]\[Sigma][r0_]:=\[ScriptCapitalF]2\[Sigma]\[ScriptCapitalE]\[ScriptCapitalI][r0]+\[ScriptCapitalF]2\[Sigma]\[ScriptCapitalE]\[ScriptCapitalH][r0];
\[ScriptCapitalF]a[r0_]:=\[ScriptCapitalF]a\[ScriptCapitalE]\[ScriptCapitalH][r0]+\[ScriptCapitalF]a\[ScriptCapitalE]\[ScriptCapitalI][r0];

E\[Delta]m[y_]:=-(y/3) (1-6y)/(1-3y)^(3/2);
EFLx[x_]:=(1/2 z[x]-x/3z'[x]-1+Sqrt[1-3x]+x/6  (7-24x)/(1-3x)^(3/2));

dEd\[CapitalOmega][r0_,\[Nu]_,at1_,at2_,\[Delta]m1_]:=Module[{M=1,x,dxd\[CapitalOmega]},
 x=M/r0;
dxd\[CapitalOmega]=(2 M)/(3 Sqrt[x]);
((M^2) (-1+6 x) )/(3 (1-3 x)^(3/2) Sqrt[x])+ ((at2 M^2 x (-5+12 x))/(3 ((1-3 x)^(3/2)) )-(2 at1 M^2 x (10-33 x+36 x^2))/(9 ((1-3 x)^(5/2)) )+\[Nu]( EFLx'[x]+\[Delta]m1 E\[Delta]m'[x])dxd\[CapitalOmega])
];
d\[CapitalOmega]dttimesdEd\[CapitalOmega][r0_,\[Nu]_,at1_,at2_,\[Delta]m1_]:=Module[{M=1,x,dxd\[CapitalOmega]},
 x=M/r0;
-\[Nu] \[ScriptCapitalF]1[1/x]+\[Nu]^2 (-\[ScriptCapitalF]2\[ScriptCapitalE]\[ScriptCapitalI][1/x]-\[Delta]m1 (Sqrt[M/r0^3](-((2 r0^(5/2))/3))\[ScriptCapitalF]1'[r0] )-(at1 \[ScriptCapitalF]a[1/x])/\[Nu]-(at2 \[ScriptCapitalF]\[Sigma][1/x])/\[Nu]-(2 x^(5/2) (-2+3 x) \[ScriptCapitalF]\[ScriptCapitalL]\[ScriptCapitalH][1/x])/(3 M (1-3 x)^(3/2))-E\[Delta]m[x] \[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH][1/x])];



(* ::Subsubsection:: *)
(*Inspiral Force terms (FIX ME - stop condition) (FIX ME - double check M dimensions are restored correctly)*)


variables={\[CapitalOmega],\[Phi],\[Nu],M,\[Chi]t1,\[Chi]t2,\[Delta]m};
evolutionequations={\[CapitalOmega]'[t]==d\[CapitalOmega]dttimesdEd\[CapitalOmega][M[t]^(1/3) \[CapitalOmega][t]^(-2/3),\[Nu][t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t]]/dEd\[CapitalOmega][M[t]^(1/3) \[CapitalOmega][t]^(-2/3),\[Nu][t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t]],\[Phi]'[t]==\[CapitalOmega][t],\[Nu]'[t]==0,\[Delta]m'[t]==\[Nu][t] \[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH][M[t]^(1/3) \[CapitalOmega][t]^(-2/3)], \[Chi]t1'[t]==\[Nu][t]^2/M[t] \[ScriptCapitalF]\[ScriptCapitalL]\[ScriptCapitalH][M[t]^(1/3) \[CapitalOmega][t]^(-2/3)],\[Chi]t2'[t]==0,M'[t]==0};
InitialConditionFormat={"\[CapitalOmega]","\[Phi]","\[Nu]","M","\[Chi]t1","\[Chi]t2","\[Delta]m"};
stopcondition = \[CapitalOmega][t]>=Sqrt[1/6.7^3] (*\[CapitalOmega]crit["\[Nu]","\[Chi]t1","\[Chi]t2"]*);
parameterspacecoverage ={Sqrt[1/30^3]<"\[CapitalOmega]"<Min[Sqrt[1/6.25^3],Sqrt[1/6.7^2](*\[CapitalOmega]crit["\[Nu]","\[Chi]t1","\[Chi]t2"]*)]};


<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat,"StopCondition" -> stopcondition, "ParameterSpaceCoverage"->parameterspacecoverage|>
