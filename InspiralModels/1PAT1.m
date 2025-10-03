(* ::Package:: *)

(* ::Input::Initialization:: *)
directory1SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Schwarz_Circ"}];
directory2SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/2SF_Flux/Schwarz_Circ"}];
directory1SFLocalInvar = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Local_Invariants/Schwarz_Circ"}];

\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH] = Interpolation[Get[FileNameJoin[{directory1SF, "1SFCircShwarzDotEHorizon.m"}]],InterpolationOrder->8];
\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI] = Interpolation[Get[FileNameJoin[{directory1SF, "1SFCircShwarzDotEInf.m"}]],InterpolationOrder->8];
\[ScriptCapitalF]2\[ScriptCapitalE]\[ScriptCapitalI] = Function[{r0},Re[Exp[Interpolation[Get[FileNameJoin[{directory2SF, "2SFCircShwarzDotEInf.m"}]],InterpolationOrder->8][Log[r0]]]]];
z = Interpolation[Get[FileNameJoin[{directory1SFLocalInvar, "1SFCircShwarzRedshfit.m"}]],InterpolationOrder->8];

\[ScriptCapitalF]1[r0_]:=\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI][r0]+\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH][r0];
EFLx[x_]:=(1/2 z[x]-x/3z'[x]-1+Sqrt[1-3x]+x/6  (7-24x)/(1-3x)^(3/2));


F0[r0_]:=With[{M=1},(3 ((1-3 M/r0)^(3/2)) Sqrt[M/r0] )/(M^2 (1-6 M/r0)) (\[ScriptCapitalF]1[r0])];
F1[r0_]:=With[{M=1},(3(1-3 M/r0)^(3/2) Sqrt[M/r0])/(M^2 (1-6 M /r0)) (\[ScriptCapitalF]2\[ScriptCapitalE]\[ScriptCapitalI][r0]+(2(1-3 M/r0)^(3/2))/(1-6M/r0) \[ScriptCapitalF]1[r0](EFLx'[M/r0] ) )];

variables={\[CapitalOmega],r0,\[Phi],\[Nu],M};
evolutionequations={\[CapitalOmega]'[t]==\[Nu][t] F0[r0[t]]+\[Nu][t]^2 F1[r0[t]],\[Phi]'[t]==\[CapitalOmega][t],\[CapitalOmega][t]==Sqrt[M[t]/r0[t]^3],\[Nu]'[t]==0 , M'[t]==0};
InitialConditionFormat={"M", "r0", "\[Nu]", "\[Phi]"};
stopcondition = r0[t] <= Max[6.25, 6.05 + 4.5 "\[Nu]" - 1.5 "\[Nu]"^2];
parameterspacecoverage = {6.05<"r0"<30};
<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat, "StopCondition" -> stopcondition, "ParameterSpaceCoverage" -> parameterspacecoverage|>
