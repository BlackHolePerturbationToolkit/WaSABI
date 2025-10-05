(* ::Package:: *)

(* ::Input::Initialization:: *)
directory1SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Schwarz_Circ"}];

\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH] = Interpolation[Get[FileNameJoin[{directory1SF, "1SFCircShwarzDotEHorizon.m"}]],InterpolationOrder->8];
\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI] = Interpolation[Get[FileNameJoin[{directory1SF, "1SFCircShwarzDotEInf.m"}]],InterpolationOrder->8];

\[ScriptCapitalF]1[r0_]:=\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalI][r0]+\[ScriptCapitalF]\[ScriptCapitalE]\[ScriptCapitalH][r0];


F0[r0_]:=With[{M=1},(3 ((1-3 M/r0)^(3/2)) Sqrt[M/r0] )/(M^2 (1-6 M/r0)) (\[ScriptCapitalF]1[r0])];
variables={\[CapitalOmega],r0,\[Phi],\[Nu],M};
evolutionequations={\[CapitalOmega]'[t]==\[Nu][t]/M[t] F0[r0[t]],\[Phi]'[t]==\[CapitalOmega][t]/M[t],\[CapitalOmega][t]==Sqrt[1/r0[t]^3],\[Nu]'[t]==0 , M'[t]==0};
InitialConditionFormat={"M", "r0", "\[Nu]", "\[Phi]"};
stopcondition = r0[t] <= 6.01;


<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat, "StopCondition" -> stopcondition|>
