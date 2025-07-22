(* ::Package:: *)

(* ::Input::Initialization:: *)
variables={\[Omega],\[Phi], x,xC,\[Nu],M,\[Chi]1,\[Chi]2};
evolutionequations={ \[Omega]'[t]==\[Nu][t](96 x[t]^(11/2))/5,\[Phi]'[t]==\[Omega][t],\[Omega][t]==x[t]^(3/2), xC[t]==x[t]*6,\[Nu]'[t]==0,M'[t]==0,\[Chi]1'[t]==0,\[Chi]2'[t]==0};
InitialConditionFormat="{x[0]==_, \[Phi][0]==_, \[Nu][0]==_, M[0]==_, \[Chi]1[0]==_, \[Chi]2[0]==_}";

<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat|>
