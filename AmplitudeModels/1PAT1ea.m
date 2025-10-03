(* ::Package:: *)

(* ::Input::Initialization:: *)
directory1SF = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Schwarz_Circ"}];
directory2SF = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/2SF/Schwarz_Circ"}];
directoryS2 = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/Leading_S2/Schwarz_Circ"}];
directoryS1 = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Linear_Kerr_Circ"}];

Do[Z\[ScriptCapitalI][ll,mm]=Interpolation[Get[FileNameJoin[{directory1SF, "1SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]],InterpolationOrder->8], {ll,2,5}, {mm,1,ll}]

Z\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^l Conjugate[Z\[ScriptCapitalI][l,-m][r0]];

Do[Z2\[ScriptCapitalI][ll,mm]=Function[{r0},Exp[Interpolation[Get[FileNameJoin[{directory2SF, "2SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]],InterpolationOrder->3][Log[r0]]]], {ll,2,5}, {mm,1,ll}]

Z2\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^l Conjugate[Z2\[ScriptCapitalI][l,-m][r0]];

Do[Za\[ScriptCapitalI][ll,mm]=Interpolation[Get[FileNameJoin[{directoryS1, "Slow_chi1_TeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]],InterpolationOrder->8], {ll,2,5}, {mm,1,ll}]

Za\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^l Conjugate[Za\[ScriptCapitalI][l,-m][r0]];

Do[C2\[Sigma]\[ScriptCapitalI][ll,mm]=Interpolation[Get[FileNameJoin[{directoryS2, "chi2_2SF_RWZampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]],InterpolationOrder->8], {ll,2,5}, {mm,1,ll}]

C2\[Sigma]\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^m Conjugate[C2\[Sigma]\[ScriptCapitalI][l,-m][r0]];


With[{M=1},
h0PAamp[l_,m_][r0_]:=(-2(-Z\[ScriptCapitalI][l,m][r0]))/(I m Sqrt[M/r0^3])^2;
h1PAamp[l_,m_][r0_]:= (-2Z2\[ScriptCapitalI][l,m][r0])/(I m Sqrt[M/r0^3])^2;
h1PASpinamp[l_,m_][r0_]:=If[EvenQ[l+m],(Sqrt[(l-1)l(l+1)(l+2)]/2)C2\[Sigma]\[ScriptCapitalI][l,m][r0], -I*(Sqrt[(l-1)l(l+1)(l+2)]/2)C2\[Sigma]\[ScriptCapitalI][l,m][r0]];
h1PAampa[l_,m_][r0_]:=(-2(-Za\[ScriptCapitalI][l,m][r0]))/(I m Sqrt[M/r0^3])^2;]

With[{M=1},
h1PAamp\[Nu][l_,m_][r0_]:=h0PAamp[l,m][r0]+h1PAamp[l,m][r0]+2/3 r0/M (1+(-(M/r0)/Sqrt[1-3 M/r0])) h0PAamp[l,m]'[r0];
h1PAamp\[Nu]S1[l_,m_][r0_]:=h1PAampa[l,m][r0];
h1PAamp\[Nu]S2[l_,m_][r0_]:=h1PASpinamp[l,m][r0];
h1PAamp\[Nu]\[Delta]m[l_,m_][r0_]:=Sqrt[M/r0^3](-((2 r0^(5/2))/3)) h0PAamp[l,m]'[r0];]


h1PAAmp\[CapitalOmega][l_,m_]/;EvenQ[m]:=\[Nu] h0PAamp[l,m][1/(M \[CapitalOmega])^(2/3)]+\[Nu] \[Chi]t1 h1PAamp\[Nu]S1[l,m][1/(M \[CapitalOmega])^(2/3)]+\[Nu] \[Chi]t2 h1PAamp\[Nu]S2[l,m][1/(M \[CapitalOmega])^(2/3)]+\[Nu]^2 (h1PAamp\[Nu][l,m][1/(M \[CapitalOmega])^(2/3)]+\[Delta]m h1PAamp\[Nu]\[Delta]m[l,m][1/(M \[CapitalOmega])^(2/3)]);
h1PAAmp\[CapitalOmega][l_,m_]/;OddQ[m]:=Sqrt[1-4 \[Nu]] (\[Nu] h0PAamp[l,m][1/(M \[CapitalOmega])^(2/3)]+\[Nu] \[Chi]t1 h1PAamp\[Nu]S1[l,m][1/(M \[CapitalOmega])^(2/3)]+\[Nu] \[Chi]t2 h1PAamp\[Nu]S2[l,m][1/(M \[CapitalOmega])^(2/3)]+\[Nu]^2 (2 h0PAamp[l,m][1/(M \[CapitalOmega])^(2/3)]+h1PAamp\[Nu][l,m][1/(M \[CapitalOmega])^(2/3)]+\[Delta]m h1PAamp\[Nu]\[Delta]m[l,m][1/(M \[CapitalOmega])^(2/3)]));

AssociationThread[Flatten[Table["("<>ToString[ll]<>","<>ToString[mm]<>")",{ll,2,5},{mm,1,ll}]],Flatten[Table[h1PAAmp\[CapitalOmega][ll,mm],{ll,2,5},{mm,1,ll}]]]
