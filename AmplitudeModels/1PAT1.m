(* ::Package:: *)

(* ::Input::Initialization:: *)
directory1SF = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Schwarz_Circ"}];
directory2SF = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/2SF/Schwarz_Circ"}];

Do[Z\[ScriptCapitalI][ll,mm]=Interpolation[Get[FileNameJoin[{directory1SF, "1SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]]], {ll,2,5}, {mm,1,ll}]

Z\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^l Conjugate[Z\[ScriptCapitalI][l,-m][r0]];

Do[Z2\[ScriptCapitalI][ll,mm]=Function[{r0},Exp[Interpolation[Get[FileNameJoin[{directory2SF, "2SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]]][Log[r0]]]], {ll,2,5}, {mm,1,ll}]

Z2\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^l Conjugate[Z2\[ScriptCapitalI][l,-m][r0]];

With[{M=1},
h0PAamp[l_,m_][r0_]:=(-2(-Z\[ScriptCapitalI][l,m][r0]))/(I m Sqrt[M/r0^3])^2;
h1PAamp[l_,m_][r0_]:= (-2Z2\[ScriptCapitalI][l,m][r0])/(I m Sqrt[M/r0^3])^2;]

With[{M=1},
h1PAamp\[Nu][l_,m_][r0_]:=h0PAamp[l,m][r0]+h1PAamp[l,m][r0]+2/3 r0/M (1+(-(M/r0)/Sqrt[1-3 M/r0])) h0PAamp[l,m]'[r0];]
h1PAAmp\[CapitalOmega][l_,m_]/;EvenQ[m]:=(\[Nu] h0PAamp[l,m][(M \[CapitalOmega])^(-2/3)]+\[Nu]^2 (h1PAamp\[Nu][l,m][(M \[CapitalOmega])^(-2/3)]));
h1PAAmp\[CapitalOmega][l_,m_]/;OddQ[m]:=Sqrt[1-4\[Nu]](\[Nu] h0PAamp[l,m][(M \[CapitalOmega])^(-2/3)]+\[Nu]^2 (2h0PAamp[l,m][(M \[CapitalOmega])^(-2/3)]+h1PAamp\[Nu][l,m][(M \[CapitalOmega])^(-2/3)]));

AssociationThread[Flatten[Table["("<>ToString[ll]<>","<>ToString[mm]<>")",{ll,2,5},{mm,1,ll}]],Flatten[Table[h1PAAmp\[CapitalOmega][ll,mm],{ll,2,5},{mm,1,ll}]]]
