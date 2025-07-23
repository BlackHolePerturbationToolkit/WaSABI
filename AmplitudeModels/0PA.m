(* ::Package:: *)

(* ::Input::Initialization:: *)
datadirectory = FileNameJoin[{WASABI`Waveform`Private`$WASABIAmplitudeDirectory, "sf_amp_data"}];
Do[Z\[ScriptCapitalI][ll,mm]=Interpolation[Get[FileNameJoin[{datadirectory,"1SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]]], {ll,2,5}, {mm,1,ll}]

Z\[ScriptCapitalI][l_,m_?Negative][r0_]:=(-1)^l Conjugate[Z\[ScriptCapitalI][l,-m][r0]];

With[{M=1},
h0PAamp[l_,m_][r0_]:=(-2(-Z\[ScriptCapitalI][l,m][r0]))/(I m Sqrt[M/r0^3])^2;]
h0PAAmp\[CapitalOmega][l_,m_]:=(\[Nu] h0PAamp[l,m][(M \[CapitalOmega])^(-2/3)]);

AssociationThread[Flatten[Table["("<>ToString[ll]<>","<>ToString[mm]<>")",{ll,2,5},{mm,1,ll}]],Flatten[Table[h0PAAmp\[CapitalOmega][ll,mm],{ll,2,5},{mm,1,ll}]]]
