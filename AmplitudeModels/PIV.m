(* ::Package:: *)

(* ::Section::Closed:: *)
(*Function on phase space*)


(* ::Subsection::Closed:: *)
(*ISCO radius*)


rISCO[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=3+Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]-Sign[\[Chi]1] \[Sqrt]((2-(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (4+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))+2 Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]));
drISCOd\[Chi]1[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/2 ((6 \[Chi]1+1/(3 (1-\[Chi]1^2)^(2/3)) 2 ((1-\[Chi]1^2) (-(1/(1-\[Chi]1)^(2/3))+1/(1+\[Chi]1)^(2/3))-2 \[Chi]1 ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))))/(Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2])-(Sign[\[Chi]1] ((2-(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (-((2 \[Chi]1 ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))/(3 (1-\[Chi]1^2)^(2/3)))+((1-\[Chi]1)^(2/3)-(1+\[Chi]1)^(2/3))/(3 (1-\[Chi]1^2)^(1/3))+(6 \[Chi]1+1/(3 (1-\[Chi]1^2)^(2/3)) 2 ((1-\[Chi]1^2) (-(1/(1-\[Chi]1)^(2/3))+1/(1+\[Chi]1)^(2/3))-2 \[Chi]1 ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))))/(Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]))+1/(3 (1-\[Chi]1^2)^(2/3)) (-((1-\[Chi]1^2) (-(1/(1-\[Chi]1)^(2/3))+1/(1+\[Chi]1)^(2/3)))+2 \[Chi]1 ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (4+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))+2 Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2])))/(\[Sqrt]((2-(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))) (4+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3))+2 Sqrt[3 \[Chi]1^2+(1+(1-\[Chi]1^2)^(1/3) ((1-\[Chi]1)^(1/3)+(1+\[Chi]1)^(1/3)))^2]))));


(* ::Subsection::Closed:: *)
(*Spin parameters*)


s[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/4 ((1+Sqrt[1-4 \[Nu]])^2 \[Chi]1+(-1+Sqrt[1-4 \[Nu]])^2 \[Chi]2);
\[Sigma][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/2 (-((1+Sqrt[1-4 \[Nu]]) \[Chi]1)-(-1+Sqrt[1-4 \[Nu]]) \[Chi]2);


(* ::Subsection::Closed:: *)
(*PN Parameter*)


x[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=\[Omega]^(2/3);


(* ::Subsection::Closed:: *)
(*WF frequency correction*)


(* ::Input::Initialization:: *)
\[Delta]x[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(4 \[Omega]^2 ((-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]))^(3/2) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+1/\[Omega])^(2/3)]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+1/\[Omega])^(2/3)]]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]))/(3 (-1+\[Chi]1 \[Omega])^7 (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2));


d\[Delta]xd\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(2 \[Omega]^2 ((-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]))^(3/2) drISCOd\[Chi]1[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] (2 (-\[Chi]1+1/\[Omega])^(2/3) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])-8 Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])-8 Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+(-\[Chi]1+1/\[Omega])^(2/3) \[Omega]^2 (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]] (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-\[Chi]1+1/\[Omega])^(1/3) (-1+\[Chi]1 \[Omega])^2 (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]] (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+4 Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "2"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-4 Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "2"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]))/(3 (-\[Chi]1+1/\[Omega])^(2/3) (-1+\[Chi]1 \[Omega])^7 (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2)^2)+(2 \[Omega]^2 Sqrt[(-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega])] rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] (12 \[Omega] (-1+\[Chi]1 \[Omega])^2 (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)] (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])-42 \[Omega] (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(-\[Chi]1+1/\[Omega])^(2/3) 16 \[Omega] (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(-\[Chi]1+1/\[Omega])^(2/3) 2 \[Omega]^2 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)] (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(-\[Chi]1+1/\[Omega])^(2/3) 4 (-1+\[Chi]1 \[Omega]) (-10+\[Chi]1 (14+9 (-\[Chi]1+1/\[Omega])^(2/3)) \[Omega]-6 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(-\[Chi]1+1/\[Omega])^(2/3) 18 \[Omega] (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (-2+\[Chi]1 (2 \[Omega]+(\[Omega] (-1+\[Chi]1 \[Omega])^2)^(1/3))) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])-1/(-\[Chi]1+1/\[Omega])^(2/3) 16 \[Omega] (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])-2 \[Omega]^3 (1+\[Chi]1 \[Omega]-3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]] (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+1/(-\[Chi]1+1/\[Omega])^(1/3) 2 \[Omega] (-1+\[Chi]1 \[Omega])^2 (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]] (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]-1/(-\[Chi]1+1/\[Omega])^(2/3) 8 \[Omega] (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "2"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+1/(-\[Chi]1+1/\[Omega])^(2/3) 8 \[Omega] (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "2"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-6 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-12 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+6 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-12 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-1/(-1+\[Chi]1) 6 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+1/(-1+\[Chi]1) 6 (-1+\[Chi]1 \[Omega])^3 (1+\[Chi]1 \[Omega]-3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+6 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-6 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((-\[Chi]1+1/\[Omega])^(1/3) \[Omega]^2 \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]/(-\[Chi]1+1/\[Omega])^(1/3)]) \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]))/(9 (-1+\[Chi]1 \[Omega])^7 ((-\[Chi]1+1/\[Omega])^(1/3)-3 \[Chi]1^2 \[Omega]+7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]-6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega])^2 (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2)^2);


d\[Delta]xdx[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(2 \[Omega] Sqrt[(-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega])] rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] (-14 \[Chi]1 \[Omega] (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((\[Omega]^5-\[Chi]1 \[Omega]^6)^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+4 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((\[Omega]^5-\[Chi]1 \[Omega]^6)^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(-\[Chi]1+1/\[Omega])^(2/3) 6 (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (1-4 \[Chi]1 \[Omega]+\[Chi]1^2 (3+(-\[Chi]1+1/\[Omega])^(2/3)) \[Omega]^2) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((\[Omega]^5-\[Chi]1 \[Omega]^6)^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(3 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) 2 (-1+\[Chi]1 \[Omega]) (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (1+6 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+3 \[Chi]1^2 (7+3 (-\[Chi]1+1/\[Omega])^(2/3)) \[Omega]^2-2 \[Chi]1 \[Omega] (7+9 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega])) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) ((\[Omega]^5-\[Chi]1 \[Omega]^6)^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])+1/(3 (-\[Chi]1+1/\[Omega])^(2/3)) 16 (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((\[Omega]^5-\[Chi]1 \[Omega]^6)^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]) (Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]-Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])-(2 \[Omega] (1+\[Chi]1 \[Omega]-3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]) (\[Omega]^4 (\[Omega]/(1-\[Chi]1 \[Omega]))^(2/3) (5-11 \[Chi]1 \[Omega]+6 \[Chi]1^2 \[Omega]^2) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]-6 \[Chi]1 (\[Omega]/(1-\[Chi]1 \[Omega]))^(2/3) (-1+\[Chi]1 \[Omega])^2 (\[Omega]^5-\[Chi]1 \[Omega]^6)^(2/3) \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]] (\[Omega]^5 \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(1-\[Chi]1 \[Omega]) (\[Omega]^5-\[Chi]1 \[Omega]^6)^(2/3) \!\(\*SuperscriptBox[\(\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]])))/(3 (\[Omega]/(1-\[Chi]1 \[Omega]))^(2/3) (\[Omega]^5-\[Chi]1 \[Omega]^6)^(2/3))+1/(3 (-\[Chi]1+1/\[Omega])^(2/3)) 8 (-1-\[Chi]1 \[Omega]+3 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]) (-(-\[Chi]1+1/\[Omega])^(1/3)+3 \[Chi]1^2 \[Omega]-7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]+6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega]) (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2) rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] ((\[Omega]^5-\[Chi]1 \[Omega]^6)^(1/3) \[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]+(-1+\[Chi]1 \[Omega])^2 \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],(\[Omega]/(1-\[Chi]1 \[Omega]))^(1/3) Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]]) (-Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Im1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "2"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]+Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)] \!\(\*SuperscriptBox[\(Re1SFAmp22int\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "2"}], ")"}],
Derivative],
MultilineFunction->None]\)[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)])))/(3 (-1+\[Chi]1 \[Omega])^7 ((-\[Chi]1+1/\[Omega])^(1/3)-3 \[Chi]1^2 \[Omega]+7 \[Chi]1 (-\[Chi]1+1/\[Omega])^(1/3) \[Omega]-6 (-\[Chi]1+1/\[Omega])^(2/3) \[Omega])^2 (Im1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2+Re1SFAmp22int[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(-\[Chi]1+1/\[Omega])^(2/3)]^2)^2);


(* ::Subsection::Closed:: *)
(*Total mass shift*)


d\[Delta]mdm[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu];


d\[Delta]md\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]m/\[Nu])


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio shift*)


d\[Delta]\[Delta]\[Nu]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(1-2 \[Delta]\[Nu] \[Nu])/\[Nu]^2


(* ::Subsection::Closed:: *)
(*Primary spin shift*)


d\[Delta]\[Chi]d\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu]


d\[Delta]\[Chi]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]\[Chi]/\[Nu])


(* ::Section::Closed:: *)
(*Construct Interpolations*)


(* ::Subsection::Closed:: *)
(*Fetch data*)


directoryamp = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Kerr_Circ"}];
directoryamp2 = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Schwarz_Circ"}];
directoryamp3 = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/2SF/Schwarz_Circ"}];
directory1SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Kerr_Circ"}];

fluxdata1SF=Get[FileNameJoin[{directory1SF,"SMRfluxdata2025_36x36.data"}]];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalI]*)


\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^5 fluxdata1SF[[i]]["inf"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalH]*)


\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^5 fluxdata1SF[[i]]["hor"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*\[Delta]x*)


Re1SFAmp22int=Interpolation[Get[FileNameJoin[{directoryamp, "ReAmp1SFKerr22.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];
Im1SFAmp22int=Interpolation[Get[FileNameJoin[{directoryamp, "ImAmp1SFKerr22.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*Real 22 amplitude  1SF*)


RealAmp1SFKerr22=Interpolation[Get[FileNameJoin[{directoryamp, "1SF_RealAmp_KerrCirc_22.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*2SF*)


Do[
Z\[ScriptCapitalI]data[ll,mm]=Get[FileNameJoin[{directoryamp2, "1SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]];
Z2\[ScriptCapitalI]data[ll,mm]=Get[FileNameJoin[{directoryamp3, "2SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]];

ReZ2\[ScriptCapitalI]data[ll,mm]=Table[{Z2\[ScriptCapitalI]data[ll,mm][[i]][[1]],Re[Z2\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z2\[ScriptCapitalI]data[ll,mm]]}];
ImZ2\[ScriptCapitalI]data[ll,mm]=Table[{Z2\[ScriptCapitalI]data[ll,mm][[i]][[1]],Im[Z2\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z2\[ScriptCapitalI]data[ll,mm]]}];
ReZ\[ScriptCapitalI]data[ll,mm]=Table[{Z\[ScriptCapitalI]data[ll,mm][[i]][[1]],Re[Z\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z\[ScriptCapitalI]data[ll,mm]]}];
ImZ\[ScriptCapitalI]data[ll,mm]=Table[{Z\[ScriptCapitalI]data[ll,mm][[i]][[1]],Im[Z\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z\[ScriptCapitalI]data[ll,mm]]}];
ReZ2\[ScriptCapitalI]interpolation[ll,mm]=Interpolation[ReZ2\[ScriptCapitalI]data[ll,mm]];
ImZ2\[ScriptCapitalI]interpolation[ll,mm]=Interpolation[ImZ2\[ScriptCapitalI]data[ll,mm]];
ImZ\[ScriptCapitalI][ll,mm]=Interpolation[ImZ\[ScriptCapitalI]data[ll,mm]];
ReZ\[ScriptCapitalI][ll,mm]=Interpolation[ReZ\[ScriptCapitalI]data[ll,mm]];

, {ll,2,(*5*)2}, {mm,1,ll}];

ReZ2\[ScriptCapitalI][ll_,mm_][r0_]:=Cos[ImZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]]Exp[ReZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]];
ImZ2\[ScriptCapitalI][ll_,mm_][r0_]:=Sin[ImZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]]Exp[ReZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]];



With[{M=1},
Reh0PAamp[l_,m_][r0_]:=(-2(-ReZ\[ScriptCapitalI][l,m][r0]))/(I m Sqrt[M/r0^3])^2;
Imh0PAamp[l_,m_][r0_]:=(-2(-ImZ\[ScriptCapitalI][l,m][r0]))/(I m Sqrt[M/r0^3])^2;
Reh1PAamp[l_,m_][r0_]:= (-2ReZ2\[ScriptCapitalI][l,m][r0])/(I m Sqrt[M/r0^3])^2;
Imh1PAamp[l_,m_][r0_]:= (-2ImZ2\[ScriptCapitalI][l,m][r0])/(I m Sqrt[M/r0^3])^2;
]


RealAmp1SFSchw22[r0_]:=((Reh0PAamp[2,2][r0])^2+(Imh0PAamp[2,2][r0])^2)^(1/2);
RealAmp2SFSchw22[r0_]:=(Imh0PAamp[2,2][r0] Imh1PAamp[2,2][r0]+Reh0PAamp[2,2][r0] Reh1PAamp[2,2][r0])/RealAmp1SFSchw22[r0];


(* ::Section::Closed:: *)
(*Hybridised mode amplitudes*)


(* ::Subsection::Closed:: *)
(*Mode amplitudes*)


(* ::Input::Initialization:: *)
<|"(2,2)"->428/21 m Sqrt[\[Pi]/5] \[Nu] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^2-44/21 m Sqrt[5 \[Pi]] \[Nu]^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^2+4/21 m Sqrt[\[Pi]/5] \[Nu] (-107+55 \[Nu]) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^2+32/3 m Sqrt[\[Pi]/5] \[Nu] \[Chi]1 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(5/2)+16/3 m Sqrt[\[Pi]/5] \[Nu] ((-1-Sqrt[1-4 \[Nu]]+\[Nu]) \[Chi]1+(-1+Sqrt[1-4 \[Nu]]+\[Nu]) \[Chi]2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(5/2)+2173/189 m Sqrt[\[Pi]/5] \[Nu] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3+1069/27 m Sqrt[\[Pi]/5] \[Nu]^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3+1/189 m Sqrt[\[Pi]/5] \[Nu] (-2173-7483 \[Nu]+2047 \[Nu]^2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3-8 m Sqrt[\[Pi]/5] \[Nu] \[Chi]1^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3-4 m Sqrt[\[Pi]/5] \[Nu] (-((1+Sqrt[1-4 \[Nu]]-2 \[Nu]) \[Chi]1^2)-4 \[Nu] \[Chi]1 \[Chi]2+(-1+Sqrt[1-4 \[Nu]]+2 \[Nu]) \[Chi]2^2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3+(856 m \[Pi]^(3/2) \[Nu] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/(21 Sqrt[5])-(272 m \[Pi]^(3/2) \[Nu]^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/(21 Sqrt[5])+(8 m \[Pi]^(3/2) \[Nu] (-107+34 \[Nu]) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/(21 Sqrt[5])+256/63 m Sqrt[5 \[Pi]] \[Nu] \[Chi]1 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2)+8/63 m Sqrt[\[Pi]/5] \[Nu] ((-80 (1+Sqrt[1-4 \[Nu]])+(101-56 Sqrt[1-4 \[Nu]]) \[Nu]+132 \[Nu]^2) \[Chi]1+(80 (-1+Sqrt[1-4 \[Nu]])+(101+56 Sqrt[1-4 \[Nu]]) \[Nu]+132 \[Nu]^2) \[Chi]2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2)-(m Sqrt[\[Pi]/5] (-292094250+14916825 \[Pi]^2) \[Nu]^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/4365900+(64 m \[Pi]^(3/2) \[Nu] \[Chi]1 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/(3 Sqrt[5])-64/63 m Sqrt[\[Pi]/5] \[Nu] \[Chi]1^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4+1/(3 Sqrt[5]) 32 m \[Pi]^(3/2) \[Nu] ((-1-Sqrt[1-4 \[Nu]]+\[Nu]) \[Chi]1+(-1+Sqrt[1-4 \[Nu]]+\[Nu]) \[Chi]2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4-8/63 m Sqrt[\[Pi]/5] \[Nu] ((-4 (1+Sqrt[1-4 \[Nu]])+(99+91 Sqrt[1-4 \[Nu]]) \[Nu]+60 \[Nu]^2) \[Chi]1^2+(271-288 \[Nu]) \[Nu] \[Chi]1 \[Chi]2+(-4+4 Sqrt[1-4 \[Nu]]+99 \[Nu]-91 Sqrt[1-4 \[Nu]] \[Nu]+60 \[Nu]^2) \[Chi]2^2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4-1/4365900 m Sqrt[\[Pi]/5] \[Nu] (1459480086-284739840 EulerGamma+23284800 \[Pi]^2-569479680 Log[2]-142369920 Log[x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4+1/4365900 m Sqrt[\[Pi]/5] \[Nu] (1459480086-284739840 EulerGamma+23284800 \[Pi]^2-292094250 \[Nu]+14916825 \[Pi]^2 \[Nu]-255288600 \[Nu]^2+40122250 \[Nu]^3-569479680 Log[2]-142369920 Log[x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4+(4346 m \[Pi]^(3/2) \[Nu] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/(189 Sqrt[5])+1996/189 Sqrt[5] m \[Pi]^(3/2) \[Nu]^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2)+(2 m \[Pi]^(3/2) \[Nu] (-2173-4990 \[Nu]+1120 \[Nu]^2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/(189 Sqrt[5])-11236/189 m Sqrt[\[Pi]/5] \[Nu] \[Chi]1 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2)-(16 m \[Pi]^(3/2) \[Nu] \[Chi]1^2 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/Sqrt[5]+16/3 m Sqrt[\[Pi]/5] \[Nu]^2 (\[Chi]1+\[Chi]2) ((1+Sqrt[1-4 \[Nu]]-2 \[Nu]) \[Chi]1^2+4 \[Nu] \[Chi]1 \[Chi]2-(-1+Sqrt[1-4 \[Nu]]+2 \[Nu]) \[Chi]2^2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2)-1/Sqrt[5] 8 m \[Pi]^(3/2) \[Nu] (-((1+Sqrt[1-4 \[Nu]]-2 \[Nu]) \[Chi]1^2)-4 \[Nu] \[Chi]1 \[Chi]2+(-1+Sqrt[1-4 \[Nu]]+2 \[Nu]) \[Chi]2^2) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2)+2/189 m Sqrt[\[Pi]/5] \[Nu] (-(1/2) Sqrt[1-4 \[Nu]] (3931+15626 \[Nu]+3075 \[Nu]^2) ((1+Sqrt[1-4 \[Nu]]) \[Chi]1+(-1+Sqrt[1-4 \[Nu]]) \[Chi]2)+9/4 (1061+4043 \[Nu]+499 \[Nu]^2) ((1+Sqrt[1-4 \[Nu]])^2 \[Chi]1+(-1+Sqrt[1-4 \[Nu]])^2 \[Chi]2)) x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2)+(m \[Nu] RealAmp1SFKerr22[\[Chi]1,-1+(2 rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(1-\[Chi]1 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(3/2))^(2/3)] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(2 (1-\[Chi]1 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(3/2))^(2/3))+\[Nu]^2 (-((\[Delta]m+m (-1+\[Delta]\[Nu])) RealAmp1SFSchw22[1/x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])+m RealAmp2SFSchw22[1/x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]]+(m (2+2 \[Delta]\[Chi] x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(3/2)-3 \[Delta]x[\[Omega],\[Phi],\[Nu],m,0,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]) Derivative[1][RealAmp1SFSchw22][1/x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])/(3 x[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]))
|>
