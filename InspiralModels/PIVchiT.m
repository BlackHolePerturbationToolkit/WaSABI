(* ::Package:: *)

(* ::Section::Closed:: *)
(*Dependent variables*)


(* ::Subsection::Closed:: *)
(*ISCO radius*)


rISCO[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=3+Sqrt[3 \[Chi]t1^2+(1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))^2]-Sign[\[Chi]t1] \[Sqrt]((2-(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))) (4+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))+2 Sqrt[3 \[Chi]t1^2+(1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))^2]));
drISCOd\[Chi]1[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/2 ((6 \[Chi]t1+1/(3 (1-\[Chi]t1^2)^(2/3)) 2 ((1-\[Chi]t1^2) (-(1/(1-\[Chi]t1)^(2/3))+1/(1+\[Chi]t1)^(2/3))-2 \[Chi]t1 ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))) (1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))))/(Sqrt[3 \[Chi]t1^2+(1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))^2])-(Sign[\[Chi]t1] ((2-(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))) (-((2 \[Chi]t1 ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))/(3 (1-\[Chi]t1^2)^(2/3)))+((1-\[Chi]t1)^(2/3)-(1+\[Chi]t1)^(2/3))/(3 (1-\[Chi]t1^2)^(1/3))+(6 \[Chi]t1+1/(3 (1-\[Chi]t1^2)^(2/3)) 2 ((1-\[Chi]t1^2) (-(1/(1-\[Chi]t1)^(2/3))+1/(1+\[Chi]t1)^(2/3))-2 \[Chi]t1 ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))) (1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))))/(Sqrt[3 \[Chi]t1^2+(1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))^2]))+1/(3 (1-\[Chi]t1^2)^(2/3)) (-((1-\[Chi]t1^2) (-(1/(1-\[Chi]t1)^(2/3))+1/(1+\[Chi]t1)^(2/3)))+2 \[Chi]t1 ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))) (4+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))+2 Sqrt[3 \[Chi]t1^2+(1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))^2])))/(\[Sqrt]((2-(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))) (4+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3))+2 Sqrt[3 \[Chi]t1^2+(1+(1-\[Chi]t1^2)^(1/3) ((1-\[Chi]t1)^(1/3)+(1+\[Chi]t1)^(1/3)))^2]))));


(* ::Subsection::Closed:: *)
(*PN Parameter*)


x[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=\[Omega]^(2/3);


(* ::Subsection::Closed:: *)
(*WF frequency correction*)


(* ::Input::Initialization:: *)
\[Delta]x[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=With[{\[Chi]1=\[Chi]t1,\[Chi]2=\[Chi]t2},(0.3333333333333333*\[Omega]^2*(-0.3333333333333333-0.3333333333333333*\[Chi]1*\[Omega]+1.*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega])*Sqrt[(-1+\[Chi]1*\[Omega])*(-1-\[Chi]1*\[Omega]+3*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]*((-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]+(1-2.*\[Chi]1*\[Omega]+\[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]])*(1.*Re1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]*Derivative[0,1][Im1SFAmp22intV2][0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]-1.*Im1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]*Derivative[0,1][Re1SFAmp22intV2][0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]))/((-1.+\[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1+\[Omega]^(-1))^(1/3)+0.5*\[Chi]1^2*\[Omega]-1.1666666666666667*\[Chi]1*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega]+1.*(-\[Chi]1+\[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]^2+Re1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]^2)*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)])];


d\[Delta]xd\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=With[{\[Chi]1=\[Chi]t1,\[Chi]2=\[Chi]t2},(-0.3333333333333333*\[Omega]^2*(0.05555555555555555/(-\[Chi]1 + \[Omega]^(-1))^(2/3) + 1.*\[Chi]1*\[Omega] + (0.3888888888888889*\[Chi]1*\[Omega])/(-\[Chi]1 + \[Omega]^(-1))^(2/3) - 
    (0.6666666666666666*\[Omega])/(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 1.1666666666666667*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   (-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]))/
  ((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
     1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])^2*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) + 
 (0.3333333333333333*\[Omega]^2*(-0.3333333333333333*\[Omega] - (0.3333333333333333*\[Omega])/(-\[Chi]1 + \[Omega]^(-1))^(2/3))*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]))/
  ((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) - 
 (2.*\[Omega]^3*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]))/
  ((-1. + \[Chi]1*\[Omega])^7*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) + 
 (0.16666666666666666*\[Omega]^2*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   ((-1 + \[Chi]1*\[Omega])*(-\[Omega] - \[Omega]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)) + \[Omega]*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]))*
   rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]))/
  ((-1. + \[Chi]1*\[Omega])^6*Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 
    0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
   (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) - 
 (0.3333333333333333*\[Omega]^2*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*(-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
    8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]))/
  ((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]*
   (4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))) - 
 (0.16666666666666666*\[Omega]^2*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
    (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
     ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
       2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]))/
  ((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3))^(3/2)) - 
 (0.3333333333333333*\[Omega]^2*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (2*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     ((1.*((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 1][Im1SFAmp22intV2][
         0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)] - 
      (0.2901432317650783*Derivative[1, 0][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
         -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/(1 - \[Chi]1)) + 
    2*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     ((1.*((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 1][Re1SFAmp22intV2][
         0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)] - 
      (0.2901432317650783*Derivative[1, 0][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
         -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/(1 - \[Chi]1))))/
  ((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)^2*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) + 
 (0.3333333333333333*\[Omega]^2*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (-1/3*(\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/
      (-\[Chi]1 + \[Omega]^(-1))^(2/3) + (-2.*\[Omega] + 2*\[Chi]1*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + 
    (-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*((((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 1][\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr][Log[1 - \[Chi]1], 
         Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/
       (2*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) - 
      Derivative[1, 0][\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr][Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]/
       (1 - \[Chi]1)) + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*((((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 1][\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr][Log[1 - \[Chi]1], 
         Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/
       (2*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) - 
      Derivative[1, 0][\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr][Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]/
       (1 - \[Chi]1))))/((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 
    1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
   (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]) + 
 (0.3333333333333333*\[Omega]^2*(-0.3333333333333333 - 0.3333333333333333*\[Chi]1*\[Omega] + 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
   Sqrt[(-1 + \[Chi]1*\[Omega])*(-1 - \[Chi]1*\[Omega] + 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
   ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
        (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
   (-1.*Derivative[0, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     ((1.*((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 1][Im1SFAmp22intV2][
         0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)] - 
      (0.2901432317650783*Derivative[1, 0][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
         -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/(1 - \[Chi]1)) + 
    1.*Derivative[0, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     ((1.*((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 1][Re1SFAmp22intV2][
         0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)] - 
      (0.2901432317650783*Derivative[1, 0][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
         -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/(1 - \[Chi]1)) + 
    1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     ((1.*((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 2][Im1SFAmp22intV2][
         0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)] - 
      (0.2901432317650783*Derivative[1, 1][Im1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
         -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/(1 - \[Chi]1)) - 
    1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*
     ((1.*((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(3*(-\[Chi]1 + \[Omega]^(-1))^(5/3)) - 
         (-6*\[Chi]1*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 8*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          ((-\[Chi]1 + \[Omega]^(-1))^(2/3)*(4*\[Chi]1 - 6*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
            2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))))*Derivative[0, 2][Re1SFAmp22intV2][
         0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)] - 
      (0.2901432317650783*Derivative[1, 1][Re1SFAmp22intV2][0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
         -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])/(1 - \[Chi]1))))/
  ((-1. + \[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) + 0.5*\[Chi]1^2*\[Omega] - 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)])];


d\[Delta]xdx[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=With[{\[Chi]1=\[Chi]t1,\[Chi]2=\[Chi]t2},(0.5*\[Omega]^(4/3)*(0.3333333333333333*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]*(1 - \[Chi]1*\[Omega])*(-1. + \[Chi]1*\[Omega])^2*
    (2.0000000000000004 + \[Chi]1*(-3. - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3))*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 
     1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*
      \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) - 
   0.3333333333333333*(1 - \[Chi]1*\[Omega])*(1. - \[Chi]1*\[Omega])*(-1 + \[Chi]1*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 
     0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*
      \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) + 
   6.*\[Chi]1*\[Omega]*(1 - \[Chi]1*\[Omega])*(1. - \[Chi]1*\[Omega])*(-1 + \[Chi]1*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 
     0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*
      \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) + 
   2.*(1 - \[Chi]1*\[Omega])*(-1 + \[Chi]1*\[Omega])*(-1. + \[Chi]1*\[Omega])^2*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 
     0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*
      \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) + 
   1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]*(-1. + \[Chi]1*\[Omega])^2*(0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 
     1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*(1. - 4.*\[Chi]1*\[Omega] + \[Chi]1^2*(3. + (-\[Chi]1 + \[Omega]^(-1))^(2/3))*\[Omega]^2)*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*
      \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) - 
   0.5*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*(1 - \[Chi]1*\[Omega])*(-1. + \[Chi]1*\[Omega])^2*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.1111111111111111 + 0.6666666666666667*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] + \[Chi]1^2*(2.3333333333333335 + 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3))*\[Omega]^2 + 
     \[Chi]1*(-1.5555555555555558*\[Omega] - 2.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2))*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*
      \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) - 
   1.3333333333333333*(1 - \[Chi]1*\[Omega])*(1. - \[Chi]1*\[Omega])*(-1 + \[Chi]1*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*
    (0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 
     0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]*
    ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
         (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + 
     1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]) - 
   2.*(1 - \[Chi]1*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 
     1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 0.5*\[Chi]1^2*\[Omega] + 
     1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    (1.*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     1.*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 1][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*(1. - \[Chi]1*\[Omega])*(0.8333333333333335 - 1.8333333333333333*\[Chi]1*\[Omega] + 1.*\[Chi]1^2*\[Omega]^2)*
      rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + 
     1.*(1 - \[Chi]1*\[Omega])*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(\[Chi]1*\[Omega]*(-1. + 3.*\[Chi]1*\[Omega] - 3.*\[Chi]1^2*\[Omega]^2 + 1.*\[Chi]1^3*\[Omega]^3)*
        \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
       0.16666666666666666*\[Omega]^2*(1 - \[Chi]1*\[Omega])^(1/3)*(-1. + 1.*\[Chi]1*\[Omega])*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]1*\[Omega])^(2/3)]*Derivative[0, 1][\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr][Log[1 - \[Chi]1], 
         \[Omega]^(1/3)*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*\[Omega])^(2/3)]] + 
       (0.16666666666666666 - 0.5*\[Chi]1*\[Omega] + 0.5*\[Chi]1^2*\[Omega]^2 - 0.16666666666666666*\[Chi]1^3*\[Omega]^3)*
        Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]*Derivative[0, 1][\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr][Log[1 - \[Chi]1], 
         Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])) + 
   1.*(1 - \[Chi]1*\[Omega])*(1. - \[Chi]1*\[Omega])*(-1 + \[Chi]1*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.3333333333333333 + 0.3333333333333333*\[Chi]1*\[Omega] - 
     1.*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])*(0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 0.5*\[Chi]1^2*\[Omega] + 
     1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])*
    (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
     Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)*
    rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]*
    ((-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
         (-\[Chi]1 + \[Omega]^(-1))^(2/3)]] + (1 - 2.*\[Chi]1*\[Omega] + \[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])*
    (0.6666666666666666*Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 2][Im1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]] - 
     0.6666666666666666*Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]*Derivative[0, 2][Re1SFAmp22intV2][
       0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
       -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]])))/
 ((-1 + \[Chi]1*\[Omega])*(-1. + \[Chi]1*\[Omega])^8*Sqrt[(1 - \[Chi]1*\[Omega])*(1 + \[Chi]1*\[Omega] - 3*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega])]*
  (0.16666666666666666*(-\[Chi]1 + \[Omega]^(-1))^(1/3) - 0.5*\[Chi]1^2*\[Omega] + 1.1666666666666667*\[Chi]1*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega] - 
    1.*(-\[Chi]1 + \[Omega]^(-1))^(2/3)*\[Omega])^2*
  (Im1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2 + 
    Re1SFAmp22intV2[0.7988880369434854 + 0.2901432317650783*Log[1 - \[Chi]1], 
      -1 + 2.*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]]^2)^2*
  Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)])];


(* ::Subsection::Closed:: *)
(*WF frequency forcing term*)


F\[Omega][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-3*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*((24*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 - 
   (24*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 - (24*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + 
   (24*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + (208*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/9 + 
   (66*\[Nu]^4*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 + (46*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
   (46*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (4352*\[Nu]^3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 + 
   (1256*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 + 
   (8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
   (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
   (46*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (46*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
   (4352*\[Nu]^4*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - (1256*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - 
   (188806*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/945 - (1240*\[Nu]^5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/81 + 
   (272*Pi*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 - (272*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 - 
   (1342*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 + (1342*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 - 
   (3788*\[Nu]^3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 - (272*Pi*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 + 
   (272*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 - (4808*\[Nu]^4*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 - 
   (1390*\[Nu]^4*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/63 - (1342*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 - 
   (3788*\[Nu]^5*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 + (77354*Pi*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/189 - 
   (72382*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/1701 + (72382*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
    1701 + (98752*\[Nu]^3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/945 + 
   (2000*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/189 - (8992*\[Nu]^4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/27 - 
   (8936*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 + (212*\[Nu]^2*\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - 
   (212*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 
   (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
   (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + (72382*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/1701 - 
   (72382*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/1701 + (98752*\[Nu]^4*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
    945 - (2000*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/189 - (8992*\[Nu]^5*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
    27 + (8936*Sqrt[1 - 4*\[Nu]]*\[Nu]^5*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 + 
   (196*\[Nu]^3*\[Chi]t1^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - 
   (196*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t1^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 52*Pi*\[Nu]^4*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
   (184*\[Nu]^4*\[Chi]t1*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 
   (196*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t1*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - (24*\[Nu]^5*\[Chi]t2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
    5 + (212*Sqrt[1 - 4*\[Nu]]*\[Nu]^5*\[Chi]t2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + (2571400*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/1701 - 
   (3157*Pi^2*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (5500*\[Nu]^5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/63 + 
   (16*\[Nu]^6*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/3 + (10069*Pi*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/210 - 
   (10069*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/210 + (27758*Pi*\[Nu]^3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 + 
   (21241*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/105 - (373*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/7 + 
   (373*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/7 - (1171*\[Nu]^3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - 
   (802*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/3 + (13474*\[Nu]^4*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - 
   (10069*Pi*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/210 + (10069*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/210 + 
   (27758*Pi*\[Nu]^4*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - (21241*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/105 + 
   (13222*\[Nu]^4*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 + (4156*\[Nu]^5*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 + 
   12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (60397*\[Nu]^4*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 - 
   (373*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/7 - (1171*\[Nu]^5*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 + 
   (802*Sqrt[1 - 4*\[Nu]]*\[Nu]^5*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/3 + (13474*\[Nu]^6*\[Chi]t2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - 
   ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
         12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
      12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
      (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
       (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
      (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
       (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (26622581*Pi*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/9072 - 
   (3719141*Pi*\[Nu]^5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/5940 + 
   (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
      12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
         (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
        (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
        (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
   (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
   (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
   (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 + 
   ((m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (m*(-7 + Sqrt[1 - 4*\[Nu]])*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/6 - 
      m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + m*\[Nu]^2*(\[Chi]t1 + \[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
      (3*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - (m*\[Nu]^2*(99 - 45*Sqrt[1 - 4*\[Nu]] + (-61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - (5*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 - 
      (5*m*\[Nu]*(-2*\[Nu]*(3 + \[Nu])*\[Chi]t1 + 2*\[Nu]^2*(-2 + 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + 
      (27*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - 
      (m*\[Nu]*(3*Sqrt[1 - 4*\[Nu]]*\[Nu]*(27 - 156*\[Nu] + 5*\[Nu]^2) - ((-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*(405 - 1101*\[Nu] + 29*\[Nu]^2))/2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - 
      m*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + (m*(1 + Sqrt[1 - 4*\[Nu]])*\[Nu]^2*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 + m*\[Nu]^2*(\[Chi]t1 + \[Nu]*\[Chi]t2)*(\[Chi]t1 - Sqrt[1 - 4*\[Nu]]*\[Chi]t1 + (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - (35*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 + 
      (7*m*\[Nu]*(-2*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]t1 + 2*\[Nu]^2*(261 - 342*Sqrt[1 - 4*\[Nu]] + (-931 + 274*Sqrt[1 - 4*\[Nu]])*\[Nu] + 59*\[Nu]^2)*\[Chi]t2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 + (m*\[Nu]^2*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
       ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))*
     (-1/15*(m*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
         12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) - 8*(-3 + Sqrt[1 - 4*\[Nu]])*\[Nu] + 18*\[Nu]^3*\[Chi]t2^2 + 3*\[Nu]^2*(-2 + (-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-8 + 3*(1 + Sqrt[1 - 4*\[Nu]] + 8*\[Nu])*\[Chi]t2^2))/
            (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
           (16*\[Nu]^4*\[Chi]t2^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))) + 
      m*\[Chi]t2*((-1 + Sqrt[1 - 4*\[Nu]])*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
          (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
          (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
          (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
          ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
                12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[
                1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
             (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
              (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
             (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
              (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
          (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
             72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
             \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
          (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
           (-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
            \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
        4*\[Nu]^2*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
          (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
          ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
                12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[
                1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
             (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
              (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
             (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
              (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
            (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*
                (-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
              ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
                (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                   (-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                   (35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
                 Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
             Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
            \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) + 
        \[Nu]*((\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
             12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
                (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*
                 (54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*
                 (9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*
                 (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^
                          2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 + 
          (Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
             12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
                (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*
                 (54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*
                 (9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*
                 (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^
                          2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/10 + 
          5*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
            (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
            ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
                  12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + 
                 Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
                  39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*
                (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
               (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
                (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
            (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 72*
                Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + 
                 \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
            (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                  x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
             (-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
              \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
          3*Sqrt[1 - 4*\[Nu]]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
            (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
            ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
                  12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + 
                 Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
                  39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*
                (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
               (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
                (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
            (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 72*
                Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + 
                 \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
            (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                  x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
             (-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
              \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)))))/
    (m^2*(-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - 4*\[Nu]]*\[Nu]) + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5*
     \[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
        (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(10/3) - 
   2*\[Delta]\[Nu]*\[Nu]^3*\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(-1)] + \[Nu]^3*\[ScriptCapitalF]\[ScriptCapitalI]int2SFSchw[-Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]] + 
   (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5*\[ScriptCapitalF]\[ScriptCapitalI]intS2Kerr[Log[1 - \[Chi]t1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(10/3) - 
   (\[Nu]^3*\[Delta]x[\[Omega], \[Phi], \[Nu], m, 0, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Derivative[1][\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw][x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(-1)])/
    x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + ((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
     (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
     (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
     (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
     ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
           12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
        (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
          Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
       (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
          \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
         ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
           (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
              (-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
          (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
          (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                 Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
     (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
        72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
        \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
     (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
     (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)*
    ((-3*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/8 - (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + 
     (\[Nu]*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + (4*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
     (\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + \[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
     (\[Nu]*(-((7 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-7 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/6 - 
     (27*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 + (19*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 + 
     (\[Nu]*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 - (\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 - 
     \[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + (\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 
     4*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (38*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 + 
     (3*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - 
     (\[Nu]*((99 + 45*Sqrt[1 - 4*\[Nu]] - (61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t1 + \[Nu]*(99 - 45*Sqrt[1 - 4*\[Nu]] + (-61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - (675*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/128 - 
     (5*(-6889 + 246*Pi^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/1152 + (5*\[Nu]*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 - (65*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + 
     (145*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 - (5*\[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 - 
     (5*\[Nu]*((-2 - 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t1^2 - 2*\[Nu]*(3 + \[Nu])*\[Chi]t1*\[Chi]t2 + \[Nu]^2*(-2 + 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (27*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
     (73*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - \[Nu]^2*\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
     (27*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - \[Nu]^2*\[Chi]t1^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
     (\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*(\[Chi]t1 - Sqrt[1 - 4*\[Nu]]*\[Chi]t1 + (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
     (\[Nu]*(3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(-\[Chi]t1 + \[Nu]*\[Chi]t2) - ((405 - 1101*\[Nu] + 29*\[Nu]^2)*(-((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2))/2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - (3969*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/256 - 
     (469*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/48 + (13223*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - 
     (35*\[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 + 
     (7*\[Nu]*((261 + 342*Sqrt[1 - 4*\[Nu]] - 931*\[Nu] - 274*Sqrt[1 - 4*\[Nu]]*\[Nu] + 59*\[Nu]^2)*\[Chi]t1^2 - 2*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]t1*\[Chi]t2 + 
        \[Nu]^2*(261 - 342*Sqrt[1 - 4*\[Nu]] + (-931 + 274*Sqrt[1 - 4*\[Nu]])*\[Nu] + 59*\[Nu]^2)*\[Chi]t2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - 
     (\[Nu]^2*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/46080 - (\[Nu]*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
        27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 + (\[Nu]^2*\[Chi]t2*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
      ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
         (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
     (\[Nu]*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
      Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
         3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
     (\[Nu]^2*(3*(-1 - \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - \[Chi]t1], 
          Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
        (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
          (2*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (3/2))^(2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (3/2))^(2/3)) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
        ((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
          (-6*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 2*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
              \[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
              4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
              \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + (Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/6) + 
   (((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
      (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
      (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
            12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
         (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
           Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
        (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
           \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
          ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
            (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 
                12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 + 
      (Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
         12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
            (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
           (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
      (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
      (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
      (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 - 
      Sqrt[1 - 4*\[Nu]]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
        (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
            (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
           (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
            (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
        (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
        (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
      4*\[Nu]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
            (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
           (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
            (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
          (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
             \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
            ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
              (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[
                PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
        (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
        (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))*
     ((-3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/8 - (m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + 
      (m*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + (4*m*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
      (2*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + 2*m*\[Nu]*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
      (m*(-((7 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-7 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/6 + 
      (m*\[Nu]*((2*\[Chi]t1)/Sqrt[1 - 4*\[Nu]] + (-7 + Sqrt[1 - 4*\[Nu]])*\[Chi]t2 - (2*\[Nu]*\[Chi]t2)/Sqrt[1 - 4*\[Nu]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/6 - 
      (27*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 + (19*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/8 + 
      (m*\[Nu]*(-57 + 2*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 + (m*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 - 
      (m*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 - 2*m*\[Nu]*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
      m*\[Nu]*\[Chi]t2*(\[Chi]t1 + \[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + (m*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 
      4*m*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (76*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 + 
      3*m*\[Nu]*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (m*((99 + 45*Sqrt[1 - 4*\[Nu]] - (61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t1 + 
         \[Nu]*(99 - 45*Sqrt[1 - 4*\[Nu]] + (-61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - 
      (m*\[Nu]*((-61 - 90/Sqrt[1 - 4*\[Nu]] - Sqrt[1 - 4*\[Nu]] + (2*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]t1 + (99 - 45*Sqrt[1 - 4*\[Nu]] + (-61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t2 + 
         \[Nu]*(-61 + 90/Sqrt[1 - 4*\[Nu]] + Sqrt[1 - 4*\[Nu]] - (2*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - 
      (675*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/128 - (5*m*(-6889 + 246*Pi^2)*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/576 + 
      (5*m*\[Nu]*(9*(-6889 + 246*Pi^2) + 3348*\[Nu] + 21*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 + 
      (5*m*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 - 
      (65*m*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (145*m*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 - 
      (5*m*\[Nu]*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/3 - (5*m*\[Nu]*((7 + 22/Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*\[Chi]t1*\[Chi]t2 - 2*(3 + \[Nu])*\[Chi]t1*\[Chi]t2 + 
         (7 - 22/Sqrt[1 - 4*\[Nu]])*\[Nu]^2*\[Chi]t2^2 + 2*\[Nu]*(-2 + 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 - 
      (5*m*((-2 - 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t1^2 - 2*\[Nu]*(3 + \[Nu])*\[Chi]t1*\[Chi]t2 + \[Nu]^2*(-2 + 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t2^2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (27*m*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
      73*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 2*m*\[Nu]*\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
      (27*m*\[Nu]*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/4 - 2*m*\[Nu]*\[Chi]t1^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
      m*\[Nu]*\[Chi]t2*(\[Chi]t1 + \[Nu]*\[Chi]t2)*(\[Chi]t1 - Sqrt[1 - 4*\[Nu]]*\[Chi]t1 + (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
      (m*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*(\[Chi]t1 - Sqrt[1 - 4*\[Nu]]*\[Chi]t1 + (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 + 
      (m*\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*((2*\[Chi]t1)/Sqrt[1 - 4*\[Nu]] + (1 + Sqrt[1 - 4*\[Nu]])*\[Chi]t2 - (2*\[Nu]*\[Chi]t2)/Sqrt[1 - 4*\[Nu]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
      (m*(3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(-\[Chi]t1 + \[Nu]*\[Chi]t2) - ((405 - 1101*\[Nu] + 29*\[Nu]^2)*(-((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2))/2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - (m*\[Nu]*(3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*\[Chi]t2 + 3*Sqrt[1 - 4*\[Nu]]*(-156 + 10*\[Nu])*(-\[Chi]t1 + \[Nu]*\[Chi]t2) - 
         (6*(27 - 156*\[Nu] + 5*\[Nu]^2)*(-\[Chi]t1 + \[Nu]*\[Chi]t2))/Sqrt[1 - 4*\[Nu]] - ((-1101 + 58*\[Nu])*(-((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2))/2 - 
         ((405 - 1101*\[Nu] + 29*\[Nu]^2)*((2*\[Chi]t1)/Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]t2 - (2*\[Nu]*\[Chi]t2)/Sqrt[1 - 4*\[Nu]]))/2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - (3969*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/256 - 
      (469*m*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/48 + (13223*m*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/216 - 
      (35*m*\[Nu]*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/4 + 
      (7*m*\[Nu]*((-931 - 684/Sqrt[1 - 4*\[Nu]] - 274*Sqrt[1 - 4*\[Nu]] + 118*\[Nu] + (548*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(429 + 106*\[Nu])*\[Chi]t1*\[Chi]t2 - 
         2*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]t1*\[Chi]t2 + \[Nu]^2*(-931 + 684/Sqrt[1 - 4*\[Nu]] + 274*Sqrt[1 - 4*\[Nu]] + 118*\[Nu] - (548*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2 + 
         2*\[Nu]*(261 - 342*Sqrt[1 - 4*\[Nu]] + (-931 + 274*Sqrt[1 - 4*\[Nu]])*\[Nu] + 59*\[Nu]^2)*\[Chi]t2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 + 
      (7*m*((261 + 342*Sqrt[1 - 4*\[Nu]] - 931*\[Nu] - 274*Sqrt[1 - 4*\[Nu]]*\[Nu] + 59*\[Nu]^2)*\[Chi]t1^2 - 2*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]t1*\[Chi]t2 + 
         \[Nu]^2*(261 - 342*Sqrt[1 - 4*\[Nu]] + (-931 + 274*Sqrt[1 - 4*\[Nu]])*\[Nu] + 59*\[Nu]^2)*\[Chi]t2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - 
      (m*\[Nu]*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/23040 - (m*\[Nu]*(2520*(-71207 + 2706*Pi^2)*\[Nu] + 325080*\[Nu]^2 + 6160*\[Nu]^3 + 
         27*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 - (m*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
         27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 + (2*m*\[Nu]*\[Chi]t2*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
       ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
      (m*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
       Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
      (\[Nu]*(3*m*(-1 - \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
         (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
            Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
           (2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
              3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
              \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
              \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))^(2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
         (m*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
            (-6*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 2*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*
                 x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
                4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
                \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, 
                \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))))/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + 
         (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
            3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
            \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
           Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/3))/(2*m) + 
   (((2*Sqrt[1 - 4*\[Nu]]*(24*m*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 - 12*m*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 6*m*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 
         6*m*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 6*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(4*(3 + Sqrt[1 - 4*\[Nu]]) + 9*\[Chi]t1^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 180*m*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 
         (m*\[Nu]^2*(3*(1 + Sqrt[1 - 4*\[Nu]])^5*(1 + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 4*\[Chi]t1^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*(-1 + 
                12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])) + 4*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]t1^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*(35 + 
                39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])) - (96*(1 + Sqrt[1 - 4*\[Nu]])*\[Nu]^3*\[Chi]t1*\[Chi]t2)/(-1 + Sqrt[1 - 4*\[Nu]]) + 
            (144*(3 + 2*Sqrt[1 - 4*\[Nu]])*\[Nu]^2*\[Chi]t1^3*\[Chi]t2)/(-1 + Sqrt[1 - 4*\[Nu]]) + 48*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
             Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/
          (1 + Sqrt[1 - 4*\[Nu]]) - 8*(24*Im[m*\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
           72*Im[m*\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
           m*\[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             (15/2)) + (15*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
            Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - (15*m*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
           (-1 + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
           \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/15 - 
      m*\[Chi]t1*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
        (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
            (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
           (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
            (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
          (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
             \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
            ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
              (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[
                PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 + (Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
          (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
             \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
            ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
              (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[
                PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
        (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
        (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 + 
        3*Sqrt[1 - 4*\[Nu]]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
          (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
          (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
          (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
          ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
                12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[
                1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
             (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
              (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
             (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
              (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
          (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
             72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
             \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
          (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
           (-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
            \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
        4*\[Nu]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
          (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
          ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
                12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[
                1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
             (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
              (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
             (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
              (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
            (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*
                (-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
              ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
                (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                   (-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                   (35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
                 Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
             Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
            \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)))*
     ((4*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + (m*(-7 - Sqrt[1 - 4*\[Nu]])*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/6 - 
      (m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
      m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + m*\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
      4*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (38*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 - 
      (m*\[Nu]*(99 + 45*Sqrt[1 - 4*\[Nu]] - (61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - 
      (65*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 + (145*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 - 
      (5*m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 - (5*m*\[Nu]*(2*(-2 - 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t1 - 2*\[Nu]*(3 + \[Nu])*\[Chi]t2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (27*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
      (73*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
      (m*\[Nu]*(-3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2) - ((-1 - Sqrt[1 - 4*\[Nu]])*(405 - 1101*\[Nu] + 29*\[Nu]^2))/2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - 
      3*m*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 2*m*\[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
      (m*(1 - Sqrt[1 - 4*\[Nu]])*\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 + 
      m*\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)*(\[Chi]t1 - Sqrt[1 - 4*\[Nu]]*\[Chi]t1 + (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
      (469*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/24 + (13223*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/216 - 
      (35*m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 + (7*m*\[Nu]*(2*(261 + 342*Sqrt[1 - 4*\[Nu]] - 931*\[Nu] - 274*Sqrt[1 - 4*\[Nu]]*\[Nu] + 59*\[Nu]^2)*\[Chi]t1 - 
         2*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 + 
      (m*\[Nu]^2*\[Chi]t2*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4/
          (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))))/((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
        Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + 
      (m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
       (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
      (m*\[Nu]^2*\[Chi]t2*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (7*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/3))/
       (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
      (m*\[Nu]*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*
        (-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
       (2*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
      (m*\[Nu]*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
             x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
          (2*Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])))/
       Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
      (\[Nu]^2*(3*m*(-2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
            (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
         (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
            Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
           (2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
              (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
              (2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            2*\[Delta]m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], 
                 \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            2*m*(2*(4 - 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
              (-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + (2*(-1 + \[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - (2*\[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                 (9/2))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (-1 + \[Delta]\[Nu])*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + (-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (-4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 6*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
              (2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
              2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
           ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
         (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
              2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*
               x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
         (9*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
              2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*
               x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(5/2)) - 
         (3*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                  \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
             (2*Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))*
           (2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
              3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
              \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
              \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))^(2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
         (m*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 9*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, 
                 \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] - 
            (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
               (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
             Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
            (x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
                 x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
               x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                   \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*(4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                 (3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
             ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2) + 
            (-6*(-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + (2*\[Delta]\[Chi] + \[Chi]t1)*
                 x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (2*\[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/
                 (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*(-4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                6*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/
                 (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - ((x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
               x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
              (-6*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 2*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*
                  x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                  (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
                 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
                 \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, 
                 \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))^2))/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + 
         (5*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], 
                  \[Delta]\[Chi]]^(3/2))*(4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                   (3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - 
                \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + (-6*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
                2*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
                \[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
              9*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(8/3)) + 
         (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*(3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
            (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
            (2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/(3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
           Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
         (m*drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
           (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], 
            Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (3*m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]*
           ((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
             (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
            3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
            \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
           Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
         3*m*(-1 - \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
          ((((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + (drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
             Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) - 
           Derivative[1, 0][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]]/(1 - \[Chi]t1)) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
           (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
           ((((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/
                (3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + (drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                 x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 2][\[CapitalDelta]Uint][Log[
                1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
                 (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                 x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) - 
            Derivative[1, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]]/(1 - \[Chi]t1)))/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/6))/
    (m^2*(1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - 4*\[Nu]])))/(2*((-3*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/4 - (m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/12 + 
   (m*\[Nu]*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/12 + (10*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/3 - 
   (5*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/6 + (5*m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 + 
   (5*m*\[Nu]*(-((7 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-7 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/12 - 
   (81*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 + (57*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 + 
   (m*\[Nu]*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 - (3*m*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/2 - 
   3*m*\[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (3*m*\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/2 + 
   14*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - (133*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/9 + 
   (21*m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/4 - 
   (7*m*\[Nu]*((99 + 45*Sqrt[1 - 4*\[Nu]] - (61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t1 + \[Nu]*(99 - 45*Sqrt[1 - 4*\[Nu]] + (-61 + Sqrt[1 - 4*\[Nu]])*\[Nu])*\[Chi]t2)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/72 - (675*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/32 - 
   (5*m*(-6889 + 246*Pi^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/288 + 
   (5*m*\[Nu]*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2592 - 
   (65*m*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 + (145*m*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 - 
   (10*m*\[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/3 - 
   (5*m*\[Nu]*((-2 - 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t1^2 - 2*\[Nu]*(3 + \[Nu])*\[Chi]t1*\[Chi]t2 + \[Nu]^2*(-2 + 11*Sqrt[1 - 4*\[Nu]] + 7*\[Nu])*\[Chi]t2^2)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 + (243*m*\[Nu]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/4 - 
   (657*m*\[Nu]^2*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/4 - (9*m*\[Nu]^2*\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + 
   (243*m*\[Nu]^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/16 - (9*m*\[Nu]^2*\[Chi]t1^2*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + 
   (9*m*\[Nu]*(\[Chi]t1 + \[Nu]*\[Chi]t2)^2*(\[Chi]t1 - Sqrt[1 - 4*\[Nu]]*\[Chi]t1 + (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/4 - 
   (3*m*\[Nu]*(3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(-\[Chi]t1 + \[Nu]*\[Chi]t2) - ((405 - 1101*\[Nu] + 29*\[Nu]^2)*(-((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]t1) + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Chi]t2))/2)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/16 - (19845*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/256 - 
   (2345*m*\[Nu]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/48 + (66115*m*\[Nu]^2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/432 - 
   (175*m*\[Nu]^2*\[Chi]t1*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/8 + 
   (35*m*\[Nu]*((261 + 342*Sqrt[1 - 4*\[Nu]] - 931*\[Nu] - 274*Sqrt[1 - 4*\[Nu]]*\[Nu] + 59*\[Nu]^2)*\[Chi]t1^2 - 2*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]t1*\[Chi]t2 + 
      \[Nu]^2*(261 - 342*Sqrt[1 - 4*\[Nu]] + (-931 + 274*Sqrt[1 - 4*\[Nu]])*\[Nu] + 59*\[Nu]^2)*\[Chi]t2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/432 - 
   (m*\[Nu]^2*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/9216 - (m*\[Nu]*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
      27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/248832 + 
   (m*\[Nu]^2*\[Chi]t2*(3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
       (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
        (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))/2))/((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
     Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
        3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + 
   (m*\[Nu]^2*\[Chi]t1*\[Chi]t2*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
    (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*Sqrt[(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
       (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
   (m*\[Nu]^2*\[Chi]t2*(\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
       (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
       ((3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
      (7*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
        (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2))/(2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
     ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
        3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
   (m*\[Nu]*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
        (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
        3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
       (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
        (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
    (2*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
        3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
   (m*\[Nu]*(-((\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
      2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
      ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
          (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
          3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)/
       (2*Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])))/
    Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
       3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
   (\[Nu]^2*(3*m*(-3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - (\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
         (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        (15*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))/2)*
       \[CapitalDelta]Uint[Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
      (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
        (2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3 + (15*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - 
           (\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - 
           (\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))/2) + 
         3*\[Delta]m*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         3*m*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
           \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         2*m*(-4 + 3*\[Delta]\[Nu] + 5*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 4*\[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + ((-1 + \[Delta]\[Nu])*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - (\[Chi]t1^2*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (-6*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 9*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - 
           (\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           2*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*m*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        (-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
        (2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
      (9*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
           (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(5/2)) - 
      (3*(-((\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + ((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            ((-3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
              (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
           (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)/
          (2*Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))*
        (2*\[Delta]m*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (4 - 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           \[Chi]t1*(-2*\[Delta]\[Chi] + (-4 + 3*\[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3) + \[Chi]t1*(-\[Delta]\[Chi] + (-1 + \[Delta]\[Nu])*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       ((1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
      (m*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-3*(\[Delta]\[Chi] + 2*\[Chi]t1)*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (27*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/2))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] - (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
           (4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] - (2*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + (x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             ((-3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*
                (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
            (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, 
                 \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*
           (4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2) + 
         (-6*(1 - 5*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 4*\[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
             (\[Delta]\[Chi]*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
             3*\[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           9*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/
                2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*(1 - 10*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             12*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - (\[Chi]t1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - (((3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + 
            (3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 
            3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*(-6*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
              2*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
              \[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            9*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))^2))/
       (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + (5*m*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        ((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (4 - 2*(\[Delta]\[Chi] + 2*\[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
         (-6*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 2*(\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             \[Chi]t1*(2*\[Delta]\[Chi] + \[Chi]t1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + \[Delta]\[Chi]*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
              (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 9*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
             4*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 3*\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
             \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
            \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))))/
       (2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(8/3)) + 
      (3*m*((\[Chi]t1*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-1 - \[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
        Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
       (2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
        (-3 + (15*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - (\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - (\[Chi]t1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + (3*\[Chi]t1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))/2)*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
      (m*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
        (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
        Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]) + (3*m*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
        Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]*
        (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + 
      (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
        ((\[Chi]t1*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
         3*\[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
         \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
        Derivative[0, 2][\[CapitalDelta]Uint][Log[1 - \[Chi]t1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)])))/6));


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio forcing term*)


F\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
  (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
  (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
  (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
    x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
  ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
        12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
     12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
     (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
      (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
     (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
      (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + 
  (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
     12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
        (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
       (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
       (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
         Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 + (Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
    (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
       \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
     (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 
       6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
       (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
       (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
         Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - (2*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
    (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
       \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
     (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 
       6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
       (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
       (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
         Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/15 - 
  (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
     72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
     \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
  (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
     Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
        (2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
  (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
     Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
        (2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 - 
  Sqrt[1 - 4*\[Nu]]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
    (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
       (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
         Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
    (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
    (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
  4*\[Nu]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
    (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
       (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
         Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
    (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
    (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(2*m);


(* ::Subsection::Closed:: *)
(*Total mass forcing term *)


Fm[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
 (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
 (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
 (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
   x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
 ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
       12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
    12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
    (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
     (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
    (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
     (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + 
 (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
    12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
       (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
      (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
      (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
      (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
        Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
 (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
    72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
    \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
 (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
    Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
       (2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
 (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
    Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
       (2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6;


(* ::Subsection::Closed:: *)
(*Primary spin forcing term*)


F\[Chi]t1[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=((2*Sqrt[1 - 4*\[Nu]]*(24*m*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 - 12*m*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 6*m*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 
     6*m*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 6*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(4*(3 + Sqrt[1 - 4*\[Nu]]) + 9*\[Chi]t1^2))*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 180*m*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 
     (m*\[Nu]^2*(3*(1 + Sqrt[1 - 4*\[Nu]])^5*(1 + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
        4*\[Chi]t1^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])) + 4*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]t1^2*
         (-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])) - (96*(1 + Sqrt[1 - 4*\[Nu]])*\[Nu]^3*\[Chi]t1*\[Chi]t2)/(-1 + Sqrt[1 - 4*\[Nu]]) + 
        (144*(3 + 2*Sqrt[1 - 4*\[Nu]])*\[Nu]^2*\[Chi]t1^3*\[Chi]t2)/(-1 + Sqrt[1 - 4*\[Nu]]) + 48*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
         Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/
      (1 + Sqrt[1 - 4*\[Nu]]) - 8*(24*Im[m*\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
       72*Im[m*\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
       m*\[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
     (15*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
     (15*m*\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/15 - 
  m*\[Chi]t1*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
    (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
       (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
         Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
      (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
         \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
       (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 
         6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 + 
    (Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
       12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
          (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
    (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
    (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 + 
    3*Sqrt[1 - 4*\[Nu]]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
      (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
      (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
            12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
         (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
           Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
      (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
      (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
      (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
    4*\[Nu]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
      (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
      (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
            12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
         (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
           Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
        (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
           \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
          ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
            (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 
                12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
      (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
      (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
      (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)))/
 (m^2*(1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - 4*\[Nu]]);


(* ::Subsection::Closed:: *)
(*Secondary spin forcing term*)


F\[Chi]t2[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-1/15*(m*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
     12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) - 8*(-3 + Sqrt[1 - 4*\[Nu]])*\[Nu] + 18*\[Nu]^3*\[Chi]t2^2 + 3*\[Nu]^2*(-2 + (-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-8 + 3*(1 + Sqrt[1 - 4*\[Nu]] + 8*\[Nu])*\[Chi]t2^2))/
        (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
       (16*\[Nu]^4*\[Chi]t2^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
       (8*\[Nu]^2*\[Chi]t2^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*
         Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))) + 
  m*\[Chi]t2*((-1 + Sqrt[1 - 4*\[Nu]])*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
      (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
      (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
            12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
         (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
           Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
      (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
      (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
      (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
    4*\[Nu]^2*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
      (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
      (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
            12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - 
         (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
           Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
        (-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + 
           \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*
          ((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*
            (1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 
                12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 - 
      (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
      (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
      (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) + 
    \[Nu]*((\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
         12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
            (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
           (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/30 + 
      (Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-24*\[Nu]*\[Chi]t2*(1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Nu]^2*\[Chi]t2^2) + 
         12*\[Nu]*\[Chi]t2*(8*(-1 + Sqrt[1 - 4*\[Nu]]) + (26 - 10*Sqrt[1 - 4*\[Nu]])*\[Nu] + 30*\[Nu]^3*\[Chi]t2^2 + \[Nu]^2*(-10 + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t2^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (-1 + Sqrt[1 - 4*\[Nu]])^3*((-3*\[Nu]^2*\[Chi]t1*\[Chi]t2*(-4 + 3*(3 + 3*Sqrt[1 - 4*\[Nu]] + 4*\[Nu])*\[Chi]t2^2))/
            (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu]) + 6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
           (16*\[Nu]^4*\[Chi]t2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (8*\[Nu]^2*\[Chi]t2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (96*\[Nu]*\[Chi]t2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] - 6*\[Nu]^2*\[Chi]t2^2)*Im[PolyGamma[0, 3 - ((4*I)*\[Nu]*\[Chi]t2)/((-1 + Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (4*\[Nu]^2*\[Chi]t2^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(-1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/10 + 
      5*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
            (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
           (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
            (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
        (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
        (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
      3*Sqrt[1 - 4*\[Nu]]*((8*\[Nu]^2*\[Chi]t1*(1 + 3*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - 
        (4*\[Nu]^2*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*\[Nu]^2*\[Chi]t1*(16 + 33*\[Chi]t1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        (2*\[Nu]^2*\[Chi]t1*(8*(1 + Sqrt[1 - 4*\[Nu]]) + 10*\[Nu]^2 + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]t1^2 - 2*\[Nu]*(13 + 5*Sqrt[1 - 4*\[Nu]] + 15*\[Chi]t1^2))*
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + 12*\[Nu]^3*\[Chi]t1^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((96*\[Chi]t1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (16*\[Chi]t1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2] + 39*Sqrt[(-1 + 4*\[Nu])*(-1 + (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/
            (1 + Sqrt[1 - 4*\[Nu]])^2 + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]t1*\[Chi]t2 - (18*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]t1^3*\[Chi]t2)/(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu]) - 
           (192*\[Chi]t1*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu] + 6*\[Chi]t1^2)*Im[PolyGamma[0, 3 + ((4*I)*\[Chi]t1)/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (4*\[Chi]t1^2)/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/
            (1 + Sqrt[1 - 4*\[Nu]])^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - 
        (8*(24*Im[\[Nu]^2*\[Chi]t1*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[\[Nu]^2*\[Chi]t1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]t1)/Sqrt[1 - \[Chi]t1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^4*(-3 + 36*Sqrt[1 - \[Chi]t1^2]) + \[Chi]t1^2*(70 + 78*Sqrt[1 - \[Chi]t1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
        (\[Nu]^3*\[Chi]t2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]t1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]t1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]t1, \[Chi]t2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))))/
 (m^2*(-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - 4*\[Nu]]*\[Nu]);


(* ::Subsection::Closed:: *)
(*WF phase evolution*)


F\[Phi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=\[Omega];


(* ::Subsection::Closed:: *)
(*Total mass shift*)


F\[Delta]m[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(Fm[\[Omega],\[Phi],\[Nu],m,\[Chi]t1,\[Chi]t2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]-\[Delta]m F\[Nu][\[Omega],\[Phi],\[Nu],m,\[Chi]t1,\[Chi]t2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu];


d\[Delta]mdm[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu];


d\[Delta]md\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]m/\[Nu])


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio shift*)


F\[Delta]\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(((-1+2 \[Delta]\[Nu] \[Nu]) F\[Nu][\[Omega],\[Phi],\[Nu],m,\[Chi]t1,\[Chi]t2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu]^2);


d\[Delta]\[Delta]\[Nu]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(1-2 \[Delta]\[Nu] \[Nu])/\[Nu]^2


(* ::Subsection::Closed:: *)
(*Primary spin shift*)


F\[Delta]\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-\[Delta]\[Chi] F\[Nu][\[Omega],\[Phi],\[Nu],m,\[Chi]t1,\[Chi]t2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]+F\[Chi]t1[\[Omega],\[Phi],\[Nu],m,\[Chi]t1,\[Chi]t2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu];


d\[Delta]\[Chi]d\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu]


d\[Delta]\[Chi]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]t1_,\[Chi]t2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]\[Chi]/\[Nu])


(* ::Section::Closed:: *)
(*Construct Interpolations (To Do: Update Stop Condition)*)


(* ::Subsection::Closed:: *)
(*Fetch data*)


directory1SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Kerr_Circ"}];
directory1SFSch = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Flux/Schwarz_Circ"}];
directory2SF = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/2SF_Flux/Schwarz_Circ"}];
directory1SFLocalInvar = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/1SF_Local_Invariants/Kerr_Circ"}];
directorySecSpin = FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/Leading_S2_Flux/Kerr_Circ"}];
directoryamp = FileNameJoin[{WaSABI`Waveform`Private`$WaSABIAmplitudeDirectory, "sf_amp_data/1SF/Kerr_Circ"}];
directorystop=FileNameJoin[{WaSABI`Inspiral`Private`$WaSABIInspiralDirectory, "sf_data/stop_conditions"}];

fluxdata2SF=Get[FileNameJoin[{directory2SF, "2SFCircShwarzDotEInf.m"}]];
fluxdata1SF=Get[FileNameJoin[{directory1SF,"SMRfluxdata2025_36x36.data"}]];
fluxdata1SFSchwInf=Get[FileNameJoin[{directory1SFSch,"1SFCircShwarzDotEInf.m"}]];
fluxdata1SFSchwHor=Get[FileNameJoin[{directory1SFSch,"1SFCircShwarzDotEHorizon.m"}]];
spinfluxdata=Get[FileNameJoin[{directorySecSpin,"SMRfluxdataWithSecondarySpin2025_36x36.m"}]];
invardata=Get[FileNameJoin[{directory1SFLocalInvar,"KerrCircularEquatorialInvariants2025b_36x36.data"}]];
\[Omega]critdata=Get[FileNameJoin[{directorystop,"StopHybridFixedChi.m"}]];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalI]*)


\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw = Interpolation[fluxdata1SFSchwInf];
\[ScriptCapitalF]\[ScriptCapitalI]int2SFSchw = Function[{logr0},Re[Exp[Interpolation[fluxdata2SF,InterpolationOrder->8][logr0]]]];

\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^5 fluxdata1SF[[i]]["inf"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];
\[ScriptCapitalF]\[ScriptCapitalI]intS2Kerr = Interpolation[Table[{{Log[1-spinfluxdata[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/spinfluxdata[[i]]["r"]]},N[spinfluxdata[[i]]["r"]^5 spinfluxdata[[i]]["chi2inf"]]},{i,1,Length[spinfluxdata]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalH]*)


\[ScriptCapitalF]\[ScriptCapitalH]int1SFSchw = Interpolation[fluxdata1SFSchwHor,InterpolationOrder->8];

\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^(15/2) fluxdata1SF[[i]]["hor"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];
\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr = Interpolation[Table[{{Log[1-spinfluxdata[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/spinfluxdata[[i]]["r"]]},N[spinfluxdata[[i]]["r"]^(9) spinfluxdata[[i]]["chi2hor"]]},{i,1,Length[spinfluxdata]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*Eb*)


\[CapitalDelta]Uint = Interpolation[Table[{{Log[1-invardata[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,invardata[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/invardata[[i]]["r"]]},N[invardata[[i]]["\[CapitalDelta]U"]["Value"]]},{i,1,Length[invardata]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*\[Delta]x*)


Re1SFAmp22intV2=Interpolation[Get[FileNameJoin[{directoryamp, "ReAmp1SFKerr22NewGrid.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];
Im1SFAmp22intV2=Interpolation[Get[FileNameJoin[{directoryamp, "ImAmp1SFKerr22NewGrid.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*Stop Condition (UPDATE ME)*)


\[Omega]crit=Interpolation[\[Omega]critdata,InterpolationOrder->All];


(* ::Section::Closed:: *)
(*Evolution equations (To Do: Update Stop Condition, check param space coverage)*)


variables={\[Omega],\[Phi],\[Nu],m,\[Chi]t1,\[Chi]t2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]};
evolutionequations={
\[Omega]'[t]==F\[Omega][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Phi]'[t]==F\[Phi][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Nu]'[t]==F\[Nu][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
m'[t]==Fm[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]], 
\[Chi]t1'[t]==F\[Chi]t1[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Chi]t2'[t]==F\[Chi]t2[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]m'[t]==F\[Delta]m[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]\[Nu]'[t]==F\[Delta]\[Nu][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]\[Chi]'[t]==F\[Delta]\[Chi][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]t1[t],\[Chi]t2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]]};
InitialConditionFormat={"\[Omega]","\[Phi]", "\[Nu]", "m","\[Chi]t1","\[Chi]t2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"};

stopcondition = {\[Omega][t] >= Min[1/((1.05rISCO["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]t1","\[Chi]t2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"])^(3/2)+"\[Chi]t1"),1/(6.26^(3/2)+"\[Chi]t1")],
\[Omega][t] >= \[Omega]crit["\[Nu]","\[Chi]t1","\[Chi]t2"]};

parameterspacecoverage = {\[Sqrt]((rISCO["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]t1","\[Chi]t2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"] x["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]t1","\[Chi]t2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"])/(1-"\[Chi]t1" x["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]t1","\[Chi]t2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"]^(3/2))^(2/3))<.998,
1/(30^(3/2)+"\[Chi]t1")<"\[Omega]"<Min[1/(6.06^(3/2)+"\[Chi]t1"),
"\[Omega]">\[Omega]crit["\[Nu]","\[Chi]t1","\[Chi]t2"]],
"\[Nu]"<.248,
Abs["\[Chi]t1"]>.000001};


<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat,"StopCondition" -> stopcondition, "ParameterSpaceCoverage"->parameterspacecoverage|>
