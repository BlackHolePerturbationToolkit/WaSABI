(* ::Package:: *)

(* ::Section::Closed:: *)
(*Dependent variables*)


(* ::Subsection::Closed:: *)
(*ISCO radius*)


rISCO[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=3+Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]-Sign[s] \[Sqrt]((2-(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))) (4+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))+2 Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]));
drISCOd\[Chi]1[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/2 ((6 s+(2 ((1-s^2) (-(1/(1-s)^(2/3))+1/(1+s)^(2/3))-2 s ((1-s)^(1/3)+(1+s)^(1/3))) (1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))))/(3 (1-s^2)^(2/3)))/Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]-(((2-(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))) (-((2 s ((1-s)^(1/3)+(1+s)^(1/3)))/(3 (1-s^2)^(2/3)))+((1-s)^(2/3)-(1+s)^(2/3))/(3 (1-s^2)^(1/3))+(6 s+1/(3 (1-s^2)^(2/3)) 2 ((1-s^2) (-(1/(1-s)^(2/3))+1/(1+s)^(2/3))-2 s ((1-s)^(1/3)+(1+s)^(1/3))) (1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))))/(Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]))+1/(3 (1-s^2)^(2/3)) (-((1-s^2) (-(1/(1-s)^(2/3))+1/(1+s)^(2/3)))+2 s ((1-s)^(1/3)+(1+s)^(1/3))) (4+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))+2 Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2])) Sign[s])/(\[Sqrt]((2-(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))) (4+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))+2 Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]))))


(* ::Subsection::Closed:: *)
(*PN Parameter*)


x[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=\[Omega]^(2/3);


(* ::Subsection::Closed:: *)
(*WF frequency correction*)


(* ::Input::Initialization:: *)
\[Delta]x[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=With[{\[Chi]1=s,\[Chi]2=\[Sigma]b},(0.3333333333333333*\[Omega]^2*(-0.3333333333333333-0.3333333333333333*\[Chi]1*\[Omega]+1.*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega])*Sqrt[(-1+\[Chi]1*\[Omega])*(-1-\[Chi]1*\[Omega]+3*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]*((-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]+(1-2.*\[Chi]1*\[Omega]+\[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]])*(1.*Re1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]*Derivative[0,1][Im1SFAmp22intV2][0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]-1.*Im1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]*Derivative[0,1][Re1SFAmp22intV2][0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]))/((-1.+\[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1+\[Omega]^(-1))^(1/3)+0.5*\[Chi]1^2*\[Omega]-1.1666666666666667*\[Chi]1*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega]+1.*(-\[Chi]1+\[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]^2+Re1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]^2)*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)])];


d\[Delta]xd\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=With[{\[Chi]1=s,\[Chi]2=\[Sigma]b},(-0.3333333333333333*\[Omega]^2*(0.05555555555555555/(-\[Chi]1 + \[Omega]^(-1))^(2/3) + 1.*\[Chi]1*\[Omega] + (0.3888888888888889*\[Chi]1*\[Omega])/(-\[Chi]1 + \[Omega]^(-1))^(2/3) - 
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


d\[Delta]xdx[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=With[{\[Chi]1=s,\[Chi]2=\[Sigma]b},(0.5*\[Omega]^(4/3)*(0.3333333333333333*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]*(1 - \[Chi]1*\[Omega])*(-1. + \[Chi]1*\[Omega])^2*
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


F\[Omega][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-3*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*((2494*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6)/105 + 
   (56*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6)/3 - (2*\[Nu]^2*(1247 + 980*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6)/105 + 
   (88*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + 8*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2) - 
   (8*\[Nu]^2*(16*s - 5*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + 
   (89422*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/2835 - (66*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 - 
   (37084*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/315 + (2*\[Nu]^2*(-44711 + 166878*\[Nu] + 32760*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/2835 - 
   (124*s*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 - (2*\[Nu]^2*(-128*s^2 + 128*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + (-33 + 128*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 + (8191*Pi*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/105 + 
   (118*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
   (2332*Pi*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/15 - (Pi*\[Nu]^2*(8191 + 16324*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/105 + 
   (26*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (2*\[Nu]^2*(8*s*(-81 + 544*\[Nu]) - 9*Sqrt[1 - 4*\[Nu]]*(-13 + 172*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
     (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
   (208*Pi*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/3 - (2444*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 - 
   ((-134543 + 6642*Pi^2)*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/1215 + (496*Pi*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 + 
   (1868*s*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/35 - (16*Pi*\[Nu]^2*(96*s - 31*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 + (4*\[Nu]^2*(-2*s^2*(3839 + 10836*\[Nu]) + 9*s*Sqrt[1 - 4*\[Nu]]*(1375 + 2408*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
      3*(-1362 + 3481*\[Nu] + 7224*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 + 
   (\[Nu]^2*(-6643739519 + 1138959360*EulerGamma - 372556800*Pi^2 + 2277918720*Log[2] + 569479680*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/10914750 - (\[Nu]^2*(-19931218557 + 3416878080*EulerGamma - 1117670400*Pi^2 + 3625933850*\[Nu] - 179001900*Pi^2*\[Nu] + 
      6542127900*\[Nu]^2 + 501270000*\[Nu]^3 + 6833756160*Log[2] + 1708439040*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/
    32744250 + (13028*Pi*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/63 - (64814*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/243 - 
   52*Pi*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + (284*s^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 
   (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - (42949*Pi*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/54 + 
   (Pi*\[Nu]^2*(-78168 + 300643*\[Nu] + 154708*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/378 - (3814*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
    21 - (504*Pi*s*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 + (76*s^2*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/3 - 
   (2*\[Nu]^2*(4*s*(-476645 - 222192*\[Nu] + 708120*\[Nu]^2) - 27*Sqrt[1 - 4*\[Nu]]*(-28605 - 14792*\[Nu] + 42028*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/8505 - (4*Pi*\[Nu]^2*(-256*s^2 + 256*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + (-65 + 256*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - (4*\[Nu]^2*(128*s^3 + 16*s^2*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 4*s*(-27 + 112*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2 + 
      Sqrt[1 - 4*\[Nu]]*(-35 + 144*\[Nu])*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^3)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - 
   ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
     (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
      10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
    20 + (5744*Pi*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/35 - (90668*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 + 
   (7163*Pi*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/105 + (497606*s*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 + 
   12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
   (\[Nu]^2*(-32*s^2*(-19325 + 21420*\[Nu] + 61236*\[Nu]^2) + 2*s*Sqrt[1 - 4*\[Nu]]*(-460265 + 1100610*\[Nu] + 979776*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
      (211462 - 1906777*\[Nu] + 2868138*\[Nu]^2 + 1959552*\[Nu]^3)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 + 
   (Pi*\[Nu]^2*(7*s*(-10455 + 55516*\[Nu]) + Sqrt[1 - 4*\[Nu]]*(-21489 + 130583*\[Nu])*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/315 - 
   ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
        (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
      (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
         Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
      (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
         12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
         12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
      12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
        Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
      (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
         39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
      (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
         3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
        Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
             Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (\[Nu]^2*(-323105549467 + 167637309840*EulerGamma - 34533298800*Pi^2 + 431686054800*Log[2] - 96050579625*Log[3] + 
      83818654920*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/496621125 - 
   (\[Nu]^3*(-11617619229032 + 1986802836480*EulerGamma - 680312365425*Pi^2 + 2549682938880*Log[2] + 1418593176000*Log[3] + 
      993401418240*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/1833678000 + 
   (\[Nu]^2*(-15509066374416 - 1657598342400*Pi^2 - 151029049977416*\[Nu] - 8844060750525*Pi^2*\[Nu] + 36035599600000*\[Nu]^2 - 1254266313300*Pi^2*\[Nu]^2 + 2081079000000*\[Nu]^3 + 
      127135008000*\[Nu]^4 + 34594560*EulerGamma*(232597 + 746604*\[Nu]) + 20720930630400*Log[2] + 33145878205440*\[Nu]*Log[2] - 4610427822000*Log[3] + 18441711288000*\[Nu]*Log[3] + 
      17297280*(232597 + 746604*\[Nu])*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/23837814000 - 
   (Pi*(2062241 + 75768*Pi^2)*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/3465 + 
   (Pi*\[Nu]^2*(797936002557 - 145786798080*EulerGamma + 207873892800*\[Nu] + 7637414400*Pi^2*\[Nu] - 1024969368500*\[Nu]^2 - 218685490800*\[Nu]^3 - 291573596160*Log[2] - 
      72893399040*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/349272000 + 
   (Pi*\[Nu]^2*(-265978667519 + 48595599360*EulerGamma + 97191198720*Log[2] + 24297799680*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/116424000 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
     ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
      (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
         (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
       (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
           (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
        2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
          ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
              (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
          (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
              (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
        (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
          (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
          Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
   (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
    (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
     \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
        (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 + 
   ((m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - m*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
      m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - (m*\[Nu]*(-4*s*Sqrt[1 - 4*\[Nu]]*\[Nu] - 2*\[Nu]*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + (3*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + 
      (m*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*(-9 + 10*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/6 - (5*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + 
      (5*m*\[Nu]*(-12*s*Sqrt[1 - 4*\[Nu]]*\[Nu]*(1 + \[Nu]) - 6*\[Nu]*(-3 + 9*\[Nu] + 4*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + 
      (27*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - m*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
      (m*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*(27 - 156*\[Nu] + 5*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - 
      m*\[Nu]*(-8*s^2*Sqrt[1 - 4*\[Nu]]*\[Nu] - 10*s*\[Nu]*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) - 3*(1 - 4*\[Nu])^(3/2)*\[Nu]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^2)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - (35*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 - 
      (7*m*\[Nu]*(-12*s*Sqrt[1 - 4*\[Nu]]*\[Nu]*(9 - 181*\[Nu] + \[Nu]^2) - 6*\[Nu]*(27 + 27*\[Nu] - 561*\[Nu]^2 + 4*\[Nu]^3)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 + 
      (m*\[Nu]^2*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (1/3)))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3))]))*((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] + 2*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
        ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
         (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
            (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
                 (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
           2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
             ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
             (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
           (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
             Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(2*m*s*(-1 + Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Nu] + 
         2*m*\[Nu]*(-1 - Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Sigma]b)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
        ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
         (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
            (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
              (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
           2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
             ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
             (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
           (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
             Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + (2*m*s*(1 - Sqrt[1 - 4*\[Nu]] - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu] - 4*\[Nu]^2) + 2*m*\[Nu]*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Sigma]b)*
       ((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
          (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
           (17/2))/20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
             (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
           (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
           (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
              12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
             Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
              39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
              39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/
                 ((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
         (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) + 
      ((-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu] + 2*\[Nu]^2)*(96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
         12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
         24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*
                (-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
              (1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 
         m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
             (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
           (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
            (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
           ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*(-1 + 
                12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
           (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*(35 + 
                39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
           (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/
                 ((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/
                2)] + 72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
           m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
         (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                 (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
         (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                 (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/60))/(2*m^2*\[Nu]^2) + 
   (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - s], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
    (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(10/3) + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5*
     \[ScriptCapitalF]\[ScriptCapitalI]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
        (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(10/3) + 
   \[Nu]^3*(-2*\[Delta]\[Nu]*\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(-1)] + \[ScriptCapitalF]\[ScriptCapitalI]int2SFSchw[-Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]] - 
     (\[Delta]x[\[Omega], \[Phi], \[Nu], m, 0, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Derivative[1][\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw][x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(-1)])/
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + ((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
     (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
        3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
     (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
     ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
        (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
        10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
      20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
     ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
          (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
        (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
        (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
           12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
           12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
        12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
        (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
           39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
        (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
           3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
          Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
       ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
        (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
             Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
             (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
          2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
            ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
            (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
            (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
            Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                 Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
         x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
        72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
        (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
     (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
      (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
       \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)*
    ((-3*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/8 - (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + 
     (\[Nu]*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + (4*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + 
     s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
     (\[Nu]*(7*s - 3*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - (27*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 - 
     (s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + (19*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 - 
     s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + (\[Nu]*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 - 
     s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - (\[Nu]*(-4*s^2 + 4*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + (-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 4*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (2*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/
      9 + (3*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + (\[Nu]*(s*(-99 + 61*\[Nu]) - 3*Sqrt[1 - 4*\[Nu]]*(-9 + 10*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/18 - (675*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/128 - 
     (65*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 - (5*(-6889 + 246*Pi^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/1152 + 
     (5*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/12 + (5*\[Nu]*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 - (5*s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + 
     (5*\[Nu]*(-2*s^2*(-5 + 6*\[Nu]) + 12*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 3*(-3 + 9*\[Nu] + 4*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (27*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
     23*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - s^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
     (27*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - s^2*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
     (\[Nu]*(s*(405 - 1101*\[Nu] + 29*\[Nu]^2) - 3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - 
     \[Nu]*(-4*s^3 + 8*s^2*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 5*s*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2 - (1 - 4*\[Nu])^(3/2)*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^3)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - (3969*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/256 - 
     (469*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/48 + (4781*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - 
     (35*s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 - (7*\[Nu]*(-4*s^2*(198 - 680*\[Nu] + 3*\[Nu]^2) + 12*s*Sqrt[1 - 4*\[Nu]]*(9 - 181*\[Nu] + \[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
        3*(27 + 27*\[Nu] - 561*\[Nu]^2 + 4*\[Nu]^3)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - 
     (\[Nu]^2*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/46080 - (\[Nu]*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
        27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 + (\[Nu]^2*\[Sigma]b*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
         (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
       Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
          3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
     (\[Nu]*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
        (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (1/3))] + (\[Nu]^2*(3*(-1 - s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - s], 
          Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] + 
        (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         (2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
           (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))]) - (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - 
             (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3)))^(3/2)) - (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
            (2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*
                x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))) + 
        (Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^
           (3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
         rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/6) + (((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
         3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
      (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
         (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
         10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
       20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
      ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
           (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
         (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
            Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
         (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
            12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
            12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
         12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
           Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
         (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
            39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
         (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
           Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
                Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
        ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
         (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
            (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
              (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
           2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
             ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
             (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
           (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
             Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
        ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
         (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
            (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
              (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
           2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
             ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
             (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
           (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
             Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
      (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
       (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
        \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 - 
      Sqrt[1 - 4*\[Nu]]*((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
           (17/2))/20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
             (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
           (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
           (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
              12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
             Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
              39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
              39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/
                 ((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
         (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
      4*\[Nu]*((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
          (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
           (17/2))/20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
             (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
           (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
           (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
              12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
             Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
              39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
              39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/
                 ((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
          ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (-11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
              10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
                Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
                (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
             2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
               ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                   (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
               (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                   (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
             (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(1 + 
                (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*Im[PolyGamma[0, 
                 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
                         Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/
         60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
         (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))*
     ((-3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/8 - (m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + 
      (m*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + (4*m*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + 
      2*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 2*m*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
      (m*\[Nu]*(-3*Sqrt[1 - 4*\[Nu]]*(s - \[Sigma]b) + (6*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
      (m*(7*s - 3*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - (27*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 - 
      (m*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + (19*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/8 - 
      2*m*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + (m*\[Nu]*(-57 + 2*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 + 
      (m*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 - 2*m*s*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
      (m*\[Nu]*(4*s*Sqrt[1 - 4*\[Nu]]*(s - \[Sigma]b) - (8*s*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]] + 2*(-1 + 4*\[Nu])*(s - \[Sigma]b)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 4*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 - (m*(-4*s^2 + 4*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + (-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 4*m*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (4*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/
       9 + 3*m*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) + (m*\[Nu]*(61*s - 3*Sqrt[1 - 4*\[Nu]]*(-9 + 10*\[Nu])*(s - \[Sigma]b) - 30*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
         (6*(-9 + 10*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/18 + 
      (m*(s*(-99 + 61*\[Nu]) - 3*Sqrt[1 - 4*\[Nu]]*(-9 + 10*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/18 - 
      (675*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/128 - (65*m*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 - 
      (5*m*(-6889 + 246*Pi^2)*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/576 + (5*m*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + 
      (5*m*\[Nu]*(9*(-6889 + 246*Pi^2) + 3348*\[Nu] + 21*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 + 
      (5*m*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 - 
      (5*m*s*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/3 + (5*m*\[Nu]*(-12*s^2 + 12*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(s - \[Sigma]b) + 12*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) - 
         (24*s*(1 + \[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]] + 6*(-3 + 9*\[Nu] + 4*\[Nu]^2)*(s - \[Sigma]b)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 3*(9 + 8*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (5*m*(-2*s^2*(-5 + 6*\[Nu]) + 12*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
         3*(-3 + 9*\[Nu] + 4*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (27*m*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
      46*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 2*m*s^3*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
      (27*m*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/4 - 2*m*s^2*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
      (m*\[Nu]*(s*(-1101 + 58*\[Nu]) - 3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(s - \[Sigma]b) - 3*Sqrt[1 - 4*\[Nu]]*(-156 + 10*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
         (6*(27 - 156*\[Nu] + 5*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - 
      (m*(s*(405 - 1101*\[Nu] + 29*\[Nu]^2) - 3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - 
      m*\[Nu]*(8*s^2*Sqrt[1 - 4*\[Nu]]*(s - \[Sigma]b) - (16*s^2*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]] + 10*s*(-1 + 4*\[Nu])*(s - \[Sigma]b)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 20*s*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2 - 
        3*(1 - 4*\[Nu])^(3/2)*(-s + \[Sigma]b)*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^2 + 6*Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^3)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
      m*(-4*s^3 + 8*s^2*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 5*s*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2 - (1 - 4*\[Nu])^(3/2)*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^3)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - (3969*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/256 - (469*m*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/
       48 + (4781*m*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/216 - (35*m*s*\[Nu]*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/4 - 
      (7*m*\[Nu]*(-4*s^2*(-680 + 6*\[Nu]) + 12*s*Sqrt[1 - 4*\[Nu]]*(9 - 181*\[Nu] + \[Nu]^2)*(s - \[Sigma]b) + 12*s*Sqrt[1 - 4*\[Nu]]*(-181 + 2*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) - 
         (24*s*(9 - 181*\[Nu] + \[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))/Sqrt[1 - 4*\[Nu]] + 6*(27 + 27*\[Nu] - 561*\[Nu]^2 + 4*\[Nu]^3)*(s - \[Sigma]b)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
         3*(27 - 1122*\[Nu] + 12*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - 
      (7*m*(-4*s^2*(198 - 680*\[Nu] + 3*\[Nu]^2) + 12*s*Sqrt[1 - 4*\[Nu]]*(9 - 181*\[Nu] + \[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 3*(27 + 27*\[Nu] - 561*\[Nu]^2 + 4*\[Nu]^3)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - (m*\[Nu]*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 
         688128*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/23040 - 
      (m*\[Nu]*(2520*(-71207 + 2706*Pi^2)*\[Nu] + 325080*\[Nu]^2 + 6160*\[Nu]^3 + 27*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 
         18579456*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 - 
      (m*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 
         18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 + 
      (2*m*\[Nu]*\[Sigma]b*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (1/3)))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3))]) - (m*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
       Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
          3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
      (\[Nu]*(3*m*(-1 - s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3) + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
          \[CapitalDelta]Uint[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
         (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
            Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                 \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(
                -3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                    (3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
              m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
                s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
            3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(
                3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3)))^(3/2)) + m*((3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
            (2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
              (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
            ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                  (1/3))]) - (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2*s - 2*\[Delta]\[Chi] + 
                3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                     \[Delta]\[Chi]]^(3/2)) + (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
              3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))) + 
         (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^
            (3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
            Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
          rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/3))/(2*m) + 
   (((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
           ((-1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
         (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
            (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
            6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
                 (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
           2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
             ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                 (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
             (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                 (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
           (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
             Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                  Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 - 2*m*s*((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
        (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
           (17/2))/20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
             (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
           (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
              Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
           (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
              12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
              12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
           12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
             Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
           (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
              39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
              39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
           (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/
                 ((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
          x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
          ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
                 Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
             (-11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
              10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
                Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
                (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
             2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
               ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                   (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
               (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                   (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
             (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(1 + 
                (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*Im[PolyGamma[0, 
                 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
                         Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/
         60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
           (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
        (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
         (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) + 
      (96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
         (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
        24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^
             2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*
         x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 
        m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
            (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
          (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
             Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
           (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
          ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*
              (-1 + 12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
          (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*
              (35 + 39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
          (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
                Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/
                ((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
         x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
              (15/2)] + 72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
          m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
        (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
        (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60)*
     ((4*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
      (m*\[Nu]*(7 - 3*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu]))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
      2*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
      (m*\[Nu]*(-8*s + 4*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu]) + 4*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 2*(1 + \[Nu])*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 4*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - 
      (2*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 + (m*\[Nu]*(-99 + 61*\[Nu] - 3*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(-9 + 10*\[Nu]))*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/18 - (65*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 + 
      (5*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 - (5*m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + 
      (5*m*\[Nu]*(12*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])^2 - 4*s*(-5 + 6*\[Nu]) + 12*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 6*(1 + \[Nu])*(-3 + 9*\[Nu] + 4*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (27*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
      23*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 3*m*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
      (m*\[Nu]*(405 - 1101*\[Nu] + 29*\[Nu]^2 - 3*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(27 - 156*\[Nu] + 5*\[Nu]^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/24 - 
      2*m*s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - m*\[Nu]*(-12*s^2 + 8*s^2*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu]) + 16*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
        10*s*(1 + \[Nu])*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 5*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2 - 3*(1 - 4*\[Nu])^(3/2)*(-1 - \[Nu])*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^2)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - (469*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/24 + 
      (4781*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/216 - (35*m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 - 
      (7*m*\[Nu]*(12*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(9 - 181*\[Nu] + \[Nu]^2) - 8*s*(198 - 680*\[Nu] + 3*\[Nu]^2) + 12*Sqrt[1 - 4*\[Nu]]*(9 - 181*\[Nu] + \[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 
         6*(1 + \[Nu])*(27 + 27*\[Nu] - 561*\[Nu]^2 + 4*\[Nu]^3)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 + 
      (m*\[Nu]^2*\[Sigma]b*(x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4/
          (3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
        Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + 
      (m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
        Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
      (m*\[Nu]^2*\[Sigma]b*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (1/3))*((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 
         (7*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
           (1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (1/3)))/3))/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
          (1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))^(3/2)) + (m*\[Nu]*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
       (2*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
      (m*\[Nu]*((-2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         ((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3)))/(2*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])))/
       Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
          3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
      (\[Nu]^2*(3*m*(-2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
            (3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
         (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
            Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                 \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(3*m*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(
                3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
            2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
                (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
                (2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
                x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
              \[Delta]m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
                 (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + m*((6 - 6*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                s*(-2 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + (-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
                (2*(-1 + \[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
                (2*s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
                s*\[Delta]\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + (s*\[Delta]\[Nu] - \[Delta]\[Chi])*
                 x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
            3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-4*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - (2*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/(3*
                (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                  \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3)))^(3/2)) - (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                 (1/3))])*(2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
                3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
              m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
                s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
            3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(
                3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3)))^(3/2)) + (9*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
              x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                 (1/3))])*(2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
                3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
              m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
                s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
                s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
            3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(
                3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3)))^(5/2)) - (3*((-2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
            ((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
                 (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                  (1/3)))/(2*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))*
           (2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
                 (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
                (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - 
                (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*
                 (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
                (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
          ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3)))^(3/2)) + m*((3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(2 + 3*s*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
              3*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
           (6*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
            (2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^2 + (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (-2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (3/2)) + 9*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
            ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                  (1/3))]) + (4*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                  (1/3))]) - (x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(
                -x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                  (2/3)) + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
                3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*
             (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
              (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
            ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - 
                s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                  (1/3)))^(3/2)) - (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2 + 
                3*s*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
              (-2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                    (3/2)) + 9*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
                (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
                 (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(3*
                (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3))))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
              3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))) + 
           (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2*s - 2*\[Delta]\[Chi] + 
                3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                     \[Delta]\[Chi]]^(3/2)) + (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))^2)) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
           (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
             (3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - (2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
             (3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
            Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
          rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - (m*drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
           (-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
           Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + 
         (3*m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]*((2*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
             (3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
            3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
            s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
            Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
          (2*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 3*m*(-1 - s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*((((2*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                 (5/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + (drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
              Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                 (2/3)]])/(2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) - Derivative[1, 0][\[CapitalDelta]Uint][Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]]/(1 - s)) + 
         (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^
            (3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*((((2*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (5/2))/(3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + (drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                 x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 2][\[CapitalDelta]Uint][Log[1 - s], Sqrt[
                (rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
             (2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                 (2/3)]) - Derivative[1, 1][\[CapitalDelta]Uint][Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
                (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]]/(1 - s)))/rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/6))/m^2))/
 (2*((-3*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/4 - (m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/12 + 
   (m*\[Nu]*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/12 + (10*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/3 + 
   (5*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 + (5*m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - 
   (5*m*\[Nu]*(7*s - 3*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/6 - (81*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 - 
   (3*m*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/2 + (57*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 - 
   3*m*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (m*\[Nu]*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 - 
   3*m*s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - (3*m*\[Nu]*(-4*s^2 + 4*s*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + (-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/2 + 14*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
   (7*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/9 + (21*m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/4 + 
   (7*m*\[Nu]*(s*(-99 + 61*\[Nu]) - 3*Sqrt[1 - 4*\[Nu]]*(-9 + 10*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/36 - 
   (675*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/32 - (65*m*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 - 
   (5*m*(-6889 + 246*Pi^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/288 + (5*m*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/3 + 
   (5*m*\[Nu]*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2592 - 
   (10*m*s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/3 + 
   (5*m*\[Nu]*(-2*s^2*(-5 + 6*\[Nu]) + 12*s*Sqrt[1 - 4*\[Nu]]*(1 + \[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 3*(-3 + 9*\[Nu] + 4*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 + (243*m*s*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/4 - 
   (207*m*s*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - (9*m*s^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + 
   (243*m*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/16 - (9*m*s^2*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - 
   (3*m*\[Nu]*(s*(405 - 1101*\[Nu] + 29*\[Nu]^2) - 3*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/16 - 
   (9*m*\[Nu]*(-4*s^3 + 8*s^2*Sqrt[1 - 4*\[Nu]]*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 5*s*(-1 + 4*\[Nu])*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2 - (1 - 4*\[Nu])^(3/2)*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)^3)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - (19845*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/256 - 
   (2345*m*s^2*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/48 + (23905*m*s^2*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/432 - 
   (175*m*s*\[Nu]^2*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/8 - 
   (35*m*\[Nu]*(-4*s^2*(198 - 680*\[Nu] + 3*\[Nu]^2) + 12*s*Sqrt[1 - 4*\[Nu]]*(9 - 181*\[Nu] + \[Nu]^2)*(s + s*\[Nu] - \[Nu]*\[Sigma]b) + 3*(27 + 27*\[Nu] - 561*\[Nu]^2 + 4*\[Nu]^3)*(s + s*\[Nu] - \[Nu]*\[Sigma]b)^2)*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/432 - (m*\[Nu]^2*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 
      688128*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/9216 - 
   (m*\[Nu]*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 
      18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/248832 + 
   (m*\[Nu]^2*\[Sigma]b*(3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
       (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (5*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
        (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))/2))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
     Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
        3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + 
   (m*s*\[Nu]^2*\[Sigma]b*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
       (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
     Sqrt[(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
        3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
   (m*\[Nu]^2*\[Sigma]b*(s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
        (1/3))*((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*((3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + 
        (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 
        3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - (7*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
         3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2))/
    (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
       (1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^
      (3/2)) + (m*\[Nu]*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
        (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
        3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (1/3)))/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
      Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
         3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
    (2*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
        3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
   (m*\[Nu]*(-((s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
      2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
      ((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
          (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
          3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))/2)/(2*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])))/
    Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
       3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
   (\[Nu]^2*(3*m*(-3*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - (s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (3*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
         (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        (15*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))/2)*
       \[CapitalDelta]Uint[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
      (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
        (3*m*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
         2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3 + (15*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - 
             (s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - 
             (s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
             (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))/2) + 
           (3*s*\[Delta]m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
               (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/2 + m*(-4 + 3*\[Delta]\[Nu] + (5*(6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                (3/2))/2 + 4*s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + (s*(-1 + \[Delta]\[Nu])*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - (s^2*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (-6*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(3 - (s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/
              (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 2*s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
         3*m*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))^(3/2)) - (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (1/3))])*(2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
         3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))^(3/2)) + (9*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
           (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (1/3)))/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
        (2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
         3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))^(5/2)) - (3*(-((s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + ((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            ((-3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
              (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
           (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)/(2*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (1/3))]))*(2*(\[Delta]m*(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (6*s - 6*s*\[Delta]\[Nu] + 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             s*(-2*s + 3*s*\[Delta]\[Nu] - 2*\[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             s*(s*\[Delta]\[Nu] - \[Delta]\[Chi])*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))) + 
         3*m*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
       ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3)))^(3/2)) + m*((9*s*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
          (2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^2 + 
        (9*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         (2*(2 - 2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))) + (2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (-2*((3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + (3*\[Delta]\[Chi]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2) + 
           d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
           (27*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/2))/
         ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))]) + (2*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
             \[Delta]\[Chi]]))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))]) + (2*(-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
           (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))]) - (x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            ((-3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
              (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
           (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*(-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
             \[Delta]\[Chi]]))/((1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3)))^(3/2)) - (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(3*s*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + (2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (2*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]) + (-2*((3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + 
               (3*\[Delta]\[Chi]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2) + d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (27*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]])/2)/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + (s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
             (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
              (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))) + 
        (3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*((3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + (3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
            (2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 3*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
          (Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
           (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
             (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
            3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))^2) - 
        (3*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2*s - 2*\[Delta]\[Chi] + 3*s*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
           (-2*(-2 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + \[Delta]\[Chi]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 
             (-6 + 9*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/(2*(1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
           3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))) + 
      (3*m*((s*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (5/3) + rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
        (-1 - s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
        Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
       (2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) + 
      (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^
         (3/2)*(-3 + (15*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - (s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - (s^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + (3*s*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))/2)*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
       rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + (m*(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^
         (3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
         (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
       (2*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]) + 
      (3*m*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]*
        (-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
          (2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
        Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + 
      (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^
         (3/2)*((s*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (5/3) + rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
        (-3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
          (2/3) + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
        Derivative[0, 2][\[CapitalDelta]Uint][Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)])))/6));


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio forcing term*)


F\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
    (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
  (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
  ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
    (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
     10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/20 + 
  12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
  ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
       (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
     (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*
       (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
     (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
        12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
        12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
     12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
       Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
     (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
        39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
        39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
     (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
        3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
       Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
            Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
    x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
    ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
     (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
        (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
      (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
       2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
             (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
             (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
       (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
         (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
    ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
     (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
        (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
      (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
       2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
             (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
             (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
       (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
         (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 - ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
    ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
     (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
        (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
      (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
       2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
             (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
             (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
       (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
         (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/15 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
     72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
     (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
  (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
     Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
   (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
    \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
       (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6 - 
  Sqrt[1 - 4*\[Nu]]*((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
     20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
         (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
       (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
       (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
          12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
       12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
       (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
          39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
          3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
     (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
      \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6) - 
  4*\[Nu]*((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
     20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
         (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
       (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
       (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
          12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
       12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
       (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
          39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
          3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
     (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
      \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(2*m);


(* ::Subsection::Closed:: *)
(*Total mass forcing term*)


Fm[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
 (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
    3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
 (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
 ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
   (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
    10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/20 + 
 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
 ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
      (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
    (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*
      (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
    (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
       12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
       12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
    12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
      Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
    (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
       39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
       39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
    (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
       3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
      Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
           Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
   x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
   ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
    (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
       (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
     (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
        (4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
         (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
      2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
        ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
            (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
        (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
            (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
      (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
        (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
        Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
             Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 - 
 (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
    72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
    (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
 (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
    Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
  (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
 (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], 
    Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
  (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6;


(* ::Subsection::Closed:: *)
(*Primary spin forcing term*)


Fs[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-1/30*(s*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
    ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
     (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
        (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
        10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
      (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
       2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
             (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
             (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
       (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
         (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/m - 
 (2*s*((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
     20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
         (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
       (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
       (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
          12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
       12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
       (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
          39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
          3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
     (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
      \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/m + 
 ((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
     ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
      (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
         (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
       (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
              (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
        2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
          ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
              (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
          (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
              (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
        (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
          (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
          Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + (96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
     12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
     24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*
      (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
       (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 
     720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
      ((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
         (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
       (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
        (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
       ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*
           (-1 + 12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
       (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*
           (35 + 39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
       (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
          3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
       72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
       m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
     (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
     (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60)/m^2;


(* ::Subsection::Closed:: *)
(*Secondary spin forcing term*)


F\[Sigma]b[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=((-1 + Sqrt[1 - 4*\[Nu]])^3*(-2*m*s*\[Nu] + 2*m*s*Sqrt[1 - 4*\[Nu]]*\[Nu] - 8*m*s*\[Nu]^2 - 2*m*\[Nu]*\[Sigma]b - 2*m*Sqrt[1 - 4*\[Nu]]*\[Nu]*\[Sigma]b + 4*m*\[Nu]^2*\[Sigma]b)*
   x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      ((-1 + Sqrt[1 - 4*\[Nu]])^2 + 3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
    (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
       (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
       10*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
     (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
        (4*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
         (1 + Sqrt[1 - 4*\[Nu]])*(-2 + (9*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
      2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
        ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
            (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
        (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
            (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
      (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
        (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
        Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
             Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
     x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/(120*m^2) + 
 ((-2*m*s*(-1 + Sqrt[1 - 4*\[Nu]]) - 2*m*s*\[Nu] - 2*m*s*Sqrt[1 - 4*\[Nu]]*\[Nu] - 8*m*s*\[Nu]^2 - 2*m*\[Nu]*\[Sigma]b + 2*m*Sqrt[1 - 4*\[Nu]]*\[Nu]*\[Sigma]b + 4*m*\[Nu]^2*\[Sigma]b)*
   ((8*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
    (2*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       10*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/
     20 + 12*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    ((1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
         (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/(2*\[Nu]) - 
       (9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 
       (6*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 
          12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] - 
          12*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^4 - 
       12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
         Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]) - 
       (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 
          39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2] + 
          39*Sqrt[(-1 + 4*\[Nu])*(-1 + (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2)]))/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
          3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 - (8*(24*Im[s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       (6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9))/15 - 
    (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
     (-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
      \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(2*m^2*\[Nu]^2) + 
 ((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
     ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
      (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
         (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
       (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
              (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
        2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
          ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
              (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
          (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
              (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
        (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
          (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
          Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + (m*(-1 + Sqrt[1 - 4*\[Nu]])^3*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
     ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
      (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
         (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
         6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
       (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
              (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
        2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
          ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
              (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
          (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
              (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
        (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
          (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
          Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + (Sqrt[1 - 4*\[Nu]]*(96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
      12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
        3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
      24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*
       (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
        (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
        6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 
      720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
       ((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
          (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
        (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
           Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
         (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
        ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*
            (-1 + 12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
        (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*
            (35 + 39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
        (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
           3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
          Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*Sqrt[
                1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
       x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
        72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
        m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
      (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
      (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/60 + 
   (-96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 - 12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
      (-(1 + Sqrt[1 - 4*\[Nu]])^2 - 3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 - 
     24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*
      (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + 
       (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 
       6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 
     720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
      ((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
         (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
       (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
          Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
        (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
       ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*
           (-1 + 12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
       (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*
           (35 + 39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
       (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
          3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
         Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
              Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
      x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
       72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
       m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) - 
     (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
     (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60 + 
   2*\[Nu]*((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
       ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
        (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
             Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
                (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
          2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
            ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
            (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
            (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
            Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                 Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
         x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + (96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
       12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
         3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
       24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
         Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^
            2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 
       m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
           (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
         (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
            Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
          (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*
             (-1 + 12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*
             (35 + 39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
         (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
           Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
                Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
         72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
         m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
       (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
       (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60) + 
   2*\[Nu]^2*((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
       ((12*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2))/(1 - Sqrt[1 - 4*\[Nu]])^3 - 
        (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 
           (3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2 + 
           6*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2))*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
         (1 - Sqrt[1 - 4*\[Nu]]) + ((3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
             Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/
                (-1 + Sqrt[1 - 4*\[Nu]])^2) + 8*\[Nu]*(1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)))/(4*\[Nu]) + 
          2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2]) + 
            ((2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*
                (-1 + 12*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^4 + 
            (2*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*
                (35 + 39*Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])))/(-1 + Sqrt[1 - 4*\[Nu]])^2) + 
          (48*(-1 + Sqrt[1 - 4*\[Nu]])*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*
            (1 + (3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(-1 + Sqrt[1 - 4*\[Nu]])^2)*
            Im[PolyGamma[0, 3 + ((2*I)*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*
                 Sqrt[1 - (2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(-1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 - Sqrt[1 - 4*\[Nu]]))*
         x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + (96*m*s*(1 + 3*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
       12*m*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(-(1 + Sqrt[1 - 4*\[Nu]])^2 - 
         3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
       24*m*s*(16 + 33*s^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^2*\[Nu]^2*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + 
         Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*(11 + 5*Sqrt[1 - 4*\[Nu]] + (3*(6 + 5*Sqrt[1 - 4*\[Nu]])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^
            2)/(1 + Sqrt[1 - 4*\[Nu]])^2 - 6*\[Nu]*(1 + (3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)/(1 + Sqrt[1 - 4*\[Nu]])^2))*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 720*m*s^3*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 
       m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*((-3*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*(s*(-3 + Sqrt[1 - 4*\[Nu]] + (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]) - (-1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b)*
           (s*(-1 + Sqrt[1 - 4*\[Nu]] + \[Nu] + Sqrt[1 - 4*\[Nu]]*\[Nu]) - (1 + Sqrt[1 - 4*\[Nu]])*\[Nu]*\[Sigma]b))/\[Nu] + 
         (9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^3*(2*s - s*(1 + \[Nu]) + \[Nu]*\[Sigma]b + 
            Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 - Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - 4*\[Nu]])^3) + 12*(1 + Sqrt[1 - 4*\[Nu]])*
          (1 + Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2]) + 
         ((2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*
             (-1 + 12*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^4 + 
         (4*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*
             (35 + 39*Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])))/(1 + Sqrt[1 - 4*\[Nu]])^2 + 
         (48*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))*((1 + Sqrt[1 - 4*\[Nu]])^2 + 
            3*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2)*
           Im[PolyGamma[0, 3 + ((2*I)*(2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b)))/((1 + Sqrt[1 - 4*\[Nu]])*
                Sqrt[1 - (2*s + s*(1 + \[Nu]) - \[Nu]*\[Sigma]b + Sqrt[1 - 4*\[Nu]]*(-(s*(1 + \[Nu])) + \[Nu]*\[Sigma]b))^2/(1 + Sqrt[1 - 4*\[Nu]])^2])]])/(1 + Sqrt[1 - 4*\[Nu]])^2)*
        x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 32*(24*Im[m*s*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
         72*Im[m*s^3*\[Nu]^2*PolyGamma[0, 3 + ((2*I)*s)/Sqrt[1 - s^2]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
         m*(6*(1 + Sqrt[1 - s^2]) + s^4*(-3 + 36*Sqrt[1 - s^2]) + s^2*(70 + 78*Sqrt[1 - s^2]))*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) + 
       (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 - 
       (60*m*\[Nu]^3*\[Sigma]b*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(-1 + Sqrt[(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                1/3))])*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - s], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + s*x[\[Omega], \[Phi], \[Nu], m, s, \[Sigma]b, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60))/(2*m^2*\[Nu]^2);


(* ::Subsection::Closed:: *)
(*WF phase evolution*)


F\[Phi][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=\[Omega];


(* ::Subsection::Closed:: *)
(*Total mass shift*)


F\[Delta]m[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(Fm[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]-\[Delta]m F\[Nu][\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu];


d\[Delta]mdm[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu];


d\[Delta]md\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]m/\[Nu])


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio shift*)


F\[Delta]\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(((-1+2 \[Delta]\[Nu] \[Nu]) F\[Nu][\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu]^2);


d\[Delta]\[Delta]\[Nu]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(1-2 \[Delta]\[Nu] \[Nu])/\[Nu]^2


(* ::Subsection::Closed:: *)
(*Primary spin shift*)


F\[Delta]\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-\[Delta]\[Chi] F\[Nu][\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]+((2 (1+Sqrt[1-4 \[Nu]]+2 (-2+Sqrt[1-4 \[Nu]]) \[Nu]+2 Sqrt[1-4 \[Nu]] \[Nu]^2) Fs[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]+4 (s (2+\[Nu]+Sqrt[1-4 \[Nu]] \[Nu])-(1+Sqrt[1-4 \[Nu]]) \[Nu] \[Sigma]b) F\[Nu][\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]-4 Sqrt[1-4 \[Nu]] \[Nu]^2 F\[Sigma]b[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/((1+Sqrt[1-4 \[Nu]])^2 Sqrt[1-4 \[Nu]])))/\[Nu];


d\[Delta]\[Chi]d\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu]


d\[Delta]\[Chi]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]\[Chi]/\[Nu])


(* ::Section:: *)
(*Construct Interpolations*)


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
\[Omega]critdata=Get[FileNameJoin[{directorystop,"StopHybridFixedSSigma.m"}]];


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
(*Stop Condition*)


\[Omega]crit=Interpolation[\[Omega]critdata,InterpolationOrder->All];


(* ::Section::Closed:: *)
(*Evolution equations  *)


variables={\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]};
evolutionequations={
\[Omega]'[t]==F\[Omega][\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Phi]'[t]==F\[Phi][\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Nu]'[t]==F\[Nu][\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
m'[t]==Fm[\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]] , 
s'[t]==Fs[\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Sigma]b'[t]==F\[Sigma]b[\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]m'[t]==F\[Delta]m[\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]\[Nu]'[t]==F\[Delta]\[Nu][\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]\[Chi]'[t]==F\[Delta]\[Chi][\[Omega][t],\[Phi][t],\[Nu][t],m[t],s[t],\[Sigma]b[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]]};
InitialConditionFormat={"\[Omega]","\[Phi]", "\[Nu]", "m","s","\[Sigma]b","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"};

stopcondition = {\[Omega][t] >= Min[1/((1.05rISCO["\[Omega]","\[Phi]","\[Nu]","m","s","\[Sigma]b","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"])^(3/2)+"s"),1/(6.26^(3/2)+"s")],
\[Omega][t] >= \[Omega]crit["\[Nu]",(2 "s"+"s" (1+"\[Nu]")-"\[Nu]" "\[Sigma]b"+Sqrt[1-4 "\[Nu]"] (-"s" (1+"\[Nu]")+"\[Nu]" "\[Sigma]b"))/(1+Sqrt[1-4 "\[Nu]"]),(2 "s"-"s" (1+"\[Nu]")+"\[Nu]" "\[Sigma]b"+Sqrt[1-4 "\[Nu]"] (-"s" (1+"\[Nu]")+"\[Nu]" "\[Sigma]b"))/(1-Sqrt[1-4 "\[Nu]"])]};

parameterspacecoverage = {\[Sqrt]((rISCO["\[Omega]","\[Phi]","\[Nu]","m","s","\[Sigma]b","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"] x["\[Omega]","\[Phi]","\[Nu]","m","s","\[Sigma]b","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"])/(1-"s" x["\[Omega]","\[Phi]","\[Nu]","m","s","\[Sigma]b","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"]^(3/2))^(2/3))<.998,
1/(30^(3/2)+"s")<"\[Omega]"<Min[1/(6.06^(3/2)+"s"),
"\[Omega]">\[Omega]crit["\[Nu]",(2 "s"+"s" (1+"\[Nu]")-"\[Nu]" "\[Sigma]b"+Sqrt[1-4 "\[Nu]"] (-"s" (1+"\[Nu]")+"\[Nu]" "\[Sigma]b"))/(1+Sqrt[1-4 "\[Nu]"]),(2 "s"-"s" (1+"\[Nu]")+"\[Nu]" "\[Sigma]b"+Sqrt[1-4 "\[Nu]"] (-"s" (1+"\[Nu]")+"\[Nu]" "\[Sigma]b"))/(1-Sqrt[1-4 "\[Nu]"])]],
"\[Nu]"<.248,
Abs["s"]>.000001};


<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat,"StopCondition" -> stopcondition, "ParameterSpaceCoverage"->parameterspacecoverage|>
