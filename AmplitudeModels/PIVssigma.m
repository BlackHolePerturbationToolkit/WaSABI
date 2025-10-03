(* ::Package:: *)

(* ::Section::Closed:: *)
(*Function on phase space*)


(* ::Subsection::Closed:: *)
(*ISCO radius*)


rISCO[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=3+Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]-Sign[s] \[Sqrt]((2-(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))) (4+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))+2 Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]));
drISCOds[\[Omega]_,\[Phi]_,\[Nu]_,m_,s_,\[Sigma]b_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/2 ((6 s+1/(3 (1-s^2)^(2/3)) 2 ((1-s^2) (-(1/(1-s)^(2/3))+1/(1+s)^(2/3))-2 s ((1-s)^(1/3)+(1+s)^(1/3))) (1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))))/(Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2])-(Sign[s] ((2-(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))) (-((2 s ((1-s)^(1/3)+(1+s)^(1/3)))/(3 (1-s^2)^(2/3)))+((1-s)^(2/3)-(1+s)^(2/3))/(3 (1-s^2)^(1/3))+(6 s+1/(3 (1-s^2)^(2/3)) 2 ((1-s^2) (-(1/(1-s)^(2/3))+1/(1+s)^(2/3))-2 s ((1-s)^(1/3)+(1+s)^(1/3))) (1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))))/(Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]))+1/(3 (1-s^2)^(2/3)) (-((1-s^2) (-(1/(1-s)^(2/3))+1/(1+s)^(2/3)))+2 s ((1-s)^(1/3)+(1+s)^(1/3))) (4+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))+2 Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2])))/(\[Sqrt]((2-(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))) (4+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3))+2 Sqrt[3 s^2+(1+(1-s^2)^(1/3) ((1-s)^(1/3)+(1+s)^(1/3)))^2]))));


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


\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr = Interpolation[Table[{{Log[1-fluxdata1SF[[i]]["a"]], Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,fluxdata1SF[[i]]["a"],\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/fluxdata1SF[[i]]["r"]]},N[fluxdata1SF[[i]]["r"]^(15/2) fluxdata1SF[[i]]["hor"]["Value"]]},{i,1,Length[fluxdata1SF]}],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*\[Delta]x*)


Re1SFAmp22intV2=Interpolation[Get[FileNameJoin[{directoryamp, "ReAmp1SFKerr22NewGrid.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];
Im1SFAmp22intV2=Interpolation[Get[FileNameJoin[{directoryamp, "ImAmp1SFKerr22NewGrid.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*Real 22 amplitude  1SF*)


RealAmp1SFKerr22=Interpolation[Get[FileNameJoin[{directoryamp, "1SF_RealAmp_KerrCirc_22_NewGrid.m"}]],Method->"Hermite",InterpolationOrder->{All,All}];


(* ::Subsection::Closed:: *)
(*2SF*)


Do[
Z\[ScriptCapitalI]data[ll,mm]=Get[FileNameJoin[{directoryamp2, "1SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]];
Z2\[ScriptCapitalI]data[ll,mm]=Get[FileNameJoin[{directoryamp3, "2SFTeukampSchwarzCirc"<>ToString[ll]<>ToString[mm]<>".m"}]];

ReZ2\[ScriptCapitalI]data[ll,mm]=Table[{Z2\[ScriptCapitalI]data[ll,mm][[i]][[1]],Re[Z2\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z2\[ScriptCapitalI]data[ll,mm]]}];
ImZ2\[ScriptCapitalI]data[ll,mm]=Table[{Z2\[ScriptCapitalI]data[ll,mm][[i]][[1]],Im[Z2\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z2\[ScriptCapitalI]data[ll,mm]]}];

ReZ\[ScriptCapitalI]data[ll,mm]=Table[{Z\[ScriptCapitalI]data[ll,mm][[i]][[1]],Re[Z\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z\[ScriptCapitalI]data[ll,mm]]}];
ImZ\[ScriptCapitalI]data[ll,mm]=Table[{Z\[ScriptCapitalI]data[ll,mm][[i]][[1]],Im[Z\[ScriptCapitalI]data[ll,mm][[i]][[2]]]},{i,1,Length[Z\[ScriptCapitalI]data[ll,mm]]}];

ReZ2\[ScriptCapitalI]interpolation[ll,mm]=Interpolation[ReZ2\[ScriptCapitalI]data[ll,mm],InterpolationOrder->3, Method->"Hermite"];
ImZ2\[ScriptCapitalI]interpolation[ll,mm]=Interpolation[ImZ2\[ScriptCapitalI]data[ll,mm],InterpolationOrder->3,Method->"Hermite"];

ImZ\[ScriptCapitalI][ll,mm]=Interpolation[ImZ\[ScriptCapitalI]data[ll,mm],InterpolationOrder->8,Method->"Hermite"];
ReZ\[ScriptCapitalI][ll,mm]=Interpolation[ReZ\[ScriptCapitalI]data[ll,mm],InterpolationOrder->8,Method->"Hermite"];

, {ll,2,(*5*)2}, {mm,1,ll}];

ReZ2\[ScriptCapitalI][ll_,mm_][r0_]:=Cos[ImZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]]Exp[ReZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]];
ImZ2\[ScriptCapitalI][ll_,mm_][r0_]:=Sin[ImZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]]Exp[ReZ2\[ScriptCapitalI]interpolation[ll,mm][Log[r0]]];



With[{M=1},
Reh0PAamp[ll_,mm_][r0_]:=(-2(-ReZ\[ScriptCapitalI][ll,mm][r0]))/(I mm Sqrt[M/r0^3])^2;
Imh0PAamp[ll_,mm_][r0_]:=(-2(-ImZ\[ScriptCapitalI][ll,mm][r0]))/(I mm Sqrt[M/r0^3])^2;

Reh1PAamp[ll_,mm_][r0_]:= (-2ReZ2\[ScriptCapitalI][ll,mm][r0])/(I mm Sqrt[M/r0^3])^2;
Imh1PAamp[ll_,mm_][r0_]:= (-2ImZ2\[ScriptCapitalI][ll,mm][r0])/(I mm Sqrt[M/r0^3])^2;
]


RealAmp1SFSchw22[r0_]:=((Reh0PAamp[2,2][r0])^2+(Imh0PAamp[2,2][r0])^2)^(1/2);
RealAmp2SFSchw22[r0_]:=(Imh0PAamp[2,2][r0] Imh1PAamp[2,2][r0]+Reh0PAamp[2,2][r0] Reh1PAamp[2,2][r0])/RealAmp1SFSchw22[r0]+2/3  (-1/Sqrt[1-3 1/r0])Derivative[1][RealAmp1SFSchw22][r0];
(*Hardcoded Berndston mass correction in second term - don't ask.*)


(* ::Section::Closed:: *)
(*Hybridised mode amplitudes*)


(* ::Subsection::Closed:: *)
(*Mode amplitudes*)


(* ::Input::Initialization:: *)
<|"(2,2)"->(428*m*Sqrt[Pi/5]*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^2)/21-(44*m*Sqrt[5*Pi]*\[Nu]^2*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^2)/21+(4*m*Sqrt[Pi/5]*\[Nu]*(-107+55*\[Nu])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^2)/21+(32*m*Sqrt[Pi/5]*s*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(5/2))/3-(16*m*Sqrt[Pi/5]*\[Nu]*(3*s+Sqrt[1-4*\[Nu]]*(-(s*(1+\[Nu]))+\[Nu]*\[Sigma]b))*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(5/2))/3+(2173*m*Sqrt[Pi/5]*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3)/189-8*m*Sqrt[Pi/5]*s^2*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3+(1069*m*Sqrt[Pi/5]*\[Nu]^2*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3)/27+(m*Sqrt[Pi/5]*\[Nu]*(-2173-7483*\[Nu]+2047*\[Nu]^2)*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3)/189-8*m*Sqrt[Pi/5]*\[Nu]*(-4*s^2+4*s*Sqrt[1-4*\[Nu]]*(s+s*\[Nu]-\[Nu]*\[Sigma]b)-(s+s*\[Nu]-\[Nu]*\[Sigma]b)^2+4*\[Nu]*(s+s*\[Nu]-\[Nu]*\[Sigma]b)^2)*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^3+(856*m*Pi^(3/2)*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/(21*Sqrt[5])+(256*m*Sqrt[5*Pi]*s*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/63-(272*m*Pi^(3/2)*\[Nu]^2*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/(21*Sqrt[5])+(8*m*Pi^(3/2)*\[Nu]*(-107+34*\[Nu])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/(21*Sqrt[5])-(8*m*Sqrt[Pi/5]*\[Nu]*(163*s+92*s*\[Nu]-3*Sqrt[1-4*\[Nu]]*(s+s*\[Nu]-\[Nu]*\[Sigma]b)+20*Sqrt[1-4*\[Nu]]*\[Nu]*(s+s*\[Nu]-\[Nu]*\[Sigma]b))*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(7/2))/63+(64*m*Pi^(3/2)*s*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/(3*Sqrt[5])-(64*m*Sqrt[Pi/5]*s^2*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/63-(m*Sqrt[Pi/5]*(-292094250+14916825*Pi^2)*\[Nu]^2*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/4365900-(32*m*Sqrt[Pi/5]*\[Nu]*(3*Pi*s+Pi*Sqrt[1-4*\[Nu]]*(-(s*(1+\[Nu]))+\[Nu]*\[Sigma]b))*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/3+(8*m*Sqrt[Pi/5]*\[Nu]*(s^2*(-497+408*\[Nu])-3*(78-347*\[Nu]+136*\[Nu]^2)*(s+s*\[Nu]-\[Nu]*\[Sigma]b)^2+s*Sqrt[1-4*\[Nu]]*(-739+408*\[Nu])*(-(s*(1+\[Nu]))+\[Nu]*\[Sigma]b))*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/63-(m*Sqrt[Pi/5]*\[Nu]*(1459480086-284739840*EulerGamma+23284800*Pi^2-569479680*Log[2]-142369920*Log[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/4365900+(m*Sqrt[Pi/5]*\[Nu]*(1459480086-284739840*EulerGamma+23284800*Pi^2-292094250*\[Nu]+14916825*Pi^2*\[Nu]-255288600*\[Nu]^2+40122250*\[Nu]^3-569479680*Log[2]-142369920*Log[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^4)/4365900+(4346*m*Pi^(3/2)*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/(189*Sqrt[5])-(11236*m*Sqrt[Pi/5]*s*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/189-(16*m*Pi^(3/2)*s^2*\[Nu]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/Sqrt[5]+(1996*Sqrt[5]*m*Pi^(3/2)*\[Nu]^2*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/189+(2*m*Pi^(3/2)*\[Nu]*(-2173-4990*\[Nu]+1120*\[Nu]^2)*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/(189*Sqrt[5])+(16*m*Pi^(3/2)*\[Nu]*(4*s^2-4*s*Sqrt[1-4*\[Nu]]*(s+s*\[Nu]-\[Nu]*\[Sigma]b)-(-1+4*\[Nu])*(s+s*\[Nu]-\[Nu]*\[Sigma]b)^2)*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/Sqrt[5]+(2*m*Sqrt[Pi/5]*\[Nu]*(9*s*(1061+4043*\[Nu]+499*\[Nu]^2)+Sqrt[1-4*\[Nu]]*(3931+15626*\[Nu]+3075*\[Nu]^2)*(-(s*(1+\[Nu]))+\[Nu]*\[Sigma]b))*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/189-(32*m*Sqrt[Pi/5]*\[Nu]*(-4*s^3+8*s^2*Sqrt[1-4*\[Nu]]*(s+s*\[Nu]-\[Nu]*\[Sigma]b)+5*s*(-1+4*\[Nu])*(s+s*\[Nu]-\[Nu]*\[Sigma]b)^2-(1-4*\[Nu])^(3/2)*(-(s*(1+\[Nu]))+\[Nu]*\[Sigma]b)^3)*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(9/2))/3-(m*Sqrt[Pi/5]*\[Nu]*(-10158690082236+3168584939520*EulerGamma-259113254400*Pi^2+6337169879040*Log[2]+1584292469760*Log[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^5)/19070251200-(m*Sqrt[Pi/5]*\[Nu]^2*(-12096209789172+5287432550400*EulerGamma-46138467375*Pi^2+10574865100800*Log[2]+2643716275200*Log[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^5)/19070251200-(m*Sqrt[Pi/5]*\[Nu]*(10158690082236+259113254400*Pi^2+12096209789172*\[Nu]+46138467375*Pi^2*\[Nu]-5277746988780*\[Nu]^2+155134980000*Pi^2*\[Nu]^2+799476032600*\[Nu]^3-128631120100*\[Nu]^4-276756480*EulerGamma*(11449+19105*\[Nu])-6337169879040*Log[2]-10574865100800*\[Nu]*Log[2]-138378240*(11449+19105*\[Nu])*Log[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]])*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^5)/19070251200+(m*\[Nu]*RealAmp1SFKerr22[0.7988880369434854+0.2901432317650783*Log[1-s],-1+2.*Sqrt[(rISCO[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(1-s*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(3/2))^(2/3)]]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/(2*(1-s*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(3/2))^(2/3))+\[Nu]^2*(-((\[Delta]m+m*(-1+\[Delta]\[Nu]))*RealAmp1SFSchw22[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(-1)])+m*RealAmp2SFSchw22[x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(-1)]+(m*(2+2*\[Delta]\[Chi]*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(3/2)-3*\[Delta]x[\[Omega],\[Phi],\[Nu],m,0,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])*Derivative[1][RealAmp1SFSchw22][x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]^(-1)])/(3*x[\[Omega],\[Phi],\[Nu],m,s,\[Sigma]b,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]))|>
