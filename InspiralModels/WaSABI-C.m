(* ::Package:: *)

(* ::Section::Closed:: *)
(*Dependent variables*)


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
\[Delta]x[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(0.3333333333333333*\[Omega]^2*(-0.3333333333333333-0.3333333333333333*\[Chi]1*\[Omega]+1.*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega])*Sqrt[(-1+\[Chi]1*\[Omega])*(-1-\[Chi]1*\[Omega]+3*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega])]*rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]*((-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega]^2*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]+(1-2.*\[Chi]1*\[Omega]+\[Chi]1^2*\[Omega]^2)*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1-\[Chi]1],Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]])*(1.*Re1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]*Derivative[0,1][Im1SFAmp22intV2][0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]-1.*Im1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]*Derivative[0,1][Re1SFAmp22intV2][0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]))/((-1.+\[Chi]1*\[Omega])^6*(-0.16666666666666666*(-\[Chi]1+\[Omega]^(-1))^(1/3)+0.5*\[Chi]1^2*\[Omega]-1.1666666666666667*\[Chi]1*(-\[Chi]1+\[Omega]^(-1))^(1/3)*\[Omega]+1.*(-\[Chi]1+\[Omega]^(-1))^(2/3)*\[Omega])*(Im1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]^2+Re1SFAmp22intV2[0.7988880369434854+0.2901432317650783*Log[1-\[Chi]1],-1+2.*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]]^2)*Sqrt[rISCO[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]/(-\[Chi]1+\[Omega]^(-1))^(2/3)]);


d\[Delta]xd\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-0.3333333333333333*\[Omega]^2*(0.05555555555555555/(-\[Chi]1 + \[Omega]^(-1))^(2/3) + 1.*\[Chi]1*\[Omega] + (0.3888888888888889*\[Chi]1*\[Omega])/(-\[Chi]1 + \[Omega]^(-1))^(2/3) - 
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
   Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]);


d\[Delta]xdx[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(0.5*\[Omega]^(4/3)*(0.3333333333333333*(-\[Chi]1 + \[Omega]^(-1))^(1/3)*\[Omega]*(1 - \[Chi]1*\[Omega])*(-1. + \[Chi]1*\[Omega])^2*
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
  Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(-\[Chi]1 + \[Omega]^(-1))^(2/3)]);


(* ::Subsection::Closed:: *)
(*WF frequency forcing term*)


F\[Omega][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-3*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*((44*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 - 
   (44*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + (48*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 - 
   (44*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + (44*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + 
   (88*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(13/2))/5 + (208*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/9 - 
   (33*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 + (33*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 - 
   (66*\[Nu]^3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 + (33*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 - 
   (33*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 - (66*\[Nu]^3*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7)/5 + 
   (59*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 - (59*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
   (3632*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 + (2804*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - 
   (2512*\[Nu]^4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - (59*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + 
   (59*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/5 + (3866*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - 
   (2804*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - (2512*\[Nu]^4*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2))/45 - 
   (188806*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/945 - (1240*\[Nu]^5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/81 + 
   (104*Pi*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/3 - (104*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/3 + 
   (544*Pi*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/15 - (1222*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 + 
   (1222*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 - (25096*\[Nu]^3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 - 
   (3236*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 + (3788*\[Nu]^4*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 - 
   (104*Pi*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/3 + (104*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/3 + 
   (208*Pi*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/3 - (4808*\[Nu]^4*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 + 
   (1222*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 - (1222*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 - 
   (25096*\[Nu]^3*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/315 + (3236*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 + 
   (3788*\[Nu]^4*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^8)/45 + (77354*Pi*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/189 - 
   (32407*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/243 + (32407*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/243 - 
   (234436*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/8505 + (7768*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 - 
   (208636*\[Nu]^4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/945 - (26948*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 + 
   (17872*\[Nu]^5*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 - 26*Pi*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + 
   26*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 52*Pi*\[Nu]^3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + 
   (142*\[Nu]^2*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - (142*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 
   (638*\[Nu]^3*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + (118*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - 
   (424*\[Nu]^4*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + (32407*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/243 - 
   (32407*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/243 - (254158*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/1215 - 
   (7768*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 - (208636*\[Nu]^4*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/945 + 
   (26948*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 + (17872*\[Nu]^5*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/135 + 
   (38*\[Nu]^3*\[Chi]1^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/3 - (38*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/3 + 
   (392*\[Nu]^4*\[Chi]1^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 26*Pi*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
   26*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 52*Pi*\[Nu]^3*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
   (38*\[Nu]^3*\[Chi]1*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/3 + (38*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/3 + 
   (392*\[Nu]^4*\[Chi]1*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - (142*\[Nu]^2*\[Chi]2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 
   (142*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + (638*\[Nu]^3*\[Chi]2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 - 
   (118*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/5 - (424*\[Nu]^4*\[Chi]2^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2))/15 + 
   (2571400*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/1701 - (3157*Pi^2*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/60 + 
   (5500*\[Nu]^5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/63 + (16*\[Nu]^6*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/3 + 
   (2872*Pi*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/35 - (2872*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/35 + 
   (318443*Pi*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/630 + (258029*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/630 - 
   (42482*Pi*\[Nu]^4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/105 - (45334*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 + 
   (45334*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 - (1315129*\[Nu]^3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/5670 - 
   (125977*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/630 + (3552*\[Nu]^4*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/5 + 
   (18767*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - (13474*\[Nu]^5*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - 
   (2872*Pi*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/35 + (2872*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/35 + 
   (361421*Pi*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/630 - (258029*Pi*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/630 - 
   (42482*Pi*\[Nu]^4*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/105 + (13222*\[Nu]^4*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 + 
   (4156*\[Nu]^5*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 + (45334*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 - 
   (45334*Sqrt[1 - 4*\[Nu]]*\[Nu]^2*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/2835 - (1315129*\[Nu]^3*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/5670 + 
   (125977*Sqrt[1 - 4*\[Nu]]*\[Nu]^3*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/630 + (3552*\[Nu]^4*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/5 - 
   (18767*Sqrt[1 - 4*\[Nu]]*\[Nu]^4*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - (13474*\[Nu]^5*\[Chi]2^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9)/45 - 
   (26622581*Pi*\[Nu]^4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/9072 - (3719141*Pi*\[Nu]^5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(19/2))/5940 + 
   ((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(12*(\[Chi]2 + 3*\[Chi]2^3) - 
      3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
      (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
          \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
        48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + 
   (((-1 + Sqrt[1 - 4*\[Nu]])^3*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*
       (12*(\[Chi]2 + 3*\[Chi]2^3) - 3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 6*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
        (3*\[Chi]1*\[Chi]2*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + 3*\[Chi]2^2) + 8*\[Nu]*(1 + 3*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
            \[Chi]2^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
          48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/(120*m) + 
     ((-1 - Sqrt[1 - 4*\[Nu]])*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
       (12*(\[Chi]2 + 3*\[Chi]2^3) - 3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
        (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
            \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
          48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/(60*m))*
    (m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (m*\[Nu]*(-2 + 2*Sqrt[1 - 4*\[Nu]] + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
     m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - (m*\[Nu]*(-4*\[Nu]*\[Chi]1 + 2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/4 + 
     (3*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - (m*\[Nu]*(72 - 72*Sqrt[1 - 4*\[Nu]] - 121*\[Nu] + 31*Sqrt[1 - 4*\[Nu]]*\[Nu] + 2*\[Nu]^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - (5*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + 
     (5*m*\[Nu]*(4*\[Nu]*(3 + \[Nu])*\[Chi]1 + 2*(13 - 13*Sqrt[1 - 4*\[Nu]] - 55*\[Nu] + 29*Sqrt[1 - 4*\[Nu]]*\[Nu] + 14*\[Nu]^2)*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/72 + 
     (27*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - (m*\[Nu]*(-6*(-1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2) + 
        (-1 + Sqrt[1 - 4*\[Nu]])^2*(405 - 1101*\[Nu] + 29*\[Nu]^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/96 - 
     m*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + (m*\[Nu]^2*(\[Chi]1 + \[Chi]2)*(4*\[Nu]*\[Chi]1 - 2*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 + (m*\[Nu]^2*((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2 + 4*\[Nu]*\[Chi]1*\[Chi]2 - (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - (35*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 - 
     (7*m*\[Nu]*(4*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]1 + 2*(603*(-1 + Sqrt[1 - 4*\[Nu]]) + (3095 - 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] + (-3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/864 + (m*\[Nu]^2*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
       Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])) + 
   (96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
     3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + 
     720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
      (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
       12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
         39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
       48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
     32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
       \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
     (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
       \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60 + 
   (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5*\[ScriptCapitalF]\[ScriptCapitalI]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(10/3) - 2*\[Delta]\[Nu]*\[Nu]^3*\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(-1)] + 
   \[Nu]^3*\[ScriptCapitalF]\[ScriptCapitalI]int2SFSchw[-Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]] + 
   (\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5*\[ScriptCapitalF]\[ScriptCapitalI]intS2Kerr[Log[1 - \[Chi]1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(10/3) - 
   (\[Nu]^3*\[Delta]x[\[Omega], \[Phi], \[Nu], m, 0, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Derivative[1][\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw][x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(-1)])/
    x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
   (((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*(12*(\[Chi]2 + 3*\[Chi]2^3) - 
        3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
        (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
            \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
          48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + 
     (96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
       3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
         (17/2) + 720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
        (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
         12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
           39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
         48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
       32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
         \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
       (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
          Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
         \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60)*
    ((-3*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/8 - (\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + 
     (\[Nu]*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + (4*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
     (5*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + \[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
     (\[Nu]*((-2*(1 + Sqrt[1 - 4*\[Nu]]) + \[Nu])*\[Chi]1 + (-2 + 2*Sqrt[1 - 4*\[Nu]] + \[Nu])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
     (27*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 + (19*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 + 
     (\[Nu]*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 - (\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 
     \[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - \[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
     (\[Nu]*(-((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2) - 4*\[Nu]*\[Chi]1*\[Chi]2 + (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/4 + 
     4*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - (74*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 + 
     (3*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 - (\[Nu]*((72*(1 + Sqrt[1 - 4*\[Nu]]) - (121 + 31*Sqrt[1 - 4*\[Nu]])*\[Nu] + 2*\[Nu]^2)*\[Chi]1 + 
        (72 - 72*Sqrt[1 - 4*\[Nu]] - 121*\[Nu] + 31*Sqrt[1 - 4*\[Nu]]*\[Nu] + 2*\[Nu]^2)*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - 
     (675*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/128 - (5*(-6889 + 246*Pi^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/1152 + 
     (5*\[Nu]*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 - 
     (65*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (275*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 - 
     (5*\[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + (5*\[Nu]*((13*(1 + Sqrt[1 - 4*\[Nu]]) - (55 + 29*Sqrt[1 - 4*\[Nu]])*\[Nu] + 14*\[Nu]^2)*\[Chi]1^2 + 
        4*\[Nu]*(3 + \[Nu])*\[Chi]1*\[Chi]2 + (13 - 13*Sqrt[1 - 4*\[Nu]] - 55*\[Nu] + 29*Sqrt[1 - 4*\[Nu]]*\[Nu] + 14*\[Nu]^2)*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/72 + 
     (27*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 50*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
     \[Nu]^2*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + (27*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/8 - 
     \[Nu]^2*\[Chi]1^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + 
     (\[Nu]^2*(\[Chi]1 + \[Chi]2)*((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2 + 4*\[Nu]*\[Chi]1*\[Chi]2 - (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 
     (\[Nu]*(-6*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2) + (405 - 1101*\[Nu] + 29*\[Nu]^2)*
         ((1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/96 - 
     (3969*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/256 - (469*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/48 + 
     (21665*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/432 - (35*\[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 - 
     (7*\[Nu]*((-603*(1 + Sqrt[1 - 4*\[Nu]]) + (3095 + 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] - (3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]1^2 + 4*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]1*\[Chi]2 + 
        (603*(-1 + Sqrt[1 - 4*\[Nu]]) + (3095 - 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] + (-3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/
      864 - (\[Nu]^2*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/46080 - (\[Nu]*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
        27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 + (\[Nu]^2*\[Chi]2*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
      ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
         (1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
     (\[Nu]*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
      Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
         3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
     (\[Nu]^2*(3*(-1 - \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - \[Chi]1], 
          Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3)]] + (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(2*(\[Delta]\[Chi] + \[Chi]1) - 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         (-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) - (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
          Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
        (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
          (2*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/
                3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-2*(\[Delta]\[Chi] + \[Chi]1) + 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
           (4 - 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/
         (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))) + (Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/6) + 
   (((-1 + Sqrt[1 - 4*\[Nu]])^3*(1 + Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
       (12*(\[Chi]2 + 3*\[Chi]2^3) - 3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
        (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
            \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
          48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/(120*m) + 
     ((1 - Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
        3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
          (17/2) + 720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
         (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
          12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
          48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
          72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
          \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
        (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(120*m))*
    ((-3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/8 - (m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + 
     (m*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/24 + (4*m*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
     (10*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + 2*m*\[Nu]*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
     (m*\[Nu]*((1 + 4/Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (1 - 4/Sqrt[1 - 4*\[Nu]])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + 
     (m*((-2*(1 + Sqrt[1 - 4*\[Nu]]) + \[Nu])*\[Chi]1 + (-2 + 2*Sqrt[1 - 4*\[Nu]] + \[Nu])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - 
     (27*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/16 + (19*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/8 + 
     (m*\[Nu]*(-57 + 2*\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 + (m*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/48 - 
     (m*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2 + 2*m*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
     2*m*\[Nu]*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - (m*\[Nu]*(-((-2 - 2/Sqrt[1 - 4*\[Nu]])*\[Chi]1^2) - 4*\[Chi]1*\[Chi]2 + (2 - 2/Sqrt[1 - 4*\[Nu]])*\[Chi]2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/4 - (m*(-((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2) - 4*\[Nu]*\[Chi]1*\[Chi]2 + (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/4 + 4*m*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - 
     (148*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 + 3*m*\[Nu]*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - 
     (m*\[Nu]*((-121 - 144/Sqrt[1 - 4*\[Nu]] - 31*Sqrt[1 - 4*\[Nu]] + 4*\[Nu] + (62*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (-121 + 144/Sqrt[1 - 4*\[Nu]] + 31*Sqrt[1 - 4*\[Nu]] + 4*\[Nu] - 
          (62*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - 
     (m*((72*(1 + Sqrt[1 - 4*\[Nu]]) - (121 + 31*Sqrt[1 - 4*\[Nu]])*\[Nu] + 2*\[Nu]^2)*\[Chi]1 + (72 - 72*Sqrt[1 - 4*\[Nu]] - 121*\[Nu] + 31*Sqrt[1 - 4*\[Nu]]*\[Nu] + 2*\[Nu]^2)*\[Chi]2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - (675*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/128 - 
     (5*m*(-6889 + 246*Pi^2)*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/576 + 
     (5*m*\[Nu]*(9*(-6889 + 246*Pi^2) + 3348*\[Nu] + 21*\[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 + 
     (5*m*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/10368 - 
     (65*m*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/36 + (275*m*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 - 
     (5*m*\[Nu]*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/3 + (5*m*\[Nu]*((-55 - 26/Sqrt[1 - 4*\[Nu]] - 29*Sqrt[1 - 4*\[Nu]] + 28*\[Nu] + (58*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 + 
        4*\[Nu]*\[Chi]1*\[Chi]2 + 4*(3 + \[Nu])*\[Chi]1*\[Chi]2 + (-55 + 26/Sqrt[1 - 4*\[Nu]] + 29*Sqrt[1 - 4*\[Nu]] + 28*\[Nu] - (58*\[Nu])/Sqrt[1 - 4*\[Nu]])*\[Chi]2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/72 + (5*m*((13*(1 + Sqrt[1 - 4*\[Nu]]) - (55 + 29*Sqrt[1 - 4*\[Nu]])*\[Nu] + 14*\[Nu]^2)*\[Chi]1^2 + 4*\[Nu]*(3 + \[Nu])*\[Chi]1*\[Chi]2 + 
        (13 - 13*Sqrt[1 - 4*\[Nu]] - 55*\[Nu] + 29*Sqrt[1 - 4*\[Nu]]*\[Nu] + 14*\[Nu]^2)*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/72 + 
     (27*m*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 100*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
     2*m*\[Nu]*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + (27*m*\[Nu]*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/4 - 
     2*m*\[Nu]*\[Chi]1^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + (m*\[Nu]^2*(\[Chi]1 + \[Chi]2)*((-2 - 2/Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 + 4*\[Chi]1*\[Chi]2 - (2 - 2/Sqrt[1 - 4*\[Nu]])*\[Chi]2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 + m*\[Nu]*(\[Chi]1 + \[Chi]2)*((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2 + 4*\[Nu]*\[Chi]1*\[Chi]2 - (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - (m*(-6*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2) + 
        (405 - 1101*\[Nu] + 29*\[Nu]^2)*((1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/96 - 
     (m*\[Nu]*(-6*Sqrt[1 - 4*\[Nu]]*(-156 + 10*\[Nu])*((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2) + 
        (12*(27 - 156*\[Nu] + 5*\[Nu]^2)*((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2))/Sqrt[1 - 4*\[Nu]] + 
        (-1101 + 58*\[Nu])*((1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]2) - 6*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*
         ((-2*\[Chi]1)/Sqrt[1 - 4*\[Nu]] - (2*\[Chi]2)/Sqrt[1 - 4*\[Nu]]) + (405 - 1101*\[Nu] + 29*\[Nu]^2)*((-4*(1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1)/Sqrt[1 - 4*\[Nu]] - 
          (4*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2)/Sqrt[1 - 4*\[Nu]]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/96 - (3969*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/
      256 - (469*m*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/48 + (21665*m*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/216 - 
     (35*m*\[Nu]*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/4 - 
     (7*m*\[Nu]*((3095 + 1206/Sqrt[1 - 4*\[Nu]] + 1889*Sqrt[1 - 4*\[Nu]] - 2*(3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu] - (3778*\[Nu])/Sqrt[1 - 4*\[Nu]] + 354*\[Nu]^2 + (1214*\[Nu]^2)/Sqrt[1 - 4*\[Nu]])*
         \[Chi]1^2 + 4*\[Nu]*(429 + 106*\[Nu])*\[Chi]1*\[Chi]2 + 4*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]1*\[Chi]2 + (3095 - 1206/Sqrt[1 - 4*\[Nu]] - 1889*Sqrt[1 - 4*\[Nu]] + 
          2*(-3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu] + (3778*\[Nu])/Sqrt[1 - 4*\[Nu]] + 354*\[Nu]^2 - (1214*\[Nu]^2)/Sqrt[1 - 4*\[Nu]])*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/864 - 
     (7*m*((-603*(1 + Sqrt[1 - 4*\[Nu]]) + (3095 + 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] - (3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]1^2 + 4*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]1*\[Chi]2 + 
        (603*(-1 + Sqrt[1 - 4*\[Nu]]) + (3095 - 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] + (-3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/
      864 - (m*\[Nu]*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/23040 - (m*\[Nu]*(2520*(-71207 + 2706*Pi^2)*\[Nu] + 325080*\[Nu]^2 + 6160*\[Nu]^3 + 
        27*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 - (m*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
        27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/1244160 + (2*m*\[Nu]*\[Chi]2*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
      ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
         (1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
     (m*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
      Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
         3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
     (\[Nu]*(3*m*(-1 - \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - \[Chi]1], 
          Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3)]] - (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
          (2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/
                3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
        m*((3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(2*(\[Delta]\[Chi] + \[Chi]1) - 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
           (-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) - (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
            Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, 
                 \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-2*(\[Delta]\[Chi] + \[Chi]1) + 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
             (4 - 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
                \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/
           (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/3) + 
   (((1 - Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*(96*m*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 - 12*m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 24*m*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 
        3*m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 6*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 
        720*m*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(12*(1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]1^2]) + 
          \[Chi]1^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]1^2])) + 4*\[Chi]1^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]1^2])) - 
          12*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 + 9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*\[Chi]1^3*\[Chi]2 + 48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*
           Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 
        32*(24*Im[m*\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
          72*Im[m*\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
          m*\[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) - 
        (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
         (-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (60*m*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(120*m^2*\[Nu]^2) + 
     ((-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1*(96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
        3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
          (17/2) + 720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
         (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
          12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
            39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
          48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
        32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
          72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
          \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
        (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*
          \[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(60*m*\[Nu]))*
    ((4*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - (5*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 + 
     (m*\[Nu]*(-2*(1 + Sqrt[1 - 4*\[Nu]]) + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/3 - m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
     2*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
     (m*\[Nu]*(-2*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1 - 4*\[Nu]*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/4 + 4*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - 
     (74*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/9 - (m*\[Nu]*(72*(1 + Sqrt[1 - 4*\[Nu]]) - (121 + 31*Sqrt[1 - 4*\[Nu]])*\[Nu] + 2*\[Nu]^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/36 - (65*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 + 
     (275*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/18 - (5*m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/6 + 
     (5*m*\[Nu]*(2*(13*(1 + Sqrt[1 - 4*\[Nu]]) - (55 + 29*Sqrt[1 - 4*\[Nu]])*\[Nu] + 14*\[Nu]^2)*\[Chi]1 + 4*\[Nu]*(3 + \[Nu])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/72 + 
     (27*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - 50*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
     (m*\[Nu]*(-6*(1 + Sqrt[1 - 4*\[Nu]])*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2) + (1 + Sqrt[1 - 4*\[Nu]])^2*(405 - 1101*\[Nu] + 29*\[Nu]^2))*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/96 - 3*m*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) - 
     2*m*\[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2) + (m*\[Nu]^2*(\[Chi]1 + \[Chi]2)*(2*(1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1 + 4*\[Nu]*\[Chi]2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 + (m*\[Nu]^2*((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2 + 4*\[Nu]*\[Chi]1*\[Chi]2 - (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/2 - (469*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/24 + 
     (21665*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/216 - (35*m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/8 - 
     (7*m*\[Nu]*(2*(-603*(1 + Sqrt[1 - 4*\[Nu]]) + (3095 + 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] - (3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]1 + 4*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]2)*
       x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^5)/864 + (m*\[Nu]^2*\[Chi]2*(x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4/
         (3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
       Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + 
     (m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
       Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
     (m*\[Nu]^2*\[Chi]2*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
         (x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
            (2/3)) - (7*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
          (1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/3))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
       ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
     (m*\[Nu]*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
        x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
      (2*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
          3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
     (m*\[Nu]*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
        ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
          x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/(2*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])))/Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
        (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
     (\[Nu]^2*(3*m*(-2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/
           (3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*\[CapitalDelta]Uint[Log[1 - \[Chi]1], 
          Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3)]] - (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
              3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
          (2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - 
             (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
             (2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
             x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           2*\[Delta]m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/
                2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*(2*(5 - 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             3*(-2 + \[Delta]\[Nu])*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + (-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 + 
             (2*(-1 + \[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
             (2*\[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(9/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
             (-2 + \[Delta]\[Nu])*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             (-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 6*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
             (2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
             2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
            \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
          ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
        (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
        (9*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
             x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*
              x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(5/2)) - 
        (3*((-2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                 (5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
              (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/(2*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - 
                \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                     (3/2))^(1/3))]))*(2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
             3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
             \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
             \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/
                3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
        m*((3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(2 - 3*\[Chi]1*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - 
             3*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) - 
          (6*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3*(2*(\[Delta]\[Chi] + \[Chi]1) - 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
           (-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^2 - (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (6*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                 (3/2)) - 9*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
           ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - (4*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
            (-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*
            Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, 
                 \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + (x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                 (5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
              (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))*(-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
           ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-2 + 3*\[Chi]1*d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], 
                 \[Delta]\[Chi]]) + (-6*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + d\[Delta]xd\[Chi][\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
                (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + 9*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, 
                 \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(4 - 
                2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
                 \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3))))/
           (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))) + (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                  (3/2))^(2/3))*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-2*(\[Delta]\[Chi] + \[Chi]1) + 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
             (4 - 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
                \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/
           (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))^2)) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
          (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
            (3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - (2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
            (3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] - (m*drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
             (2/3))^(3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
           Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
              (2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (3*m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]*
          ((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
            (3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
        3*m*(-1 - \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
         ((((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/
              (3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + (drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, 
                \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
             Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (2/3)]])/(2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) - Derivative[1, 0][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
            Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(
                2/3)]]/(1 - \[Chi]1)) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          ((((2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/(3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                    (3/2))^(5/3)) + (drISCOd\[Chi]1[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 2][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
              Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
                    (3/2))^(2/3)]])/(2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]) - Derivative[1, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
             Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
                (2/3)]]/(1 - \[Chi]1)))/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/6)))/
 (2*m*((-3*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/4 - (m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/12 + 
   (m*\[Nu]*(9 + \[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/12 + (10*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/3 - 
   (25*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/6 + (5*m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 + 
   (5*m*\[Nu]*((-2*(1 + Sqrt[1 - 4*\[Nu]]) + \[Nu])*\[Chi]1 + (-2 + 2*Sqrt[1 - 4*\[Nu]] + \[Nu])*\[Chi]2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/6 - 
   (81*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 + (57*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 + 
   (m*\[Nu]*(81 - 57*\[Nu] + \[Nu]^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/16 - (3*m*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/2 + 
   3*m*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - 3*m*\[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - 
   (3*m*\[Nu]*(-((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2) - 4*\[Nu]*\[Chi]1*\[Chi]2 + (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/4 + 
   14*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) - (259*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/9 + 
   (21*m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/4 - 
   (7*m*\[Nu]*((72*(1 + Sqrt[1 - 4*\[Nu]]) - (121 + 31*Sqrt[1 - 4*\[Nu]])*\[Nu] + 2*\[Nu]^2)*\[Chi]1 + (72 - 72*Sqrt[1 - 4*\[Nu]] - 121*\[Nu] + 31*Sqrt[1 - 4*\[Nu]]*\[Nu] + 2*\[Nu]^2)*\[Chi]2)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/72 - (675*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/32 - 
   (5*m*(-6889 + 246*Pi^2)*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/288 + 
   (5*m*\[Nu]*(10935 + 9*(-6889 + 246*Pi^2)*\[Nu] + 1674*\[Nu]^2 + 7*\[Nu]^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/2592 - 
   (65*m*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 + (275*m*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/9 - 
   (10*m*\[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/3 + 
   (5*m*\[Nu]*((13*(1 + Sqrt[1 - 4*\[Nu]]) - (55 + 29*Sqrt[1 - 4*\[Nu]])*\[Nu] + 14*\[Nu]^2)*\[Chi]1^2 + 4*\[Nu]*(3 + \[Nu])*\[Chi]1*\[Chi]2 + 
      (13 - 13*Sqrt[1 - 4*\[Nu]] - 55*\[Nu] + 29*Sqrt[1 - 4*\[Nu]]*\[Nu] + 14*\[Nu]^2)*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/18 + 
   (243*m*\[Nu]*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/4 - 225*m*\[Nu]^2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2) - 
   (9*m*\[Nu]^2*\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + (243*m*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/16 - 
   (9*m*\[Nu]^2*\[Chi]1^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/2 + 
   (9*m*\[Nu]^2*(\[Chi]1 + \[Chi]2)*((1 + Sqrt[1 - 4*\[Nu]] - 2*\[Nu])*\[Chi]1^2 + 4*\[Nu]*\[Chi]1*\[Chi]2 - (-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/4 - 
   (3*m*\[Nu]*(-6*Sqrt[1 - 4*\[Nu]]*(27 - 156*\[Nu] + 5*\[Nu]^2)*((1 + Sqrt[1 - 4*\[Nu]])*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2) + 
      (405 - 1101*\[Nu] + 29*\[Nu]^2)*((1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]1 + (-1 + Sqrt[1 - 4*\[Nu]])^2*\[Chi]2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/64 - 
   (19845*m*\[Nu]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/256 - (2345*m*\[Nu]*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/48 + 
   (108325*m*\[Nu]^2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/432 - (175*m*\[Nu]^2*\[Chi]1*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/8 - 
   (35*m*\[Nu]*((-603*(1 + Sqrt[1 - 4*\[Nu]]) + (3095 + 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] - (3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]1^2 + 4*\[Nu]*(-135 + 429*\[Nu] + 53*\[Nu]^2)*\[Chi]1*\[Chi]2 + 
      (603*(-1 + Sqrt[1 - 4*\[Nu]]) + (3095 - 1889*Sqrt[1 - 4*\[Nu]])*\[Nu] + (-3017 + 607*Sqrt[1 - 4*\[Nu]])*\[Nu]^2 + 118*\[Nu]^3)*\[Chi]2^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/
    864 - (m*\[Nu]^2*(494684 - 1376256*EulerGamma - 135555*Pi^2 - 2752512*Log[2] - 688128*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/9216 - (m*\[Nu]*(-19289340 + 1260*(-71207 + 2706*Pi^2)*\[Nu]^2 + 108360*\[Nu]^3 + 1540*\[Nu]^4 + 
      27*\[Nu]*(-494684 + 1376256*EulerGamma + 135555*Pi^2 + 2752512*Log[2]) + 18579456*\[Nu]*Log[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^4)/248832 + 
   (m*\[Nu]^2*\[Chi]2*(3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 + (\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
       (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (5*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
        (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))/2))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
     Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
        3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + 
   (m*\[Nu]^2*\[Chi]1*\[Chi]2*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - 
      x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/
    (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*Sqrt[(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
       (1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
   (m*\[Nu]^2*\[Chi]2*(\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 - x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*
       (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*
       ((3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) - 
      (7*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(4/3)*
        (1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)*
     ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(7/3)*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 
        3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
   (m*\[Nu]*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
        (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
        3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
        (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
       (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
        (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))/
    (2*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
        3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
   (m*\[Nu]*(-((\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
      2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
      ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
          (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
          3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
          (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)/
       (2*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])))/
    Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
       3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))] + 
   (\[Nu]^2*(3*m*(-3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - (\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - (3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3)/
         (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
        (15*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))/2)*
       \[CapitalDelta]Uint[Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]] - 
      (3*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
            3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*
        (2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-3 + (15*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - 
           (\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - 
           (\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))/2) + 
         3*\[Delta]m*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
           \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         3*m*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*(-4 + 3*\[Delta]\[Nu] + 5*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             (3/2) + 4*\[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
           ((-1 + \[Delta]\[Nu])*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - 
           (\[Chi]1^2*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(7/2))/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
           3*\[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(-6*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
           9*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2 - (\[Chi]1^3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2))/
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 2*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
         3*m*(1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
           \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
        ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - 
      (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
           \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*
        ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
      (9*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
           (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
           3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
           (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*(-1 + 2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
           (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))])*(2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
           \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
        ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(5/2)) - 
      (3*(-((\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
         2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            ((-3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
              (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + 
           (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], 
                \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)/(2*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
             (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]))*(2*\[Delta]m*(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
          (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 2*m*((-4 + 3*\[Delta]\[Nu])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
           2*(\[Delta]\[Chi] + (5 - 3*\[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + \[Chi]1*(-2*\[Delta]\[Chi] + 3*(-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             4 - (-1 + \[Delta]\[Nu])*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*(-\[Delta]\[Chi] + (-2 + \[Delta]\[Nu])*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^
             3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 3*m*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (1 - 4*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
           \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
          \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
        ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) + 
      m*((-9*\[Chi]1*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
         (-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) - (9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2*
          (2*(\[Delta]\[Chi] + \[Chi]1) - 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^2 + 
        (9*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(2*(\[Delta]\[Chi] + \[Chi]1) - 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         (2*(-2 + 2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))) - (2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (3*(\[Delta]\[Chi] + 3*\[Chi]1)*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) - (27*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
             \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/2))/((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*
          Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*(-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
             3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - 
        (2*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) - (2*(-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*Sqrt[(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))]) + (x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          ((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*((-3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 - 
             (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) + 
             3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)) + (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
             (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
               (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))/2)*(-4 + 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 
           (6 - 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/
         ((1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)*((-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
            (-1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))^(3/2)) - (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          (3*\[Chi]1*d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + 
           (-2*(\[Delta]\[Chi] + \[Chi]1) + 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(2*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]) + 
           (-3*(\[Delta]\[Chi] + 3*\[Chi]1)*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]] + d\[Delta]xdx[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
              (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)) + (27*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, 
                \[Delta]\[Nu], \[Delta]\[Chi]])/2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + (\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*
             (4 - 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*\[Delta]x[\[Omega], \[Phi], \[Nu], m, 
                \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]))/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)))/
         (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))) + (3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
          ((3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/2 + (3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
            (2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)) - 3*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
          (Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-2*(\[Delta]\[Chi] + \[Chi]1) + 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
           (4 - 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/
         (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
             (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))^2) - 
        (3*(Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(-2*(\[Delta]\[Chi] + \[Chi]1) + 3*\[Chi]1*\[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]) + 
           (4 - 2*(\[Delta]\[Chi] + 3*\[Chi]1)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) + (-6 + 9*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))*
              \[Delta]x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)))/
         (2*(1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2) - 3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
            (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3)))) + 
      (3*m*((\[Chi]1*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-1 - \[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^3 + 
         x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3))*
        Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/
       (2*Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (2/3)]) + (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*(-3 + (15*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/2 - 
         (\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) - 
         (\[Chi]1^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^2)/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(1/3) + 
         (3*\[Chi]1*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))/2)*
        Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
      (m*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))^(3/2)*
        (-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
            (2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]) + 
      (3*m*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*Sqrt[rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (2/3)]*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + 
         (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*Derivative[0, 1][\[CapitalDelta]Uint][Log[1 - \[Chi]1], 
         Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
            (2/3)]])/(2*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3)) + 
      (m*Sqrt[x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]]*(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (2/3))^(3/2)*((\[Chi]1*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(5/3) + rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]/
          (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*(-3*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
         3*\[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(5/2) + (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3) + 
         \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)*(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3))*
        Derivative[0, 2][\[CapitalDelta]Uint][Log[1 - \[Chi]1], Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/
           (1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^(2/3)]])/(2*rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*
        Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
           (2/3)])))/6));


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio forcing term*)


F\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=((-1 + Sqrt[1 - 4*\[Nu]])^3*(1 + Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
   (12*(\[Chi]2 + 3*\[Chi]2^3) - 3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
    (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
        \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
      48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/(120*m) + 
 ((1 - Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*(96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
    3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + 
    720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
     (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
      12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
        39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
      48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
    (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
          (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
    (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
          (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(120*m);


(* ::Subsection::Closed:: *)
(*Total mass forcing term*)


Fm[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=((-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
   (12*(\[Chi]2 + 3*\[Chi]2^3) - 3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
    (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
        \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
      48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + 
 (96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
    x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
   3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + 
   720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
    (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
     12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
       39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
     48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
   32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
     72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
     \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
   (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
   (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/60;


(* ::Subsection::Closed:: *)
(*Primary spin forcing term*)


F\[Chi]1[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(96*m*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 - 12*m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6 + 
   24*m*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 - 3*m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*
    (11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 6*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^7 + 
   720*m*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + m*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(12*(1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]1^2]) + 
     \[Chi]1^4*(-72*\[Nu] + 6*(1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]1^2])) + 4*\[Chi]1^2*(-6*\[Nu] + (1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]1^2])) - 
     12*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 + 9*(-1 + Sqrt[1 - 4*\[Nu]] - 8*\[Nu])*\[Chi]1^3*\[Chi]2 + 48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*
      Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 
   32*(24*Im[m*\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
     72*Im[m*\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)] + 
     m*\[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)) - 
   (60*m*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
   (60*m*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], 
      Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
         (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6)/(15*m^2*(1 + Sqrt[1 - 4*\[Nu]])^2) - 
 (\[Chi]1*(96*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) - 12*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*(\[Chi]1 + 3*\[Chi]1^3)*
     x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2) + 24*\[Nu]^2*\[Chi]1*(16 + 33*\[Chi]1^2)*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) - 
    3*(1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*\[Chi]1*(11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]1^2 - 10*\[Nu]*(1 + 3*\[Chi]1^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(17/2) + 
    720*\[Nu]^3*\[Chi]1^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - (1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*
     (6*\[Chi]1^4*(1 + Sqrt[1 - 4*\[Nu]] + 18*\[Nu] - 12*Sqrt[1 - \[Chi]1^2] - 12*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 
      12*(1 + Sqrt[1 - 4*\[Nu]] + Sqrt[1 - \[Chi]1^2] + Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) - 4*\[Chi]1^2*(35 + 35*Sqrt[1 - 4*\[Nu]] - 9*\[Nu] + 39*Sqrt[1 - \[Chi]1^2] + 
        39*Sqrt[(-1 + 4*\[Nu])*(-1 + \[Chi]1^2)]) + 6*(-1 + Sqrt[1 - 4*\[Nu]] + 2*\[Nu])*\[Chi]1*\[Chi]2 - 9*(-3 + 3*Sqrt[1 - 4*\[Nu]] - 4*\[Nu])*\[Chi]1^3*\[Chi]2 - 
      48*(1 + Sqrt[1 - 4*\[Nu]])*(\[Chi]1 + 3*\[Chi]1^3)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9 - 
    32*(24*Im[\[Nu]^2*\[Chi]1*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      72*Im[\[Nu]^2*\[Chi]1^3*PolyGamma[0, 3 + ((2*I)*\[Chi]1)/Sqrt[1 - \[Chi]1^2]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9] + 
      \[Nu]^2*(6*(1 + Sqrt[1 - \[Chi]1^2]) + \[Chi]1^4*(-3 + 36*Sqrt[1 - \[Chi]1^2]) + \[Chi]1^2*(70 + 78*Sqrt[1 - \[Chi]1^2]))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9) - 
    (60*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*\[ScriptCapitalF]\[ScriptCapitalH]int1SFKerr[Log[1 - \[Chi]1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
          (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^5 + 
    (60*\[Nu]^3*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^9*\[ScriptCapitalF]\[ScriptCapitalH]intS2Kerr[Log[1 - \[Chi]1], 
       Sqrt[(rISCO[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]])/(1 - \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^
          (2/3)]])/(-1 + \[Chi]1*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2))^6))/(15*m*(1 + Sqrt[1 - 4*\[Nu]]));


(* ::Subsection::Closed:: *)
(*Secondary spin forcing term*)


F\[Chi]2[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(4*((m*(-1 + Sqrt[1 - 4*\[Nu]])^3*\[Nu]^2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^6*(12*(\[Chi]2 + 3*\[Chi]2^3) - 
      3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 6*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
      (3*\[Chi]1*\[Chi]2*((1 + Sqrt[1 - 4*\[Nu]])*(-4 + 3*\[Chi]2^2) + 8*\[Nu]*(1 + 3*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
          \[Chi]2^4*(36*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(6*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
        48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60 + 
   (m*(-1 + Sqrt[1 - 4*\[Nu]])^4*\[Nu]^2*\[Chi]2*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(15/2)*
     (12*(\[Chi]2 + 3*\[Chi]2^3) - 3*\[Chi]2*(-11 + 5*Sqrt[1 - 4*\[Nu]] + 3*(-6 + 5*Sqrt[1 - 4*\[Nu]])*\[Chi]2^2 + 10*\[Nu]*(1 + 3*\[Chi]2^2))*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]] + 
      (3*\[Chi]1*\[Chi]2*(4*\[Nu]*(1 + 3*\[Chi]2^2) + (1 + Sqrt[1 - 4*\[Nu]])*(-2 + 9*\[Chi]2^2)) + 2*(6*(-1 + Sqrt[1 - 4*\[Nu]])*(1 + Sqrt[1 - \[Chi]2^2]) + 
          \[Chi]2^4*(54*\[Nu] + 3*(-1 + Sqrt[1 - 4*\[Nu]])*(-1 + 12*Sqrt[1 - \[Chi]2^2])) + 2*\[Chi]2^2*(9*\[Nu] + (-1 + Sqrt[1 - 4*\[Nu]])*(35 + 39*Sqrt[1 - \[Chi]2^2]))) + 
        48*(-1 + Sqrt[1 - 4*\[Nu]])*\[Chi]2*(1 + 3*\[Chi]2^2)*Im[PolyGamma[0, 3 + ((2*I)*\[Chi]2)/Sqrt[1 - \[Chi]2^2]]])*x[\[Omega], \[Phi], \[Nu], m, \[Chi]1, \[Chi]2, \[Delta]m, \[Delta]\[Nu], \[Delta]\[Chi]]^(3/2)))/60))/
 (m^2*(-1 + Sqrt[1 - 4*\[Nu]])^2);


(* ::Subsection::Closed:: *)
(*WF phase evolution*)


F\[Phi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=\[Omega];


(* ::Subsection::Closed:: *)
(*Total mass shift*)


F\[Delta]m[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(Fm[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]-\[Delta]m F\[Nu][\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu];


d\[Delta]mdm[\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu];


d\[Delta]md\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]m/\[Nu])


(* ::Subsection::Closed:: *)
(*Symmetric mass ratio shift*)


F\[Delta]\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(((-1+2 \[Delta]\[Nu] \[Nu]) F\[Nu][\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu]^2);


d\[Delta]\[Delta]\[Nu]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(1-2 \[Delta]\[Nu] \[Nu])/\[Nu]^2


(* ::Subsection::Closed:: *)
(*Primary spin shift*)


F\[Delta]\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=(-\[Delta]\[Chi] F\[Nu][\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]]+F\[Chi]1[\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]])/\[Nu];


d\[Delta]\[Chi]d\[Chi][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=1/\[Nu]


d\[Delta]\[Chi]d\[Nu][\[Omega]_,\[Phi]_,\[Nu]_,m_,\[Chi]1_,\[Chi]2_,\[Delta]m_,\[Delta]\[Nu]_,\[Delta]\[Chi]_]:=-(\[Delta]\[Chi]/\[Nu])


(* ::Section::Closed:: *)
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
\[Omega]critdata=Get[FileNameJoin[{directorystop,"StopHybridFixedChi.m"}]];


(* ::Subsection::Closed:: *)
(*\[ScriptCapitalF]\[ScriptCapitalI]*)


\[ScriptCapitalF]\[ScriptCapitalI]int1SFSchw = Interpolation[fluxdata1SFSchwInf,InterpolationOrder->8];
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
(*Evolution equations*)


variables={\[Omega],\[Phi],\[Nu],m,\[Chi]1,\[Chi]2,\[Delta]m,\[Delta]\[Nu],\[Delta]\[Chi]};
evolutionequations={
\[Omega]'[t]==F\[Omega][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Phi]'[t]==F\[Phi][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Nu]'[t]==F\[Nu][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
m'[t]==Fm[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]] , 
\[Chi]1'[t]==F\[Chi]1[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Chi]2'[t]==F\[Chi]2[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]m'[t]==F\[Delta]m[\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]\[Nu]'[t]==F\[Delta]\[Nu][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]],
\[Delta]\[Chi]'[t]==F\[Delta]\[Chi][\[Omega][t],\[Phi][t],\[Nu][t],m[t],\[Chi]1[t],\[Chi]2[t],\[Delta]m[t],\[Delta]\[Nu][t],\[Delta]\[Chi][t]]};
InitialConditionFormat={"\[Omega]","\[Phi]","\[Nu]","m","\[Chi]1","\[Chi]2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"};
stopcondition = {\[Omega][t] >= Min[1/((1.05rISCO["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]1","\[Chi]2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"])^(3/2)+"\[Chi]1"),1/(6.26^(3/2)+"\[Chi]1")],
\[Omega][t] >= \[Omega]crit["\[Nu]","\[Chi]1","\[Chi]2"]};
parameterspacecoverage = {\[Sqrt]((rISCO["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]1","\[Chi]2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"] x["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]1","\[Chi]2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"])/(1-"\[Chi]1" x["\[Omega]","\[Phi]","\[Nu]","m","\[Chi]1","\[Chi]2","\[Delta]m","\[Delta]\[Nu]","\[Delta]\[Chi]"]^(3/2))^(2/3))<.998,
1/(30^(3/2)+"\[Chi]1")<"\[Omega]"<Min[1/(6.06^(3/2)+"\[Chi]1"),\[Omega]crit["\[Nu]","\[Chi]1","\[Chi]2"]],
"\[Nu]"<.248,
Abs["\[Chi]1"]>.000001};


<|"IntegrationVariable"->t, "Parameters"->variables, "InspiralEquations"->evolutionequations, "InitialConditionsFormat"->InitialConditionFormat,"StopCondition" -> stopcondition, "ParameterSpaceCoverage"->parameterspacecoverage|>
