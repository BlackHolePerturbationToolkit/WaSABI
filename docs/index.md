{% include head.html %}

<p>
 <h1 style="display:inline">WaSABI</h1> <span style="float:right;"><a href="https://bhptoolkit.org/mathematica-install.html" class = "code_btn">Install this package!</a></span>
 <h2>Waveform Simulations of Asymmetric Binary Inspirals</h2>
</p>

A package to generate gravitational waveforms from SF theory and SF-PN Hybrids. Currently available models only include quasicircular inspirals.

Among self-force models, you can find:
- $\texttt{1PAT1ea}$ and $\texttt{1PAT1R}$: two 1PA models for spinning binaries, where the primary black hole is slowly rotating.
- $\texttt{1PAT1}$: a 1PA model for non-spinning binaries.
- $\texttt{0PAKerr}$: an adiabatic inspiral model for spinning binaries.
- $\texttt{0PASchwarz}$: an adiabatic inspiral model for non-spinning binaries.

Among self-force/post-Newtonian hybrids, you can find:
- $\texttt{WaSABI-C}$: an SF+PN hybrid for spin (anti-)aligned spinning binaries.
- $\texttt{WaSABI-C\_v0.9}$: an SF+PN hybrid with a spinning primary and a non-spinning secondary.


 <h2>A workflow example</h2>

 <h3>Getting information about a model</h3>

You can list the implemented models with:

```Mathematica
ListModels[]
```
```
{"0PAKerr", "0PASchwarz", "1PAT1ea", "1PAT1", "1PAT1R","WaSABI-C","WaSABI-C_v0.9"}
```

Say you want to generate a gravitational waveform with the model $\texttt{1PAT1}$. You can query the initial condition format that this model requires using:

```Mathematica
InitialConditions["1PAT1"]
```
```
<|"M" -> _, "r0" -> _, "\[Nu]" -> _, "\[Phi]" -> _|>
```

You can query which modes are available using $\texttt{ListModes["1PAT1"]}$

```Mathematica
ListModes["1PAT1"]
```
```Mathematica
{"(2,1)", "(2,2)", "(3,1)", "(3,2)", "(3,3)", "(4,1)", "(4,2)", "(4,3)", "(4,4)", "(5,1)", "(5,2)", "(5,3)", "(5,4)", "(5,5)"}
```

Let's say you want to use it for your study. However, you know nothing about it. Then you probably first want to read how $\texttt{1PAT1}$ has been constructed. You can use $\texttt{CiteModel}[]$ to get the article in which the model has been developed:

```Mathematica
CiteModel["1PAT1"]
````
```
@article{Wardell:2021fyy,
    author = "Wardell, Barry and Pound, Adam and Warburton, Niels and Miller, Jeremy and Durkan, Leanne and Le Tiec, Alexandre",
    title = "{Gravitational Waveforms for Compact Binaries from Second-Order Self-Force Theory}",
    eprint = "2112.12265",
    archivePrefix = "arXiv",
    primaryClass = "gr-qc",
    doi = "10.1103/PhysRevLett.130.241402",
    journal = "Phys. Rev. Lett.",
    volume = "130",
    number = "24",
    pages = "241402",
    year = "2023"
}
```

<h3>Generating a waveform</h3>

Use $\texttt{BinaryInspiral[]}$ to generate a BinaryInspiral object that stores the waveforms and the phase space trajectory for a given set of initial conditions:

```Mathematica
inspiral = BinaryInspiral[<|"r0" -> 10, "M" -> 1, "\[Nu]" -> 1/10, "\[Phi]" -> 0|>, "Model" -> "1PAT1"];
````


You can then plot the gravitational waveform for the $\ell=2,m=2$ mode with:
```Mathematica
tmax = inspiral["Duration"];
Plot[Evaluate[ReIm[inspiral["Waveform"][2, 2][t]]], {t, 0, tmax}]
```
<p align="center"><img width="50%" src="waveform.png" alt="Waveform"/></p>

You can also plot the orbital trajectory:
```Mathematica
r0 = inspiral["Trajectory"]["r0"];
\[Phi] = inspiral["Trajectory"]["\[Phi]"];
ParametricPlot[{r0[t] Cos[\[Phi][t]], r0[t] Sin[\[Phi][t]]}, {t, 0, tmax}]
```
<p align="center"><img width="50%" src="orbit.png" alt="Orbit"/></p>


 <h3>The forcing functions</h3>

Our models are built upon evolving a set of first-order coupled ODEs through phase space. For a set of parameters $x_i$, the evolution equations are given on phase space by $x_i(t)=F_{x_i}(x_j(t))$. The functions $F_{x_i}$ are called the forcing functions. If you want to diagnose the dynamics, you can do so by calling $\texttt{ForcingTerms["1PAT1"]}$:


```Mathematica
FT = ForcingTerms["1PAT1"]
```

This object represents the set of forcing functions of the model $\texttt{1PAT1}$. This is a function on phase space, and it can be evaluated at a particular point on parameter space with:

```Mathematica
FT[<|"r0" -> 10, "\[Phi]" -> 0, "\[Nu]" -> 0.1, "M" -> 1|>]
```
```Mathematica
<|"d\[CapitalOmega]/dt" -> 7.86312*10^-6, "d\[Phi]/dt" -> 0.0316228, 
 "d\[Nu]/dt" -> 0., "dM/dt" -> 0.|>
```
You can plot for example the forcing term $d\Omega/dt$ as a function of the orbital separation $r_0$, and compare it with the forcing term of $\texttt{WaSABI-C}$:

```Mathematica
FT1PA = ForcingTerms["1PAT1"];
FTHyb = ForcingTerms["WaSABI-C"];

Plot[{
  FT1PA[<|"r0" -> r, "\[Phi]" -> 0, "\[Nu]" -> 0.1, "M" -> 1|>][["d\[CapitalOmega]/dt"]],
  FTHyb[<|"\[Omega]" -> 1/r^(3/2), "\[Phi]" -> 0, "\[Nu]" -> 0.1, "M" -> 1, "\[Chi]1" -> 10^-5, "\[Chi]2" -> 0., "\[Delta]m" -> 0, "\[Delta]\[Nu]" -> 0, "\[Delta]\[Chi]" -> 0|>][["d\[Omega]/dt"]]
  }, {r, 20, 6.4}, PlotRange -> All, GridLines -> Automatic, 
 Frame -> True]
```
<p align="center"><img width="50%" src="forcingterm.png" alt="Orbit"/></p>


## Citing

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16358046.svg)](https://doi.org/10.5281/zenodo.16358046)

In addition to acknowledging the Black Hole Perturbation Toolkit as suggested on the [front page](https://bhptoolkit.org) we also recommend citing the specific package version you use via the citation information on the package’s Zenodo page linked from the above DOI.

Please also make sure you cite the original authors of the models. You can retrieve that information using $\texttt{CiteModel[]}$.

For example, if you make use of the $\texttt{1PAT1}$, you should cite:
```Mathematica
CiteModel["1PAT1"]
````
```
@article{Wardell:2021fyy,
    author = "Wardell, Barry and Pound, Adam and Warburton, Niels and Miller, Jeremy and Durkan, Leanne and Le Tiec, Alexandre",
    title = "{Gravitational Waveforms for Compact Binaries from Second-Order Self-Force Theory}",
    eprint = "2112.12265",
    archivePrefix = "arXiv",
    primaryClass = "gr-qc",
    doi = "10.1103/PhysRevLett.130.241402",
    journal = "Phys. Rev. Lett.",
    volume = "130",
    number = "24",
    pages = "241402",
    year = "2023"
}
```


## Authors and contributors

Barry Wardell, Josh Mathews, Loïc Honet
