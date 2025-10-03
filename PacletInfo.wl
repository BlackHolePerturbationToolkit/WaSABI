(* ::Package:: *)

Paclet[
  "Name" -> "WaSABI",
  "Version" -> "0.9.0",
  "MathematicaVersion" -> "10.2+",
  "Creator" -> "Black Hole Perturbation Toolkit",
  "Description" -> "A package to generate gravitational waveforms from SF theory and SF-PN Hybrids.",
  "URL" -> "https://bhptoolkit.org/WaSABI/",
  "License" -> "MIT",
  "Extensions" ->
  {
    { "Kernel",
      "Root" -> "Kernel",
      "Context" -> {
        "WaSABI`"
      }
    },
    { "Documentation",
      "Language" -> "English", 
      "MainPage" -> "Guides/WaSABI",
      "Resources" -> 
      {
        "Guides/WaSABI"
      }
    },
    {"Asset", "Root" -> ".", "Assets" -> {{"AmplitudeModels", "AmplitudeModels"}, {"InspiralModels", "InspiralModels"}}}
  }
]
