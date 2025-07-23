(* ::Package:: *)

Paclet[
  "Name" -> "WASABI",
  "Version" -> "0.1.0",
  "MathematicaVersion" -> "10.2+",
  "Creator" -> "Black Hole Perturbation Toolkit",
  "Description" -> "A package to generate gravitational waveforms from SF theory and SF-PN Hybrids.",
  "URL" -> "https://bhptoolkit.org/WASABI/",
  "License" -> "MIT",
  "Extensions" ->
  {
    { "Kernel",
      "Root" -> "Kernel",
      "Context" -> {
        "WASABI`"
      }
    },
    { "Documentation",
      "Language" -> "English", 
      "MainPage" -> "Guides/WASABI",
      "Resources" -> 
      {
        "Guides/WASABI"
      }
    },
    {"Asset", "Root" -> ".", "Assets" -> {{"AmplitudeModels", "AmplitudeModels"}, {"InspiralModels", "InspiralModels"}}}
  }
]
