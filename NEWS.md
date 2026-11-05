# BiomeBGCR 0.0.1

First release.

* The package uses Biome-BGC's license (`LICENSE`), in place of `License: NA` (#5).
* `bgcExecute()` runs Biome-BGC 4.2 simulations through the bundled C library, one site per `.ini` file, in a directory given by `path`; `bgcExecuteSpinup()` runs a spinup (#10).
* `binToText()` converts Biome-BGC's binary output to text, and `compareASCIIFiles()` compares two text outputs.
* `iniRead()`, `iniGet()`, `iniSet()` and `iniWrite()` read, change and write Biome-BGC `.ini` files.
  `iniFixPaths()` makes their paths absolute, as the C library requires; `iniMakeSpinup()` and `iniMakeSingleStep()` set a file up for a spinup or single-step run.
