# BiomeBGCR

Package for running Biome-BGC models in R

<!-- badges: start -->
<!-- badges: end -->

## Installation

### Current stable release

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/BiomeBGCR")
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/BiomeBGCR@development")
```

## Contributions

Please see [`CONTRIBUTING.md`](CONTRIBUTING.md) for information on how to contribute to this project.

### Package development guide

#### Windows & Linux

1. (Windows Only) Install RTools 4.0 from url
2. Open the BiomeBGCR package project file (`BiomeBGCR.Rproj`)
3. Install Rcpp package in R using command `install.packages("Rcpp")`
4. Build package

#### Linux

Debugging the C++ code using Visual Studio Code can only be achieved under linux because of a limitation with gdb and Mingw64
(see https://stackoverflow.com/questions/64723109/gdb-cant-debug-running-process-using-vs-code-but-can-through-command-line).

##### Debugging setup for Visual Studio Code

1. Install Makefile-tools package

2. Open the `BiomeBGCR/src` folder as the root folder in Visual Studio Code

3. Create a launch configuration similar to:

    ```
    {
    	"name": "(gdb) Attach",
    	"type": "cppdbg",
    	"request": "attach",
    	"program": "/usr/lib/rstudio/bin/rsession",
    	"processId": "${command:pickProcess}",
    	"MIMode": "gdb",
    	"setupCommands": [
    		{
    			"description": "Enable pretty-printing for gdb",
    			"text": "-enable-pretty-printing",
    			"ignoreFailures": true
    		}
    	]
    }
    ```

3. Set the breakpoints in Visual Studio Code

4. Launch and attach the debugger to the `rsession` process after making sure the `BiomeBGCR` package is loaded

5. Execute the R code calling the `BiomeBGCR` package
