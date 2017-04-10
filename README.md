# rfret: Analyze FRET Binding Data with R

This R package allows you to analyze FRET binding data and produce this kind of
binding curve figure:

![Binding curve](binding-curve.png)

Given raw fluorescence data from a FRET binding experiment, you can:

1. plot all channels (donor, acceptor, FRET) to visually inspect raw data and
   find possible outliers;
2. average fluorescence values of technical replicates of a same experiment
   (currently limited to two replicates);
3. correct FRET signal by subtracting signal from a blank experiment;
4. guess initial values for the parameters of the binding model equation (`kd`,
   `fret_min`, `fret_max`);
5. fit a binding model equation to the data;
6. report the value of Kd;
7. plot the corrected FRET signal and the binding curve obtained by fitting
   the data.

Optionally, the package also allows to perform steps 2 and 3 in
a batch-processing mode.

## Installation

First, install the `devtools` package, if not already present on your system:

```R
install.packages("devtools")
```

You can then install `rfret` from GitHub, right from within R:

```R
devtools::install_github("Guilz/rfret", build_vignettes = TRUE)
```

## Usage

You can access a detailed tutorial using the following commands:

```R
library(rfret)
vignette("using-rfret")
```
