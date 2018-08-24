# rfret: Analyze FRET Binding Data with R

[![Build Status](https://travis-ci.org/Guilz/rfret.svg?branch=master)](https://travis-ci.org/Guilz/rfret)
[![codecov](https://codecov.io/gh/Guilz/rfret/branch/master/graph/badge.svg)](https://codecov.io/gh/Guilz/rfret)


This R package allows you to analyze FRET binding data and produce this kind of
binding curve figure:

![Binding curve](binding-curve.png)

Given raw fluorescence data from a FRET binding experiment, you can:

1. plot all channels (donor, acceptor, FRET) to visually inspect raw data and
   find possible outliers;
2. average fluorescence values of technical replicates of a same experiment;
3. correct FRET signal by subtracting signal from a blank experiment;
4. guess initial values for the parameters of the binding model equation (`kd`,
   `signal_min`, `signal_max`);
5. fit a binding model equation to the data;
6. report the value of Kd;
7. plot the corrected FRET signal and the binding curve obtained by fitting
   the data.

This package allows batch processing and analysis of any number of datasets at
a time. It can also process and analyze fluorescence polarization or anisotropy
binding data. Support for fluorescence quenching data is also planned.

## Installation

First, install the `devtools` package, if not already present on your system:

```r
install.packages("devtools")
```

You can then install `rfret` from GitHub, right from within R:

```r
devtools::install_github("Guilz/rfret", build_vignettes = TRUE)
```

## Usage

You can access a detailed tutorial using the following commands:

```r
library(rfret)
vignette("analyzing_fret_data")
```
