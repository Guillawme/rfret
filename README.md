# rfret: Analyze FRET Binding Data with R

This is an attempt of building an R package to analyze FRET binding data.
Given raw fluorescence data from a FRET binding experiment, this package will
ultimately allow to:

- [x] 1. plot all channels (donor, acceptor, FRET) to visually inspect raw data;
- [x] 2. average fluorescence values of technical replicates of a same experiment;
- [x] 3. correct FRET signal by subtracting signal from a blank experiment;
- [x] 4. guess initial values for the parameters of the quadratic binding equation
      (Kd, signal_min, signal_max);
- [x] 5. fit the quadratic binding equation to the data;
- [x] 6. report the value of Kd;
- [x] 7. plot the corrected FRET signal and the binding curve obtained by fitting
      the data;
- [ ] 8. optionally perform steps 2 to 6 in a batch processing mode.

A typical session would look like:

```R
my_raw_data <- read.csv("my_data_file.csv")
library(rfret)
my_plots <- inspect_raw_data(my_raw_data) # Save plots
my_plots # Display plots
my_fret_corrected <- correct_fret_signal(my_raw_data) # Correct FRET signal (subtract signal from a blank experiment)
my_initial_parameters <- guess_fit_parameters(my_fret_corrected) # Guess Kd, signal_min and signal_max
my_fit <- fit_quadratic_equation(my_fret_corrected, my_initial_parameters) # Fit model to data
my_figure <- final_figure(my_fret_corrected, my_fit) # Save final figure (data and binding curve)
my_figure # Display final plot
```
