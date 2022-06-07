# RJafroc 2.0.1.9000


# RJafroc 0.1.1
* An error in the *p* value calculation that gave incorrect *p* value (possibly exceeding one) when the first modality performed better than 2nd has been fixed. Thanks to Lucy D'Agostino McGowan for pointing out the error and the fix. This error, which does not occur in Windows version of JAFROC V 4.2.1, was not noticed as in all example files the 2nd modality performed better.


# RJafroc 0.1.0
* A "shiny" based GUI has been added, accessed by the function `RJafrocGui()`. This allows a user only interested in
analyzing a data file to access the underlying code in a "user friendly" way. The GUI is similar in functionality to
that of Windows JAFROC 4.2.1 software.

* For the curve plotting functions, legend position and direction are automatically decided if they are not explicityly specified.
* The the output number of significant digits for statistical power in power table has been set to 3.
* Variance and covariance calculation error for ROI data has been fixed.
* A bug in the JAFROC data reading function that caused an error when encountering non-numeric values has been fixed.
* Floating point ratings are rounded to 6 significant digits when saving a dataset in JAFROC format.
* A bug in the plotting routine that affected plots for a single rating FROC dataset has been fixed.
* A bug in the plotting of AFROC curves for a dataset containing only non-diseased cases has been fixed.


# RJafroc 0.0.1
* Original version posted to CRAN (by Xuetong Zhai)
