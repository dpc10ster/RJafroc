## Removed `stringsAsFactors` arguments in all calls except...
* In all calls to data.frame, except in plotting functions, `PlotEmpiricalOperatingCharacteristics`, where factors are used
* This is based on my "better" or rather evolving understanding of how `data.frames` work
* In current `R3.6.3` `option($stringsAsFactors) = TRUE` (`option` is case sensitive!)
* This means that functions that don't require factors, such as `SsPowerTable()` should set   `options(stringsAsFactors = FALSE)` explicitly at the beginning of the code, in order to work in current and previous version of R (i.e., release and old-release)
* Functions that do use factors/levels such as plotting functions (`PlotRsmOperatingCharacteristics` and `FitCorCbmRoc`) should set `options(stringsAsFactors = TRUE)` in order to work in `developer` version of R, where default is `options("stringsAsFactors") = FALSE`
* This is all very confusing as I am having to dive into code written 6 years ago by someone else
* Going to test on Travis now

## Returning transpose for `foms` member of StSignificanceTesting return object
* For consistency with OR DBM MRMC 2.51, and also for cleaner output, as number of treatments is usually less than number of readers
* R CMD check passes

## Moved official good value files to Dropbox
* They generate non-portable file name warning on R CMD check

## Compared to latest official code
* `mrmc_setup_w10_July_2019.exe`; VanDyke `VanDyke.lrc` dataset; `Dropobox/IowaSoftware/VanDyke.lrc`
* `OR DBM MRMC 2.51 <beta> Build 20181028 </beta>` `miplmrmc`
* Software only runs under Windows XP
* Tried Windows 8 on different machines (iMac and MacBookPro) under `VmWare Fusion`; no luck, even after following directins twice on [website](https://perception.lab.uiowa.edu/OR-DBM-MRMC-program-manual)
* Need to compare OR ouputs - WIP
* Need to fix documentation on `StSignificanceTesting` - WIP

## Discovered error 
* For `StSignificanceTesting(dataset02, method = "ORH", option = "FRRC")` - done
* Need to put in `testthat` all combinations of `method` and `option` - done
* Different objects returned by `StSignificanceTesting` depending on choice of `option` - almost done
* Need to standardize as otherwise `RJafrocBook` is klutzy  - WIP

## Added seed specification to UtilVarComponentOR
* Added seed specification to `UtilVarComponentOR` to allow comparison with `RJafrocBook`
* Added to tests: `covEstMethod` = `jackknife`, `bootstrap` and `DeLong` for two datasets
* `dataset02` and `dataset04` converted to ROC
* Will merge to `master` so that `RJafrocBook` code passes Travis

## Fixing significance testing with independent calculations in `RJafrocBook` 
* Need to modify `RJafroc` to eliminate code duplication and improve style in all signficance testing functions - move this to issues
* I am only getting to understand it now (as I work on `RJafrocBook`)
* One reader case can now be handled by `StSignificanceTesting(rocData1R, FOM = "Wilcoxon", method = "ORH")`
* May not need `StSignificanceTestingSingleFixedFactor` which currently only handles `DBMH` method - add to issues
* Removed restriction of `StSignificanceTesting` to `J` > 1
* Will merge to `master` so that `RJafrocBook` code passes Travis

## Fixed error with `msTC` 
* Found another error in `msTC` calculation in `UtilMeanSquares`
* Was trying to be too cute for my own good (collapsing two for-loops into one)
* Discoverd error while doing first principles calculation in `RJafocBook`, DBMH chapter, so there is at least one person who benefited from `RJafrocBook`
* Changed `covEstMethod` argument to `ORH` method to lower case ("jackknife" or "bootstrap")

## Fixed issue with `optim` when flipping groups
* see issues #50 (closed) on `RJafroc/master`
* Allow a paramter of binormal model to be between -4 and 4
* Removed all vignettes; since these are now in [RJafrocBook](https://dpc10ster.github.io/RJafrocBook/)
* Removed all but one dataset (`FZ_ALL.xlsx`) from `extdata/datasets` so I dont get file size error (extdata was 2.5 MB, reduced to 1.3)


# RJafroc 1.3.2

## After work on cran2-update work
* Copied altered files from R directory (which commented out examples which were taking lots of CPU time)
* Includes a correction to `Compare3ProperRocFits.R`
* Pulled out all vignettes (these have been moved to repository `RJafrocBook`)
* This gets file size below 5 MB

## Work post acceptance of v1.3.2, as of 3/7/20
* Going back to work interrupted by having to fix the errors on R-devel, see next section below.
* This is v1.3.2.9000
* Got all tests working! Resulted in fix to `StDBMHAnalysis.R` that fixed test that I had to skip on mac for `context("SignificanceTestingAllCombinations")`. Need to get this fix (lines 45-51) over to cran2 branch as I am thinking of splitting the package up by separating the `cran2` branch as the base package `RJafroc` and `depending` on `RJafroc` for new package `RJafroc2`. This would solve the file size problems that I am running into. Just an idea.
* Current file size is 18.4 Mb! 
* Synced with `developer` branch on `GitHub` and merged with `master`.


## After email from Kurt Hornik <Kurt.Hornik@r-project.org>
* Created new branch off `cran2` 1.3.1 called `cran2-fix`
* Bumped version to 1.3.2
* RJafroc failing on Linux
    + r-devel-linux-x86_64-debian-clang 
    + r-devel-linux-x86_64-debian-gcc
    + r-devel-linux-x86_64-fedora-clang (this showed up post email)
    + r-devel-linux-x86_64-fedora-gcc (this showed up post email)
* Has to do with new default (R 4.0.0) for `options(stringsAsFactors = FALSE)`
* To recreate this problem in `R CMD check` I set `options(stringsAsFactor=FALSE)` near beginning of each plotting function (3 functions) using `data.frame()` and `levels()` to convert strings to factor levels
* To make problem go away I explicitly specified `stringsAsFactor=TRUE` in each call to `data.frame()` where necessary.
* Removed examples from FitCorCbmRoc() as they were generating excessive CPU time NOTES. Will need to add these to vignettes, later.
* Ran `R CMD check` successfully
* Ran all checks in ScriptsForCranSubmission.R
* Submitted to CRAN
* Accepted by CRAN
* See `cran2-update/master` branch for content relating to this version 


## Modified `UtilPseudoValues.R` to work with SPLIT-PLOT data
* Created simulated SP datafile `inst/extdata/toyFiles/FROC/FrocDataSpVaryK1K2.xlsx`.
* Created simulated SP dataset `datasetFROCSp` corresponding to modalities 4,5 of `dataset04`
* Update vignette `Ch00Vig5SimulateSplitPlotDataset.Rmd`.
* Modified `StORHAnalysis.R` and to work with SP dataset provided `method = "ORH"` and `covEstMethod` = "jackknife" is used
* Corrected an error in analysis; see `~Dropbox/RJafrocChecks/StfrocSp.xlsx` for details.
* Updated this file 2/19/20
* R CMD check successful ... except for file size NOTE (18.4Mb)


## Created split plot dataset; update all datasets; confirm truthTableStr and DfReadDataFile()
* v1.3.1.9000
* created simulated split plot Excel dataset from Fed dataset: `Ch00Vig5CreateSplitPlotDataset.Rmd`; confirmed it is read without error!!
* updated datasets - see `inst/FixRJafrocDatasets/ConvertDataset.R`; worked on `DfReadDataFile` function
* **Discoverd that `.xls` input does not work***; took it out as an allowed option; probably has to do with `openxlsx`
* checked `truthTableStr` with a data file that has only 1 and 3 lesions per case; was concerned about 4th dimension of `truthTableStr`; see `Dropbox/RJafrocChecks/truthTableStr.xlsx` for results of checks; note that fourth dimension will be 4, even though there are no cases with 2 lesions 
* I think I need a separate vignette on `truthTableStr` - more for my sake
* added raw excel file datasets corresponding to included datasets to `inst/extdata/datasets`; found missing file `SimulateFrocFromLrocDataset.R` - not sure why I took it out;
* Added sheet AnnK to `truthTableStr` in `Dropbox/RJafrocChecks` 
* Also tests that `OldFormat` file when read creates identical dataset to that created by `NewFormat`: basically two Excel fiies are identical except old format lacks the three extra columns; see `checkDfReadDataFile.R`
* Modified `UtilFigureOfMerit` to accomodate split plot dataset with varying number of cases for each reader
* Created a datafile `inst/extdata/toyFiles/FROC/FrocDataSpVaryK1K2.xlsx` that really exercises the `DfReadDataFile` function (case index is unsorted); resorted to data frames and sorting to successfully read it (it is used in three places - truthTableStr, NL and LL). See `inst/extdata/testUtilFigureOfMerit/*.R` for exercising files
* Need to include this file in `tests`
* Updated this file 2/10/20
* R CMD check successful

# RJafroc 1.3.1

## Extended dataset object structure
* Bumped version number to 1.3.1 after corrections to DESCRIPTION file
* Version on CRAN is 1.3.1

## Extended dataset object structure
* Bumped version number to 1.3.0 as I am moving towards a CRAN submission
* Lost `development` branch while using `GitHub`; decided to do `Git` manually
* Why is `.gitignore` not working?
* Additional members added 12/27/2019 by DPC
* Ann discovered bug in code: does not handle single reader properly
* Ann uncovered another bug in code: did not handle diseased cases first in Excel Truth sheet
* Both bugs have been fixed 
* Make it easier to correlate the NL and LL values with those in the Excel file and catch data entry errors in `DfReadDataFile()`
   + design = design,
   +  normalCases = normalCases,
   +  abnormalCases = abnormalCases,
   +  truthTableStr = truthTableStr
* Need to update all datasets and check all occurences where `DfReadDataFile()` is used
* Included my own CPP coded wAFROC plot function. Learning a lot form Dirk's book website https://teuder.github.io/rcpp4everyone_en/.

## Split plot dataset
* Modifications to `DfReadDataFile()` to allow for split plot datasets completed.
* Must use `newExcelFileFormat = TRUE` as otherwise the code defaults to the old Excel format.
* The new format includes more stringent tests, IMHO, to catch data entry errors:
* `TruthTableStr` is created in `checkTruthTable()` which is used in subsequent read NL and LL worksheets. 
* Work to be done to include split plot in significance testing.
* Corrected `dataset03` which had `-Inf`s for 1-ratings; need to check other ROC data files.
* Added vignettes describing data format using toyFiles and use of `DfReadDataFile()`.
* Corrected error in old DfReadDataFile function.
* Passes R CMD check with file size note.

## Error in MS_TC corrected
* Noted by Erin Greco
* The missing "-1": `UtilMeanSquares()` line 88 `msTC <- msTC * J/((I - 1) * (Ktemp - 1))` has been corrected
* Reset goodValues values in `test-StSignificance-testing.R` at line 128

## Extended plotting function to LROC data
* `PlotEmpiricalOperatingCharacteristics()` now accepts ROC, FROC **and** LROC datasets. 
* Simplified code.
* Included in unit tests.
* Added `legend.position` argument to allow better positioning of legend.

## Added FROC sample size vignettes and functions
* `Ch19Vig1FrocSampleSize.Rmd`: Compares FROC power to ROC power.
* `Ch19Vig2FrocSampleSize.Rmd`: FROC power calculation for a number of situations.
* `SsFrocNhRsmModel()`: constructs an RSM-based model, which allows one to relate an ROC effect size to a wAFROC effect size, and returns parameters of model to allow FOM estimation for ROC and wAFROC. Following functions are used to calculate the lesion distribution and lesion weights arrays:
* `UtilLesionDistribution`: renamed to `UtilLesionDistr`
* `UtilLesionWeightsDistr`:

## Significance testing functions
* `StSignificanceTesting()`: corrects errors affecting `method = "ORH"` and `covEstMethod = "Jackknife"`. I messed up while trying to simplify XZ code. It calls:
* StDBMHAnalysis(): 
* StORHAnalysis(): 
* Ran Windows `JAFROC` on virtual Windows 8 machine and saved results (inst/VarCompDiscrepancy/includedFrocData_Inferred_ROC.txt) to validate current significance testing functions. Included unit tests in `tests/testthat`.
* Ran first XZ CRAN upload (version 0.0.1) code (`StOldCode.R`) to compare against current significance testing code. Included unit tests in `tests/testthat`.
* test-St-Compare2JAFROC.R: compares current code to Windows JAFROC results.
* test-St-Compare2Org.R: compares current code to RJafroc 0.0.1.
* test-St-CompareDBM2OR.R: compares current code DBM to current code OR results, when appropriate.

## CAD and LROC 
* `gpfMyFOM()`: interpolation error in LROC PCL and ALROC FOMs. Hand calculations showed that the `approx` function did not work for small datasets. Wrote my own simple interpolation code. See `LrocFoms()` in `gpfMyFOM.R`. See **ChkLrocFoms.xlsx** in `inst/StSigTesting` for details on hand calculation of LROC FOMs. 
* LROC FOMs now apply to UtilFigureOfMerit() and all significance testing functions. **These changes only affected values at small `FPFValue`, 0.2 or less.**
* Most FOM related functions now accept `FPFValue` to accommodate LROC datasets.
* `StSignificanceTestingCadVsRadiologists()`: CAD results updated (only values for `FPFValue` 0.2 or less were affected); see `CadFunctionTests.R` in `inst/CadTesting`. See **CadTestingNicoData.xlsx** in `inst/CadTesting`. Included unit tests in `tests/testthat`.
* `StSignificanceTestingCadVsRadiologists()`: cleaned up and now runs all FOMs.
* `SimulateLrocDataset()`: FROC to LROC simulator based on RSM. Could be used for NH testing. RSM can now predict all paradigm data.
* `DfFroc2Lroc`(): Simulates an "AUC-equivalent" LROC dataset from an FROC dataset. This is neat!
* `DfLroc2Froc`(): Simulates an "AUC-equivalent" FROC dataset from an LROC dataset.
* `DfLroc2Roc`(): convert LROC dataset to ROC dataset.
* An error in `dataset2ratings()` has been corrected.

## Variance component input
* `SignificanceTesting` functions now accept variance components, without having to specify a dataset.

## Other affected functions and new functions: 
* `UtilVarComponentsDBM()`: 
* `UtilVarComponentsOR()`:
* `SsPowerGivenJKDbmVarComp`:
* `SsPowerGivenJKOrVarComp`:
* `SsSampleSizeKGivenJ`:
* `SsPowerGivenJK`:
* `StSingleTreatmentRandomReader`:
* Ensured that `FPFValue` argument immediately follows `FOM`, where applicable.

## Needs further testing 
* `StSignificanceTestingSingleFixedFactor`:

## Extensions needed 
* The `addPlot` routine in `StSignificanceTestingCadVsRadiologists` has been renamed to `CadVsRadPlots()`. It should be deprecated in future as `PlotRsmOperatingCharacteristics()` has more consistent visual output (and capabilities like handling lists of treatments and readers). 
* Need a function that checks validity of FOM for dataset: `isValidFom`?
* Need to compare predicted curves for LROC and FROC data: does `SimulateLrocDataset()` predict **both** flattening out of LROC plot and wAFROC going to (1,1)?
* Split plot analysis


# RJafroc 1.2.0
* Corrected all references to package name to `RJafroc` (note capitalization)
* Checked downstream dependencies - none as of July 23, 2019: revdep("RJafroc") yields character(0)
* Corrected error that was causing `Solaris` failure (Peter Philips)
* Corrected error in `UtilPseudoValues.R` that was caught by `testthat`
* Corrected `StSignificanceTesting.R` that was caught by `testthat` (Peter Philips)
* `R CMD check` generates `testthat` failure when run under `RStudio`, see following output, but not when run as
`devtools::test()`:

````
* checking tests ...
 Running ‘testthat.R’ [158s/160s]
 ERROR
 Running the tests in ‘tests/testthat.R’ failed.
 Last 13 lines of output:
   Component "Source": Attributes: < Component "levels": 3 string mismatches >
   List member = 2, Dataset = dataset02, FOM = Wilcoxon, method = DBMH
   
   ── 2. Failure: SignificanceTestingAllCombinations (@test-significance-te
   CurrentValues[[listMem]] not equal to GoodValues[[listMem]].
   Component "Source": Attributes: < Component "levels": 3 string mismatches >
   List member = 2, Dataset = dataset05, FOM = HrAuc, method = DBMH
````

# RJafroc 1.1.0
* Added `travis-ci` testing after each push; and build passing badges, etc. 
* Removed dependence on `caTools` package, which was not being supported; extracted function `trapz()` from it
   and inserted directly into `gpfMyFOM.R` - see comments in that file of what led to this
* Removed dependence on `xlsx` package, which requires `rJava` and `JAVA`, replaced with dependence on
   `openxlsx` package. Was having difficulty installing `rJava` correctly after each `OSX` or `R` update.
* Corrected errors in `UtilOutputReport.R`.
* Fixed bug in `UtilOutputReport` that was preventing overwriting of existing output file, even
   when the user keys "y" in response to prompt
* Added correlated contaminated binormal model, `CORCBM`, fitting and related functions to
   make package current with 2017 CORCBM publication.
* Fixed error in `PlotEmpiricalCharacteristics.R` that was giving incorrect plots for other than
   `ROC` and `wAFROC` plots
* Added `ChisqrGoodnessOfFit` function, replacing 3 functions
* Cleaned up plotting code; using one function `genericPlotROC.R` instead of 3 functions
* Updated results of CBM, PROPROC and RSM fitting after discovering error in df
   calculation in RSM chisquare statistic; book results are wrong; only 2/236 fits
   yield a valid chisquare statistic
* Renamed `ExampleCompare3ProperRocFits()` to `Compare3ProperRocFits()`
* Corrected overwriting error in value returned by `Compare3ProperRocFits()`
* Added two vignettes: `QuickStartDBMH` and `QuickStartDBMHExcelOutput`
* Checked downstream dependencies - none as of Nov 11, 2018: `revdep("rjafroc")` yields `character(0)`

   
# RJafroc 1.0.2
* StSignificanceTestingCadVsRadiologists was not working for different numbers of readers.
   As noted by Alejandro, the number of readers was hard coded. Fixed this and extended
   DfExtractDataset to include LROC datasets.
* Removed function `SsFROCPowerGivenJK`: FROC power is implemented in Online
  Appendix Chapter 19 (see email exchange with Kota Aoyagi)
* This version installed on SOLARIS!
   

# RJafroc 1.0.1
* Package was not installing on Solaris - overloading errors. Changed sqrt(2) in 
   RsmFuncs.cpp to sqrt(2.0). However, Solaris is incompatible with ggplot2; 
   so will recommend that Solaris version not be distributed on CRAN.
* Sorry, but I'm not sure what's different between the CRAN Solaris machine and 
   R-hub's Solaris machine. You could prepare a new package submission for CRAN 
   with the caveat that, since you do not have access to a Solaris machine, 
   your fix is speculative and may yet fail to compile on the CRAN Solaris machine.
* The CRAN Repository Policy (https://cran.r-project.org/web/packages/policies.html) 
   also states:  
   _Package authors should make all reasonable efforts to provide cross-platform 
   portable code. Packages will not normally be accepted that do not run on at 
   least two of the major R platforms. Cases for Windows-only packages will be 
   considered, but CRAN may not be the most appropriate place to host them. So 
   you could in theory argue your case that your package does not support Solaris, 
   and request that CRAN not distribute your package on that platform. But given 
   that the issue you're bumping to is (not) documented explicitly in the R manuals, 
   I'm not sure how much success you would have._


# RJafroc 1.0.0 
* Renamed functions for better organization; 
* Removed shiny GUI interface
* Support for LROC datasets and cross-modality datasets
* CAD vs. radiologist analysis, both single modality and dual modality


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
