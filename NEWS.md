# RJafroc 2.0.1.9000


## Corrected sample size vignettes 4/12/21 and 4/14/21
* `Ch19Vig1FrocSampleSize.Rmd` and `Ch19Vig2FrocSampleSize.Rmd`
* Fixed `SsFrocNhRsmModel.R` to not return lesion distribution and weights
* Fixed vignettes that were using the old structure returned by sig. testing function
* Fixed 2 FROC SS vignettes; fixed `SsFrocNhRsmModel.R` to do binning internal to the function
* Added a test for `SsFrocNhRsmModel()`.
* Updated `Rcpp` to 1.0.6. NOTE: version 1.0.6.6 created horrendous errors - R aborts.


## Intrinsic vs. physical RSM parameters 4/2/21
* All C++ functions take physical parameters 
* Rest take intrinsic parameters (2 exceptions, like `RSM_xROC` and `RSM_pdfN`)
* Cleanup:
    + `PlotRsmOperatingCharacteristics.R`, 
    + `UtilAnallyticalAucsRSM.R`, 
    + `rsmFormulae.R`
    + affected related test files: `test-RSM-formulae.R` and `test-model-aucs.R`
    + Used `goodValues` to check that nothing has changed


## Moved to `RJafrocBook` 1/3/21
* Vignette `Ch10Vig1QuickStart` 
* Vignette `Ch10Vig2QuickStart`
* Function `Compare3ProperRocFits.R`
* Associated files in `inst`: `MRMCRuns` and `ANALYZED`


## Added functions RSM_pdfN and RSM_pdfD 
* Needed for Swets predictions in book; but of general utility.
* Other new functions added of type `RSM_*()`
* Need to vectorize all Cpp functions; no need to carry both scalar and vector types.
* Add `tests` for new functions `RSM_*()`


## CRAN submission process
* Version 2.0.1
* This is on `cran3` branch.
* Steps to reduce file size to less than 5 Mb:
    + Removed `tests` and `vignettes` (this needs to be done on all computers I am using).
    + Removed all files from `ints/MRMCRuns` except `Tony`, the one that is used in an example.
    + Removed `CrossedModalities.xlsx` and references to it.
    + Removed `DfReadLrocDataFile.R` and `findings.txt`. Ran `devtools::document()` to fix `NAMESPACE`.
    + Removed `RoiData.xlsx`.
* Otherwise identical to `developer` and `master` as of 12/8/20.
* `testthat` failure on Ubuntu developer is resolved, see `master` branch: `checkEnvironment = FALSE` in `expect_equal()` on `ggplot2` comparisons to `goodValues`.
* On CRAN


## Simplify handling of lesion distribution and lesion weights
* Motivation: basically only need to specify two 1D arrays: `lesDistr` and `relWeights`.
* Since these are involved in the C++ calls, cannot totally eliminate the 2D arrays; and they are also useful for printouts.
* Eliminated two functions and much simplification:
* Eliminated  `UtilSpecifyLesionWeightsDistr`
* Eliminated `Convert2lesDistr`
* Simplified system for specifying lesion distribution `lesDistr`
* Simplified system for specifying lesion weights distribution `relWeights`
* Affected many functions
* Basic idea is to keep the complexity of weights etc. concealed from the user
* Passed tests; 12/3/20

## Working on analytical AUCs from RSM
* Motivation: could obviate the long simulations in CAD optimization chapter in book
* Added `UtilSpecifyLesionWeightsDistr()` which is distinct from `UtilLesionWeightsDistr()`, as the latter works on datasets.
* Observed that for equal weights AFROC and wAFROC analytical AUCs are identical. Need to think this out.
* Changed function name from `UtilAucRSM()` to `UtilAnalyticalAucsRSM()` to distinguish from `UtilFigureOfMerit()` which works on datasets.
* Added function `Convert2lesDistr()` to save me time converting from 1D lesion distribution to 2D version which is the standard in the rest of the package. It is currently not called anywhere.
* Added `zeta1` dependence to `UtilAnalyticalAucsRSM()`.
* Nov 30, 2020

## Update `StSignificanceTestingCadVsRad()`
* Shortened the name of the function as shown above
* Needed to standardized output to avoid klutzy code in `RJafrocBook`
* Fixed variance components returns in `DualModalityRRRC()`
* Fixed CI returned object - difference was incorrect, changed sign

## Modified `seed` behaviour, no need for `SimulateFrocDatasetNoSeed()`
* Needed for compatibility with plots in `14-froc-meanings-xx.Rmd` chapter.
* Otherwise different random numbers are generated and it throws all the plots off.
* If `seed` is not supplied, then `SimulateFrocDataset()` behaves as before the Nov 17 change
* Nov 20. 2020

## Added `seed` to SimulateFrocDataset()
* Ability to specify `seed` in order to reproduce FROC datasets.
* In book chapter 13- on effect of zeta1 on FOM and finding the zeta1 that maximizes wFROC AUC.
* Had to fix several `test` files.
* **Consider adding `seed` to other simulation functions.**
* Nov 17, 2020.

## Fixed errors reading FROC file with no non-diseased cases
* Toy file with no non-diseased cases: `frocLocatClass.xlsx`.
* Symptom: `UtilFigureOfMerit`, with "wAFROC1" FOM failed in C++ code in function `double wAFROC1()` with message `Not compatible with requested type: [type=character; target=double]`

* The problem was traced to `ReadJAFROCOldFormat.R` (I was using `OldExcelFileFormat`) which was returning `NL` and `LL` as characters, not numerics.
* Fix: convert `NL` and `LL` from character using `as.numeric`.
```
NLRating <- as.numeric(NLTable[[4]])
LLRating <- as.numeric(LLTable[[5]])
```

* Then I tried `NewExcelFileFormat`, which gave following error:
`stop("Error in reading LL/TP table")`
Replaced code in `ReadJAFROCNewFormat.R` as follows:
```
if (is.na(tt2)) next else { # this is the change
  if (tt2 != 1)  stop("Error in reading LL/TP table") else
  # the is.na() check ensures that an already recorded mark is not overwritten
  if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
}
# if (is.na(tt2)) stop("Error in reading LL/TP table") else {
#   if (tt2 != 1)  stop("Error in reading LL/TP table") else
#     # the is.na() check ensures that an already recorded mark is not overwritten
#     if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
# }
```
Also replaced

```
el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l]) - 1
```

with

```
if (K1 != 0) {
  # this gives 0,1,2,..,max num of lesions
  # which includes zero, hence the minus 1
  el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l]) - 1
} else {
  # this gives 1,2,..,max num of lesions
  # which does not include zero, hence no minus 1
  el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l])
}

```
* Added a test comparing FOMs with new and old formats for wAFROC and K1 = 0 file
* Added a test comparing FOMs with new and old formats for wAFROC and K1 != 0 file
* Added to tests: file `test-UtilFigureOfMerit.R`
* Ran R CMD check


## Fixing non-character input error
* Finished October 23, 2020.
* Fixed error in `DfReadDataFile.R` in function `checkTruthTable()`; this bug discovered in working with HUGE one reader dataset.
* For single reader or single modality dataset, if input is not converted to character, then error results in `I <- length(strsplit(modalityIDCol[1], split = ",")[[1]])` and similar expression for `J`.
* Fix is to use `as.character` as in:
```
  readerIDCol <- as.character(truthTable$ReaderID) # bug fix to avoid non-character input error below
  modalityIDCol <- as.character(truthTable$ModalityID) # do:
```


## Simplified plotting routines
* Finished October 2, 2020.
* Necessitated by work on CAD vs. RAD plots in `RJafrocBook`.
* `PlotEmpiricalOperatingCharacteristics.R`.
* Especially function `gpfPlotGenericEmpiricalOperatingCharacteristic`.
* The revisions are tested via `R` files in `inst/fixPlots`.
* I am following the help page of `ggplot2`
* Did away with `with` function usage in this function: hard to tell what is going on and the help page on this function seems to discourage this type of usage in packages
* But, get silly NOTE about undefined global variables: `genAbscissa`, `genOrdinate`, `Reader`, `Modality`.
* These are members of a dataframe, so I dont see why they are visible at global level.
* Does not happen with the other dataframes in this packages.
* So I assume it is a `ggplot2` related issue.
* I solved it by initializing these at the very beginning of the `gpfPlotGenericEmpiricalOperatingCharacteristic` function to `NULL`s.

## Added extensive comments in StORSummaryFRRC.R
* Added extensive comments in StORSummaryFRRC.R on how I am calculating `CI` for individual treatments averaged over readers;
* The method was "reverse-engineered" from `inst/Iowa/VanDyke.txt`, as I cannot find a better reference for the equations used.
* This created debugging problems as break points did not work; added # at the *end* of each inserted commented line, 86-103; this seemed to solve problem;
* passes R CMD check


## Updated package documentation and Ch00 Vignettes
* Commented out examples in `DfSaveDataFile.R` as it creates  non-standard files in `doc` directory; this did not happen before the R update.
* Also, the ggplot output structure appears to have changed; had to regenerate goodvalues in `test-PlotEmpiricalOperatingCharacteristics.R`.
* Need to update documentation and DESCRIPTION.
* Need to use artificial intelligence instead of CAD as this is the new thing.
* Moved split-plot vignettes to Dropbox; cleaned up Ch00 vignettes - `RocFctrl`, `FrocFctrl`, `RocSpA` and `FrocSpA`
* Moved description of other's data formats to Dropbox.


## Added stats to ORAnalysisSplitPlotA
* `ORAnalysisSplitPlotA` returns `list` containing `FOMs`, `ANOVA` and `RRRC`
* Implements formulae in Hillis 20-14 paper on page following Table VII.
* Tested with toy file and collaborator dataset
* Merged to `developer` branch and deleted `SplitPlotA` branch
* ran `pkgdown::build_site()`
* Updated R and RStudio


## Handling of FOMs that depend on single-truth-state cases
* Some confusion in my mind about handlling of normal case only or abnormal case only FOMs, like `MaxNLF`, `MaxLLF`, `HrSe`, `HrSp`, etc. Resolved after studyin XZ code, StOldCode.R. The handling is shown in UtilPseudoValues.R. I believe it is now correct. For `MaxNLF`, `HrSp`, and `ExpTrnsfmSp` the relevant number of cases is `K1`, for `MaxLLF` and `HrSe` it is `K2` and for all the rest it is `K`.   


## Revised UtilPseudoValues 8/7/20
* More compact code handles all FOMs - no exceptions, as before, for MaxLLF, etc. Extensive simplification to accomplish handling of different FOMs.
* Modified `FOMijk2VarCovSpA` and `FOMijk2VarCov` to accept a `varInflFactor` logical argument, allowing jackknife, bootstrap and DeLong - based estimates to be more compactly handled.
* `UtilPseudoValues` handles all designs without resorting to separate functions for FCTRL, SPLIT-PLOT-A and SPLIT-PLOT-C
* Modified `frocSpA` toy dataset to stress code (unequal numbers of abnormal and normal cases, multiple marks on NL and LL worksheets, etc)
* Checked `dataset05` `MaxLLF` `MaxNLF` vs. JAFROC
* See below, note added today relating to handling of `descriptions$fileName`. This fixed problem with `expect_equal()` failing depending on how the `goodValues` were generated - from R `command line` vs. `Run Tests`. Also, in creating a dataset object, where appropriate, `fileName` <- "NA" instead of `fileName` <- `NA`; the latter generates a `character expected` error when an attempt is made to strip path name and extension in `convert2dataset` and `convert2Xdataset`.
* Updated and reorganized tests
* Implemented SPLIT-PLOT-A analysis for unequal numbers of readers in the two groups. The formulae (from Hillis 2014) are modified to use treatment-specific components, i.e. `Var_i`, `Cov2_i` and `Cov3_i`. The modified formulae reduce to Hillis' formulae when the number of readers in each group are identical. Communicated results to collaborator.
* Corrected error in handling of `MaxNLFAllCases` FOM; see comments in `UtilMeanSquares()`; regenerated one `goodValue` file.


## Read real SPLIT-PLOT-A dataset
* Did not find any data entry errors in `/toyFiles/FROC/1T3Rvs4R.xlsx`
* Simplified `rdrArr` handling: this is done in `checkTruthTable`, where SPLIT-PLOT-A is handled separately;
* Added source `fileName` to `descriptions$fileName` field of `DfReadDataFile()` return; this will keep a record of how the dataset was generated
* Note added 8/7/20: above change created problems passing tests in R CMD check, as long file names may not agree; simplified to remove all but the file base name; regenerated many `goodValues` files


## Update for reading SPLIT-PLOT-A data files
* After emails with collaborator, need for this type of analysis.
* Need to comment `DfReadDataFile.R` and `ReadJAFROCNewFormat.R` and add more checks in the code for illegal values.
* Indiscriminate sorting introduced problems; now, sorted `caseID` column is used now in only 3 places to find the correct case indices, where normal cases are ordered first, regardless of how they are entered in the `Truth` worksheet:

```
k <- which(unique(truthTableSort$CaseID) == truthTable$CaseID[l])
k <- which(unique(truthTableSort$CaseID) == NLCaseIDCol[l])
k <- which(unique(truthTableSort$CaseID) == LLCaseIDCol[l]) - K1
```

## Tests for UtilOutputReport
* Included tests for `UtilOutputReport()` for text output only
* Could not get version that compared actual outputs to work in R CMD check.
* Only works at command line (running tests one at a time)
* Failing code is commented out


## Renamed SP datasets 7/9/20
* Renamed the split plot dataset to datasetFROCSpC
* The C stands for Table VII Part (C) in Hillis 2014
* Need to distinguish between different types of split-plot datasets
* What I was doing so far was split-plot-c
* The new analysis requires split-plot-a


## Code for checking for non-sequential lesionIDs in TRUTH 7/8/20
* Updated all tests to display contexts more consistently.
* Moved unnecessary files from `inst` to `Dropbox`.
* Main work was on `DfReadDataFile.R` and testing code for non-sequential `lesionID`s in `TRUTH` worksheet.
* Thinking about modifications to handle split-plot data along the lines of HYK datasets
* Also learning about `grep`.  


## Implemented extensive testing comparing `RJafroc` to Iowa software
* See `test-StCompare2Iowa.R` in `testthat` directory.
* Does line by line comparison of `RJafroc` to results of `OR-DBM MRMC 2.51 Build 20181028` for VanDyke and Franken datasets.
* These, and many other tests, are run automatically every time the `RJafroc` software is checked using `R CMD check`.


## Note on discrepancy vis-a-vis Iowa software
* Noted was a discrepancy between `Var(R)` and `Var(TR)` values reported by `OR-DBM MRMC 2.51 Build 20181028` and `RJafroc` for Franken dataset.
* Their code does not implement the required `max(Cov2 - Cov3, 0)` constraint while `RJafroc` does.
* `RJafroc` reports `VarTR` = -0.00068389146 while their code reports `VarTR` = -0.00071276.
* Specifically, `msTR - Var + Cov1 + max(Cov2 - Cov3, 0) = -0.00068389146` and `msTR - Var + Cov1 + Cov2 - Cov3 = -0.00071276`.
* This also affects the `VarR` values (see block of comments in `UtilORVarComponentsFactorial` near line 161). `Cov1`, `Cov2`, `Cov3` and `Var` are the same between both codes.
* I am aware that these discrepancies do not affect sample size estimates, but can cause confusion for the code maintainer and the end user.


## Updated sample size routines
* 3 Papers by Hillis et al on SS estimation for ROC studies: 2004, 2011 and 2018
   + Hillis SL, Berbaum KS (2004). Power Estimation for the Dorfman-Berbaum-Metz Method. Acad Radiol, 11, 1260--1273.
   + Hillis SL, Obuchowski NA, Berbaum KS (2011). Power Estimation for Multireader ROC Methods: An Updated and Unified Approach. Acad Radiol, 18, 129--142.
   + Hillis SL, Schartz KM (2018). Multireader sample size program for diagnostic studies: demonstration and methodology. Journal of Medical Imaging, 5(04).
* The sample size routines have been updated to reflect the most recent publication. The equations in the code now have documentation as to their source(s).
* The routines have also been checked using the Java calculator provided on the U of Iowa website (`pss20190918.jar`).
* Briefly, the procedure defaults to the OR method, even when DBM variance components are provided, in line with the recommendations in the 2011 paper. For continuity with prior work a `LegacyCode` flag is provided to force execution of the original DBM method (2004 paper).
* Added a few tests using the Franken dataset which yields negative `Var(TR)`. Noticed a small difference in predicted power between forced DBM (0.78574588) and OR methods (0.8004469).
* Updated source of equations in `SsPowerGivenJKDbmVarCom`.
* The code rewrite was conducted on a new branch, `UpdateSsRoutines` off the `developer` branch.
* The changes were merged to the `developer` branch and then to the `master` branch.


## Major reorganization of `dataset` structure
* Instead of varying lengths for ROC/FROC, LROC and SPLIT-PLOT, all `datasets` now are `lists` of length 3, with each member (`ratings`, `lesions`, `descriptions`) consisting of sub-lists:
  + `ratings` contain 3 elements: `$NL`, `$LL` and `$LL_IL`.
  + `$lesions` contains 3 elements: `$perCase`, `$IDs` and `$weights`.
  + `$descriptions` contains 7 elements: `$fileName`, `$type`, `$name`, `$truthTableStr`, `$design`, `$modalityID` and `$readerID`.
* This considerably simplified the handling of different types of datasets.
* The version number will be bumped to 2.0.1 on final submission to CRAN.
* Since there are no downstream dependencies, I feel this big change is justified at this time. It will make it easier for me to maintain the code.
* The code rewrite was conducted on a new branch, `SimplifyDatasets` off the `developer` branch.
* The changes were merged to the `developer` branch and then to the `master` branch.


## Major simplifications to all significance testing `St` functions
* Separated `RRRC` branches etc to separate files; likewise for DBM and OR branches, now the files are much shorter and easier to maintain
* Changed returned data structure to a `list` of `dataframes`, see next comment; this makes for much cleaner and easier printing
* Consistent returned objects from all `St` functions: `list` with data frames `FOM`, `ANOVA`, `RRRC`, `FRRC` and `RRFC`.
* Output now **closely follows** that of Iowa software OR-DBM MRMC 2.51 which I ran using VmWare, Windows XP; Iowa software did not run on Windows 8 on two different machines (see below)
* Ran detailed comparison to OR-DBM MRMC 2.51 and coded the checks in  `tests/testthat/test-St-Compare2Iowa.R` for VanDyke dataset
* Also visually compared `RJafroc` results against OR-DBM MRMC 2.51 for `dataset04` converted to ROC (see Iowa code results in `inst/Iowa/FedRoc.txt`);
* Shortened `UtilOutputReport` *considerably*, by using `print(dataframe)` instead of reading values from `list` or `dataframe` variables and then using `sprintf` with unreadable C-style format codes
* Shortened `SPLIT-PLOT` analysis by returning `Cov2` = `Cov3` = 0 instead of `NA`
* Confirmed that this gives same results as the version in `master` branch
* Added vignettes back, rebuilt website
* Passes all OSX checks except for file size NOTE: checking installed package size ... NOTE installed size is 16.7Mb; sub-directories of 1Mb or more:doc 12.7Mb;extdata 1.3Mb
* Marked regions of code **requiring further** inspection by TBA - handle this next
* Will merge this code into `master` branch if it passes Travis


## Replaced stringsAsFactors = FALSE everywhere data.frame is used except ...
* Except in `PlotEmpiricalOperatingCharacteristics`, where `factors` and `levels` are used
* Must specify this anytime the variable is used in a `test` as otherwise different versions will give `factors` or `characters` and not match those in goodValues folder
* After old versions are phased out, this can be safely removed, as all versions will have this as the default
* Setting `options(stringsAsFactors = FALSE)` at beginning of a function, e.g., `StSignificanceTesting`, passes this option onto called functions, e.g, `StDBMHAnalyis`
* Use `getOption("stringsAsFactors")` to determine state of this option; must restart `R` to obtain default value (`TRUE` on the release version); calling a function that sets it to `FALSE` keeps the new value after exiting from function; must restart `R` again to get proper default.


## After repeated Travis failures
* same issue; have to specify `stringsAsFactors` explicitly for each `data.frame` call, due to different defaults in different verions of `R`
* Cannot set to TRUE at beginning of function, as in `options("stringsAsFactors" = TRUE)`, **as this is `deprecated`**
* Basically undid all changes in next note (see below)
* passes R CMD check on OSX and Travis checks


## Removed `stringsAsFactors` arguments in all calls except...
* In all calls to data.frame, except in plotting functions, `PlotEmpiricalOperatingCharacteristics`, where factors are used
* In current `R3.6.3` `option($stringsAsFactors) = TRUE` (`stringsAsFactors` is case sensitive!)
* This means that functions that don't require factors, such as `SsPowerTable()` should set   `options(stringsAsFactors = FALSE)` explicitly at the beginning of the code, in order to work in current and previous version of R (i.e., release and old-release)
* Functions that do use factors/levels such as plotting functions (`PlotRsmOperatingCharacteristics` and `FitCorCbmRoc`) should set `options(stringsAsFactors = TRUE)` in order to work in `developer` version of R, where default is `options("stringsAsFactors") = FALSE`
* This is all very confusing as I am having to dive into code written 6 years ago by someone else
* Going to test on Travis now


## Return `transpose` for `foms` member of `StSignificanceTesting` return object
* For consistency with OR DBM MRMC 2.51, and also for cleaner output, as number of treatments is usually less than number of readers
* Note added 5/30/20: this was undone; transpose is no longer used


## Moved official good value files to Dropbox
* They generate non-portable file name warning on R CMD check
* Moved VanDyke results to inst/IowaResults/VanDyke.txt


## Compared to latest official code
* `mrmc_setup_w10_July_2019.exe`; VanDyke `VanDyke.lrc` dataset; `Dropobox/IowaSoftware/VanDyke.lrc`
* `OR DBM MRMC 2.51 <beta> Build 20181028 </beta>` `miplmrmc`
* Software only runs under Windows XP
* Tried Windows 8 on different machines (iMac and MacBookPro) under `VmWare Fusion`; no luck, even after following directions twice on [website](https://perception.lab.uiowa.edu/OR-DBM-MRMC-program-manual)
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
* This is v1.3.2.9000.
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
* Modified `StORHAnalysis.R` and to work with SP-A dataset provided `method = "ORH"` and `covEstMethod` = "jackknife" is used
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
* `UtilORVarComponentsFactorial()`:
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
