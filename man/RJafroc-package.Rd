\name{RJafroc-package}
\alias{RJafroc-package}
\docType{package}
\title{
  Artificial Intelligence Systems and Observer Performance
}
\description{
\code{RJafroc} analyzes the performance of artificial intelligence (AI) systems/algorithms characterized
    by a \emph{search-and-report} strategy. Historically observer performance has dealt with measuring radiologists'
    performances in search tasks, e.g., searching for lesions in medical images and reporting them, but the implicit
    location information has been ignored. The methods here apply to \emph{any} task involving searching for
    and reporting arbitrary targets in images. The implemented methods apply
    to analyzing the absolute and relative performances of AI systems, comparing AI performance to a group of human
    readers or optimizing the reporting threshold of an AI system. In addition to performing historical receiver operating
    characteristic (ROC) analysis (localization information ignored), the software also performs free-response receiver operating
    characteristic (FROC) analysis, where the implicit lesion localization information is used. A book describing the
    underlying methodology and which uses the software has been
    published: \strong{Chakraborty DP: Observer Performance Methods for Diagnostic Imaging - Foundations, Modeling, and Applications
    with R-Based Examples, Taylor-Francis LLC; 2017}: \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}.
    Online updates to this book, which use the software, are at
    \url{https://dpc10ster.github.io/RJafrocQuickStart/}, \url{https://dpc10ster.github.io/RJafrocRocBook/} and at
    \url{https://dpc10ster.github.io/RJafrocFrocBook/}. Supported data collection paradigms are the ROC, FROC and the location ROC (LROC).
    ROC data consists of single ratings per images, where a rating is the perceived confidence level that the image is that of a
    diseased patient. An ROC curve is a plot of true positive fraction vs. false
    positive fraction. FROC data consists of a variable number (zero or more) of mark-rating pairs per image, where a mark is the
    location of a reported suspicious region and the rating is the confidence level that it is a real lesion. LROC data
    consists of a rating and a location of the most suspicious region, for every image. Four models of observer performance,
    and curve-fitting software, are implemented: the binormal model (BM), the contaminated binormal model (CBM), the
    correlated contaminated binormal model (CORCBM), and the radiological search model (RSM). Unlike the binormal model, CBM,
    CORCBM and RSM predict "proper" ROC curves that do not inappropriately cross the chance diagonal. Additionally, RSM
    parameters are related to search performance (not measured in conventional ROC analysis) and classification performance.
    Search performance refers to finding lesions, i.e., true positives, while simultaneously not finding false positive
    locations. Classification performance measures the ability to distinguish between true and false positive locations. Knowing
    these separate performances allows principled optimization of reader or AI system performance. This package supersedes Windows
    \strong{JAFROC} (jackknife alternative FROC) software V4.2.1, \url{https://github.com/dpc10ster/WindowsJafroc}. Package functions
    are organized as follows. Data file related function names are preceded by \emph{Df}, curve fitting functions by \emph{Fit}, included
    data sets by \emph{dataset}, plotting functions by \emph{Plot}, significance testing functions by \emph{St}, sample size related functions
    by \emph{Ss}, data simulation functions by \emph{Simulate} and utility functions by \emph{Util}. Implemented are figures of merit (FOMs)
    for quantifying performance, functions for visualizing empirical operating characteristics: e.g., ROC, FROC, alternative FROC
    (AFROC) and weighted AFROC (wAFROC) curves. For fully crossed study designs significance testing of reader-averaged FOM
    differences between modalities is implemented via both Dorfman-Berbaum-Metz and the Obuchowski-Rockette methods. Also
    implemented are single treatment analyses, allowing comparison of performance of a group of radiologists to a specified
    value, or comparison of AI to a group of radiologists/algorithms interpreting the same cases. Crossed-modality analysis is implemented
    wherein there are two crossed treatment factors and the aim is to determined performance in each treatment factor averaged
    over all levels of the second factor. Sample size estimation tools are provided for ROC and FROC studies; these use estimates
    of the relevant variances from a pilot study to predict required numbers of readers and cases in a pivotal study to achieve the
    desired power. Utility and data file manipulation functions allow data to be read in any of the currently used input
    formats, including Excel, and the results of the analysis can be viewed in text or Excel output files. The methods are
    illustrated with several included datasets from the author's collaborations. This update includes improvements to the code,
    some as a result of user-reported bugs and new feature requests, and others discovered during ongoing testing and code simplification.
    All changes are noted in \strong{NEWS.md}.
}


\details{
  \tabular{ll}{
    Package: \tab RJafroc\cr
    Type: \tab Package\cr
    Version: \tab 2.1.1.9000\cr
    Date: \tab 2022-08-13\cr
    License: \tab GPL-3\cr
    URL: \tab \url{https://dpc10ster.github.io/RJafroc/}\cr
  }
}


\section{Definitions and abbreviations}{
  \itemize{
    \item \emph{a}: The separation or "a" parameter of the binormal model
    \item AFROC curve: plot of LLF (ordinate) vs. FPF, where FPF is inferred using
       highest rating of NL marks on \strong{non-diseased cases}
    \item AFROC: alternative FROC, see Chakraborty 1989
    \item AFROC1 curve: plot of LLF (ordinate) vs. FPF1, where FPF1 is inferred using
       highest rating of NL marks on \strong{ALL cases}
    \item \eqn{alpha}: The significance level \eqn{\alpha} of the test of the null
       hypothesis of no treatment effect
    \item AUC: area under curve; e.g., ROC-AUC = area under ROC curve, an
       example of a FOM
    \item \emph{b}: The width or "b" parameter of the conventional binormal
       model
    \item Binormal model: two unequal variance normal distributions, one at zero
       and one at \eqn{mu}, for modeling ROC ratings, \eqn{sigma} is the
       std. dev. ratio of diseased to non-diseased distributions
    \item CAD: computer aided detection algorithm
    \item CBM: contaminated binormal model (CBM): two equal variance normal
       distributions for modeling ROC ratings, the diseased distribution is
       bimodal, with a peak at zero and one at \eqn{\mu}, the integrated fraction
       at \eqn{\mu} is \eqn{\alpha} (not to be confused with \eqn{\alpha} of NH testing)
    \item CI: The (1-\eqn{\alpha}) confidence interval for the stated statistic
    \item Crossed modality: a dataset containing two modality (treatment) factors, with
       the levels of the two factors crossed, see paper by Thompson et al
    \item DBM: Dorfman-Berbaum-Metz, a significance testing method for
       detecting a treatment effect in MRMC studies
    \item DBMH: Hillis' modification of the DBM method
    \item ddf: Denominator degrees of freedom of appropriate \eqn{F}-test;
       the corresponding ndf is \code{I} - 1
    \item Empirical AUC: trapezoidal area under curve, same as the Wilcoxon
       statistic for ROC paradigm
    \item FN: false negative, a diseased case classified as non-diseased
    \item FOM: figure of merit, a quantitative measure of performance,
       performance metric
    \item FP: false positive, a non-diseased case classified as diseased
    \item FPF: number of FPs divided by number of non-diseased cases
    \item FROC curve: plot of LLF (ordinate) vs. NLF
    \item FROC: free-response ROC (a data collection paradigm where each image
       yields a random number, 0, 1, 2,..., of mark-rating pairs)
    \item FRRC: Analysis that treats readers as fixed and cases as random factors
    \item I: total number of modalities, indexed by \eqn{i}
    \item image/case: used interchangeably; a case can consist of several images
       of the same patient in the same treatment
    \item iMRMC: A text file format used for ROC data by FDA/CDRH researchers
    \item individual: A single-treatment single-reader dataset.
    \item Intrinsic: Used in connection with RSM; a parameter that is independent of
       the RSM \eqn{\mu} parameter, but whose meaning may not be as transparent as
       the corresponding physical parameter
    \item J: number of readers, indexed by \code{j}
    \item JAFROC file format: A .xlsx format file, applicable to ROC, ROI, FROC and
       LROC paradigms
    \item JAFROC: jackknife AFROC: Windows software for analyzing observer performance
       data: no longer updated, replaced by current package; the name is a misnomer
       as the jackknife is used only for significance testing; alternatively,
       the bootstrap could be used; what distinguishes FROC from ROC analysis is the
       use of the AFROC-AUC as the FOM. With this change, the DBM or the OR method
       can be used for significance testing
    \item \code{K}: total number of cases, \code{K} = \code{K1} + \code{K2}, indexed by \eqn{k}
    \item \code{K1}: total number of non-diseased cases, indexed by \eqn{k1}
    \item \code{K2}: total number of diseased cases, indexed by \eqn{k2}
    \item LL: lesion localization i.e., a mark that correctly locates an existing
       localized lesion; TP is a special case, when the proximity criterion is
      lax  (i.e., "acceptance radius" is large)
    \item LLF: number of LLs divided by the total number of lesions
    \item LROC: location receiver operating characteristic, a data collection paradigm
       where each image yields a single rating and one location
    \item lrc/MRMC: A text file format used for ROC data by University of Iowa
       researchers
    \item mark: the location of a suspected diseased region
    \item maxLL: maximum number of lesions per case in dataset
    \item maxNL: maximum number of NL marks per case in dataset
    \item MRMC: multiple reader multiple case (each reader interprets each case in
       each treatment, i.e. fully crossed study design)
    \item ndf: Numerator degrees of freedom of appropriate \eqn{F}-test, usually
       number of treatments minus one
    \item NH: The null hypothesis that all treatment effects are zero; rejected
       if the \eqn{p}-value is smaller than \eqn{\alpha}
    \item NL: non-lesion localization, of which FP is a special case, i.e., a
       mark that does not correctly locate any existing localized lesion(s)
    \item NLF: number of NLs divided by the total number of cases
    \item Operating characteristic: A plot of normalized correct decisions on
       diseased cases along ordinate vs. normalized incorrect decisions on
       non-diseased cases
    \item Operating point: A point on an operating characteristic, e.g.,
       (FPF, TPF) represents an operating point on an ROC
    \item OR: Obuchowski-Rockette, a significance testing method for detecting
       a treatment effect in MRMC studies
    \item ORH: Hillis' modification of the OR method
    \item Physical parameter: Used in connection with RSM; a parameter whose
       meaning is more transparent than the corresponding intrinsic parameter,
       but which depends on the RSM \eqn{\mu} parameter
    \item Proximity criterion / acceptance radius: Used in connection with FROC
       (or LROC data); the "nearness" criterion is used to determine if a mark
       is close enough to a lesion to be counted as a LL (or correct localization);
       otherwise it is counted as a NL (or incorrect localization)
    \item p-value: the probability, under the null hypothesis, that the observed
       treatment effects, or larger, could occur by chance
    \item Proper: a proper fit does not inappropriately fall below the chance
       diagonal, does not display a "hook" near the upper right corner
    \item PROPROC: Metz's binormal model based fitting of proper ROC curves
    \item RSM, Radiological Search Model: two unit variance normal distributions
       for modeling NL and LL ratings; four parameters, \eqn{\mu}, \eqn{\nu}',
       \eqn{\lambda}' and \eqn{\zeta}1
    \item Rating: Confidence level assigned to a case; higher values indicate greater
       confidence in presence of disease; \code{-Inf} is allowed but \code{NA} is
       not allowed
    \item Reader/observer/radiologist/CAD: used interchangeably
    \item RJafroc: the current software
    \item ROC: receiver operating characteristic, a data collection paradigm where
       each image yields a single rating and location information is ignored
    \item ROC curve: plot of TPF (ordinate) vs. FPF, as threshold is varied;
       an example of an operating characteristic
    \item ROCFIT: Metz software for binormal model based fitting of ROC data
    \item ROI: region-of-interest (each case is divided into a number of ROIs and
       the reader assigns an ROC rating to each ROI)
    \item FRRC: Analysis that treats readers as fixed and cases as random factors
    \item RRFC: Analysis that treats readers as random and cases as fixed factors
    \item RRRC: Analysis that treats both readers and cases as random factors
    \item RSCORE-II: original software for binormal model based fitting of ROC data
    \item RSM: Radiological search model, also method for fitting a proper ROC
       curve to ROC data
    \item RSM-\eqn{\zeta}1: Lowest reporting threshold, determines if suspicious
       region is actually marked
    \item RSM-\eqn{\lambda}: Intrinsic parameter of RSM corresponding to
       \eqn{\lambda}', independent of \eqn{\mu}
    \item RSM-\eqn{\lambda}': Physical Poisson parameter of RSM, average number of
       latent NLs per case; depends on \eqn{\mu}
    \item RSM-\eqn{\mu}: separation of the unit variance distributions of RSM
    \item RSM-\eqn{\nu}: Intrinsic parameter of RSM, corresponding to \eqn{\nu}',
       independent of \eqn{\mu}
    \item RSM-\eqn{\nu}': binomial parameter of RSM, probability that lesion is found
    \item SE: sensitivity, same as \eqn{TPF}
    \item Significance testing: determining the p-value of a statistical test
    \item SP: specificity, same as \eqn{1-FPF}
    \item Threshold: Reporting criteria: if confidence exceeds a threshold value,
       report case as diseased, otherwise report non-diseased
    \item TN: true negative, a non-diseased case classified as non-diseased
    \item TP: true positive, a diseased case classified as diseased
    \item TPF: number of TPs divided by number of diseased cases
    \item Treatment/modality: used interchangeably, for example, computed tomography
       (CT) images vs. magnetic resonance imaging (MRI) images
    \item wAFROC curve: plot of weighted LLF (ordinate) vs. FPF, where FPF is
       inferred using highest rating of NL marks on \strong{non-diseased cases ONLY}
    \item wAFROC1 curve: plot of weighted LLF (ordinate) vs. FPF1, where FPF1 is
       inferred using highest rating of NL marks on \strong{ALL cases}
    \item wAFROC1 FOM: weighted trapezoidal area under AFROC1 curve: only use
       if there are zero non-diseased cases is always number of treatments minus one
    }
}

\section{Dataset}{
  The \code{dataset} object has 3 \code{list} elements: \code{$ratings}, \code{$lesions}
     and \code{$descriptions}, where:

     \itemize{
     \item \code{dataset$ratings}: contains 3 elements as sub-lists: \code{$NL},
        \code{$LL} and \code{$LL_IL}; these describe the structure of the ratings;

     \item \code{dataset$lesions}: contains 3 elements as sub-lists: \code{$perCase},
        \code{$IDs} and \code{$weights}; these describe the structure of the lesions;

     \item \code{dataset$descriptions}: contains 7 elements as sub-lists: \code{$fileName},
        \code{$type}, \code{$name}, \code{$truthTableStr}, \code{$design}, \code{$modalityID}
        and \code{$readerID}; these describe other characteristics of the dataset as
        detailed next.
    }

    \strong{Note: \code{-Inf} is used to indicate the ratings of unmarked lesions
        and/or missing values.} As an example of the latter, if the maximum
        number of NLs in a dataset is 4, but some images have fewer than 4 NL marks,
        the corresponding "empty" positions would be filled with
        \code{-Inf}s. \strong{Do not use \code{NA} to denote a missing rating.}

    \strong{Note}: \bold{"dataset" in this package always represents
    \code{R} object(s) with the following structure(s):}

    \subsection{General data structure, e.g., \code{dataset02}, an ROC dataset, and
        \code{dataset05}, an FROC dataset.}{
    \itemize{
    \item \code{ratings$NL}: a float array with dimensions
        \code{c(I, J, K, maxNL)}, containing the ratings of NL marks. The first
        \code{K1} locations of the third index corresponds to NL marks on non-diseased
        cases and the remaining locations correspond to NL marks on diseased
        cases. The 4th dimension allows for multiple NL marks
        on a case: the first index holds the first NL rating on the image,
        the second holds the second NL rating on the image, etc. The value of
        \code{maxNL} is determined by the case with the maximum number of lesions
        per case in the dataset. For \bold{FROC} datasets missing NL ratings are assigned the
        \code{-Inf} rating. For \bold{ROC} datasets, FP ratings are assigned
        to the first \code{K1} elements of \code{NL[,,1:K1,1]} and the remaining
        \code{K2} elements of \code{NL[,,(K1+1):K,1]} are set to \code{-Inf}.

    \item \code{ratings$LL}: for non-LROC datasets a float array with dimensions
        \code{c(I, J, K2, maxLL)} containing the ratings of LL marks. The value of
        \code{maxLL} is determined by the maximum number of lesions per case in the
        dataset. Unmarked lesions are assigned the \code{-Inf} rating.
        For ROC datasets \code{TP} ratings are assigned to \code{LL[,,1:K2,1]}.
        For \bold{LROC} datasets it is a float array with dimensions
        \code{c(I, J, K2, 1)} containing the ratings of correct localizations,
        otherwise the rating is recorded in the incorrect localization array
        described next.

    \item \code{ratings$LL_IL}: for LROC datasets the ratings
        of incorrect localization marks on abnormal cases. It is a float array
        with dimensions \code{c(I, J, K2, 1)}. For non-LROC datasets this array
        is filled with NAs.

    \item \code{lesions$perCase}: an integer array with length \code{K2},
        the number of lesions on each diseased case. The
        maximum value of this array equals \code{maxLL}. For example,
        \code{dataset05$lesions$perCase[4} is 2, meaning the
        4th diseased case has two lesions.

    \item \code{lesions$IDs}: an integer array with dimensions [\code{K2}, \code{maxLL}],
        labeling (or naming) the lesions on the diseased cases. For example,
        \code{dataset05$lesions$IDs[4,]} is \code{c(1,2,-Inf)}, meaning the
        4th diseased case has two lesions, labeled 1 and 2.

    \item \code{lesions$weights}: a floating point array with dimensions
        \code{c(K2, maxLL)}, representing the relative importance of detecting
        each lesion. The weights for an abnormal case must sum to unity.
        For example, \code{dataset05$lesions$weights[4,]} is \code{c(0.5,0.5, -Inf)},
        corresponding to equal weights (0.5) assigned to of the two lesions in the
        case.

    \item \code{descriptions$fileName}: a \code{character} variable containing the file
        name of the source data for this dataset. This is generated automatically
        by the \link{DfReadDataFile} function used to read the file. For a simulalated
        dataset it is set to "NA" (i.e., a character vector, not the variable \code{NA}).

    \item \code{descriptions$type}: a \code{character} variable describing the data type:
        "\code{ROC}", "\code{LROC}", "\code{ROI}" or "\code{FROC}".

    \item \code{descriptions$name}: a \code{character} variable containing the name of
        the dataset: e.g., "dataset02" or "dataset05". This is generated automatically
        by the \link{DfReadDataFile} function used to read the file.

    \item \code{descriptions$truthTableStr}: a \code{c(I, J, L, maxLL+1)} object. For
        normal cases elements \code{c(I, J, L, 1)} are filled with 1s if the corresponding
        interpretations occurred or NAs otherwise. For abnormal cases elements
        \code{c(I, J, L, 2:(maxLL+1))} are filled with 1s if the corresponding
        interpretations occurred or NAs otherwise. This object is necessary for analyzing
        more complex designs, e.g., split-plot, as described next.

    \item \code{descriptions$design}: a \code{character} variable: "\code{FCTRL}",
        "\code{SPLIT-PLOT-A}" or "\code{SPLIT-PLOT-A}", corresponding to factorial,
        split-plot-A or split-plot-C designs. The A and C refer to subparts of Table VII
        in a Hillis 2014 publication.

    \item \code{descriptions$modalityID}: a \code{character} vector of length \eqn{I},
        which labels/names the modalities in the dataset. For non-JAFROC data file formats,
        they must be unique integers.

    \item \code{descriptions$readerID}: a \code{character} vector of length \eqn{J},
        which labels/names the readers in the dataset. For non-JAFROC data file formats,
        they must be unique integers.
    }
  }

  \subsection{ROI data structure, example \code{datasetROI}}{
  Only changes from the previously described structure are described below:
    \itemize{
    \item \code{ratings$NL}: a float array with dimensions \code{c(I, J, K, Q)}
        containing the ratings of each of Q quadrants for each non-diseased case.
    \item \code{ratings$LL}: a float array with dimensions
        \code{c(I, J, K2, Q)} containing the ratings of quadrants for each
        diseased case.
    \item \code{lesions$perCase}: this contains the locations, on abnormal cases,
    containing at least one lesion.
    }
  }

  \subsection{Crossed modality data structure, example \code{datasetCrossedModality}}{
  Only changes from the previously described structure are described below:
    \itemize{
    \item \code{ratings$NL}: a float array with dimension \code{c(I1, I2, J, K, maxNL)}
        containing the ratings of NL marks. Note the existence of two modality indices.
    \item \code{LL}: a float array with dimension \code{c(I1, I2, J, K2, maxLL)}
        containing the ratings of all LL marks. Note the existence of two modality indices.
    \item \code{modalityID1}: corresponding to first modality factor.
    \item \code{modalityID2}: corresponding to second modality factor.
    }
  }
}

\section{Df: Datafile Related Functions}{
  \itemize{
        \item \code{\link{Df2RJafrocDataset}}: Convert a ratings array to a
           dataset object.
        \item \code{\link{DfBinDataset}}: Return a binned dataset.
        \item \code{\link{DfCreateCorCbmDataset}}:Create paired dataset for
           testing \link{FitCorCbm}.
        \item \code{\link{DfExtractDataset}}: Extract a subset of modalities
           and readers from a dataset.
        \item \code{\link{DfFroc2Roc}}: Convert an FROC dataset to a highest
           rating inferred ROC dataset.
        \item \code{\link{DfLroc2Roc}}: Convert an LROC dataset to a highest
           rating inferred ROC dataset.
        \item \code{\link{DfLroc2Froc}}: Simulates an
           "AUC-equivalent" FROC dataset from a supplied LROC dataset.
        \item \code{\link{DfFroc2Lroc}}: Simulates an
           "AUC-equivalent" LROC dataset from a supplied FROC dataset.
        \item \code{\link{DfReadCrossedModalities}}: Read a crossed-modalities
           data file.
        \item \code{\link{DfReadDataFile}}: Read a general data file.
        \item \code{\link{DfSaveDataFile}}: Save ROC data file in a different format.
        \item \code{\link{DfExtractCorCbmDataset}}: Extract two arms of a pairing
           from an MRMC ROC dataset suitable for using \link{FitCorCbm}.
  }
}


\section{Fitting Functions}{
  \itemize{
        \item \code{\link{FitBinormalRoc}}: Fit the binormal model to ROC data
           (R equivalent of ROCFIT or RSCORE).
        \item \code{\link{FitCbmRoc}}: Fit the contaminated binormal model (CBM)
           to ROC data.
        \item \code{\link{FitRsmRoc}}: Fit the radiological search model (RSM)
           to ROC data.
        \item \code{\link{FitCorCbm}}: Fit the correlated contaminated binormal model
             (CORCBM) to paired ROC data.
        \item \code{\link{FitRsmRoc}}: Fit the radiological search model (RSM)
           to ROC data.
  }
}

\section{Plotting Functions}{
  \itemize{
        \item \code{\link{PlotBinormalFit}}: Plot binormal-predicted ROC curve with
           provided BM parameters.
        \item \code{\link{PlotEmpiricalOperatingCharacteristics}}: Plot empirical
           operating characteristics for specified dataset.
        \item \code{\link{PlotRsmOperatingCharacteristics}}: Plot RSM-fitted ROC curves.
  }
}

\section{Simulation Functions}{
  \itemize{
        \item \code{\link{SimulateFrocDataset}}: Simulates an uncorrelated FROC dataset
           using the RSM.
        \item \code{\link{SimulateRocDataset}}: Simulates an uncorrelated binormal
           model ROC dataset.
        \item \code{\link{SimulateCorCbmDataset}}: Simulates an uncorrelated binormal
           model ROC dataset.
        \item \code{\link{SimulateLrocDataset}}: Simulates an uncorrelated LROC dataset.
  }
}

\section{Sample size Functions}{
  \itemize{
        \item \code{\link{SsPowerGivenJK}}: Calculate statistical power given
           numbers of readers J and cases \code{K}.
        \item \code{\link{SsPowerTable}}: Generate a power table.
        \item \code{\link{SsSampleSizeKGivenJ}}: Calculate number of cases \code{K}, for
           specified number of readers J, to achieve desired power for an ROC study.
  }
}

\section{Significance Testing Functions}{
  \itemize{
        \item \code{\link{StSignificanceTesting}}: Perform significance testing,
           DBM or OR.
        \item \code{\link{StSignificanceTestingCadVsRad}}: Perform
           significance testing, CAD vs. radiologists.
        \item \code{\link{StSignificanceTestingCrossedModalities}}: Perform
           significance testing using crossed modalities analysis.
  }
}

\section{Miscellaneous and Utility Functions}{
  \itemize{
        \item \code{\link{UtilAucBinormal}}: Binormal model AUC function.
        \item \code{\link{UtilAucCBM}}: CBM AUC function.
        \item \code{\link{UtilAucPROPROC}}: PROPROC AUC function.
        \item \code{\link{UtilAnalyticalAucsRSM}}: RSM ROC/AFROC AUC calculator.
        \item \code{\link{UtilFigureOfMerit}}: Calculate empirical figures of merit
           (FOMs) for specified dataset.
        \item \code{\link{UtilIntrinsic2RSM}}: Convert from intrinsic to
           physical RSM parameters.
        \item \code{\link{UtilLesionWeightsMatrix}}: Calculates the lesion weights  matrix.
        \item \code{\link{UtilMeanSquares}}: Calculates the mean squares used in the
           DBMH and ORH methods.
        \item \code{\link{UtilOutputReport}}: Generate a formatted report file.
        \item \code{\link{UtilRSM2Intrinsic}}: Convert from RSM parameters to
           intrinsic RSM parameters.
        \item \code{\link{UtilPseudoValues}}: Return jackknife pseudovalues.
        \item \code{\link{UtilVarComponentsDBM}}: Utility for Dorfman-Berbaum-Metz
           variance components.
        \item \code{\link{UtilORVarComponentsFactorial}}: Utility for Obuchowski-Rockette
           variance components.
  }
}

\author{
\itemize{
  \item Author: Dev Chakraborty \email{dpc10ster@gmail.com}.
  \item Author: Xuetong Zhai \email{xuetong.zhai@gmail.com}.
  \item Contributor: Peter Phillips \email{peter.phillips@cumbria.ac.uk}.
}
}

\references{
\bold{Basics of ROC}

  Metz, CE (1978). Basic principles of ROC analysis. In Seminars in nuclear medicine
     (Vol. 8, pp. 283--298). Elsevier.

  Metz, CE (1986). ROC Methodology in Radiologic Imaging. Investigative Radiology,
     21(9), 720.

  Metz, CE (1989). Some practical issues of experimental design and data analysis in
     radiological ROC studies. Investigative Radiology, 24(3), 234.

  Metz, CE (2008). ROC analysis in medical imaging: a tutorial review of the
     literature. Radiological Physics and Technology, 1(1), 2--12.

  Wagner, R. F., Beiden, S. V, Campbell, G., Metz, CE, & Sacks, W. M. (2002).
     Assessment of medical imaging and computer-assist systems: lessons from recent experience. Academic Radiology, 9(11), 1264--77.

  Wagner, R. F., Metz, CE, & Campbell, G. (2007). Assessment of medical imaging
     systems and computer aids: a tutorial review. Academic Radiology, 14(6), 723--48.

\bold{DBM/OR methods and extensions}

  DORFMAN, D. D., BERBAUM, KS, & Metz, CE (1992). Receiver operating characteristic
     rating analysis: generalization to the population of readers and patients with
     the jackknife method. Investigative Radiology, 27(9), 723.

  Obuchowski, NA, & Rockette, HE (1994). HYPOTHESIS TESTING OF DIAGNOSTIC ACCURACY
     FOR MULTIPLE READERS AND MULTIPLE TESTS: AN ANOVA APPROACH WITH DEPENDENT
     OBSERVATIONS. Communications in Statistics-Simulation and Computation,
     24(2), 285--308.

  Hillis, SL, Berbaum, KS, & Metz, CE (2008). Recent developments in the
     Dorfman-Berbaum-Metz procedure for multireader ROC study analysis.
     Academic Radiology, 15(5), 647--61.

  Hillis, SL, Obuchowski, NA, & Berbaum, KS (2011). Power Estimation for
     Multireader ROC Methods: An Updated and Unified Approach. Acad Radiol, 18, 129--142.

  Hillis, SL SL (2007). A comparison of denominator degrees of freedom methods for
     multiple observer ROC analysis. Statistics in Medicine, 26(3), 596--619.

\bold{FROC paradigm}

  Chakraborty DP. Maximum Likelihood analysis of free-response receiver operating
     characteristic (FROC) data. Med Phys. 1989;16(4):561--568.

  Chakraborty, DP, & Berbaum, KS (2004). Observer studies involving detection and
     localization: modeling, analysis, and validation. Medical Physics, 31(8), 1--18.

  Chakraborty, DP (2006). A search model and figure of merit for observer data acquired
     according to the free-response paradigm. Physics in Medicine and Biology,
     51(14), 3449--62.

  Chakraborty, DP (2006). ROC curves predicted by a model of visual search. Physics in
     Medicine and Biology, 51(14), 3463--82.

  Chakraborty, DP (2011). New Developments in Observer Performance Methodology in
     Medical Imaging. Seminars in Nuclear Medicine, 41(6), 401--418.

  Chakraborty, DP (2013). A Brief History of Free-Response Receiver Operating
     Characteristic Paradigm Data Analysis. Academic Radiology, 20(7), 915--919.

  Chakraborty, DP, & Yoon, H.-J. (2008). Operating characteristics predicted
     by models for diagnostic tasks involving lesion localization. Medical
     Physics, 35(2), 435.

  Thompson JD, Chakraborty DP, Szczepura \code{K}, et al. (2016) Effect of reconstruction methods
     and x-ray tube current-time product  on nodule detection in an anthropomorphic
     thorax phantom: a crossed-modality JAFROC observer study. Medical Physics.
     43(3):1265-1274.

  Zhai X, Chakraborty DP. (2017) A bivariate contaminated binormal model for robust
     fitting of proper ROC curves to a pair of correlated, possibly degenerate,
     ROC datasets. Medical Physics. doi: 10.1002/mp.12263:2207--2222.

  Hillis SL, Chakraborty DP, Orton CG. ROC or FROC? It depends on the research
     question. Medical Physics. 2017.

  Chakraborty DP, Nishikawa RM, Orton CG. Due to potential concerns of bias and
     conflicts of interest, regulatory bodies should not do evaluation methodology
     research related to their regulatory missions. Medical Physics. 2017.

  Dobbins III JT, McAdams HP, Sabol JM, Chakraborty DP, et al. (2016) Multi-Institutional
     Evaluation of Digital Tomosynthesis, Dual-Energy Radiography, and Conventional
     Chest Radiography for the Detection and Management of Pulmonary Nodules. Radiology. 282(1):236-250.

  Warren LM, Mackenzie A, Cooke J, et al. Effect of image quality on calcification
     detection in digital mammography. Medical Physics. 2012;39(6):3202-3213.

  Chakraborty DP, Zhai X. On the meaning of the weighted alternative free-response
     operating characteristic figure of merit. Medical physics. 2016;43(5):2548-2557.

  Chakraborty DP. (2017) Observer Performance Methods for Diagnostic Imaging -
     Foundations, Modeling, and Applications with R-Based Examples.
     Taylor-Francis, LLC.

}
