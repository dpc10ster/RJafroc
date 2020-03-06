\name{RJafroc-package}
\alias{RJafroc-package}
\docType{package}
\title{
  RJafroc: Analyzing Diagnostic Observer Performance Studies
}
\description{
RJafroc implements software for assessing medical imaging systems, radiologists or computer aided detection algorithms. Models of observer performance are implemented, including the binormal model (BM), the contaminated binormal model (CBM), the correlated contaminated binormal model (CORCBM), and the radiological search model (RSM). The software and applications are described in a book - \strong{Chakraborty DP: Observer Performance Methods for Diagnostic Imaging - Foundations, Modeling, and Applications with R-Based Examples. Taylor-Francis LLC; 2017} and its vignettes \url{https://dpc10ster.github.io/RJafroc/}. Observer performance data collection paradigms are the receiver operating characteristic (ROC) and its location specific extensions, primarily free-response ROC (FROC) and the location ROC (LROC). ROC data consists of single ratings per images. A rating is the perceived confidence level that the image is that of a diseased patient. FROC data consists of a variable number (including zero) of mark-rating pairs per image, where a mark is the location of a clinically reportable suspicious region and the rating is the corresponding confidence level that it is a real lesion. LROC data consists of a rating and a forced localization of the most suspicious region on every image. RJafroc supersedes the Windows version of \strong{JAFROC} software V4.2.1, \url{http://www.devchakraborty.com}. Package functions are organized as follows. Data file related function names are preceded by \emph{Df}, curve fitting functions by \emph{Fit}, included data sets by \emph{dataset}, plotting functions by \emph{Plot}, significance testing functions by \emph{St}, sample size related functions by \emph{Ss}, data simulation functions by \emph{Simulate} and utility functions by \emph{Util}. Implemented are figures of merit (FOMs) for quantifying performance, functions for visualizing empirical operating characteristics: e.g., ROC, FROC, alternative FROC (AFROC) and weighted AFROC (wAFROC) curves. Four maximum likelihood curve-fitting algorithms are implemented: the binormal model (BM), the contaminated binormal model (CBM), the correlated contaminated binormal model (CORCBM) and the radiological search model (RSM). Unlike the binormal model, CBM, CORCBM and RSM predict "proper" ROC curves that do not cross the chance diagonal. RSM fitting additionally yields measures of search and lesion-classification performances. Search performance is \emph{the ability to find lesions while avoiding finding non-lesions}. Lesion-classification performance is the ability to correctly classify found lesions from found non-lesions. For fully crossed study designs significance testing of reader-averaged FOM differences between modalities is implemented via both Dorfman-Berbaum-Metz and the Obuchowski-Rockette methods, including Hillis' extensions. Also implemented are single treatment analyses, which allow comparison of performance of a group of radiologists to a specified value, or comparison to CAD to a group of radiologists interpreting the same cases. Crossed-modality analysis is implemented wherein there are two crossed treatment factors and the desire is to determined performance in each treatment factor averaged over all levels of the other factor. Sample size estimation tools are provided for ROC and FROC studies; these use estimates of the relevant variances from a pilot study to predict required numbers of readers and cases in a pivotal study to achieve a desired power. Utility and data file manipulation functions allow data to be read in any of the currently used input formats, including Excel, and the results of the analysis can be viewed in text or Excel output files. The methods are illustrated with several included datasets from the author's international collaborations. This version corrects a few bugs noticed by users (listed in NEWS.md) and extends the Excel file input format for greater flexibility going forward in handling non-crossed datasets.
}
\details{
  \tabular{ll}{
    Package: \tab RJafroc\cr
    Type: \tab Package\cr
    Version: \tab 1.3.2\cr
    Date: \tab 2020-01-13\cr
    License: \tab GPL-3\cr
    URL: \tab \url{https://dpc10ster.github.io/RJafroc/}\cr
  }
}


\section{Abbreviations and definitions}{
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
    \item JAFROC AUC: trapezoidal area under AFROC curve; deprecated, 
       use AFROC AUC instead
    \item JAFROC: jackknife AFROC: Windows software for analyzing observer performance 
       data: no longer updated, replaced by current package; the name is a misnomer 
       as the jackknife is used only for significance testing; alternatively, 
       the bootstrap could be used; what distinguishes FROC from ROC analysis is the 
       use of the AFROC-AUC as the FOM. With this change, the DBM or the OR method 
       can be used for significance testing
    \item FOM_AFROC FOM: trapezoidal area under AFROC curve; this term is obsolete; 
       use AFROC-AUC instead
    \item FOM_AFROC1 FOM: trapezoidal area under AFROC1 curve; this term is obsolete; 
       use AFROC1-AUC instead
    \item K: total number of cases, K = K1 + K2, indexed by \eqn{k}
    \item K1: total number of non-diseased cases, indexed by \eqn{k1}
    \item K2: total number of diseased cases, indexed by \eqn{k2}
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
    \item FOM_wAFROC FOM: weighted trapezoidal area under AFROC curve: this term is 
       obsolete; use wAFROC-AUC instead;\strong{this is the recommended FOM}
    \item FOM_wAFROC1 FOM: weighted trapezoidal area under AFROC1 curve: only use 
       if there are zero non-diseased cases is always number of treatments minus one
    } 
}

\section{Dataset}{
  An R object either created by the user or read from an external text or 
     Excel file. It is a \code{list} generally containing 8 members (9 members 
     for crossed-modality or LROC datasets): \strong{Note: \code{-Inf} is used to 
     indicate the ratings of unmarked lesions and/or to indicate missing array 
     items. An example of the latter would be if the maximum number of NLs in a 
     dataset was 4, but some images had fewer than 4 NLs, in which case the corresponding 
     "empty" positions would be filled with \code{-Inf}s. Do not use \code{NA} to 
     denote a rating.}
  
  \strong{Note}: \bold{"dataset" in this package always represents an 
     \code{R} object with one of the following structures:}
  \subsection{General data structure, example \code{dataset02} and \code{dataset05}}{
    \itemize{
          \item \code{NL}: a floating-point array with dimension  
             \code{c(I, J, K, maxNL)} containing the ratings of NL marks. The first 
             K1 locations of the third index corresponds to NL marks on non-diseased 
             cases and the remaining locations correspond to NL marks on diseased 
             cases. The 4th dimension allows for the possibility of multiple NL marks 
             on a case. For \bold{FROC} datasets unavailable or missing NL ratings are assigned 
             \code{-Inf}. For \bold{ROC} datasets FP ratings are assigned to the first 
             \code{K1} elements of \code{NL[,,1:K1,1]} and the remaining \code{K2} 
             elements of \code{NL[,,(K1+1):K,1]} are set to \code{-Inf}. When 
             converting from FROC to ROC data the software assigns \code{-Inf} 
             to cases with no marks.
          \item \code{LL}: a floating-point array with dimension  
             \code{c(I, J, K2, maxLL)} that contains the ratings of all LL marks. 
             For ROC datasets \code{TP} ratings are assigned to \code{LL[,,1:K2,1]}.
          \item \code{lesionVector}: a integer vector with length \code{K2}, whose 
             elements indicate the number of lesions in each diseased case. 
          \item \code{lesionID}: an integer array with dimension [K2, maxLL]. 
             Its contents label lesions on diseased cases. For example,  
             \code{dataset05$lesionID[40,]} is \code{c(1,2,-Inf)}, meaning the 
             40th diseased case in this dataset has two lesions, labeled 1 and 2. 
             The \code{lesionID} of an LL in the \samp{TP} or \samp{LL} worksheet 
             must correspond to the \code{lesionID} for that case in the 
             \samp{Truth} worksheet. For example, if the \code{lesionID} for the 
             40th diseased case in the \samp{TP} or \samp{LL} worksheet is 2, 
             then the associated rating must correspond to the lesion labeled 2 
             in the \samp{Truth} worksheet, etc. 
          \item \code{lesionWeight}: a floating point array with dimension 
             \code{c(K2, maxLL)}, representing the relative importance of detecting 
             each lesion. For each case, the weights sum to unity. If zero is assigned 
             to the \code{Weight} field in the \samp{Truth} worksheet,  the 
             software automatically assigns equal weighting, e.g., 
             \code{dataset05$lesionWeight[40,]} is \code{c(0.5,0.5)}, corresponding to 
             equal weights (1/2) to each lesion on an image with two lesions.
          \item \code{dataType}: a \code{character} variable: "\code{ROC}", "\code{LROC}", "\code{ROI}" or "\code{FROC}".
          \item \code{modalityID}: a \code{character} vector of length \eqn{I}, which labels the 
             modalities in the dataset.
          \item \code{readerID}: a \code{character} vector of length \eqn{J}, which labels the 
             readers. For example, \code{NL[1, 2, , ]} indicates the NL-rating of the 
             reader identified with the second label in \code{readerID} using the 
             modality identified with the first label in \code{modalityID}.
    }
  }
  
  \subsection{ROI data structure, example \code{TBA}}{
    \itemize{
          \item \code{NL}: a floating-point array with dimension \code{c(I, J, K, Q)} 
             that contains the ratings of Q quadrants for each non-diseased case (k <= K1). 
             For k > K1,  
             elements contain valid ratings while the rest are filled with \code{-Inf}s.
          \item \code{LL}: a floating-point array with dimension  
             \code{c(I, J, K2, 1)} that contains the ratings of all \emph{correct} 
             localization (CL) marks. A \code{-Inf} indicates a case with no CL mark. 
          \item \code{LLIl}: a floating-point array with dimension  
             \code{c(I, J, K2, 1)} that contains the ratings of all \emph{incorrect} 
             localization (IL) marks. A \code{-Inf} indicates a case with no IL mark.
          \item \code{lesionVector}: same as general case. 
          \item \code{lesionID}: lesionID: an integer vector with length K2 
             containing ones.
          \item \code{lesionWeight}: a floating point array with dimension 
             \code{c(K2, 1)} containing ones.
          \item \code{dataType}: a \code{character} variable: "\code{ROI}".
          \item \code{modalityID}: same as general case.
          \item \code{readerID}: same as general case.
    }
  }
  
  \subsection{LROC data structure, example \code{datasetCadLroc}}{
    \itemize{
          \item \code{NL}: a floating-point array with dimension  \code{c(I, J, K, 1)} 
             that contains the ratings of FP marks. For the third index, the first K1 
             elements contain valid ratings while the rest are filled with \code{-Inf}s.
          \item \code{LLCl}: a floating-point array with dimension  
             \code{c(I, J, K2, 1)} that contains the ratings of all \emph{correct} 
             localization (CL) marks. A \code{-Inf} indicates a case with no CL mark. 
          \item \code{LLIl}: a floating-point array with dimension  
             \code{c(I, J, K2, 1)} that contains the ratings of all \emph{incorrect} 
             localization (IL) marks. A \code{-Inf} indicates a case with no IL mark.
          \item \code{lesionVector}: same as general case. 
          \item \code{lesionID}: lesionID: an integer vector with length K2 
             containing ones.
          \item \code{lesionWeight}: a floating point array with dimension 
             \code{c(K2, 1)} containing ones.
          \item \code{dataType}: a \code{character} variable: "\code{LROC}".
          \item \code{modalityID}: same as general case.
          \item \code{readerID}: same as general case.
    }
  }
  
  \subsection{Crossed modality data structure, example \code{datasetCrossedModality}}{
    \itemize{
          \item \code{NL}: a floating-point array with dimension  
             \code{c(I1, I2, J, K, maxNL)} that contains the ratings of NL marks. Note 
                the existence of two modality indices.
          \item \code{LL}: a floating-point array with dimension  
             \code{c(I1, I2, J, K2, maxLL)}that contains the ratings of all LL marks. 
             Note the existence of two modality indices.
          \item \code{lesionVector}: same as general case. 
          \item \code{lesionID}: same as general case.
          \item \code{lesionWeight}: same as general case.
          \item \code{dataType}: a \code{character} variable: "\code{ROC}" or "\code{FROC}".
          \item \code{modalityID1}: same as general case, corresponding to first 
             modality factor.
          \item \code{modalityID2}: same as general case, corresponding to second 
             modality factor.
          \item \code{readerID}: same as general case.
    }
  }
  
  \subsection{Data file format}{
    The package reads JAFROC, MRMC (ROC data only) and iMRMC (ROC data only) 
       data files. The data can be imported by using the function 
       \link{DfReadDataFile}.
    \itemize{
    \item{\strong{JAFROC data file format}}{
      The JAFROC data file is an Excel file containing three worksheets (*.xls and 
         *.xlsx are supported): (1) the \samp{Truth} worksheet, (2) the \samp{TP} 
         or \samp{LL} worksheet and (3) the \samp{FP} or \samp{NL} worksheet. 
         Except for the \samp{Truth} worksheet, where each case must occur at least 
         once, the number of rows in the other worksheets is variable.
      \enumerate{
            \item \samp{Truth} worksheet consists of
          \itemize{
                \item \samp{CaseID}, an integer field uniquely labeling the cases 
                   (images). It must occur at least once for each case, and since a 
                   case may have multiple lesions, it can occur multiple times, 
                   once for each lesion.
                \item \samp{LesionID}, an integer field uniquely labeling the lesions 
                   in each case. This field is zero for non-diseased cases.
                \item \samp{Weight}, a floating-point field, which is the relative 
                   importance of detecting each lesion. This field is zero for 
                   non-diseased cases and for equally weighted lesions; otherwise 
                   the weights must sum to unity for each case. Unless a weighted 
                   figure of merit is selected, this field is irrelevant.
          }
            \item \samp{TP} worksheet consists of
          \itemize{
                  \item \samp{ReaderID}, a \code{character} field uniquely labeling the readers 
                     (radiologists).
                  \item \samp{ModalityID}, a \code{character} field uniquely labeling the 
                     modalities.
                  \item \samp{CaseID}, see \samp{Truth} worksheet. A non-diseased 
                     case in this field will generate an error.
                  \item \samp{LesionID}, see \samp{Truth} worksheet. An entry in this 
                     field that does not appear in the \samp{Truth} worksheet will 
                     generate an error. It is the user's responsibility to ensure 
                     that the entries in the \samp{Truth} and \samp{TP} worksheets 
                     correspond to the same physical lesions.
                  \item \samp{TP_Rating}, a positive floating-point field denoting the 
                     rating assigned to a particular lesion-localization mark, with 
                     higher numbers represent greater confidence that the location 
                     is actually a lesion.
            }
            \item \samp{FP} worksheet consists of
          \itemize{
                  \item \samp{ReaderID}, see \samp{TP} worksheet.
                  \item \samp{ModalityID}, see \samp{TP} worksheet.
                  \item \samp{CaseID}, see \samp{TP} worksheet.
                  \item \samp{FP_Rating}, a positive floating-point field denoting the 
                     rating assigned to a particular non-lesion-localization mark, 
                     with higher numbers represent greater confidence that the location 
                     is actually a lesion.
            }        
         }
      }
    \item{\strong{MRMC data file format / LABMRMC format}}{
    \itemize{
          \item \emph{Input format for MRMC.} This format is described in the Medical 
             Image Perception Laboratory website, currently 
             \url{http://perception.radiology.uiowa.edu/}.
          \item \emph{LABMRMC data format.} The data file includes following parts. 
             The file must be saved as plain text file with *.lrc extension. All items 
             in the file are separated by one or more blank spaces.
        \enumerate{
              \item The first line is a free text description of the file.
              \item The second line is the name or ID of the first reader.
              \item The third line has the names or IDs of all the modalities. Each 
                 name or ID must be enclosed by double quotes(" ").
              \item The fourth line must have the letter (l or s) or word (large or 
                 small) for each modality. The letter or word indicates that smaller 
                 or larger ratings represent stronger confidence of presence of disease.
              \item The following lines contain the ratings in all modalities, 
                 separated by spaces or tabs, of the non-diseased cases, one case per 
                 line. The cases must appear in the same order for all readers. 
                 Missing value is not allowed. 
              \item After the last non-diseased case insert a line containing the 
                 asterisk (*) symbol. 
              \item Repeat steps 5 and 6 for the diseased cases. 
              \item Repeat steps 2, 5, 6 and 7 for the remaining readers. 
              \item The last line of the data file must be a pound symbol (#). 
        }
    }
    
    \item{\strong{iMRMC data format}}{
      This  is described in the iMRMC website, currently 
         \url{https://code.google.com/p/imrmc/}.
    }
    }
  }
}
}


\section{Df: Datafile Related Functions}{
  \itemize{    
        \item \code{\link{Df2RJafrocDataset}}: Convert a ratings array to a 
           dataset object.
        \item \code{\link{DfBinDataset}}: Return a binned dataset.
        \item \code{\link{DfCreateCorCbmDataset}}:Create paired dataset for 
           testing \link{FitCorCbmRoc}.
        \item \code{\link{DfExtractDataset}}: Extract a subset of modalities 
           and readers from a dataset.
        \item \code{\link{DfFroc2Afroc}}: Convert an FROC dataset to an AFROC 
           dataset.
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
        \item \code{\link{DfReadLrocDataFile}}: Read a LROC data file.
        \item \code{\link{DfSaveDataFile}}: Save ROC data file in a different format.
        \item \code{\link{DfExtractCorCbmDataset}}: Extract two arms of a pairing 
           from an MRMC ROC dataset suitable for using \link{FitCorCbmRoc}.
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
        \item \code{\link{FitCorCbmRoc}}: Fit the correlated contaminated binormal model 
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
           numbers of readers J and cases K.
        \item \code{\link{SsPowerTable}}: Generate a power table.
        \item \code{\link{SsSampleSizeKGivenJ}}: Calculate number of cases K, for 
           specified number of readers J, to achieve desired power for an ROC study.
  }
}

\section{Significance Testing Functions}{
  \itemize{    
        \item \code{\link{StSignificanceTesting}}: Perform significance testing, 
           DBM or OR.
        \item \code{\link{StSignificanceTestingCadVsRadiologists}}: Perform 
           significance testing, CAD vs. radiologists.
        \item \code{\link{StSignificanceTestingCrossedModalities}}: Perform 
           significance testing using crossed modalities analysis.
        \item \code{\link{StSignificanceTestingSingleFixedFactor}}: Perform 
           significance testing for single fixed factor analysis.
  }
}

\section{Miscellaneous and Utility Functions}{
  \itemize{    
        \item \code{\link{Compare3ProperRocFits}}: Compare three proper-ROC 
           curve fitting models.
        \item \code{\link{UtilAucBinormal}}: Binormal model AUC function.
        \item \code{\link{UtilAucCBM}}: CBM AUC function.
        \item \code{\link{UtilAucPROPROC}}: PROPROC AUC function.
        \item \code{\link{UtilAucsRSM}}: RSM ROC/AFROC AUC calculator.
        \item \code{\link{UtilFigureOfMerit}}: Calculate empirical figures of merit 
           (FOMs) for specified dataset.
        \item \code{\link{UtilIntrinsic2PhysicalRSM}}: Convert from intrinsic to 
           physical RSM parameters. 
        \item \code{\link{UtilLesionDistr}}: Calculates the lesion distribution  
           matrix.
        \item \code{\link{UtilLesionWeightsDistr}}: Calculates the lesion weights  matrix.
        \item \code{\link{UtilMeanSquares}}: Calculates the mean squares used in the 
           DBMH and ORH methods.  
        \item \code{\link{UtilOutputReport}}: Generate a formatted report file.
        \item \code{\link{UtilPhysical2IntrinsicRSM}}: Convert from physical to 
           intrinsic RSM parameters.  
        \item \code{\link{UtilPseudoValues}}: Return jackknife pseudovalues.  
        \item \code{\link{UtilVarComponentsDBM}}: Utility for Dorfman-Berbaum-Metz 
           variance components.  
        \item \code{\link{UtilVarComponentsOR}}: Utility for Obuchowski-Rockette 
           variance components.  
  }
}

\author{
\itemize{
  \item Creator: Dev Chakraborty \email{dpc10ster@gmail.com}.
  \item Creator: Xuetong Zhai \email{xuetong.zhai@gmail.com}.
  \item Contributor: Lucy D'Agostino McGowan \email{ld.mcgowan@vanderbilt.edu}.
  \item Contributor: Alejandro RodriguezRuiz \email{Alejandro.RodriguezRuiz@radboudumc.nl}.
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

  Thompson JD, Chakraborty DP, Szczepura K, et al. (2016) Effect of reconstruction methods 
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
