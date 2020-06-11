#' 
#' TONY FROC dataset
#'
#' This is referred to in the book as the "TONY" dataset. It consists of 185 cases, 
#' 89 of which are diseased, interpreted in two treatments
#' ("BT" = breast tomosynthesis and "DM" = digital mammography) by five radiologists using the FROC paradigm. 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:5, 1:185, 1:3], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:5, 1:89, 1:2], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:89], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:89, 1:2], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:89, 1:2], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset01", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "TONY", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:5, 1:185, 1:4] 1 1 1 1 ..., truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "BT" "DM", treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Chakraborty DP, Svahn T (2011) Estimating the parameters of a model 
#' of visual search from ROC data: an alternate method for fitting proper ROC curves. 
#' PROC SPIE 7966.
#' 
#' @examples
#' str(dataset01)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset01, opChType = "wAFROC")$Plot
#'
"dataset01"
#'
#'
#'
#' Van Dyke ROC dataset
#'
#' This is referred to in the book as the "VD" dataset. It consists of 114 cases,
#'    45 of which are diseased, interpreted in two treatments ("0" = single spin echo 
#'    MRI, "1" = cine-MRI) by five radiologists using the ROC 
#'    paradigm. Each diseased cases had an aortic dissection; the ROC paradigm 
#'    generates one rating per case.  Often referred to in the ROC literature as the 
#'    Van Dyke dataset, which, along with the Franken dataset, has been widely 
#'    used to illustrate advances in ROC methodology. The example below displays 
#'    the ROC plot for the first treatment and first reader.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:5, 1:114, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:5, 1:45, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:45], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:45, 1], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:45, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset02", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "VAN-DYKE", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:5, 1:114, 1:2] 1 1 1 1 ..., truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "0" "1", treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "0" "1" "2" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Van Dyke CW, et al. Cine MRI in the diagnosis of thoracic 
#' aortic dissection. 79th RSNA Meetings. 1993.
#' 
#' @examples
#' str(dataset02)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset02, opChType = "ROC")$Plot
#'
#'
#'
"dataset02"
#'
#' 
#' 
#' Franken ROC dataset
#'
#' This is referred to in the book as the "FR" dataset. It consists of 100 cases, 
#' 67 of which are diseased, interpreted in two treatments,
#' "0" = conventional film radiographs, "1" = digitized  images viewed on monitors, by four 
#' radiologists using the ROC paradigm. Often referred to in the ROC literature as the 
#' Franken-dataset, which, along the the Van Dyke dataset, has been widely used to illustrate
#' advances in ROC methodology. 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:4, 1:100, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:4, 1:67, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:67], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:67, 1], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:67, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset03", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "FRANKEN", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:4, 1:100, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "TREAT1" "TREAT2", treatment labels}
#' \item{\code{descriptions$readerID}}{, chr chr [1:4] "READER_1" "READER_2" "READER_3" "READER_4", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Franken EA, et al. Evaluation of a Digital Workstation for Interpreting 
#' Neonatal Examinations: A Receiver Operating Characteristic Study. Investigative Radiology. 
#' 1992;27(9):732-737.
#' 
#' @examples
#' str(dataset03)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset03, opChType = "ROC")$Plot
#'
#'
"dataset03"
#'
#'
#'
#' Federica Zanca FROC dataset
#'
#' This is referred to in the book as the "FED" dataset. It consists of 200 mammograms, 
#' 100 of which contained one to 3 simulated microcalcifications,
#' interpreted in five treatments (basically different image processing algorithms) by four 
#' radiologists using the FROC paradigm and a 5-point rating scale. The maximum number of NLs 
#' per case, over the entire dataset was 7 and the dataset contained at least one diseased 
#' mammogram with 3 lesions. The Excel file containing this dataset is 
#' /inst/extdata/datasets/FZ_ALL.xlsx. The normal cases are labeled 100:199 while the normal 
#' cases are labeled 0:99.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:5, 1:4, 1:200, 1:7], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:5, 1:4, 1:100, 1:3], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:100], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:100, 1:3], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:100, 1:3], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset04", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "FEDERICA", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:5, 1:4, 1:200, 1:4], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:5] "1" "2" "3" "4" "5", treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:4] "1" "3" "4" "5", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Zanca F et al. Evaluation of clinical image processing algorithms used 
#' in digital mammography. Medical Physics. 2009;36(3):765-775.
#' 
#' @examples
#' str(dataset04)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset04, opChType = "wAFROC")$Plot
#'
#'
"dataset04"
#'
#'
#'
#' John Thompson FROC dataset
#'
#' This is referred to in the book as the "JT" dataset. It consists of 92 cases, 47 of 
#' which are diseased, interpreted in two treatments 
#' ("1" = CT images acquired for attenuation correction, "2" = diagnostic CT images), by nine 
#' radiographers using the FROC paradigm. Each case was a slice of an anthropomorphic phantom
#' 47 with inserted nodular lesions (max 3 per slice). The maximum number of NLs per case, over the entire 
#' dataset was 7.  
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:9, 1:92, 1:7], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:9, 1:47, 1:3], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:47], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:47, 1:3], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:47, 1:3], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset05", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "THOMPSON", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:9, 1:92, 1:4], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "1" "2", treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:4] "1" "2" "3" "4", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Thompson JD  et al. Effect of reconstruction methods and x-ray tube 
#' current-time product  on nodule detection in an anthropomorphic thorax phantom: 
#' a crossed-treatment JAFROC observer study. Medical Physics. 2016;43(3):1265-1274.
#' 
#' @examples
#' str(dataset05)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset05, opChType = "wAFROC")$Plot
#'
#'
"dataset05"
#'
#'
#'
#' Magnus FROC dataset
#'
#' This is referred to in the book as the "MAG" dataset (after Magnus Bath, 
#' who conducted the JAFROC analysis). It consists of 100 cases, 69 of which are diseased, 
#' interpreted in two treatments ("1" = conventional chest, "1" = chest tomosynthesis) by four 
#' radiologists using the FROC paradigm.  
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:4, 1:89, 1:17], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:4, 1:42, 1:15], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:42], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:42, 1:15], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:42, 1:15], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset06", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "MAGNUS", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:4, 1:89, 1:16], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "1" "2", treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:4] "1" "2" "3" "4", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Vikgren J  et al. Comparison of Chest Tomosynthesis and Chest Radiography 
#' for Detection of Pulmonary Nodules: Human Observer Study of Clinical Cases. 
#' Radiology. 2008;249(3):1034-1041.
#' 
#' @examples
#' str(dataset06)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset06, opChType = "wAFROC")$Plot
#'
#'
"dataset06"
#'
#' 
#' 
#' Lucy Warren FROC dataset
#'
#' This is referred to in the book as the "OPT" dataset (for OptiMam). It consists of 162 cases, 
#' 81 of which are diseased, interpreted in five treatments (see reference, basically different ways
#' of acquiring the images) by seven radiologists using the FROC paradigm.  
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:5, 1:7, 1:162, 1:4], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:5, 1:7, 1:81, 1:3], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:81], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:81, 1:3], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:81, 1:3], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset07", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "LUCY-WARREN", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:5, 1:7, 1:162, 1:4], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, [1:5] "1" "2" "3" "4" ..., treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:7] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Warren LM, Mackenzie A, Cooke J, et al. Effect of image quality on 
#' calcification detection in digital mammography. 
#' Medical Physics. 2012;39(6):3202-3213.
#' 
#' @examples
#' str(dataset07)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset07, opChType = "wAFROC")$Plot
#'
#'
"dataset07"
#'
#'
#'
#' Monica Penedo ROC dataset
#'
#' This is referred to in the book as the "PEN" dataset. It consists of 112 cases, 
#' 64 of which are diseased, interpreted in five treatments (basically different image compression
#' algorithms) by five 
#' radiologists using the FROC paradigm (the inferred ROC dataset is included; the original FROC data
#' is lost). 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:5, 1:5, 1:112, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:5, 1:5, 1:64, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:64], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:64, 1], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:64, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset08", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "PENEDO", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:5, 1:5, 1:112, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:5] "0" "1" "2" "3" ..., treatment labels}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "0" "1" "2" "3" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Penedo et al. Free-Response Receiver Operating Characteristic 
#' Evaluation of Lossy JPEG2000 and Object-based Set Partitioning in 
#' Hierarchical Trees Compression of Digitized Mammograms. 
#' Radiology. 2005;237(2):450-457.
#' 
#' @examples
#' str(dataset08)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset08, opChType = "ROC")$Plot
#'
#'
"dataset08"
#'
#' 
#' 
#' Nico Karssemeijer ROC dataset (CAD vs. radiologists)
#'
#' This is referred to in the book as the "NICO" dataset. It consists of 200 mammograms, 
#' 80 of which contain one malignant mass, 
#' interpreted by a CAD system and nine radiologists using the 
#' LROC paradigm. The first reader is CAD. The highest rating was used to convert this to an ROC 
#' dataset. The original LROC data is \code{datasetCadLroc}. Analyzing this 
#' data requires methods described in the book, implemented in the function  
#' \code{\link{StSignificanceTestingCadVsRadiologists}}.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1:10, 1:200, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1, 1:10, 1:80, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:80], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:80, 1], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:80, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset09", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "NICO-CAD-ROC", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1, 1:10, 1:200, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:10] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Hupse R  et al. Standalone computer-aided detection compared to radiologists' 
#' performance for the detection of mammographic masses. Eur Radiol. 2013;23(1):93-100.
#' 
#' @examples
#' str(dataset09)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset09, rdrs = 1:10, opChType = "ROC")$Plot
#'
#'
"dataset09"
#'
#'
#'
#' Marc Ruschin ROC dataset
#'
#' This is referred to in the book as the "RUS" dataset. It consists of 90 cases, 
#' 40 of which are diseased, the images were
#' acquired at three dose levels, which can be regarded as treatments.
#' "0" = conventional film radiographs, "1" = digitized  images viewed on monitors, Eight 
#' radiologists interpreted the cases using the FROC paradigm. These have been reduced to 
#' ROC data by using the highest ratings (the original FROC data is lost). 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:3, 1:8, 1:90, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:3, 1:8, 1:40, 1] , ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:40], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:40, 1], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:40, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset10", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "RUSCHIN", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:3, 1:8, 1:90, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:3] "1" "2" "3", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:8] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Ruschin M, et al. Dose dependence of mass and microcalcification 
#' detection in digital mammography: free response human observer studies. 
#' Med Phys. 2007;34:400 - 407.
#' 
#' 
#' @examples
#' str(dataset10)
#' PlotEmpiricalOperatingCharacteristics(dataset = dataset10, opChType = "ROC")$Plot
#'
#'
"dataset10"
#'
#' 
#' 
#' Dobbins 1 FROC dataset
#'
#' This is referred to in the book as the "DOB1" dataset. Dobbins et al conducted a 
#' multi-institutional, MRMC study to compare the performance of digital tomosynthesis 
#' (GE's VolumeRad device), dual-energy (DE) imaging, and conventional chest 
#' radiography for pulmonary nodule detection and management. 
#' All study images were obtained with a flat-panel detector developed by GE. 
#' The case set consisted of 158 subjects, of which 43 were non-diseased and 
#' the rest had 1 - 20 pulmonary nodules independently verified, using with CT 
#' images, by 3 experts who did not participate in the observer study. The 
#' study used FROC paradigm data collection. There are 
#' 4 treatments labeled 1 - 4 (conventional chest x-ray, CXR, CXR augmented
#' with dual-energy (CXR+DE), VolumeRad digital tomosynthesis images and 
#' VolumeRad augmented with DE (VolumeRad+DE). 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:4, 1:5, 1:158, 1:4], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:4, 1:5, 1:115, 1:20], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:115], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:115, 1:20], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:115, 1:20], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset11", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "DOBBINS-1", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:4, 1:5, 1:158, 1:21], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:4] "1" "2" "3" "4", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Dobbins III JT et al. Multi-Institutional Evaluation of Digital 
#' Tomosynthesis, Dual-Energy Radiography, and Conventional Chest Radiography 
#' for the Detection and Management of Pulmonary Nodules. Radiology. 2016;282(1):236-250.
#' 
#' @examples
#' str(dataset11)
#'
#'
"dataset11"
#'
#'
#'
#' Dobbins 2 ROC dataset
#'
#' This is referred to in the code as the "DOB2" dataset. It contains actionability
#' ratings, i.e., do you recommend further follow up on the patient, one a 1 (definitely not)
#' to 5 (definitely yes), effectively an ROC dataset using a 5-point rating scale.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:4, 1:5, 1:152, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:4, 1:5, 1:88, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:88], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:88, 1], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:88, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset12", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "DOBBINS-2", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:4, 1:5, 1:152, 1:2] , truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:4] "1" "2" "3" "4", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Dobbins III JT et al. Multi-Institutional Evaluation of Digital 
#' Tomosynthesis, Dual-Energy Radiography, and Conventional Chest Radiography 
#' for the Detection and Management of Pulmonary Nodules. Radiology. 2016;282(1):236-250.
#' 
#' @examples
#' str(dataset12)
#'
#'
"dataset12"
#'
#' 
#' 
#' Dobbins 3 FROC dataset
#'
#' This is referred to in the code as the "DOB3" dataset. This is a subset of DOB1 which includes
#' data for lesions not-visible on CXR, but visible to truth panel on all treatments.  
#' 
##' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:4, 1:5, 1:158, 1:4], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:4, 1:5, 1:106, 1:15], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:106], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:106, 1:15], numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:106, 1:15], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset13", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "DOBBINS-3", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:4, 1:5, 1:158, 1:16], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:4] "1" "2" "3" "4", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Dobbins III JT et al. Multi-Institutional Evaluation of Digital 
#' Tomosynthesis, Dual-Energy Radiography, and Conventional Chest Radiography 
#' for the Detection and Management of Pulmonary Nodules. Radiology. 2016;282(1):236-250.
#' 
#' @examples
#' str(dataset13)
#'
#'
"dataset13"
#'
#'
#'
#' Federica Zanca real (as opposed to inferred) ROC dataset
#'
#' This is referred to in the book as the "FZR" dataset. It is a real ROC study, 
#' conducted on the same images and using the same radiologists, on treatments
#' "4" and "5" of dataset04. This was compared to highest rating inferred ROC
#' data from dataset04 to conclude, erroneously, that the highest rating assumption
#' is invalid. See book Section 13.6.2.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:4, 1:200, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:4, 1:100, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:100], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:100, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:100, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "dataset14", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "FEDERICA-REAL-ROC", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:4, 1:200, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "4" "5", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:4] "1" "2" "3" "4", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Zanca F, Hillis SL, Claus F, et al (2012) Correlation of free-response and 
#' receiver-operating-characteristic area-under-the-curve estimates: Results from 
#' independently conducted FROC/ROC studies in mammography. 
#' Med Phys. 39(10):5917-5929.
#' 
#' @examples
#' str(dataset14)
#'
#'
"dataset14"
#'
#' 
#' 
#' Binned dataset suitable for checking \code{\link{FitCorCbm}}; seed = 123
#'
#' A binned dataset suitable for analysis by \code{\link{FitCorCbm}}. It was generated by
#'    \link{DfCreateCorCbmDataset} by setting the \code{seed} variable to 123. Note
#'    the formatting of the data as a single treatment two reader dataset, even though
#'    the actual pairing might be different, see \code{\link{FitCorCbm}}. The dataset is 
#'    intentionally large so as to demonstrate the asymptotic convergence of ML estimates, 
#'    produced by \code{\link{FitCorCbm}}, to the population values. The data was generated
#'    by the following argument values to \code{\link{DfCreateCorCbmDataset}}: seed = 123, 
#'    K1 = 5000, K2 = 5000, desiredNumBins = 5, muX = 1.5, muY = 3, alphaX = 0.4, 
#'    alphaY = 0.7, rhoNor = 0.3, rhoAbn2 = 0.8. 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1:2, 1:10000, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1, 1:2, 1:5000, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:5000], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:5000, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:5000, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetBinned123", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "SIM-CORCBM-SEED-123", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:2] "1" "2", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Zhai X, Chakraborty DP (2017). A bivariate contaminated binormal model for robust 
#'    fitting of proper ROC curves to a pair of correlated, possibly degenerate, 
#'    ROC datasets. Medical Physics. 44(6):2207--2222.

#' @examples
#' str(datasetBinned123)
#'
"datasetBinned123"
#'
#'
#'
#' Binned dataset suitable for checking \code{\link{FitCorCbm}}; seed = 124
#'
#' A binned dataset suitable for analysis by \code{\link{FitCorCbm}}. It was generated by
#'    \code{\link{DfCreateCorCbmDataset}} by setting the \code{seed} variable to 124. 
#'    Otherwise similar to \code{\link{datasetBinned123}}. 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1:2, 1:10000, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1, 1:2, 1:5000, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:5000], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:5000, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:5000, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetBinned124", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "SIM-CORCBM-SEED-124", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:2] "1" "2", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Zhai X, Chakraborty DP (2017). A bivariate contaminated binormal model for robust 
#'    fitting of proper ROC curves to a pair of correlated, possibly degenerate, 
#'    ROC datasets. Medical Physics. 44(6):2207--2222.

#' @examples
#' str(datasetBinned124)
#'
"datasetBinned124"
#'
#'
#' 
#' Binned dataset suitable for checking \code{\link{FitCorCbm}}; seed = 125
#'
#' A binned dataset suitable for analysis by \code{\link{FitCorCbm}}. It was generated by
#'    \code{\link{DfCreateCorCbmDataset}} by setting the \code{seed} variable to 125. 
#'    Otherwise similar to \code{\link{datasetBinned123}}. 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1:2, 1:10000, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1, 1:2, 1:5000, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:5000], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:5000, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:5000, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetBinned125", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "SIM-CORCBM-SEED-125", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:2] "1" "2", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Zhai X, Chakraborty DP (2017). A bivariate contaminated binormal model for robust 
#'    fitting of proper ROC curves to a pair of correlated, possibly degenerate, 
#'    ROC datasets. Medical Physics. 44(6):2207--2222.

#' @examples
#' str(datasetBinned125)
#'
"datasetBinned125"
#'
#'
#'
#' 
#' 
#' Nico Karssemeijer LROC dataset (CAD vs. radiologists)
#'
#' This is the actual LROC data corresponding to \code{dataset09}, which was the inferred
#' ROC data. Note that the \code{LL} field is split into two, \code{LLCl}, representing true 
#' positives where the lesions were correctly localized, and \code{LLIl}, representing true 
#' positives where the lesions were incorrectly localized. The first reader is CAD
#' and the remaining readers are radiologists. 
#' ##
#' ## The function TBA
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1:10, 1:200, 1], ratings of localizations on normal cases}
#' \item{\code{rating$LL}}{, num [1, 1:10, 1:80, 1], ratings of correct localizations on abnormal cases}
#' \item{\code{rating$LL_IL}}{num [1, 1:10, 1:80, 1], ratings of incorrect localizations on abnormal cases}
#' \item{\code{lesions$perCase}}{, int [1:80], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:80, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:80, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetCadLroc", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "LROC", the data type}
#' \item{\code{descriptions$name}}{, chr "NICO-CAD-LROC", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:4, 1:200, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:10] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Hupse R  et al. Standalone computer-aided detection compared to radiologists' 
#' performance for the detection of mammographic masses. Eur Radiol. 2013;23(1):93-100.
#' 
#' @examples
#' str(datasetCadLroc)
#'
#'
"datasetCadLroc"
#'
#'
#'
#' 
#'
#' Simulated FROC CAD vs. RAD dataset
#'
#' Simulated FROC CAD vs. RAD dataset suitable for checking code. It was generated 
#'    from datasetCadLroc using SimulateFrocFromLrocData.R. The LROC paradigm always 
#'    yields a single mark per case. Therefore 
#'    the equivalent FROC will also have only one mark per case. The NL arrays 
#'    of the two datasets are identical. The LL array is created by copying the
#'    LL (correct localiztion) array of the LROC dataset to the LL array of the FROC 
#'    dataset, from 
#'    diseased case index k2 = 1 to k2 = K2. Additionally, the LL_IL array of the 
#'    LROC dataset is copied to the NL array of the FROC dataset, starting at case 
#'    index k1 = K1+1 to k1 = K1+K2. Any zero ratings are replace by -Infs. The 
#'    equivalent FROC dataset has the same HrAuc as the original LROC dataset. 
#'    See example. The main use of this dataset & function is to test the CAD significance
#'    testing functions using CAD FROC datasets, which I currently don't have.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1:10, 1:200, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1, 1:10, 1:80, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:80], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:80, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:80, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetCadSimuFroc", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "LROC", the data type}
#' \item{\code{descriptions$name}}{, chr "NICO-CAD-LROC", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, num [1:2, 1:4, 1:200, 1:2], truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:10] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
"datasetCadSimuFroc"
#'
#'
#'
#' 
#' John Thompson crossed treatment FROC dataset
#'
#' This is a crossed treatment dataset, see book Section 18.5. There are two treatment factors. 
#' The first treatment factor \code{modalityID1} can be "F" or "I", which represent two CT reconstruction
#' algorithms. The second treatment factor \code{modalityID2} can be "20" "40"  "60"  "80", which 
#' represent the mAs values of the image acquisition. The factors are fully crossed. The function 
#' \code{\link{StSignificanceTestingCrossedModalities}} analyzes such datasets.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:4, 1:11, 1:68, 1:5], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:4, 1:11, 1:34, 1:3], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:34], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:34, 1:3] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:34, 1:3], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetCrossedModality", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "THOMPSON-X-MOD", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "F" "I", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:4] "20" "40" "60" "80", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @references Thompson JD, Chakraborty DP, Szczepura K, et al. (2016) Effect of reconstruction 
#' methods and x-ray tube current-time product  on nodule detection in an 
#' anthropomorphic thorax phantom: a crossed-treatment JAFROC observer study. 
#' Medical Physics. 43(3):1265-1274.
#' 
#' @examples
#' str(datasetCrossedModality)
#'
#'
"datasetCrossedModality"
#'
#' 
#' 
#'
#' Simulated degenerate ROC dataset (for testing purposes)
#'
#' A simulated degenerated dataset. A degenerate dataset is defined as one with
#' no interior operating points on the ROC plot. Such data tend to be observed with expert level 
#' radiologists. This dataset is used to illustrate the robustness of two fitting models, namely
#' CBM and RSM. The widely used binormal model and PROPROC fail on such datasets. 
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1, 1, 1:15, 1], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1, 1, 1:10, 1], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:10], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:10, 1] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:10, 1], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetDegenerate", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROC", the data type}
#' \item{\code{descriptions$name}}{, chr "SIM-DEGENERATE", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr "1", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr "1", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' str(datasetDegenerate)
#'
#'
"datasetDegenerate"
#'
#'
#' 
#'
#'
#' Simulated FROC SPLIT-PLOT dataset
#'
#' Simulated from FED Excel dataset by successively ignoring readers 3:4, c(1,3:4), 
#' c(1:2,4), etc.
#' created simulated split plot Excel dataset from Fed dataset: 
#' confirmed it is read without error 
#'    
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:4, 1:200, 1:7], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:4, 1:100, 1:3], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:100], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:100, 1:3] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:100, 1:3], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetFROCSp", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "FROC", the data type}
#' \item{\code{descriptions$name}}{, chr "SIM-FROC-SPLIT-PLOT", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "4" "5", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:4] "1" "3" "4" "5", reader labels}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' str(datasetFROCSp)
#'
"datasetFROCSp"
#'
#'
#'
#'
#' 
#' Simulated ROI dataset
#'
#' TBA Simulated ROI dataset: assumed are 4 ROIs per case, 5 readers, 50 non-dieased and 40 diseased cases.
#' 
#' @format A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
#'    contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
#'    contain 3 elements, \code{$perCase}, \code{$IDS} and \code{$weights} as sub-lists; \code{$descriptions}
#'    contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
#'    \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
#' \itemize{
#' \item{\code{rating$NL}}{, num [1:2, 1:5, 1:90, 1:4], ratings of non-lesion localizations, NLs}
#' \item{\code{rating$LL}}{, num [1:2, 1:5, 1:40, 1:4], ratings of lesion localizations, LLs}
#' \item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
#' \item{\code{lesions$perCase}}{, int [1:40], number of lesions per diseased case}
#' \item{\code{lesions$IDs}}{, num [1:40, 1:4] , numeric labels of lesions on diseased cases}
#' \item{\code{lesions$weights}}{, num [1:40, 1:4], weights (or clinical importances) of lesions}
#' \item{\code{descriptions$fileName}}{, chr, "datasetROI", base name of dataset in `data` folder}
#' \item{\code{descriptions$type}}{, chr "ROI", the data type}
#' \item{\code{descriptions$name}}{, chr "SIM-ROI", the name of the dataset}
#' \item{\code{descriptions$truthTableStr}}{, NA, truth table structure}
#' \item{\code{descriptions$design}}{, chr "FCTRL-X-MOD", study design, factorial dataset}
#' \item{\code{descriptions$modalityID}}{, chr [1:2] "1" "2", treatment label(s)}
#' \item{\code{descriptions$readerID}}{, chr [1:5] "1" "2" "3" "4" ..., reader labels}
#' }
#'
#' @keywords datasets
#'
#' 
#' @examples
#' str(datasetROI)
#'
#'
"datasetROI"
#'
#'




