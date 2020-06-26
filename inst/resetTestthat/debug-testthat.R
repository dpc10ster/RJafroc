CompareLists <- function(x1, x2, d = 0, i = 0, j = 0) 
{
  for (t in 1:length(x1)) {
    y1 <- unlist(x1[[t]]); attributes(y1) <- NULL; y1 <- y1[!is.na(y1)]
    y2 <- unlist(x2[[t]]); attributes(y2) <- NULL; y2 <- y2[!is.na(y2)]
    # x11 <- x1[[t]][q]; attributes(x11) <- NULL
    # x22 <- x2[[t]][q]; attributes(x22) <- NULL
    expect_equal(y1, y2, 
                 info = 
                   paste0 ("Dataset = ",d,", FOM = ",
                           i,", method = ",j, ", t = ", t))
  }
}



dataset_arr <- list(dataset02, dataset05) # deparse(substitute(dataset02)) does not work below
dataset_arr_str <- c("dataset02", "dataset05")
FOM_arr <- c("Wilcoxon", "HrAuc") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
method_arr <- c("DBM", "OR")

i <- 2;d <- 2;j <- 1

dataset <- dataset_arr[[d]]

fn <- paste0(test_path(), "/goodValues361/SigTest/", dataset_arr_str[d], FOM_arr[i], method_arr[j], ".rds")
if (!file.exists(fn)) {
  warning(paste0("File not found - generating new ",fn))
  x1 <- StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j])
  saveRDS(x1, file = fn)
}

x1 <- readRDS(fn)
x2 <- StSignificanceTesting(dataset, FOM = FOM_arr[i],method = method_arr[j])

CompareLists(x1,x2, d, i, j)
