#
# helper-qof.R
#

qof_root = c(
    "qof-1718", "qof-1617"
    , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
    , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
    , "qof-0506", "qof-0405"
)[2]

bExtractFromRaw = FALSE
bWriteCSV = FALSE
bSaveData = FALSE
bLoadData = FALSE

qof_roots <- c(
    "qof-1718", "qof-1617"
    , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
    , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
    , "qof-0506", "qof-0405"
)[c(2, 3)]


# find unique values across a number of dataframes
#
# function to take a list of dataframes, extract values from common field
# name, and find unique value
#
l__unique <- function(x, method = c("sapply", "bindrows")) {
    method <- match.arg(method)
    if (method == "bindrows") {
        x %>% bind_rows() %>% .$qof_period %>% unique()
    } else {
        x %>%
            lapply(function(y) {
                y$qof_period %>% unique()}
            ) %>%
            unlist() %>% unique()
    }
}
