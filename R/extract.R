#
# extract.R
#
# Extract data and put into R data object
#

#' load raw QOF data
#'
#' put in an R list for later analysis
#'
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   \code{qof-YYZZ}
#'
#' @param bSaveData (boolean) Flag to determine whether to save datasets as
#'   \code{devtools::use_data}
#'
#' @return a list of lists with named items
#' \describe{
#'     \item{reference}{
#'         \itemize{\item{orgref}\item{indmap}}
#'     }
#'     \item{data}{
#'         \itemize{\item{prev}\item{ind}}
#'     }
#' }
#'
# @importFrom purrr walk2
# @importFrom devtools use_data
#'
#' @family Internal routines
#' @family Load routines
#'
f__extract__load_raw <- function(
    qof_root = c("qof-1617", "qof-1516", "qof-1415", "qof-1314")[4]
    , bSaveData = FALSE
) {
    cat("INFO: f__extract__load_raw: loading data ...", "\n")

    qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")

    if (qof_root %in% c("qof-1617", "qof-1516")) {

        # 1617, 1516 ####

        this.file <- proj_path(qof_data_path, "ORGANISATION_REFERENCE.csv")
        qof.orgref <- fread(file = this.file) %>% setnames.clean()

        this.file <- proj_path(qof_data_path, "INDICATOR_MAPPINGS.csv")
        qof.indmap <- fread(file = this.file) %>% setnames.clean()

        this.file <- proj_path(qof_data_path, "PREVALENCE.csv")
        qof.prev <- fread(file = this.file) %>% setnames.clean()

        this.file <- proj_path(qof_data_path, "ACHIEVEMENT_EXCEPTIONS.csv")
        if (!(file.exists(this.file)))
            this.file <- proj_path(qof_data_path, "ACHIEVEMENT.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean()

    } else {

        cat("WARNING: extract not defined for", qof_root, "\n")

        qof.orgref <- NA
        qof.indmap <- NA
        qof.prev <- NA
        qof.ind <- NA
    }

    retval <- list(
        meta_org <- qof.orgref
        , meta_ind <- qof.indmap
        , data_prev <- qof.prev
        , data_ind <- qof.ind
    )

    generic_names <- c("meta_org", "meta_ind", "data_prev", "data_ind")

    # save

    if (bSaveData == TRUE) {


        these_names <- paste(gsub("-", "_", qof_root), generic_names, sep = "_")

        names(retval) <- these_names

        retval %>% purrr::walk2(
            ., names(.)
            , function(obj, name) {
                assign(name, obj)
                do.call("use_data", list(as.name(name), overwrite = TRUE))
                rm(name)
            }
        )
    }

    # return

    return(list(
        reference = list(
            orgref = qof.orgref
            , indmap = qof.indmap
        )
        , data = list(
            prev = qof.prev
            , ind = qof.ind
        )
    ))
}
