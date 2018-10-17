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
#'   "qof-YYZZ"
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
#' @family Internal routines
#' @family Load routines
#'
f__91__load_raw <- function(
    qof_root
    , bSaveData = FALSE
) {
    cat("INFO: f__91__load_raw: loading data ...", "\n")

    require("data.table")

    taskdir <- proj_root()

    this.file <- paste_paths("./data-raw/", paste0(qof_root, "-csv/ORGANISATION_REFERENCE.csv"))
    qof.orgref <- fread(file = this.file) %>% setnames.clean()

    this.file <- paste_paths("./data-raw/", paste0(qof_root, "-csv/INDICATOR_MAPPINGS.csv"))
    qof.indmap <- fread(file = this.file) %>% setnames.clean()

    this.file <- paste_paths("./data-raw/", paste0(qof_root, "-csv/PREVALENCE.csv"))
    qof.prev <- fread(file = this.file) %>% setnames.clean()

    this.file <- paste_paths("./data-raw/", paste0(qof_root, "-csv/ACHIEVEMENT_EXCEPTIONS.csv"))
    if (!(file.exists(this.file)))
        this.file <- paste_paths("./data-raw/", paste0(qof_root, "-csv/ACHIEVEMENT.csv"))
    qof.ind <- fread(file = this.file) %>% setnames.clean()

    retval <- list(
        meta_org <- qof.orgref
        , meta_ind <- qof.indmap
        , data_prev <- qof.prev
        , data_ind <- qof.ind
    )

    generic_names <- c("meta-org", "meta-ind", "data-prev", "data-ind")

    # save

    if (bSaveData == TRUE) {

        require(purrr)
        require(devtools)

        these_names <- paste(qof_root, generic_names, sep = "-") %>%
            gsub("-", "_", .)

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
