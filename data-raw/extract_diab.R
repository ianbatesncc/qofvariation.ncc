#' Extract diabetes prevelance from PHE model
#'

# Download ####

#' Download source
#'
download_diab <- function(bForceOverwrite = FALSE) {
    this_xlsx <- "./data-raw/phe-dm/Diabetes_prevalence_estimates_for_CCGs_by_GP_registered_populations.xlsx"

    # this_url <- "https://www.gov.uk/government/publications/diabetes-prevalence-estimates-for-local-populations"
    this_url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/611266/Diabetes_prevalence_estimates_for_CCGs_by_GP_registered_populations.xlsx"

    if ((!file.exists(this_xlsx)) || (bForceOverwrite == TRUE)) {
        cat("INFO: download_diab: downloading", "...", "\n")
        cat("INFO: URL :", this_url, "\n")
        cat("INFO: DEST:", this_xlsx, "\n")

        status <- download.file(url = this_url, destfile = this_xlsx, mode = "wb")
    } else {
        status <- (!file.exists(this_xlsx)) + 0L # 0 - exists, 1 - does not exist
    }

    invisible(status)
}

# Extract ####
# Transform ####

#' Function to grab xl and put into an R object for later manipulation.
#'
#'
extract_diab <- function(
    bWriteCSV = TRUE
) {
    require("readxl")
    require("data.table", warn.conflicts = FALSE)
    require("dplyr", warn.conflicts = FALSE)

    this_xlsx <- "./data-raw/phe-dm/Diabetes_prevalence_estimates_for_CCGs_by_GP_registered_populations.xlsx"

    these_sheets <- excel_sheets(this_xlsx)
    list(wb = this_xlsx, ws = these_sheets)

    rv <- these_sheets[-1] %>% lapply(
        function(x, y) {
            cat("INFO: reading \\", x, "/", "...", "\n")
            read_xlsx(path = y, sheet = x, range = "b12:e256") %>%
                setDT() %>%
                setnames(c("org_name", "org_code", "numerator", "value")) %>%
                mutate(period = as.numeric(x))
        }
        , this_xlsx
    ) %>%
        bind_rows() %>%
        filter(!is.na(org_name)) %>%
        mutate(
            denominator = numerator / value
            , units = 1
        )

    if (bWriteCSV) {
        this_csv <- "./data-raw/phe-dm/phe_diab_data.csv"

        cat("INFO: extract_diab: saving", this_csv, "...", "\n")
        fwrite(rv, this_csv)
    }

    invisible(rv)
}

#' load the data
#'
load_diab <- function() {
    require("data.table")

    this_csv <- "./data-raw/phe-dm/phe_diab_data.csv"

    cat("INFO: load_diab: loading", this_csv, "...", "\n")
    if (file.exists(this_csv)) {
        fread(this_csv)
    } else {
        cat("WARNING: file not found", "\n")
        NULL
    }
}

# Load ####

#' Load dataset
#'
#' Proces sif necessary, or just load existing.
#'
main__download_diab <- function(
    bForceDownload = FALSE
    , bWriteCSV = FALSE
) {

    if (!any(c(bForceDownload, bWriteCSV))) {
        rv <- load_diab()

    } else {
        download_diab(bForceOverwrite = bForceDownload)
        rv <- extract_diab(bWriteCSV = bWriteCSV)

        if (bWriteCSV)
            rv <- load_diab()
    }

    invisible(rv)
}
