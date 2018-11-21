#' Extract diabetes prevelance from PHE model
#'

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
