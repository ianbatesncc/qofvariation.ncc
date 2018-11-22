#' Download disease models from PHE fingertips
#'
#'  Two sets - fingertips and diabetes separate.
#'  Combine for later processing and merging into QOFe
#'
#'

# Download ####

#' Download PHE fingertips model
#'
#'
download_ft <- function(
    bWriteCSV = FALSE
) {
    require("dplyr")
    require("data.table")
    require("fingertipsR")

    # I know the name of the profile and the domain I want

    this_profile_name <- "Modelled prevalence estimates"
    this_domain_name <- "Modelled estimates"

    # API: get profile id
    cat("INFO: download_ft: API: get profile id", "...", "\n")

    this_profile <- profiles(ProfileName = this_profile_name) %>%
        filter(DomainName == this_domain_name)

    # API: get profile indicator ids
    cat("INFO: download_ft: API: get profile indicator ids", "...", "\n")

    these_indicators <- indicators(
        #ProfileID = this_profile$ProfileID
        DomainID = this_profile$DomainID
    )

    # API: get indicator metadata
    cat("INFO: download_ft: API: get indicator metadata", "...", "\n")

    ft_meta <- indicator_metadata(DomainID = this_profile$DomainID)

    # I want GP and CCGs

    # API: get area type ids
    cat("INFO: download_ft: API: get area type ids", "...", "\n")

    all_areatypes <- area_types()

    gp_areatype <- all_areatypes %>% filter(AreaTypeName %like% "Practice") %>%
        select(starts_with("Area")) %>%
        unique()

    # API: get indicator specific area types
    cat("INFO: download_ft: API: get indicator specific area types", "...", "\n")

    these_indicator_areatypes <- these_indicators$IndicatorID %>%
        lapply(indicator_areatypes) %>%
        bind_rows() %>%
        filter(AreaTypeID == gp_areatype$AreaTypeID)

    # API: get data
    cat("INFO: download_ft: API: get data", "...", "\n")

    ft_data <- fingertips_data(
        IndicatorID = these_indicator_areatypes$IndicatorID
        , AreaTypeID = gp_areatype$AreaTypeID
    )

    ft <- list(
        ft_meta = ft_meta %>% setnames.clean()
        , ft_data = ft_data %>% setnames.clean()
    )

    # Save

    if (bWriteCSV)
        save_ft(ft, prefix = "__raw")

    # Return

    invisible(ft)
}

#' Download source diabetes model
#'
download_dm <- function(bForceOverwrite = FALSE) {
    this_xlsx <- "./data-raw/phe-dm/Diabetes_prevalence_estimates_for_CCGs_by_GP_registered_populations.xlsx"

    # this_url <- "https://www.gov.uk/government/publications/diabetes-prevalence-estimates-for-local-populations"
    this_url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/611266/Diabetes_prevalence_estimates_for_CCGs_by_GP_registered_populations.xlsx"

    if ((!file.exists(this_xlsx)) || (bForceOverwrite == TRUE)) {
        cat("INFO: download_dm: downloading", "...", "\n")
        cat("INFO: URL :", this_url, "\n")
        cat("INFO: DEST:", this_xlsx, "\n")

        status <- download.file(url = this_url, destfile = this_xlsx, mode = "wb")
    } else {
        status <- (!file.exists(this_xlsx)) + 0L # 0 - exists, 1 - does not exist
    }

    invisible(status)
}


#' Save ft data and meta
#'
#' @param ft (list of data.frames) with named items ft_data, ft_meta
#'
#' @return (bool) TRUE
#'
save_ft <- function(
    ft
    , bWriteData = TRUE
    , bWriteMeta = TRUE
    , prefix = ""
) {
    require("data.table")

    these_csvs <- c(
        ft_meta = paste0("./data-raw/phe-dm/ft_meta", prefix, ".csv")
        , ft_data = paste0("./data-raw/phe-dm/ft_data", prefix, ".csv")
    )

    if (bWriteMeta) {
        cat("INFO: save_ft: saving", these_csvs["ft_meta"], "...", "\n")
        fwrite(ft$ft_meta, these_csvs["ft_meta"])
    }

    if (bWriteData) {
        cat("INFO: save_ft: saving", these_csvs["ft_data"], "...", "\n")
        fwrite(ft$ft_data, these_csvs["ft_data"])
    }

    return(TRUE)
}

#' load the ft meta and data
#'
load_ft <- function(prefix = "") {
    require("data.table")

    these_csvs <- c(
        ft_meta = paste0("./data-raw/phe-dm/ft_meta", prefix, ".csv")
        , ft_data = paste0("./data-raw/phe-dm/ft_data", prefix, ".csv")
    )

    lapply(
        these_csvs
        , function(x) {
            cat("INFO: load_ft: loading", x, "...", "\n")
            if (file.exists(x)) {
                fread(x)
            } else {
                cat("WARNING: load_ft: file not found", "\n")
                NULL
            }
        }
    )
}

#' load the diabetes data
#'
load_dm <- function() {
    require("data.table")

    this_csv <- "./data-raw/phe-dm/dm_data.csv"

    cat("INFO: load_dm: loading", this_csv, "...", "\n")
    if (file.exists(this_csv)) {
        fread(this_csv)
    } else {
        cat("WARNING: load_dm: file not found", "\n")
        NULL
    }
}

# Extract ####

#' Function to grab xl and put into an R object for later manipulation.
#'
#'
extract_dm <- function(
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
        this_csv <- "./data-raw/phe-dm/dm_data.csv"

        cat("INFO: extract_dm: saving", this_csv, "...", "\n")
        fwrite(rv, this_csv)
    }

    invisible(rv)
}

# Transform ####

#' Process the ft download
#'
#'  - obtain CCG level estimates based on all age denominator
#'
#'
process_ft <- function(
    ft
    , bWriteCSV = FALSE
) {
    require("dplyr")

    # AreaType == GP, Age == "All ages" has a denominator ... useful for
    # estimating / approximating CCG level values
    #
    # Gather denominator
    # tag back into indicators
    # weighted sum at CCG level
    #
    # when incorporating with QOF data ... need to recall that the value, not
    # the counts/denominator, are more accurate ... and to rebase counts on qof
    # list sizes

    gp_allage_den <- ft$ft_data %>%
        filter(AreaType == "GP", Age == "All ages") %>%
        select(
            starts_with("Area")
            , starts_with("Parent")
            , Sex, Age
            , Denominator
            , starts_with("Time")
        ) %>%
        filter(!is.na(Denominator)) %>%
        unique()

    # assemble GP denominator

    gp_data_allage <- ft$ft_data %>%
        filter(AreaType == "GP") %>%
        select(-Denominator) %>%
        merge(
            gp_allage_den %>% select(AreaCode, Denominator)
            , by = "AreaCode"
            , all.x = TRUE, all.y = FALSE
        )

    # estimate CCG value

    ccg_data_allage <- gp_data_allage %>%
        mutate(Valuenote = trimws(paste(Valuenote, "all age denominator"))) %>%
        mutate(Count = Value * Denominator) %>%
        group_by_at(vars(
            -Value, -Count, -Denominator
            , -AreaCode, -AreaName
            , -ends_with("limit")
        )) %>%
        summarise_at(vars(Count, Denominator), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(Value = Count / Denominator) %>%
        rename(AreaCode = "ParentCode", AreaName = "ParentName") %>%
        mutate(AreaType = "CCGs (2017/18)") %>%
        select(starts_with("Indicator"), starts_with("Area"), Value, Valuenote) %>%
        setDT()

    # tag back into dataset

    ft$ft_data <- ft$ft_data %>% setDT()

    ft$ft_data[
        ccg_data_allage, on = c("AreaType", "AreaCode")
        , `:=`(Value = i.Value, Valuenote = i.Valuenote)
        ]

    # Save

    if (bWriteCSV)
        save_ft(ft, bWriteMeta = FALSE)

    # return

    invisible(ft)
}

# Load ####

#' Process the chain
#'
#'  - download
#'  - add CCG totals
#'  - restore
#'
main__download_ft <- function(
    bDownload = FALSE
    , bAddCCGs = FALSE
    , bWriteCSV = FALSE
) {

    if (!any(c(bDownload, bAddCCGs, bWriteCSV))) {
        ft <- load_ft()

    } else {

        if (bDownload) {
            ft <- download_ft(bWriteCSV = TRUE)
        } else {
            ft <- load_ft(prefix = "__raw")
        }

        if (bAddCCGs) {
            ft <- process_ft(ft, bWriteCSV = bWriteCSV)
        }
    }

    invisible(ft)
}

#' Load diabetes dataset
#'
#' Process if necessary, or just load existing.
#'
main__download_dm <- function(
    bForceDownload = FALSE
    , bWriteCSV = FALSE
) {

    if (!any(c(bForceDownload, bWriteCSV))) {
        rv <- load_dm()

    } else {
        download_dm(bForceOverwrite = bForceDownload)
        rv <- extract_dm(bWriteCSV = bWriteCSV)

        if (bWriteCSV)
            rv <- load_dm()
    }

    invisible(rv)
}

main__load_dpm <- function() {
    ft <- main__download_ft()
    dm <- main__download_dm()
}
