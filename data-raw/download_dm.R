#' Download disease models from PHE fingertips
#'
# Download ####

#' Download PHE fingertips model
#'
#'
download_dm <- function(
    bWriteCSV = FALSE
) {
    require("dplyr")
    require("data.table")
    require("fingertipsR")

    # I know the name of the profile and the domain I want

    this_profile_name <- "Modelled prevalence estimates"
    this_domain_name <- "Modelled estimates"

    # API: get profile id
    cat("INFO: download_dm: API: get profile id", "...", "\n")

    this_profile <- profiles(ProfileName = this_profile_name) %>%
        filter(DomainName == this_domain_name)

    # API: get profile indicator ids
    cat("INFO: download_dm: API: get profile indicator ids", "...", "\n")

    these_indicators <- indicators(
        #ProfileID = this_profile$ProfileID
        DomainID = this_profile$DomainID
    )

    # API: get indicator metadata
    cat("INFO: download_dm: API: get indicator metadata", "...", "\n")

    ft_meta <- indicator_metadata(DomainID = this_profile$DomainID)

    # I want GP and CCGs

    # API: get area type ids
    cat("INFO: download_dm: API: get area type ids", "...", "\n")

    all_areatypes <- area_types()

    gp_areatype <- all_areatypes %>% filter(AreaTypeName %like% "Practice") %>%
        select(starts_with("Area")) %>%
        unique()

    # API: get indicator specific area types
    cat("INFO: download_dm: API: get indicator specific area types", "...", "\n")

    these_indicator_areatypes <- these_indicators$IndicatorID %>%
        lapply(indicator_areatypes) %>%
        bind_rows() %>%
        filter(AreaTypeID == gp_areatype$AreaTypeID)

    # API: get data
    cat("INFO: download_dm: API: get data", "...", "\n")

    ft_data <- fingertips_data(
        IndicatorID = these_indicator_areatypes$IndicatorID
        , AreaTypeID = gp_areatype$AreaTypeID
    )

    # Save

    if (bWriteCSV) {

        these_csvs <- c(
            ft_meta = "./data-raw/phe-dm/ft_dm_meta.csv"
            , ft_data = "./data-raw/phe-dm/ft_dm_data__noccg.csv"
        )

        cat("INFO: download_dm: saving", these_csvs["ft_meta"], "...", "\n")
        fwrite(ft_meta, these_csvs["ft_meta"])

        cat("INFO: download_dm: saving", these_csvs["ft_data"], "...", "\n")
        fwrite(ft_data, these_csvs["ft_data"])
    }

    # Return

    retval <- list(
        ft_meta = ft_meta
        , ft_data = ft_data
    )

}

#' load the meta and data
#'
load_dm <- function(
    bWithCCG = TRUE
) {
    require("data.table")

    these_csvs <- c(
        ft_meta = "./data-raw/phe-dm/ft_dm_meta.csv"
        , ft_data = "./data-raw/phe-dm/ft_dm_data__noccg.csv"
    )

    if (bWithCCG == TRUE) {
        these_csvs <- gsub("__noccg\\.csv", ".csv", these_csvs)
    }


    lapply(
        these_csvs
        , function(x) {
            cat("INFO: load_dm: loading", x, "...", "\n")
            if (file.exists(x)) {
                fread(x)
            } else {
                cat("WARNING: file not found", "\n")
                NULL
            }
        }
    )
}

# Extract ####
# Transform ####

#' Process the ft download
#'
#'  - obtain CCG level estimates based on all age denominator
#'
#'
process_dm <- function(
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

    if (bWriteCSV) {

        these_csvs <- c(
            ft_data = "./data-raw/phe-dm/ft_dm_data.csv"
        )

        cat("INFO: process_dm: saving", these_csvs["ft_data"], "...", "\n")
        fwrite(ft$ft_data, these_csvs["ft_data"])
    }

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
main__download_dm <- function(
    bDownload = FALSE
    , bAddCCGs = FALSE
    , bWriteCSV = FALSE
) {

    if (!any(c(bDownload, bAddCCGs, bWriteCSV))) {
        ft <- load_dm(bWithCCG = TRUE)

    } else {

        if (bDownload) {
            ft <- download_dm(bWriteCSV = bWriteCSV)
        } else {
            ft <- load_dm(bWithCCG = FALSE)
        }

        if (bAddCCGs) {
            ft <- process_dm(ft, bWriteCSV = bWriteCSV)
        }
    }

    invisible(ft)
}

