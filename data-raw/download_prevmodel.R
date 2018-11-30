#' Download disease models from PHE fingertips
#'
#'  Two sets - fingertips and diabetes separate.
#'
#'  This script makes the raw data files available.  Further processing is done
#'  within the package including maing the datasets available as ackage data
#'  items.
#'
#'  Combine for later processing and merging into QOFe
#'
#' DM:
#' "./data-raw/phe-dm/Diabetes_prevalence_estimates_for_CCGs_by_GP_registered_populations.xlsx"
#' FT:
#' "./data-raw/phe-dm/ft_data__raw.csv"
#' "./data-raw/phe-dm/ft_meta__raw.csv"
#' POP:
#' ./data-raw/phe-dm/*MYEB1*.csv

setnames.clean <- function(x) {
    setnames(x, gsub("\\.*$", "", make.names(tolower(colnames(x)))))
}

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

#' Download England population for ageband proportions
#'
#' https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
#'
#' \code{*MYEB*.csv}
#'
download_pop <- function(
    bForceDownload = FALSE
    , bForceUnzip = FALSE
) {
    this_url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2017detailedtimeseries/ukdetailedtimeseries2001to2017.zip"
    this_zip <- "./data-raw/phe-dm/ukdetailedtimeseries2001to2017.zip"

    if ((!file.exists(this_zip)) | (bForceDownload == TRUE)) {
        cat("INFO: download_pop: downloading zip", "...", "\n")

        status <- download.file(url = this_url, destfile = this_zip, mode = "wb")
    }

    if (file.exists(this_zip)) {

        these_csvs <- unzip(this_zip, list = TRUE)
        this_csv <- these_csvs %>% filter(Name %like% "MYEB1") %>% .$Name

        this_csv <- paste(dirname(this_zip), this_csv, sep = "/")

        if ((!file.exists(this_csv)) || (bForceUnzip == TRUE)) {
            cat("INFO: download_pop: unzipping csv", "...", "\n")

            unzip(
                this_zip
                , files = this_csv
                , overwrite = TRUE
                , exdir = dirname(this_zip)
            )
        }

        if (file.exists(this_csv)) {
            retval <- this_csv

        } else {
            cat("WARNING: download_pop: csv file", this_csv, "not found ...", "\n")
            retval <- NA
        }

    } else {
        cat("WARNING: download_pop: zip file", this_zip, "not found ...", "\n")
        retval <- NA
    }

    return(retval)
}


#' Save ft data and meta
#'
#' @param ft (list of data.frames) with named items ft_data, ft_meta
#'
#' @return (bool) TRUE
#'
save_ft <- function(
    ft
    , prefix = "__raw"
) {
    require("data.table")
    require("purrr")

    these_csvs <- c(
        ft_meta = paste0("./data-raw/phe-dm/ft_meta", prefix, ".csv")
        , ft_data = paste0("./data-raw/phe-dm/ft_data", prefix, ".csv")
    )

    list(x = ft, y = these_csvs) %>%
        purrr::pwalk(function(x, y) {
            cat("INFO: save_ft: saving", y, "...", "\n")
            fwrite(x, y)
        })

    invisible(these_csvs)
}

save_dm <- function(
    dm
    , this_csv = "./data-raw/phe-dm/dm_data__raw.csv"
) {
    cat("INFO: save_dm: saving", this_csv, "...", "\n")
    fwrite(dm, this_csv)

    invisible(this_csv)
}

load_dm <- function(
    this_csv = "./data-raw/phe-dm/dm_data__raw.csv"
) {
    cat("INFO: load_dm: loading", this_csv, "...", "\n")

    fread(this_csv)
}

#' load the ft meta and data
#'
load_ft <- function(prefix = "__raw") {
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

load_pop <- function() {
    require("data.table")

    this_csv <- download_pop()

    cat("INFO: load_pop: loading", this_csv, "...", "\n")
    if (file.exists(this_csv)) {
        fread(this_csv) %>% setnames.clean()
    } else {
        cat("WARNING: load_pop: file not found", "\n")
        NULL
    }
}

# Extract ####

#' Function to grab xl and put into an R object for later manipulation.
#'
#'
extract_dm <- function() {
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
                setnames(c("areaname", "areacode", "numerator", "value")) %>%
                mutate(period = as.numeric(x))
        }
        , this_xlsx
    ) %>%
        bind_rows() %>%
        filter(!is.na(areaname)) %>%
        mutate(
            denominator = numerator / value
            , multiplier = 1
        )

    invisible(rv)
}

# Transform ####

#' Process the ft download
#'
#'  - obtain CCG level estimates based on all age denominator
#'
#'
transform_ft__add_ccgs <- function(
    ft
) {
    require("dplyr")

    cat("INFO: transform_ft__add_ccgs: adding CCGs", "...", "\n")

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
        filter(areatype == "GP", age == "All ages") %>%
        select(
            starts_with("area")
            , starts_with("parent")
            , sex, age
            , denominator
            , starts_with("time")
        ) %>%
        filter(!is.na(denominator)) %>%
        unique()

    # assemble GP denominator

    gp_data_allage <- ft$ft_data %>%
        filter(areatype == "GP") %>%
        select(-denominator) %>%
        merge(
            gp_allage_den %>% select(areacode, denominator)
            , by = "areacode"
            , all.x = TRUE, all.y = FALSE
        )

    # estimate CCG value

    ccg_data_allage <- gp_data_allage %>%
        mutate(count = value * denominator) %>%
        select(
            -starts_with("area")
            , -ends_with("limit")
            , -starts_with("comparedto")
            , -valuenote, -newdata, -recenttrend
        ) %>%
        group_by_at(vars(
            -value, -count, -denominator
        )) %>%
        summarise_at(vars(count, denominator), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(value = count / denominator) %>%
        rename(areacode = "parentcode", areaname = "parentname") %>%
        mutate(areatype = "CCGs (2017/18)") %>%
        mutate(valuenote = "value weighted by all age denominator") %>%
        select(starts_with("indicator"), starts_with("area"), value, valuenote) %>%
        setDT()

    # tag back into dataset

    ft$ft_data <- ft$ft_data %>% setDT()

    ft$ft_data[
        ccg_data_allage, on = c("indicatorid", "areatype", "areacode")
        , `:=`(value = i.value, valuenote = i.valuenote)
        ]

    # return

    invisible(ft)
}

#' Align to QOF
#'
#' - add list_type
#' - add domain_code
#' - additionally strip to minimum fields
#'
#' May need to map domain_code to qof periods ...
#'
transform_ft__align_qof <- function(
    ft
) {
    cat("INFO: transform_ft__align_qof: aligning", "...", "\n")

    lu <- fread(input = "
indicatorid, indicatorname, age, indicator_group_code, model_list_type
92658, Estimated prevalence of COPD (all ages),                               All ages,  COPD, TOTAL
92659, Estimated prevalence of diagnosed hypertension (16+),                  16+ yrs,   HYP_DIAG, 16OV
92660, Estimated prevalence of undiagnosed hypertension (16+),                16+ yrs,   HYP_UNDIAG, 16OV
92661, Estimated prevalence of depression (all ages),                         All ages,  DEP, TOTAL
92663, Estimated prevalence of stroke (55-79 yrs),                            55-79 yrs, STIA, 55_79
92783, Estimated prevalence of Heart failure (16+),                           16+ yrs,   HF, 16OV
92847, Estimated prevalence of CHD (55-79 yrs),                               55-79 yrs, CHD, 55_79
92848, Estimated prevalence of peripheral arterial disease (PAD) (55-79 yrs), 55-79 yrs, PAD, 55_79
99265960, Estimated prevalence of diagnosed AND undiagnosed hypertension (16+),  16+ yrs,   HYP, 16OV
")

    lu_hyp <- fread(input = "
indicatorid, indicatorid_new, indicatorname, age, indicator_group_code, model_list_type
92659, 99265960, Estimated prevalence of diagnosed AND undiagnosed hypertension (16+),  16+ yrs,   HYP, 16OV
92660, 99265960, Estimated prevalence of diagnosed AND undiagnosed hypertension (16+),  16+ yrs,   HYP, 16OV
")

    # Care needed when readjusting model estimatesd proportions to QOF
    #
    # - some models use different definitions so not appropriate for direct
    # comparison
    # - where age bands are different can adjust.
    #
    # - model and qof SAME: no adjustment
    # - model and qof DIFFERENT:
    #   ? model narrower: assume outside narrow band numbers are zero: reduce
    #     model proportion by population weighted factor
    #   ? model wider: assume small numbers in different age band difference no
    #     adjustment to model
    #

    # merge the two hypertension estimates into one

    ft$ft_data <- list(
        ft$ft_data %>%
            filter(!(indicatorid %in% lu_hyp$indicatorid))

        , ft$ft_data %>%
            filter((indicatorid %in% lu_hyp$indicatorid)) %>%
            mutate(
                indicatorid = unique(lu_hyp$indicatorid_new)
                , indicatorname = unique(lu_hyp$indicatorname)
            ) %>%
            group_by_at(vars(
                -value
                , -ends_with("limit")
                , -starts_with("compared")
            )) %>%
            summarise_at(vars(value), sum) %>%
            ungroup()
    ) %>% bind_rows()

    # tag on group_code and list_type

    ft$ft_data <- ft$ft_data %>%
        select_at(setdiff(names(.), c("indicator_group_code", "model_list_type"))) %>%
        setDF() %>%
        merge(
            lu %>% select(indicatorid, indicator_group_code, model_list_type)
            , by = "indicatorid"
            , all.x = TRUE, all.y = FALSE
        )

    invisible(ft)
}

#' Add more fields to align with qof
#'
#'  - indicator_group, list_type
#'
#'
transform_dm__align_qof <- function(
    dm
) {
    cat("INFO: transform_dm__align_qof: aligning", "...", "\n")

    dm %>%
        mutate(
            indicator_group_code = "DM"
            , model_list_type = "16OV"
        )
}

transform_pop__eng_totals <- function(
    pop
) {
    require("data.table")
    require("dplyr")
    require("stringr")

    cat("INFO: transform_pop__eng_totals: aggregating", "...", "\n")

    # get England totals, colapse on gender, melt on period

    pop_eng <- pop %>%
        filter(country == "E") %>%
        setDT() %>%
        melt(
            measure.vars = patterns("^population_")
            , variable.name = "period", variable.factor = FALSE
        ) %>%
        mutate(period = as.numeric(str_sub(period, -4, -1))) %>%
        group_by_at(vars(age, period)) %>%
        summarise_at(vars(value), sum) %>%
        ungroup() %>%
        mutate(org_code = "E92000001", org_name = "England")

    return(pop_eng)
}

#' Find england totals and proportions
#'
#'
transform_pop__model_bands <- function(
    pop_eng
) {

    cat("INFO: transform_pop__model_bands: calculating", "...", "\n")

    lu_bands <- fread(input = "
list_type,age_start,age_end
TOTAL,0,90
16OV,16,90
17OV,17,90
18OV,18,90
55_79,55,79
30_74,30,74
50OV,50,90
")

    pop_eng_ab <- pop_eng %>%
        merge(lu_bands) %>%
        filter(age >= age_start, age <= age_end) %>%
        group_by_at(vars(-age, -value)) %>%
        summarise_at(vars(value), sum) %>%
        ungroup() %>%
        mutate(nyears = age_end - age_start + 1) %>%
        select(-starts_with("age"))

    lu_measures <- merge(
        data.frame(from = lu_bands$list_type)
        , data.frame(to = lu_bands$list_type)
    )

    pop_conversions <- lu_measures %>%
        merge(
            pop_eng_ab %>%
                select(list_type, period, value_from = value)
            , by.x = "from", by.y = "list_type"
        ) %>%
        merge(
            pop_eng_ab %>%
                select(list_type, period, value_to = value)
            , by.x = c("to", "period"), by.y = c("list_type", "period")
        ) %>%
        mutate(p_from_to = value_to / value_from, multiplier = 1)

    return(pop_conversions)
}

#' Combine models into one R object
#'
transform_combine <- function(
    ft
    , dm
) {
    list(
        ft$ft_data %>%
            filter(areatype %like% "CCG") %>%
            mutate(org_type = "ccg::instance") %>%
            select(
                indicator_group_code
                , model_list_type
                , period = timeperiod
                , org_code = areacode
                , org_name = areaname
                , org_type
                , value
            )

        , dm %>%
            mutate(org_type = "ccg::instance") %>%
            select(
                indicator_group_code
                , model_list_type
                , period
                , org_code
                , org_name
                , org_type
                , value
            )
    ) %>%
        bind_rows()
}

# Load ####

#' Process the chain
#'
#'  - download
#'  - add CCG totals
#'  - restore
#'
dpm__download_prevmodel <- function(
    bRefreshDownloads = FALSE
    , bForceDownload = FALSE
) {
    require("dplyr")

    if (bRefreshDownloads | bForceDownload) {
        download_ft(bWriteCSV = TRUE)
        download_dm(bForceOverwrite = bForceDownload)
        extract_dm() %>% save_dm()
    }

    ft <- load_ft()
    dm <- load_dm()

    invisible(list(ft = ft, dm = dm))
}



if (FALSE) {
    lu_ccgs_geo <- ft$ft_data %>%
        filter(areatype %like% "CCG") %>%
        filter(grepl("Basset|Mansf|Newark|Notting|Rush", areaname)) %>%
        count(areatype, areacode, areaname)

    ft <- ft %>%
        transform_ft__add_ccgs() %>%
        transform_ft__align_qof()

    dm <- dm %>%
        transform_dm__align_qof()

    dpm <- transform_combine(ft, dm)

    dpm %>%
        #filter(period == 2015) %>%
        filter(org_code %in% lu_ccgs_geo$areacode) %>%
        dcast(
            period + org_code ~ indicator_group_code
            , value.var = "value", fun = sum
            , fill = NA_real_
        ) %>%
        View()

    # pop_conversions <- load_pop() %>%
    #     transform_pop__eng_totals() %>%
    #     transform_pop__model_bands()

    invisible(dpm)
}

#' add health codes to ccg gss geo codes
#'
#' uses an internal fuzzy name to ccg health code lookup
#' matches on specified field containing name and adds the health code in th
#'
#' @param lu (data.frame) the data to tag health codes to
#' @param field_lu_name (character) Name of the field containing the CCG name
#' @param field_lu_code (character) Name of the field to retunrn health code in
#'
#' @example
#' my_lu <- data.frame(
#'   source = "example"
#'   , areaname = "Example CCG"
#'   , areacode = "E3800999"
#' )
#'
#' my_lu_new <- dpm_transform__create_ccg_gss_health_map(my_lu)
#'
#' data.frame(
#'   source = "example"
#'   , areaname = "Example CCG"
#'   , areacode = "E3800999"
#'   , areacode_health = "SOMETHINGNEW"
#' )
#'
dpm_transform__create_ccg_gss_health_map <- function(
    lu
    , field_lu_name = "areaname"
    , field_lu_code = "areacode_health"
) {
    require("dplyr")
    require("purrr")

    lu_ccg_partial <- fread(input = "
partial,code_health
Bass,02Q
City,04K
Mans,04E
Newa,04H
Nort,04L
West,04M
Rush,04N
"
    )

    lu_match <- lu_ccg_partial %>%
        purrr::pmap(
            function(partial, code_health, this_lu) {
                this_lu %>%
                    filter_at(
                        vars(field_lu_name)
                        , all_vars(. %like% partial)
                    ) %>%
                    mutate(!!field_lu_code := code_health)
            }
            , this_lu = lu %>% select_at(vars(field_lu_name))
        ) %>% bind_rows()

    lu <- merge(lu, lu_match, by = field_lu_name, all.x = TRUE, all.y = FALSE)

    # guarantee field_lu_code is added (can be missing if no matches)
    if (!(field_lu_code %in% names(lu_new))) {
        lu <- lu %>% mutate(!!field_lu_code := NA_character_)
    }

    return(lu)
}
