#
# extract.R
#
# Extract data and put into R data object
#

options(warn = 1)

#' Store an object
#'
#' Save an object with specified name.
#'
#' @param obj (object) object to save
#' @param name (char) name to use
#' @param dir (char) directory to save to.
#'
#' if \code{dir} is \code{data} (default) then usethis::use_data is used.  if is
#' \code{inst/data-int} then a call to \code{save} is made to the inst/data-int
#' dir.
#'
#' @importFrom usethis use_data
#' @importFrom usethis proj_path
#'
store_data <- function(obj, name, dir = c("inst/data-int", "data")) {
    dir <- match.arg(dir)

    if (verbosity.showatlevel("chatty")) {
        cat("INFO: dir, (name, obj) =", dir, ", (", name, ", ", "\n")
        fglimpse(obj)
        cat(")", "\n")
    }

    retval <- switch(
        dir
        , "data" = {
            # to be explicit about the function to call
            l_use_data <- usethis::use_data

            assign(name, obj)
            do.call("l_use_data", list(as.name(name), overwrite = TRUE))
            rm(name)
        }
        , "inst/data-int" = {
            this_rds <- usethis::proj_path("inst/data-int/", name, ext = "rds")
            saveRDS(obj, file = this_rds, compress = "bzip2")
        }
    )

    return(TRUE)
}

#' Restore an object
#'
#' Load an object with specified name.
#'
#' @inheritParams store_data
#'
restore_data <- function(name, dir = c("inst/data-int", "data")) {
    dir <- match.arg(dir)

    retval <- switch(
        dir
        , "data" = {
            cat("WARNING: restore_data: not yet implemented", "\n")
            NULL
        }
        , "inst/data-int" = {
            this_rds <- usethis::proj_path("inst/data-int/", name, ext = "rds")
            if (file.exists(this_rds)) {
                if (verbosity.showatlevel("chatty"))
                    cat("INFO: restore_data: reading", this_rds, "...", "\n")

                readRDS(file = this_rds)
            } else {
                if (verbosity.showatlevel("chatty"))
                    cat("WARNING: restore_data: NOT found", this_rds, "...", "\n")
                NULL
            }
        }
    )

    return(retval)
}


#' local worker to loop through list of xls
#'
#' @param this.file pathname of xl workbook
#' @param f_readxl_ws function to parse the workbook
#' @param skipsheets list of sheet anems to skip
#' @param ... additional arguments passed to f_readxl_ws
#'
#' @return (data.frame likely) results of parsing workbook with
#'   \code{f_readxl_ws}
#'
#' @import readxl
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
#'
#' @family Extract routines
#'
l_readxl_wb <- function(
    this.file
    , f_readxl_ws = NULL
    , skipsheets = NULL
    , ...
) {
    if (is.null(f_readxl_ws)) {
        #stop("need to specify a function to loop over the workbook")
        f_readxl_ws <- function(...){}
    }

    l_fileexists_canreadsheets <- function(this.file) {
        bFileExists <- file.exists(this.file)
        if (bFileExists)
            bCanReadSheets <- !(
                "try-error" %in%
                    class(try(readxl::excel_sheets(this.file), silent = TRUE))
            )
        else
            bCanReadSheets <- FALSE

        return(bFileExists && bCanReadSheets)
    }

    # try alternative .xl version, i.e. .xls if .xlsx, .xlsx if .xls
    if (!l_fileexists_canreadsheets(this.file)) {
        msg <- "WARNING: specified xl not found"

        this_ext <- tools::file_ext(this.file)
        alt_ext <- switch(this_ext, xlsx = "xls", xls = "xlsx", NULL)

        this.file.alt <- paste(
            tools::file_path_sans_ext(this.file), alt_ext
            , sep = "."
        )

        if (l_fileexists_canreadsheets(this.file.alt)) {
            msg <- paste(msg, "...", "alternative found")
            this.file <- this.file.alt
        } else {
            msg <- paste(msg, "...", "alternative NOT found")
        }

        if (verbosity.showatlevel("chatty"))
            cat(msg, "\n")
    }

    if (verbosity.showatlevel("chatty"))
        cat("INFO: workbook: [", basename(this.file), "]", "\n")

    # may fail if workbook does not exist, or cannot open for whatever reason.
    # ... so skip!
    xl <- try(
        list(
            wb = this.file
            , sheets = readxl::excel_sheets(this.file)
        )
    )

    if (!("try-error" %in% class(xl))) {
        retval <- xl$sheets %>%
            setdiff(skipsheets) %>%
            lapply(f_readxl_ws, this.file, ...) %>%
            bind_rows()
    } else {
        cat("WARNING: error reading", this.file, "...", "skipping", "\n")
        retval <- NULL
    }

    return(retval)
}

#' local worker to read through worksheets in workbook
#'
#' variation for ind
#'
#' @param this.sheet worksheet to parse the workbook
#' @param this.file pathname of xl workbook
#'
#' @return (data.frame likely) result of parsing the worksheet
#'
#' @import readxl
#'
#' @family Extract routines
#'
l_readxl_ws_ind <- function(
    this.sheet
    , this.file
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: - worksheet: \\", this.sheet, "/") #, "\n")

    ws <- readxl::read_excel(
        path = this.file
        , sheet = this.sheet
        , skip = 13
    )

    # choose fields to keep

    all_fields <- names(ws)
    id_fields <- ws[FALSE, ] %>%
        select(ends_with(" Code"), ends_with(" Name")) %>%
        names()

    is_prac <- intersect(id_fields, "Practice Code")
    is_num <- setdiff(all_fields, id_fields)
    is_ndep <- grep(" (Numerator|Denominator|Exceptions|Points)$", is_num, value = TRUE)

    is_char <- ws[FALSE, ] %>% select_if(is.character) %>% names()
    is_ndep_char <- intersect(is_ndep, is_char)

    if (length(is_ndep_char) > 0) {
        # want to convert char to num, any str goes to NA
        suppressWarnings(
            ws <- ws %>% mutate_at(vars(is_ndep_char), as.numeric)
        )
    }

    if (verbosity.showatlevel("chatty"))
        cat(", measures [", paste(is_ndep, collapse = " | "), "]", "\n")

    dat <- ws %>%
        select_at(vars(is_prac, is_ndep)) %>%
        setDT() %>%
        melt(
            id.vars = is_prac
            , variable = "qof_measure", variable.factor = FALSE
        ) %>%
        rename(practice_code = "Practice Code")
}


#' local worker to read through worksheets in workbook
#'
#' variation for prev
#'
#' @param this.sheet worksheet to parse the workbook
#' @param this.file pathname of xl workbook
#'
#' @return (data.frame likely) result of parsing the worksheet
#'
#' @import readxl
#'
#' @family Extract routines
#'
l_readxl_ws_prev <- function(this.sheet, this.file) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: - worksheet: \\", this.sheet, "/") #, "\n")

    ws <- readxl::read_excel(
        path = this.file
        , sheet = this.sheet
        , skip = 13
        , col_types = "text"
    ) %>% mutate(sheet = this.sheet)

    # choose fields to keep

    all_fields <- names(ws)
    id_fields <- ws[FALSE, ] %>%
        select(ends_with(" Code"), ends_with(" Name"), sheet) %>%
        names()

    is_prac <- intersect(id_fields, c("Practice Code", "sheet"))
    is_num <- setdiff(all_fields, id_fields)

    # suppress expected warnings issued when encountering NA in numeric fields.
    suppressWarnings(
        ws <- ws %>% mutate_at(vars(is_num), as.numeric)
    )

    if (verbosity.showatlevel("chatty"))
        cat(", measures [", paste(is_num, collapse = " | "), "]", "\n")

    dat <- ws %>%
        select_at(vars(is_prac, is_num)) %>%
        setDT() %>%
        melt(
            id.vars = is_prac
            , variable = "tbl_heading", variable.factor = FALSE
        ) %>%
        rename(practice_code = "Practice Code")
}


#' load raw QOF data
#'
#' put in an R list for later analysis
#'
#' @param qof_root (character)
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   \code{qof-YYZZ}
#'
#' @param bSaveData (bool) Flag to determine whether to save datasets as
#'   \code{devtools::use_data}
#'
#' @return a list with named items
#' \tabular{ll}{
#'   \code{meta_org}  \tab Organisation metadata \cr
#'   \code{meta_ind}  \tab Indicator metadata \cr
#'   \code{data_prev} \tab Prevalence data \cr
#'   \code{data_ind}  \tab Indicator data \cr
#' }
#'
#' @importFrom purrr pwalk
#' @import readxl
#'
#' @family Internal routines
#' @family Load routines
#' @family Extract routines
#'
f__extract__load_raw <- function(
    qof_root = c(
        "qof-1718", "qof-1617"
        , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
        , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
        , "qof-0506", "qof-0405"
    )
    , bSaveData = FALSE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__extract__load_raw: loading data", "...", "\n")

    qof_root <- match.arg(qof_root)

    qof_data_path <- paste("data-raw", paste0(qof_root, "-csv"), sep = "/")

    if (qof_root %in% c("qof-1718", "qof-1617", "qof-1516")) {

        # 1718, 1617, 1516 ####
        # qof_root <- "qof-1718" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")
        # qof_root <- "qof-1617" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")
        # qof_root <- "qof-1516" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")

        this.file <- usethis::proj_path(qof_data_path, "ORGANISATION_REFERENCE.csv")
        qof.orgref <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_MAPPINGS.csv")
        qof.indmap <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "PREVALENCE.csv")
        qof.prev <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "ACHIEVEMENT_EXCEPTIONS.csv")
        if (!(file.exists(this.file)))
            this.file <- usethis::proj_path(qof_data_path, "ACHIEVEMENT.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean()


    } else if (qof_root %in% c("qof-1415")) {

        # 1415 ####
        # qof_root <- "qof-1415" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")

        # qof.orgref ####

        this.file <- usethis::proj_path(qof_data_path, "PRAC_CONTROL.csv")
        qof.orgref <- fread(file = this.file) %>% setnames.clean() %>%
            select(grep("_code$|_name$", names(.), value = TRUE)) %>%
            rename(
                stp_code = "area_team_code"
                , stp_name = "area_team_name"
                , subregion_code = "sub_region_code"
                , subregion_geography_code = "sub_region_geography_code"
                , subregion_name = "sub_region_name"
            )

        # qof.indmap ####

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_REFERENCE_FINAL.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_GRP_REFERENCE_FINAL.csv")
        qof.grp <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_DOM_REFERENCE_FINAL.csv")
        qof.dom <- fread(file = this.file) %>% setnames.clean()

        qof.indmap <- qof.ind %>%
            select(
                -ends_with("_threshold")
                , -indicator_type
                , -indicator_version
                , indicator_group_code = "indicator_group"
            ) %>%
            merge(
                qof.grp %>% select(starts_with("indicator_group_"), domain_code)
                , by = "indicator_group_code"
                , all.x = TRUE, all.y = FALSE
            ) %>%
            merge(
                qof.dom %>% select(starts_with("domain_"), -domain_points)
                , by = "domain_code"
                , all.x = TRUE, all.y = FALSE
            ) %>%
            mutate(patient_list_type = NA_character_)

        rm(qof.ind, qof.grp, qof.dom)

        # qof.prev ####

        this.file <- usethis::proj_path(qof_data_path, "PREVALENCE_BY_PRAC_v2.csv")
        qof.prev <- fread(file = this.file) %>% setnames.clean() %>%
            select(
                -ends_with("_name")
                , -ends_with("_description")
                , -prevalence
                , register = register_size
                , patient_list_size = practice_list_size
            ) %>%
            mutate(patient_list_type = NA_character_) %>%
            mutate_at(vars(register), as.numeric)


        # qof.ind ####

        # Need to merge in register and exceptions

        this.file <- usethis::proj_path(qof_data_path, "EXCEPTION_OUTPUT.csv")
        qof.exc <- fread(file = this.file) %>% setnames.clean() %>%
            select(
                practice_code
                , indicator_code
                , exceptions = exception_count
            )

        this.file <- usethis::proj_path(qof_data_path, "REGISTER_SIZES.csv")
        qof.reg <- fread(file = this.file) %>% setnames.clean() %>%
            rename(register = register_size) %>%
            mutate(indicator_group_code = substr(indicator_code, 1, nchar(indicator_code) - 3)) %>%
            select(-indicator_code)

        this.file <- usethis::proj_path(qof_data_path, "INDICATORS_BY_PRAC.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean() %>%
            rename(indicator_group_code = "indicator_group") %>%
            merge(
                qof.reg %>% select(ends_with("_code"), register)
                , by = c("practice_code", "indicator_group_code")
                , all.x = TRUE
            ) %>%
            merge(
                qof.exc %>% select(ends_with("_code"), exceptions)
                , by = c("practice_code", "indicator_code")
                , all.x = TRUE
            ) %>%
            mutate_if(is.numeric, as.numeric) %>%
            setDT() %>%
            melt(
                id.vars = grep("_code$", names(.))
                , variable.name = "measure", variable.factor = FALSE
            ) %>%
            mutate(measure = toupper(measure)) %>%
            select(-indicator_group_code)

        rm(qof.reg)

    } else if (qof_root %in% c("qof-1314")) {

        # 1314 ####
        # qof_root <- "qof-1314" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")

        # qof.orgref ####

        this.file <- usethis::proj_path(qof_data_path, "PRAC_CONTROL.csv")
        qof.orgref <- fread(file = this.file) %>% setnames.clean()

        names(qof.orgref) <- gsub("(code|name)$", "_\\1", names(qof.orgref))
        add_names <- c(
            "ccg_geography_code"
            , "subregion_code", "subregion_geography_code", "subregion_name"
            , "region_geography_code"
            , "country"
            , "revised_maximum_points"
        )
        df_new_names <- data.frame(matrix(NA_character_, nrow(qof.orgref), length(add_names)))
        names(df_new_names) <- add_names

        qof.orgref <- bind_cols(qof.orgref, df_new_names) %>%
            rename(stp_code = "at_code", stp_name = "at_name") %>%
            mutate_at(vars(revised_maximum_points), as.numeric) %>%
            mutate_if(is.factor, as.character)

        rm(add_names, df_new_names)

        # qof.indmap ####

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_REFERENCE.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_GRP_REFERENCE.csv")
        qof.grp <- fread(file = this.file) %>% setnames.clean()

        this.file <- usethis::proj_path(qof_data_path, "INDICATOR_DOM_REFERENCE.csv")
        qof.dom <- fread(file = this.file) %>% setnames.clean()

        qof.indmap <- qof.ind %>%
            select(
                -ends_with("_threshold")
                , -indicator_type
                , -indicator_version
                , indicator_group_code = "indicator_group"
                , indicator_description = "indicator_desc"
            ) %>%
            merge(
                qof.grp %>% select(starts_with("indicator_group_"), domain_code)
                , by = "indicator_group_code"
                , all.x = TRUE, all.y = FALSE
            ) %>%
            rename(indicator_group_description = "indicator_group_desc") %>%
            merge(
                qof.dom %>% select(starts_with("domain_"), -domain_points)
                , by = "domain_code"
                , all.x = TRUE, all.y = FALSE
            ) %>%
            rename(domain_description = "domain_desc") %>%
            mutate(patient_list_type = NA_character_)

        rm(qof.ind, qof.grp, qof.dom)

        # qof.prev ####

        #' @note HF double counted - register_description has HF and HF with LVD
        #'   - keep just HF

        this.file <- usethis::proj_path(qof_data_path, "PREVALENCEBYPRAC.csv")
        qof.prev <- fread(file = this.file) %>% setnames.clean() %>%
            filter(register_description != "Heart Failure due to LVD") %>%
            select(
                -ends_with("_description")
                , -prevalence
                , practice_code = practicecode
                , indicator_group_code = indicator_group
                , register = disease_register_size
                , patient_list_size = practice_listsize
            ) %>%
            mutate(patient_list_type = NA_character_) %>%
            mutate_at(vars(register), as.numeric)

        # qof.ind ####

        # Need to merge in register and exceptions

        this.file <- usethis::proj_path(qof_data_path, "exception_output_all_denoms.csv")
        qof.exc <- fread(file = this.file) %>% setnames.clean() %>%
            select(
                practice_code = practicecode
                , indicator_code
                , exceptions = exception_count
            )

        this.file <- usethis::proj_path(qof_data_path, "INDICATORSBYPRAC.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean() %>%
            rename(
                indicator_group_code = "indicator_group"
                , practice_code = "practicecode"
                , indicator_code = "indicatorcode"
            ) %>%
            setDT() %>%
            merge(
                qof.prev %>% select(ends_with("_code"), register)
                , by = c("practice_code", "indicator_group_code")
                , all.x = TRUE
            ) %>%
            merge(
                qof.exc %>% select(ends_with("_code"), exceptions)
                , by = c("practice_code", "indicator_code")
                , all.x = TRUE
            ) %>%
            mutate_if(is.numeric, as.numeric) %>%
            setDT() %>%
            melt(
                id.vars = grep("_code$", names(.))
                , variable.name = "measure", variable.factor = FALSE
            ) %>%
            mutate(measure = toupper(measure)) %>%
            select(-indicator_group_code)

        rm(qof.exc)

    } else if (qof_root %in% c("qof-1213")) {

        # 1213 ####
        # qof_root <- "qof-1213" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")

        qof_stem <- sub("([0-9]{2})([0-9]{2})", "\\1-\\2", qof_root)

        if (verbosity.showatlevel("chatty"))
            cat("WARNING: extract WIP for", qof_root, "\n")

        # File layout
        #
        # {National,CCG,Practice}
        # - {Clinical, Prevalence, ...}
        #  - {qof-12-13-data-tab-prac-xxxx}
        #
        # * orgref from clinical summary
        # * indmap incremental
        # * prev from Prevlance
        # * ind from Clinical
        #

        these_orgs <- c(eng = "National", ccg = "CCG", prac = "Practice")
        these_doms <- c(clin = "Clinical", prev = "Prevalence")

        # qof.orgref ####

        # get orgref from Clinical summary

        this.file <- usethis::proj_path(
            qof_data_path
            , these_orgs["prac"]
            , paste0(qof_stem, "-data-tab-prac-clin-summ.xlsx")
        )
        xl <- list(wb = this.file, sheets = readxl::excel_sheets(this.file))
        ws <- readxl::read_excel(path = xl$wb, sheet = xl$sheets[1], skip = 13) %>% setnames.clean()

        qof.orgref <- ws %>%
            select_if(is.character) %>%
            rename_all(function(x){gsub("\\.", "_", x)})

        add_names <- c(
            "ccg_geography_code"
            , "subregion_code", "subregion_geography_code", "subregion_name"
            , "region_geography_code"
            , "country"
            , "revised_maximum_points"
        )
        df_new_names <- data.frame(matrix(NA_character_, nrow(qof.orgref), length(add_names)))
        names(df_new_names) <- add_names

        qof.orgref <- bind_cols(qof.orgref, df_new_names) %>%
            rename(stp_code = "at_code", stp_name = "at_name") %>%
            mutate_at(vars(revised_maximum_points), as.numeric) %>%
            mutate_if(is.factor, as.character)

        rm(add_names, df_new_names)

        # qof.indmap ####

        # Start with manual extract from technical annex for descriptions

        # Depression: there is no indicator to hold the register - create DEP00
        # Depression: Depression1 Indicator register - implicit in DEP01
        # indicator - so drop
        # Heart Failure: HF01 is the register.  LVD regisetr implicit in HF03 -
        # so drop
        # CVDPP : there is no indicator to hold the register - create PP00

        this.file <- usethis::proj_path(qof_data_path, paste0(qof_root, "-meta-ind.xlsx"))
        xl <- list(wb = this.file, sheets = readxl::excel_sheets(this.file))
        qof.indmap <- readxl::read_excel(path = xl$wb, sheet = xl$sheets[1]) %>%
            setnames.clean() %>%
            select(starts_with("indicator_"), -ends_with("2"))

        qof.indmap.manual <- fread(input = "
indicator_code,indicator_description,indicator_group_description
DEP00,The practice can produce a register of patients with Depression - pseudo-register,Depression
PP00,The practice can produce a register of patients with Cardiovascular Disease Primary Prevention - pseudo-register,Cardiovascular Disease Primary Prevention
PC00,The practice can produce a register of patients with Palliative Care - pseudo-register,Palliative Care
SMOKE00,The practice can produce a register of patients with Smoking - pseudo-register,Smoking Indicators
")

        qof.indmap <- qof.indmap %>%
            status("INFO: manual insertion: DEP00 for pseudo-register") %>%
            bind_rows(qof.indmap.manual) %>%
            mutate(
                indicator_group_code = gsub("[0-9]*", "", indicator_code)
                , indicator_point_value = NA_integer_
                , domain_code = "CL"
                , domain_description = "Clinical"
                , patient_list_type = "TOTAL"
                ) %>%
            status("INFO: manual override: THYROI -> THYROID") %>%
            mutate(indicator_group_code = sub("^(THYROI)$", "\\1D", indicator_group_code)) %>%
            status("INFO: patient_list_type to be updated once prevalence extracted")

        # store pure group
        #
        # at this point the group description is a cut and paste from the QOF
        # technical annex

        qof.grpmap <- qof.indmap %>%
            filter(!indicator_code %like% "[0-9]") %>%
            select(
                indicator_group_code = indicator_code
                , indicator_group_desc_prev = indicator_group_description
                , indicator_group_description = indicator_description
            )

        # drop pure group
        # tag with indicator group description

        qof.indmap <- qof.indmap %>%
            select(-indicator_group_description) %>%
            filter(indicator_code %like% "[0-9]") %>%
            merge(
                qof.grpmap %>% select(-ends_with("_desc_prev"))
                , by = "indicator_group_code", all.x = TRUE
            )

        # qof.ind ####

        # Recursive extraction through a list of xl files

        these_files <- list.files(
            usethis::proj_path(qof_data_path, these_orgs["prac"], these_doms["clin"])
            , pattern = "^qof.*\\.xlsx$"
            , include.dirs = FALSE
            , recursive = FALSE
            , full.names = TRUE
        )

        qof.ind <- these_files %>%
            lapply(l_readxl_wb, f_readxl_ws = l_readxl_ws_ind) %>%
            bind_rows() %>%
            setDT() %>%
            status("INFO: creating indicator_code, measure fields") %>%
            .[, c("indicator_code", "measure") := data.table::tstrsplit(qof_measure, " ")] %>%
            mutate_at(vars(measure), toupper) %>%
            select(-qof_measure) %>%
            setDT() %>%
            .[measure == "POINTS", measure := "ACHIEVED_POINTS"]

        # qof.prev ####

        these_files <- list.files(
            usethis::proj_path(qof_data_path, these_orgs["prac"], these_doms["prev"])
            , pattern = "^qof.*\\.xlsx$"
            , include.dirs = FALSE
            , recursive = FALSE
            , full.names = TRUE
        )

        qof.prev <- these_files %>%
            lapply(l_readxl_wb, f_readxl_ws = l_readxl_ws_prev) %>%
            bind_rows() %>%
            setDT()

        # '- metadata update and prev data reshaping ####
        #
        # lot of information embedded ... need some clarity to extract
        #
        # * disease : needs to map to qof vocabulary from indmap
        #
        # Need patient_list_size and patient_list_type ... information will be
        # contained in indmap ... but at this point is incomplete ... age info
        # embedded in prev fields ... but indicator group is a free text field
        # ... use that free text field to link into indmap to get
        # indicator_group_code ... then inspect prev fields to get
        # indicator_group and list_type link ... then finally update indmap ...

        # '- amend prev field labels to something more uniform ####

        qof.prev.labels <- qof.prev %>%
            count(sheet, tbl_heading) %>% select(-n) %>%
            #rename(id = "tbl_heading") %>%
            mutate(
                s1 = gsub(" (Prevalence|Register|\\(per cent\\))", ";\\1", tbl_heading)
                , s1 = sub("^Sum of ", "", s1)
                , s1 = sub("(Register) ", "\\1;", s1)
                , s1 = sub("^(Estimated) (%age|number) (.*)$", "\\1;\\3;\\2", s1)
                , s1 = sub("%age", "(per cent)", s1)
                , s1 = sub("(Prevalence|Register)$", "\\1;(count)", s1)
                , s1 = sub(";(number)$", ";(\\1)", s1)
                , s1 = sub("^(Register);for ([a-zA-Z0-9]* Indicator[s]*)$", "\\2;\\1;(count)", s1)
            ) %>% setDT() %>%
            .[, c("disease", "measure", "statistic") := data.table::tstrsplit(s1, ";")] %>%
            .[, disease := data.table::tstrsplit(disease, ":", keep = 1)] %>%
            .[, statistic := gsub("[\\(\\)]", "", statistic)] %>%
            .[tolower(disease) == "list size", measure := "all ages"] %>%
            mutate(list_type = ifelse(statistic %like% "age", statistic, "all ages")) %>%
            mutate_at(vars(disease, measure, statistic), trimws) %>%
            select(-s1)

        # '- Split into List and Register
        #
        # ensure qof vocabulary used - indicator_group_code, patient_list_type

        lu_age <- fread(input = "
list_type,patient_list_type
all ages,TOTAL
ages 16+,16OV
ages 17+,17OV
ages 18+,18OV
ages 50+,50OV
")

        # '-- List ####

        qof.prev.labels.list <- qof.prev.labels %>%
            filter(
                sheet == "Prac_AgeSpecificPrevalence"
                , tolower(disease) %like% "estimated|list"
            ) %>% setDT() %>%
            .[is.na(statistic), statistic := "number"] %>%
            .[tbl_heading %like% "^Estimated", list_type := paste("ages", measure)] %>%
            filter(statistic == "number") %>%
            mutate(disease = "List size") %>%
            rename(indicator_group_description = "disease") %>%
            merge(lu_age, by = "list_type", all.x = TRUE) %>%
            select(-list_type, -measure, -statistic)

        qof.prev.list <- qof.prev %>%
            merge(
                qof.prev.labels.list
                , by = c("sheet", "tbl_heading")
                , all.x = FALSE, all.y = TRUE
            ) %>%
            select(-sheet, -tbl_heading, -indicator_group_description)

        # '-- Register ####

        qof.prev.labels.reg <- qof.prev.labels %>%
            filter(
                sheet == "Prac_Prevalence"
                , measure == "Register"
            ) %>% setDT() %>%
            merge(lu_age, by = "list_type", all.x = TRUE) %>%
            select(-list_type) %>%
            rename(indicator_group_desc_prev = "disease") %>%
            merge(qof.grpmap, by = "indicator_group_desc_prev", all.x = TRUE) %>%
            select(-indicator_group_desc_prev) %>%
            status("INFO: dropping depression1 hf lvd pseudo-registers") %>%
            filter(!is.na(indicator_group_description)) %>%
            select(-measure, -statistic, -indicator_group_description)

        qof.prev.reg <- qof.prev %>%
            merge(
                qof.prev.labels.reg
                , by = c("sheet", "tbl_heading")
                , all.x = FALSE, all.y = TRUE
            ) %>%
            select(-sheet, -tbl_heading)

        # '-- Combine ####

        qof.prev <- qof.prev.reg %>%
            rename(register = "value") %>%
            setDT() %>%
            merge(
                qof.prev.list %>% rename(patient_list_size = "value")
                , by = c("practice_code", "patient_list_type")
                , all.x = TRUE
                , suffixes = c(".reg", ".list")
            )

        # '- update indmap with list_type ####
        # can update indmap with patient_list_type

        qof.indmap <- qof.indmap %>%
            status("INFO: updating indmap with patient_list_type from prev") %>%
            select(-patient_list_type) %>%
            merge(
                qof.prev.labels.reg %>%
                    select(indicator_group_code, patient_list_type)
                , by = "indicator_group_code"
                , all.x = TRUE
            )

        # '- update ind with register for relevant indicators ####
        # can update ind with register

        qof.indmap.isregister <- qof.indmap %>%
            filter(indicator_description %like% "produce a register") %>%
            select(indicator_code, indicator_group_code)

        qof.ind <- list(
            qof.ind
            , qof.prev.reg %>%
                select(practice_code, value, indicator_group_code) %>%
                mutate(measure = toupper("register")) %>%
                setDT() %>%
                merge(
                    qof.indmap.isregister
                    , by = "indicator_group_code"
                    , all.x = TRUE
                ) %>%
                select(-indicator_group_code)
        ) %>% data.table::rbindlist(use.names = TRUE)

        # Cross check data and metadata ####

        ind.data <- qof.ind$indicator_code %>% unique()
        ind.meta <- qof.indmap$indicator_code %>% unique()

        if (!setequal(ind.data, ind.meta)) {
            cat(
                "WARNING: indicator mismatch between data and meta:", "\n"
                , " - in data but not meta:"
                , paste(setdiff(ind.data, ind.meta), sep = ", "), "\n"
                , " - in meta but not data:"
                , paste(setdiff(ind.meta, ind.data), sep = ", "), "\n"
            )
        }

        rm(
            qof.prev.labels
            , qof.prev.labels.list, qof.prev.list
            , qof.prev.labels.reg, qof.prev.reg
        )

    } else if (qof_root %in% c("qof-1112")) {

        # 1112 ####
        # qof_root <- "qof-1112" ; qof_data_path <- paste(".", "data-raw", paste0(qof_root, "-csv"), sep = "/")

        qof_stem <- sub("([0-9]{2})([0-9]{2})", "\\1-\\2", qof_root)

        if (verbosity.showatlevel("chatty"))
            cat("WARNING: extract WIP for", qof_root, "\n")

        # File layout
        #
        # {National,CCG,Practice}
        # - {Clinical, Prevalence, ...}
        #  - {qof-12-13-data-tab-prac-xxxx}
        #
        # * orgref from clinical summary
        # * indmap incremental
        # * prev from Prevlance
        # * ind from Clinical
        #

        these_orgs <- c(eng = "pub08661", sha = "pub08713", pct = "pub08722", prac = "pub08715")

        # qof.orgref ####

        # get orgref from Clinical summary

        this.file <- usethis::proj_path(
            qof_data_path
            , these_orgs["prac"]
            , paste0(qof_stem, "-data-tab-pracs-clin-sum.xls")
        )
        xl <- list(wb = this.file, sheets = readxl::excel_sheets(this.file))
        ws <- readxl::read_excel(path = xl$wb, sheet = xl$sheets[1], skip = 13) %>%
            setnames.clean()

        qof.orgref <- ws %>%
            select_if(is.character) %>%
            rename_all(function(x){gsub("\\.", "_", x)})

        add_names <- c(
            "ccg_geography_code"
            , "subregion_code", "subregion_geography_code", "subregion_name"
            , "region_geography_code"
            , "country"
            , "revised_maximum_points"
        )
        df_new_names <- data.frame(
            matrix(NA_character_, nrow(qof.orgref), length(add_names))
            , stringsAsFactors = FALSE
        )
        names(df_new_names) <- add_names

        qof.orgref <- bind_cols(qof.orgref, df_new_names) %>%
            rename(
                stp_code = "sha_code", stp_name = "strategic_health_authority_name"
                , ccg_code = "pct_code", ccg_name = "pct_name"
            ) %>%
            mutate_at(vars(revised_maximum_points), as.numeric) %>%
            mutate_if(is.factor, as.character)

        rm(add_names, df_new_names)

        # qof.indmap ####

        # Start with manual extract from technical annex for descriptions

        # Depression: there is no indicator to hold the register - create DEP00
        # Depression: Depression1 Indicator register - implicit in DEP01
        # indicator - so drop
        # Heart Failure: HF01 is the register.  LVD regisetr implicit in HF03 -
        # so drop
        # CVDPP : there is no indicator to hold the register - create PP00

        this.file <- usethis::proj_path(qof_data_path, paste0(qof_root, "-meta-ind.xlsx"))
        xl <- list(wb = this.file, sheets = readxl::excel_sheets(this.file))
        qof.indmap <- readxl::read_excel(path = xl$wb, sheet = xl$sheets[1]) %>%
            setnames.clean() %>%
            select(starts_with("indicator_"), -ends_with("2"))

        qof.indmap.manual <- fread(input = "
indicator_code,indicator_description,indicator_group_description
DEP00,The practice can produce a register of patients with Depression - pseudo-register,Depression
PP00,The practice can produce a register of patients with Cardiovascular Disease Primary Prevention - pseudo-register,Cardiovascular Disease Primary Prevention
SMOKE00,The practice can produce a register of patients with Smoking - pseudo-register,Smoking Indicators
")
        #PC00,The practice can produce a register of patients with Palliative Care - pseudo-register,Palliative Care

        qof.indmap <- qof.indmap %>%
            status("INFO: manual insertion: pseudo-registers") %>%
            bind_rows(qof.indmap.manual) %>%
            mutate(
                indicator_group_code = gsub("[0-9]*", "", indicator_code)
                , indicator_point_value = NA_integer_
                , domain_code = "CL"
                , domain_description = "Clinical"
                , patient_list_type = "TOTAL"
            ) %>%
            status("INFO: manual override: THYROI -> THYROID") %>%
            mutate(indicator_group_code = sub("^(THYROI)$", "\\1D", indicator_group_code)) %>%
            status("INFO: patient_list_type to be updated once prevalence extracted")

        # store pure group
        #
        # at this point the group description is a cut and paste from the QOF
        # technical annex

        qof.grpmap <- qof.indmap %>%
            filter(!indicator_code %like% "[0-9]") %>%
            select(
                indicator_group_code = indicator_code
                , indicator_group_desc_prev = indicator_group_description
                , indicator_group_description = indicator_description
            )

        # drop pure group
        # tag with indicator group description

        qof.indmap <- qof.indmap %>%
            select(-indicator_group_description) %>%
            filter(indicator_code %like% "[0-9]") %>%
            merge(
                qof.grpmap %>% select(-ends_with("_desc_prev"))
                , by = "indicator_group_code", all.x = TRUE
            )

        # qof.ind ####

        # Recursive extraction through a list of xl files

        these_files <- list.files(
            usethis::proj_path(qof_data_path, these_orgs["prac"])
            , pattern = "^qof.*\\.xls"
            , include.dirs = FALSE
            , recursive = FALSE
            , full.names = TRUE
        ) %>%
            # skip summaries
            grep("sum[m]{0,1}\\.", ., invert = TRUE, value = TRUE)

        qof.ind <- these_files %>%
            sort() %>%
            lapply(
                l_readxl_wb
                , f_readxl_ws = l_readxl_ws_ind
                , skipsheets = "QOF Export"
            ) %>%
            bind_rows() %>%
            setDT() %>%
            status("INFO: creating indicator_code, measure fields") %>%
            .[, c("indicator_code", "measure") := data.table::tstrsplit(qof_measure, " ")] %>%
            mutate_at(vars(measure), toupper) %>%
            select(-qof_measure) %>%
            setDT() %>%
            .[measure == "POINTS", measure := "ACHIEVED_POINTS"]
        # qof.prev ####

        these_files <- list.files(
            usethis::proj_path(qof_data_path, these_orgs["prac"])
            , pattern = "^q.*prev.*\\.xls$"
            , include.dirs = FALSE
            , recursive = FALSE
            , full.names = TRUE
        )

        qof.prev <- these_files %>%
            lapply(l_readxl_wb, f_readxl_ws = l_readxl_ws_prev) %>%
            bind_rows() %>%
            setDT()

        # '- metadata update and prev data reshaping ####
        #
        # lot of information embedded ... need some clarity to extract
        #
        # * disease : needs to map to qof vocabulary from indmap
        #
        # Need patient_list_size and patient_list_type ... information will be
        # contained in indmap ... but at this point is incomplete ... age info
        # embedded in prev fields ... but indicator group is a free text field
        # ... use that free text field to link into indmap to get
        # indicator_group_code ... then inspect prev fields to get
        # indicator_group and list_type link ... then finally update indmap ...

        # '- amend prev field labels to something more uniform ####

        qof.prev.labels <- qof.prev %>%
            count(sheet, tbl_heading) %>% select(-n) %>%
            #rename(id = "tbl_heading") %>%
            mutate(
                s1 = gsub(" (Prevalence|Register|\\(per cent\\))", ";\\1", tbl_heading)
                , s1 = sub("^Sum of ", "", s1)
                , s1 = sub("(Register) ", "\\1;", s1)
                , s1 = sub("^(Estimated) (%age|number) (.*)$", "\\1;\\3;\\2", s1)
                , s1 = sub("%age", "(per cent)", s1)
                , s1 = sub("(Prevalence|Register)$", "\\1;(count)", s1)
                , s1 = sub(";(number)$", ";(\\1)", s1)
                , s1 = sub("^(Register);for ([a-zA-Z0-9]* Indicator[s]*)$", "\\2;\\1;(count)", s1)
            ) %>% setDT() %>%
            .[, c("disease", "measure", "statistic") := data.table::tstrsplit(s1, ";")] %>%
            .[, disease := data.table::tstrsplit(disease, ":", keep = 1)] %>%
            .[, statistic := gsub("[\\(\\)]", "", statistic)] %>%
            .[tolower(disease) == "list size", measure := "all ages"] %>%
            mutate(list_type = ifelse(statistic %like% "age", statistic, "all ages")) %>%
            mutate_at(vars(disease, measure, statistic), trimws) %>%
            select(-s1)

        # '- Split into List and Register
        #
        # ensure qof vocabulary used - indicator_group_code, patient_list_type

        lu_age <- fread(input = "
list_type,patient_list_type
all ages,TOTAL
ages 16+,16OV
ages 17+,17OV
ages 18+,18OV
ages 50+,50OV
")

        # '-- List ####

        qof.prev.labels.list <- qof.prev.labels %>%
            filter(
                sheet %like% "_AgeSpecificPrevalence"
                , tolower(disease) %like% "estimated|list"
            ) %>% setDT() %>%
            .[is.na(statistic), statistic := "number"] %>%
            .[tbl_heading %like% "^Estimated", list_type := paste("ages", measure)] %>%
            filter(statistic == "number") %>%
            mutate(disease = "List size") %>%
            rename(indicator_group_description = "disease") %>%
            merge(lu_age, by = "list_type", all.x = TRUE) %>%
            select(-list_type, -measure, -statistic)

        qof.prev.list <- qof.prev %>%
            merge(
                qof.prev.labels.list
                , by = c("sheet", "tbl_heading")
                , all.x = FALSE, all.y = TRUE
            ) %>%
            select(-sheet, -tbl_heading, -indicator_group_description)

        # '-- Register ####

        qof.prev.labels.reg <- qof.prev.labels %>%
            filter(
                sheet %like% "_Prevalence"
                , measure == "Register"
            ) %>% setDT() %>%
            merge(lu_age, by = "list_type", all.x = TRUE) %>%
            select(-list_type) %>%
            rename(indicator_group_desc_prev = "disease") %>%
            merge(qof.grpmap, by = "indicator_group_desc_prev", all.x = TRUE) %>%
            select(-indicator_group_desc_prev) %>%
            status("INFO: dropping depression1 hf lvd pseudo-registers") %>%
            filter(!is.na(indicator_group_description)) %>%
            select(-measure, -statistic, -indicator_group_description)

        qof.prev.reg <- qof.prev %>%
            merge(
                qof.prev.labels.reg
                , by = c("sheet", "tbl_heading")
                , all.x = FALSE, all.y = TRUE
            ) %>%
            select(-sheet, -tbl_heading)

        # '-- Combine ####

        qof.prev <- qof.prev.reg %>%
            rename(register = "value") %>%
            setDT() %>%
            merge(
                qof.prev.list %>% rename(patient_list_size = "value")
                , by = c("practice_code", "patient_list_type")
                , all.x = TRUE
                , suffixes = c(".reg", ".list")
            )

        # '- update indmap with list_type ####
        # can update indmap with patient_list_type

        qof.indmap <- qof.indmap %>%
            status("INFO: updating indmap with patient_list_type from prev") %>%
            select(-patient_list_type) %>%
            merge(
                qof.prev.labels.reg %>%
                    select(indicator_group_code, patient_list_type)
                , by = "indicator_group_code"
                , all.x = TRUE
            )

        # '- update ind with register for relevant indicators ####
        # can update ind with register

        qof.indmap.isregister <- qof.indmap %>%
            filter(
                (indicator_description %like% "produce a register")
                | (indicator_description %like% "has a complete register")
            ) %>%
            select(indicator_code, indicator_group_code)

        qof.ind <- list(
            qof.ind
            , qof.prev.reg %>%
                select(practice_code, value, indicator_group_code) %>%
                mutate(measure = toupper("register")) %>%
                setDT() %>%
                merge(
                    qof.indmap.isregister
                    , by = "indicator_group_code"
                    , all.x = TRUE
                ) %>%
                select(-indicator_group_code)
        ) %>% data.table::rbindlist(use.names = TRUE)

        # Cross check data and metadata ####

        ind.data <- qof.ind$indicator_code %>% unique() %>% sort()
        ind.meta <- qof.indmap$indicator_code %>% unique() %>% sort()

        if (!setequal(ind.data, ind.meta)) {
            cat("WARNING: indicator mismatch between data and meta:", "\n")

            data_m_meta <- setdiff(ind.data, ind.meta) #%>% sort()
            meta_m_data <- setdiff(ind.meta, ind.data) #%>% sort()

            if (length(data_m_meta) > 0)
                cat(
                    " - in data but not meta:"
                    , paste(data_m_meta, sep = ", ")
                    , "\n"
                )

            if (length(meta_m_data) > 0)
                cat(
                    " - in meta but not data:"
                    , paste(meta_m_data, sep = ", ")
                    , "\n"
                )

            if (length(meta_m_data) == 0) {
                if (verbosity.showatlevel("chatty"))
                    cat("INFO: stripping out all indicators not in meta", "...", "\n")

                qof.ind <- qof.ind[indicator_code %in% ind.meta, ]
            }
        }

    } else {

        cat("WARNING: extract not defined for", qof_root, "\n")

        qof.orgref <- NA
        qof.indmap <- NA
        qof.prev <- NA
        qof.ind <- NA
    }

    # flip measure backup
    qof.ind <- qof.ind %>%
        data.table::dcast(... ~ measure, fun.aggregate = sum, fill = NA)

    retval <- list(
        meta_org = qof.orgref
        , meta_ind = qof.indmap
        , data_prev = qof.prev
        , data_ind = qof.ind
    ) %>%
        # tag on qof_period
        lapply(function(x) {x %>% mutate(qof_period = qof_root)})

    generic_names <- c("meta_org", "meta_ind", "data_prev", "data_ind")

    # save ####

    if (bSaveData == TRUE) {

        these_names <- paste(gsub("-", "_", qof_root), generic_names, sep = "_")
        names(retval) <- these_names

        retval %>% {
            purrr::pwalk(
                list(obj = ., name = names(.), dir = "inst/data-int")
                , store_data
            )
        }
    }

    # return

    names(retval) <- generic_names

    invisible(retval)
}

#' @describeIn f__extract__load_raw Extract all qof data
#'
#' Extracts data from multiple qof periods.  Binds qof periods into data.frames.
#'
#' @inheritParams f__extract__load_raw
#'
#' @return a list with named items
#' \tabular{ll}{
#'   \code{meta_org}  \tab Organisation metadata \cr
#'   \code{meta_ind}  \tab Indicator metadata \cr
#'   \code{data_prev} \tab Prevalence data \cr
#'   \code{data_ind}  \tab Indicator data \cr
#' }
#'
f__extract_any <- function(
    qof_root = c(
        "qof-1718", "qof-1617"
        , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
        , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
        , "qof-0506", "qof-0405"
    )
    , bSaveData = FALSE
) {
    qof_root <- match.arg(qof_root, several.ok = TRUE)

    retval <- qof_root %>%
        status("INFO: extracting ...") %>%
        lapply(f__extract__load_raw, bSaveData = bSaveData)

    # transform "qof-1617_xxxxx" to "qof_xxxxx"
    names(retval) <- qof_root %>%
        gsub("-", "_", .) %>%
        gsub("_[0-9]*_", "_", .)

    retval <- retval %>%
        status("INFO: binding ...") %>%
        purrr::transpose() %>%
        lapply(bind_rows)

    invisible(retval)
}

#' Merge all qof data into one dataset
#'
#' Merges data from multiple qof periods.  Uses the post-extract results.  Binds
#' qof periods into relevant data.frames.
#'
#' @inheritParams f__extract__load_raw
#'
#' @importFrom data.table setDT rbindlist
#' @importFrom purrr pwalk
#'
#' @return a list with named items
#' \tabular{ll}{
#'   \code{meta_org}  \tab Organisation metadata \cr
#'   \code{meta_ind}  \tab Indicator metadata \cr
#'   \code{data_prev} \tab Prevalence data \cr
#'   \code{data_ind}  \tab Indicator data \cr
#' }
#'
f__combine_any <- function(
    qof_root = c(
        "qof-1718", "qof-1617"
        , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
        , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
        , "qof-0506", "qof-0405"
    )
    , bSaveData = FALSE
) {

    generic_names <- c("meta_org", "meta_ind", "data_prev", "data_ind")

    # Return a data.table for given period and given variable

    l_period_variable <- function(this_qof, this_var) {
        this_varname <- paste(gsub("-", "_", this_qof), this_var, sep = "_")

        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__combine_any: l_period_variable: considering", this_varname, "...", "\n")

        retval <- restore_data(this_varname) %>% setDT()

        invisible(retval)
    }

    # Return period tagged data for given variable (looped over periods)

    l_period <- function(this_var, these_qof) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__combine_any: l_period: considering", this_var, "...", "\n")

        lapply(these_qof, l_period_variable, this_var = this_var) %>%
            rbindlist(use.names = TRUE, fill = TRUE)
    }

    # Return a list of variables for do the loop

    retval <- generic_names %>%
        lapply(l_period, these_qof = qof_root)

    # save
    #
    # one file for each data item, meta_ind, meta_org, data_ind, data_prev

    if (bSaveData == TRUE) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: saving", "...", "\n")

        names(retval) <- paste("qof", generic_names, sep = "_")

        retval %>% {
            purrr::pwalk(list(., names(.), dir = "data"), store_data)
        }
    }

    names(retval) <- generic_names

    # return
    #
    # list of data items

    invisible(retval)
}

#' Return extracted dataset
#'
#' Either extract from raw values or load pre-extracted values.
#'
#' @inheritParams f__extract__load_raw
#'
#' @param bExtractFromRaw (bool) Flag to do process raw or load pre-extracted.
#'
f__extract <- function(
    qof_root = c(
        "qof-1718", "qof-1617"
        , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
        , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
        , "qof-0506", "qof-0405"
    )
    , bExtractFromRaw = FALSE
) {

    qof_root <- match.arg(qof_root, several.ok = TRUE)

    if (bExtractFromRaw)
        f__extract_any(qof_root)
    else
        f__combine_any(qof_root)
}
