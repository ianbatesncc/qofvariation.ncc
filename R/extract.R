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


    } else if (qof_root %in% c("qof-1415")) {

        # 1415 ####

        cat("WARNING: extract WIP for", qof_root, "\n")

        # qof.orgref

        this.file <- proj_path(qof_data_path, "PRAC_CONTROL.csv")
        qof.orgref <- fread(file = this.file) %>% setnames.clean() %>%
            select(grep("_code$|_name$", names(.), value = TRUE)) %>%
            rename(stp_code = "area_team_code", stp_name = "area_team_name")

        # qof.indmap

        this.file <- proj_path(qof_data_path, "INDICATOR_REFERENCE_FINAL.csv")
        qof.ind <- fread(file = this.file) %>% setnames.clean()

        this.file <- proj_path(qof_data_path, "INDICATOR_GRP_REFERENCE_FINAL.csv")
        qof.grp <- fread(file = this.file) %>% setnames.clean()

        this.file <- proj_path(qof_data_path, "INDICATOR_DOM_REFERENCE_FINAL.csv")
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

        # qof.prev

        this.file <- proj_path(qof_data_path, "PREVALENCE_BY_PRAC_v2.csv")
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


        # qof.ind

        # Need to merge in register and exceptions

        this.file <- proj_path(qof_data_path, "EXCEPTION_OUTPUT.csv")
        qof.exc <- fread(file = this.file) %>% setnames.clean() %>%
            select(
                practice_code
                , indicator_code
                , exceptions = exception_count
            )

        this.file <- proj_path(qof_data_path, "REGISTER_SIZES.csv")
        qof.reg <- fread(file = this.file) %>% setnames.clean() %>%
            rename(register = register_size) %>%
            mutate(indicator_group_code = substr(indicator_code, 1, nchar(indicator_code) - 3)) %>%
            select(-indicator_code)

        this.file <- proj_path(qof_data_path, "INDICATORS_BY_PRAC.csv")
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
