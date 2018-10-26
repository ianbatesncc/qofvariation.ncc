#' Download raw QOF data
#'
#' Programatically download QOF data for given periods. Different periods have
#' different format and layout and none are easily discoverable - this is the
#' result of manual inspection and collection.
#'
#' Download of the files is the first step.  Extracting them is a second step.
#'
# @importFrom tools file_ext
# @import data.table
# @import dplyr
#'
download_qof <- function(
    period = NULL
    , exts = NULL
    , recursive = TRUE
    , bWriteCSV = FALSE
    , junkpaths = TRUE
    , dryrun = FALSE
) {

    require("data.table")
    require("dplyr")

    these_urls <- scrape_urls(period = period, exts = exts, recursive = recursive)

    datadir = "./data-raw/"

    # download worker routine for one url
    l_download <- function(
        this_period
        , this_url
        , this_datadir
        , bOverwrite = FALSE
        , bjunkpaths = junkpaths
        , bdryrun = dryrun
    ) {

        this_dir <- paste0(this_datadir, "qof-", this_period, "-csv")

        if (bjunkpaths == TRUE) {
            this_path <- paste0(this_dir, "/", basename(this_url))
        } else {
            this_url_parent <- basename(dirname(this_url))
            this_path <- paste0(this_dir, "/", this_url_parent, "/", basename(this_url))
        }

        this_path_dir <- dirname(this_path)

        if (!dir.exists(this_path_dir)) {
            cat("INFO: creating", this_path_dir, "...", "\n")
            dir.create(this_path_dir, recursive = TRUE)
        }


        if ((!file.exists(this_path)) | (bOverwrite == TRUE)) {
            cat("INFO: downloading", this_url, "...", "\n")
            if (!bdryrun) {
                download.file(this_url, destfile = this_path, mode = "wb", quiet = TRUE)
            } else {
                cat("INFO: NOT downloading (dryrun)", this_url, "...", "\n")
            }
        } else {
            cat("INFO: NOT downloading (file exists)", this_url, "...", "\n")
        }

        return(data.frame(
            period = this_period
            , path = this_path
            , stringsAsFactors = FALSE
        ))
    }

    these_files <- these_urls %>% {
        mapply(
            l_download, .$qof_period, .$url, datadir, bOverwrite = FALSE, bdryrun = dryrun
            , SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
    } %>% bind_rows()

    # write out list of files as .csv
    if (bWriteCSV == TRUE) {
        these_urls$qof_period %>%
            unique() %>%
            sapply(function(x, d, datadir) {
                this_df <- d %>% filter(qof_period == x)

                this_dir <- paste0(datadir, "qof-", x, "-csv")
                this_csv <- paste0(
                    this_dir, "/"
                    , "qof-", x, "-list-of-files" , ".csv"
                )

                if (file.exists(this_csv)) {
                    cat("INFO: merging with existing list", "\n")
                    existing_list <- fread(this_csv, colClasses = "character")
                    this_df <- bind_rows(existing_list, this_df) %>% unique()
                    rm(existing_list)
                }

                cat("INFO: saving", this_csv, "...", "\n")
                fwrite(this_df %>% arrange(qof_period, url), this_csv)

                invisible(this_csv)
            }
            , d = these_urls
            , datadir = datadir
            )
    }

    #unzip_method = "/usr/bin/unzip"
    #unzip_method = "C:/Program Files/7-Zip/7z.exe"
    #unzip_method = "C:/Program Files/Git/usr/bin/unzip.exe"
    unzip_method = "internal"

    # unzip worker routine for one zip file
    l_unzip <- function(this_period, this_path, bOverwrite = FALSE, bdryrun = dryrun) {
        retval <- NULL
        this_dir <- dirname(this_path)
        if (this_period %in% c("1617", "1516", "1415", "1314", "1213")) {

            l_junkpaths <- switch(this_period, "1213" = FALSE, TRUE)

            if (tools::file_ext(this_path) %in% c("zip")) {
                prev_opt_warn <- getOption("warn") ; options(warn = 1)
                if (!bdryrun) {
                    unzip(
                        this_path
                        , junkpaths = l_junkpaths
                        , overwrite = bOverwrite
                        , exdir = this_dir
                        , unzip = unzip_method
                    ) %>% list() -> retval
                } else {
                    data.frame(
                        Name = as.character()
                        , Length = as.integer()
                        , Date = as.POSIXct(Sys.Date()[FALSE]), stringsAsFactors = FALSE
                    ) %>% list() -> retval
                }
                options(warn = prev_opt_warn)

            }
        }
        invisible(retval)
    }

    these_files_unzipped <- these_files %>% {
        mapply(
            l_unzip, .$period, .$path, bdryrun = dryrun
            , SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
    }

    return(list(
        these_files = these_files
        , these_files_unzipped = these_files_unzipped
    ))
}

#' Scrape NHSD QOF pages for data workbooks
#'
#' Specifying a url overrides the internal dtabase.  Still need to specify the
#' period though.
#'
scrape_urls <- function(
    period = c(
        "all", "all_post1213", "all_pre1213"
        , "1617", "1516", "1415", "1314", "1213"
        , "1112", "1011", "0910", "0809", "0708", "0607", "0506", "0405"
    )
    , url = NULL
    , exts = c("xls", "pdf", "zip")
    , recursive = FALSE
) {
    require("rvest")

    period <- match.arg(period, several.ok = TRUE)
    exts <- match.arg(exts, several.ok = TRUE)

    urls <- fread(
        blank.lines.skip = TRUE
        , input = "
qof_period,url

all_post1213,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data
all_pre1213,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data

1617,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/quality-and-outcomes-framework-qof-2016-17
1516,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/quality-and-outcomes-framework-qof-2015-16
1415,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/quality-and-outcomes-framework-qof-2014-15
1314,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/quality-and-outcomes-framework-qof-2013-14
1213,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2012-13
1112,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2011-12
1011,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2010-11
0910,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2009-10
0809,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2008-09
0708,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2007-08
0607,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2006-07
0506,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/national-quality-and-outcomes-framework-achievement-data-england-2005-06
0405,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/national-quality-and-outcomes-framework-statistics-for-england-2004-05
0405,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/quality-and-outcomes-framework-statistics-england-2004-05-sha-level
        "
    ) %>% filter(!qof_period %like% "^#")

    if (!missing(url)) {
        cat("INFO: using specified url", "\n")
        #period <- period
        urls <- data.frame(
            qof_period = period
            , url = url
            , stringsAsFactors = FALSE
        )
    } else {
        cat("INFO: using internal url database", "\n")
    }

    retval <- urls %>%
        filter((qof_period %in% period) | any(period %like% "^all")) %>% {
            mapply(
                function(this_period, this_url, these_exts) {
                    this_dest <- paste0("./data-raw/", "qof-", this_period, "-csv/", basename(this_url), ".html")
                    this_dest_dir <- dirname(this_dest)
                    if (!dir.exists(this_dest_dir))
                        dir.create(this_dest_dir, recursive = TRUE)

                    if (!file.exists(this_dest)) {
                        cat("INFO: downloading", basename(this_url), "...", "\n")
                        download.file(url = this_url, destfile = this_dest, quiet = TRUE)
                    }

                    cat("INFO: reading", this_dest, "...", "\n")

                    this_html <- read_html(this_dest)
                    these_urls <- this_html %>%
                        html_nodes("a") %>%
                        html_attr("href")

                    these_urls_ext <- these_urls %>%
                        grep(paste(exts, collapse = "|"), ., value = TRUE)

                    links_subdirs_df <- NULL

                    if (recursive == TRUE) {
                        n_thisurl <- nchar(this_url)
                        lookslikedir <- paste0(
                            "https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-"
                            , "[^/]*", "/"
                            , "[a-z-]*quality-and-outcomes-framework-"
                        )
                        these_urls_subdirs <- these_urls %>%
                            # skip and self references and shorter paths
                            # keep others that look like dirs ...
                            setdiff(., this_url) %>%
                            grep(
                                paste0(c(dirname(this_url), lookslikedir), collapse = "|")
                                , .
                                , value = TRUE
                            ) %>%
                            .[nchar(.) > n_thisurl]

                        if (length(these_urls_subdirs) > 0) {
                            links_subdirs_df <- scrape_urls(
                                period = this_period
                                , url = these_urls_subdirs %>% sort()
                                , exts = exts
                                , recursive = recursive
                            )
                        } else {
                            links_subdirs_df <- NULL
                        }
                    }

                    these_urls_df <- NULL
                    if (length(these_urls_ext) > 0) {
                        these_urls_df <- data.frame(
                            qof_period = this_period
                            , url = these_urls_ext
                            , stringsAsFactors = FALSE
                        )
                    }

                    bind_rows(these_urls_df, links_subdirs_df) %>% unique()
                }
                , this_period = .$qof_period
                , this_url = .$url
                , MoreArgs = list(these_exts = exts)
                , SIMPLIFY = FALSE
            ) %>%
                bind_rows() %>%
                unique()
        }
}
