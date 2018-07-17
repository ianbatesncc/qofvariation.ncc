#' Download raw QOF data
#'
#' @importFrom tools file_ext
#'
download_qof <- function(
    period = c("1617", "1516", "1415", "1314", "1213", "1112")
) {
    period <- match.arg(period, several.ok = TRUE)

    require("data.table")
    require("dplyr")

    urls <- fread(
        blank.lines.skip = TRUE
        , input = "
qof_period,url

all,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data

1617,https://files.digital.nhs.uk/zip/p/i/qof-1617-csv.zip

1516,https://files.digital.nhs.uk/publicationimport/pub22xxx/pub22266/qof-1516-csv.zip

1415,https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18887/qof-1415-csvfiles-v2.zip

1314,https://files.digital.nhs.uk/publicationimport/pub15xxx/pub15751/qof-1314-csvfilescqrsdata.zip

1213,https://files.digital.nhs.uk/publicationimport/pub12xxx/pub12262/qof-12-13-data-tab-eng.zip
1213,https://files.digital.nhs.uk/publicationimport/pub12xxx/pub12262/qof-12-13-data-tab-ccg.zip
1213,https://files.digital.nhs.uk/publicationimport/pub12xxx/pub12262/qof-12-13-data-tab-prac.zip

#1112,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2011-12-england-level
#1112,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2011-12-practice-level

1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-nat-prev.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-atfib.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-end-asthma.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-canc.xlsx
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-clin-cvd.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-ckd.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-copd.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-clin-chd.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-dem.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-end-dm.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-dep.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-epil.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-hf.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-bp.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-nat-thy.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-ld.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-mh.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-obes.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-nat-pc.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-nat-smoking.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08661/qof-11-12-data-tab-eng-nat-stroke.xls

1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qual-outc-fram-11-12-prac-prev.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-at-fib.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-asth.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-cancer.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-set-pracs-cardio.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-kid-dis.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-copd.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-con-hea-dis.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-dem.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-dep.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tabs-pracs-diab-mel.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-epil.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-heart-fail.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-hyper.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-thyroid.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-learn-dis.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tabs-pracs-men-heal.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-obes.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-pall-care.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-smoking.xls
1112,https://files.digital.nhs.uk/publicationimport/pub08xxx/pub08715/qof-11-12-data-tab-pracs-stroke.xls

#1011,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2010-11-england-level
#1011,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2010-11-practice-level

#0910,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2009-10-england-level
#0910,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2009-10-practice-level

#0809,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2008-09-england-level
#0809,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2008-09-practice-level

#0708,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2007-08-england-level
#0708,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2007-08-practice-level

#0607,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2006-07-england-level
#0607,https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-data/quality-and-outcomes-framework-2006-07-practice-level

0506,https://files.digital.nhs.uk/publicationimport/pub01xxx/pub01950/qof-eng-05-06-gp-code-tab-anx.xls
"
    )

    these_urls <- urls[qof_period %in% period]

    datadir = "./data-raw/"

    l_download <- function(this_period, this_url, this_datadir, bOverwrite = FALSE) {
        this_dir <- paste0(this_datadir, "qof-", this_period, "-csv")
        this_path <- paste0(this_dir, "/", basename(this_url))

        if (!dir.exists(this_dir)) {
            cat("INFO: creating", this_dir, "...", "\n")
            dir.create(this_dir)
        }

        if (!file.exists(this_path) | bOverwrite == TRUE) {
            cat("INFO: downloading", this_url, "...", "\n")
            download.file(this_url, destfile = this_path, mode = "wb")
        } else {
            cat("INFO: NOT downloading", this_url, "...", "\n")
        }

        return(data.frame(
            period = this_period
            , path = this_path
            , stringsAsFactors = FALSE
        ))
    }

    these_files <- these_urls %>% {
        mapply(
            l_download, .$qof_period, .$url, datadir, bOverwrite = FALSE
            , SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
    } %>% bind_rows()


    #unzip_method = "/usr/bin/unzip"
    #unzip_method = "C:/Program Files/7-Zip/7z.exe"
    #unzip_method = "C:/Program Files/Git/usr/bin/unzip.exe"
    unzip_method = "internal"

    l_unzip <- function(this_period, this_path, bOverwrite = FALSE) {
        retval <- NULL
        this_dir <- dirname(this_path)
        if (this_period %in% c("1617", "1516", "1415", "1314", "1213")) {

            l_junkpaths <- switch(this_period, "1213" = FALSE, TRUE)

            if (tools::file_ext(this_path) %in% c("zip")) {
                prev_opt_warn <- getOption("warn") ; options(warn = 1)
                unzip(
                    this_path
                    , junkpaths = l_junkpaths
                    , overwrite = bOverwrite
                    , exdir = this_dir
                    , unzip = unzip_method
                ) %>% list() -> retval
                options(warn = prev_opt_warn)

            }
        }
        invisible(retval)
    }

    these_files_unzipped <- these_files %>% {
        mapply(
            l_unzip, .$period, .$path
            , SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
    }

    return(list(
        these_files = these_files, these_files_unzipped = these_files_unzipped
    ))
}
