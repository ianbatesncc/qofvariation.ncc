#
# data.R
#

#' QOF datasets
#'
#' @family qof_datasets
#' @family qof_data
#' @family qof_meta
#'
#' @name qof_datasets
NULL

#' QOF data
#'
#' @family qof_datasets
#' @family qof_data
#'
#' @name qof_data
NULL

#' QOF meta
#'
#' @family qof_datasets
#' @family qof_meta
#'
#' @name qof_meta
NULL

#' QOF reference data for organisations
#'
#' \preformatted{
#' $ practice_code           : chr
#' $ practice_name           : chr
#' $ ccg_code                : chr
#' $ ccg_geography_code      : chr
#' $ ccg_name                : chr
#' $ stp_code                : chr
#' $ stp_name                : chr
#' $ subregion_code          : chr
#' $ subregion_geography_code: chr
#' $ subregion_name          : chr
#' $ region_code             : chr
#' $ region_geography_code   : chr
#' $ region_name             : chr
#' $ country                 : chr
#' $ revised_maximum_points  : int
#' }
#'
#' @family qof_meta
#' @name qof_meta_org
#'
"qof_1617_meta_org"

#' @name qof_meta_org
"qof_1516_meta_org"

#' @name qof_meta_org
"qof_1415_meta_org"

#' @name qof_meta_org
"qof_1314_meta_org"

#' @name qof_meta_org
"qof_1213_meta_org"


#' QOF reference data for organisations
#'
#' \preformatted{
#' $ indicator_code             : chr
#' $ indicator_description      : chr
#' $ indicator_point_value      : int
#' $ indicator_group_code       : chr
#' $ indicator_group_description: chr
#' $ domain_code                : chr
#' $ domain_description         : chr
#' $ patient_list_type          : chr
#' }
#'
#' \describe{
#'   \item{\code{patient_list_type}}{
#'   One of
#'   \code{TOTAL}
#'   , \code{15OV}, \code{16OV}, \code{17OV}, \code{18OV}
#'   , \code{45OV}, \code{50OV}
#'   , \code{25_64_F}
#'   , \code{30_74}
#'   , \code{54UN_F}
#'   }
#' }
#'
#' @family qof_meta
#' @name qof_meta_ind
#'
"qof_1617_meta_ind"

#' @name qof_meta_ind
"qof_1516_meta_ind"

#' @name qof_meta_ind
"qof_1415_meta_ind"

#' @name qof_meta_ind
"qof_1314_meta_ind"

#' @name qof_meta_ind
"qof_1213_meta_ind"

#' QOF data for prevalence
#'
#' \preformatted{
#' $ practice_code       : chr
#' $ indicator_group_code: chr
#' $ register            : int
#' $ patient_list_type   : chr
#' $ patient_list_size   : int
#' }
#'
#' @family qof_data
#' @name qof_data_prev
#'
"qof_1617_data_prev"

#' @name qof_data_prev
"qof_1516_data_prev"

#' @name qof_data_prev
"qof_1415_data_prev"

#' @name qof_data_prev
"qof_1314_data_prev"

#' @name qof_data_prev
"qof_1213_data_prev"


#' QOF data for indicators
#'
#' \preformatted{
#' $ practice_code : chr
#' $ indicator_code: chr
#' $ measure       : chr
#' $ value         : num
#' }
#'
#' \describe{
#'   \item{\code{measure}}{
#'   One of
#'   \code{ACHIEVED_POINTS}
#'   , \code{DENOMINATOR}
#'   , \code{EXCEPTIONS}
#'   , \code{NUMERATOR}
#'   , \code{REGISTER}
#'   }
#' }
#'
#' @family qof_data
#' @name qof_data_ind
#'
"qof_1617_data_ind"

#' @name qof_data_ind
"qof_1516_data_ind"

#' @name qof_data_ind
"qof_1415_data_ind"

#' @name qof_data_ind
"qof_1314_data_ind"

#' @name qof_data_ind
"qof_1213_data_ind"
