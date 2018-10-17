#
# data.R
#

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
#'
"qof_1617_meta_org"


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
#' @family qof_meta
#'
"qof_1617_meta_ind"


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
#'
"qof_1617_data_prev"


#' QOF data for indicators
#'
#' \preformatted{
#' $ practice_code : chr
#' $ indicator_code: chr
#' $ measure       : chr
#' $ value         : num
#' }
#'
#' @family qof_data
#'
"qof_1617_data_ind"
