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
#' \tabular{ll}{
#'  \code{practice_code} \tab {<chr n=8210>} \cr
#'  \code{practice_name} \tab {<chr n=17148>} \cr
#'  \code{ccg_code} \tab {<chr n=369>} \cr
#'  \code{ccg_geography_code} \tab {<chr n=223>} \cr
#'  \code{ccg_name} \tab {<chr n=377>} \cr
#'  \code{stp_code} \tab {<chr n=83>} \cr
#'  \code{stp_name} \tab {<chr n=92>} \cr
#'  \code{subregion_code} \tab {<chr n=23>} \cr
#'  \code{subregion_geography_code} \tab {<chr n=22>} \cr
#'  \code{subregion_name} \tab {<chr n=35>} \cr
#'  \code{region_code} \tab {<chr n=7>} \cr
#'  \code{region_geography_code} \tab {<chr n=7>} \cr
#'  \code{region_name} \tab {<chr n=12>} \cr
#'  \code{country} \tab {<chr n=2>} \cr
#'  \code{revised_maximum_points} \tab {<num>} \cr
#'  \code{qof_period} \tab {<chr n=7>} \cr
#' }
#'
#' @family qof_meta
#'
"qof_meta_org"

#' @name qof_meta_org
"qof_1718_meta_org"

#' @name qof_meta_org
"qof_1617_meta_org"

#' @name qof_meta_org
"qof_1516_meta_org"

# @name qof_meta_org
# "qof_1415_meta_org"

# @name qof_meta_org
# "qof_1314_meta_org"

# @name qof_meta_org
# "qof_1213_meta_org"


#' QOF reference data for indicators
#'
#' \tabular{ll}{
#'  \code{indicator_code} \tab {<chr n=255>} \cr
#'  \code{indicator_description} \tab {<chr n=270>} \cr
#'  \code{indicator_point_value} \tab {<int>} \cr
#'  \code{indicator_group_code} \tab {<chr n=38>} \cr
#'  \code{indicator_group_description} \tab {<chr n=43>} \cr
#'  \code{domain_code} \tab {<chr n=6>} \cr
#'  \code{domain_description} \tab {<chr n=7>} \cr
#'  \code{patient_list_type} \tab {<chr n=11>} \cr
#'  \code{qof_period} \tab {<chr n=7>} \cr
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
#'
"qof_meta_ind"

#' @name qof_meta_ind
"qof_1718_meta_ind"

#' @name qof_meta_ind
"qof_1617_meta_ind"

#' @name qof_meta_ind
"qof_1516_meta_ind"

# @name qof_meta_ind
# "qof_1415_meta_ind"

# @name qof_meta_ind
# "qof_1314_meta_ind"

# @name qof_meta_ind
# "qof_1213_meta_ind"

#' QOF data for prevalence
#'
#' \tabular{ll}{
#'  \code{practice_code} \tab {<chr n=8210>} \cr
#'  \code{indicator_group_code} \tab {<chr n=33>} \cr
#'  \code{register} \tab {<num>} \cr
#'  \code{patient_list_type} \tab {<chr n=7>} \cr
#'  \code{patient_list_size} \tab {<num>} \cr
#'  \code{qof_period} \tab {<chr n=7>} \cr
#' }
#'
#' @family qof_data
#'
"qof_data_prev"

#' @name qof_data_prev
"qof_1718_data_prev"

#' @name qof_data_prev
"qof_1617_data_prev"

#' @name qof_data_prev
"qof_1516_data_prev"

# @name qof_data_prev
# "qof_1415_data_prev"

# @name qof_data_prev
# "qof_1314_data_prev"

# @name qof_data_prev
# "qof_1213_data_prev"


#' QOF data for indicators
#'
#' \tabular{ll}{
#'  \code{practice_code} \tab {<chr n=8209>} \cr
#'  \code{indicator_code} \tab {<chr n=255>} \cr
#'  \code{ACHIEVED_POINTS} \tab {<num>} \cr
#'  \code{DENOMINATOR} \tab {<num>} \cr
#'  \code{EXCEPTIONS} \tab {<num>} \cr
#'  \code{NUMERATOR} \tab {<num>} \cr
#'  \code{REGISTER} \tab {<num>} \cr
#'  \code{qof_period} \tab {<chr n=7>} \cr
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
#'
"qof_data_ind"

#' @name qof_data_ind
"qof_1718_data_ind"

#' @name qof_data_ind
"qof_1617_data_ind"

#' @name qof_data_ind
"qof_1516_data_ind"

# @name qof_data_ind
# "qof_1415_data_ind"

# @name qof_data_ind
# "qof_1314_data_ind"

# @name qof_data_ind
# "qof_1213_data_ind"

#' List of CCGs considered local
#'
"lu_ccgs"

#' Local CCG groups
#'
#' \tabular{ll}{
#'  \code{ccg_group_type} \tab {<chr n=4>} \cr
#'  \code{ccg_group_type_name} \tab {<chr n=4>} \cr
#'  \code{ccg_code} \tab {<chr n=7>} \cr
#'  \code{ccg_group_code} \tab {<chr n=7>} \cr
#'  \code{ccg_group_name} \tab {<chr n=7>} \cr
#'  \code{type_display_order} \tab {<int>} \cr
#' }
#'
"lu_ccg_groups"
