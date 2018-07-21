# #
# # scratch
# #
# #
# # For testing code snippets
# #
#
# if (FALSE) {
#
# taskdir <- normalizePath(".")
# source(file = paste(taskdir, "R/main.R", sep = "/"))
#
# #dat <- main(qof_period = "1516", bProcessRaw = FALSE, bLoadData = FALSE)
# dat <- main(qof_period = "1617", bProcessRaw = FALSE, bLoadData = FALSE)
#
# l_add_orgtag <- function(x, c1 = "ccg_code", c2 = "practice_code") {
#     x %>%
#         mutate(
#             orgtag = paste(
#                 UQ(rlang::sym(c1))
#                 , UQ(rlang::sym(c2))
#             )
#         ) %>% invisible()
# }
#
# # Add ccg group lookup
# lu.orgs.ccgs.groups <- fread(
#     file = paste_paths(taskdir, "./data-raw/lu__ccg_groups.csv")
# ) #%>% l_add_orgtag("ccg_group_type", "ccg_group_code")
#
# # Add display order
# #
# # Based on ccg_group_type.  CCGs go to 10, practices go to 20.
# l_add_display_order <- function(x, c1 = "org.type") {
#     #names(x) %>% print()
#     x %>%
#         merge(
#             lu.orgs.ccgs.groups %>%
#                 select(ccg_group_code, display_order = type_display_order) %>%
#                 unique()
#             , by.x = "practice_code", by.y = "ccg_group_code"
#             , all.x = TRUE
#         ) %>%
#         mutate(display_order = ifelse(UQ(rlang::sym(c1)) == "england", 0, display_order)) %>%
#         mutate(display_order = ifelse(UQ(rlang::sym(c1)) == "ccg", 10, display_order)) %>%
#         mutate(display_order = ifelse(is.na(display_order), 20, display_order))
# }
#
# setnames.display <- function(x) {setnames(x, gsub("_|\\.", " ", names(x)))}
#
# # actually load data
# orgref <- dat$reference$orgref #%>% l_add_orgtag()
# indmap <- dat$reference$indmap
# measures <- dat$measures %>% l_add_display_order() #%>% l_add_orgtag()
# compare <- dat$compare %>% l_add_display_order("org.type.var") #%>% l_add_orgtag()
#
# practice_grouping <- list(
#     orgref %>%
#         # filter out existing ccg groups
#         filter(nchar(practice_code) > 3) %>%
#         select(practice_code, ccg_code) %>%
#         merge(lu.orgs.ccgs.groups, by = "ccg_code", all.y = TRUE) %>%
#         select(-type_display_order)
#
#     , orgref %>%
#         # filter out existing ccg groups
#         filter(nchar(practice_code) > 3) %>%
#         # only local ccgs
#         filter(ccg_code %in% c(lu.orgs.ccgs.groups$ccg_code %>% unique())) %>%
#         select(practice_code, ccg_code) %>%
#         mutate(
#             ccg_group_type = "ccg"
#             , ccg_group_type_name = "ccg"
#             , ccg_group_code = ccg_code
#             , ccg_group_name = ccg_code
#         )
# ) %>%
#     # lapply(function(x){sort(names(x))})
#     rbindlist(use.names = TRUE)
#
# # For inputs
#
# these_orgtypes <- measures$org.type %>% unique() %>% sort()
# these_ccg_groups <- practice_grouping$ccg_group_name %>% unique() %>% sort()
# these_ccgs <- measures$ccg_code %>% unique() %>% sort()
# these_practices <- practice_grouping$practice_code %>% unique() %>% sort()
#
# these_ind_groups <- measures$indicator_group_code %>% unique() %>% sort()
# these_inds <- measures$indicator_code %>% unique() %>% sort()
#
# these_measures <- measures$m.name %>% unique() %>% sort()
#
# these_comparisons <- compare$compare.type %>% unique() %>% sort()
# these_comparisons_param <- compare$compare.param  %>% unique() %>% sort()
#
# these_comparators <- compare$org.type.ref %>% unique() %>% sort()
#
#
# # for e.g. Heatmap
#
# lu_statsig_num <- fread(input = "
# statsig,istatsig
# Better,1
# Higher,1
# Similar,0
# Lower,-1
# Worse,-1
# Not tested,NA
# "
# )
#
#
# # stacked bar ####
#
# ## data - global
#
# ## reconstruct data from measures - fora all organisations
#
# ## grab counts for treat (achievement), exceptions
# ## grab counts for denominator (from achieve)
# ## grab counts for register (prevalance)
#
# l__register <- measures %>%
#     filter(
#         m.type == "prevalence"
#         , m.name == "qofprevalence"
#         , m.stat == "numerator"
#     ) %>%
#     select(-m.type, -m.name) %>%
#     dcast(... ~ m.stat) %>%
#     rename(register = "numerator") %>%
#     select(
#         ccg_code, practice_code, indicator_group_code
#         , register
#     )
#
# l__counts <- measures %>%
#     filter(
#         m.type == "performance"
#         , m.stat %in% c("numerator", "denominator")
#     ) %>%
#     select(-m.type, -org.type, -data_source) %>%
#     dcast(... ~ m.name + m.stat) %>%
#     merge(
#         l__register
#         , by = c("ccg_code", "practice_code", "indicator_group_code")
#         , all.x = TRUE
#     ) %>%
#     select(
#         ccg_code, practice_code, indicator_group_code, indicator_code
#         , register
#         , denominator = "achievement_denominator"
#         , optimal = "treatment_numerator"
#         , exceptions = "exceptions_numerator"
#         # at some point attach a modelled value
#         , model = 0
#     ) %>%
#     mutate(
#         suboptimal = denominator - optimal
#         # when patients reports as excepted, but included anyway ... National RA seems a bother
#         , exceptions__effective = pmax(pmin(register - denominator, exceptions), 0)
#         , exclusions = register - denominator - exceptions__effective
#         # where there is no register .. reconstruct
#         , register__effective = denominator + exceptions
#         , exclusions__effective = 0
#     ) %>%
#     # substitute the effective bits
#     mutate(
#         register = ifelse(is.na(register), register__effective, register)
#         , exclusions = ifelse(is.na(exclusions), exclusions__effective, exclusions)
#         , exceptions = ifelse(!is.na(exceptions__effective), exceptions__effective, exceptions)
#     ) %>%
#     select(-ends_with("__effective"))
#
# # %>%
# #     mutate(
# #         assert__optim_lte_denom = optimal <= denominator
# #         , assert__denom_lte_reg = denominator <= register
# #         , assert__opt_subopt_except_lte_reg = (optimal + suboptimal + exceptions__effective) <= register
# #         , assert__opt_subopt_except_excl_eq_reg = (optimal + suboptimal + exceptions__effective + exclusions) == register
# #         , assert__excl_gte_zero = exclusions >= 0
# #     )
# l__counts %>% mutate_if(is.character, as.factor) %>% head()
# l__counts %>% filter(!is.na(register)) %>% mutate_if(is.character, as.factor) %>% head()
#
# ## data ####
#
# l_ind_num <- measures %>%
#     filter(
#         m.type == "performance"
#         , ccg_code == "02Q"
#         , indicator_code == "DM002"
#         , m.stat == "numerator"
#         , !m.name %in% "achievement"
#     ) %>%
#     select(-m.type, -m.stat) %>%
#     dcast(... ~ m.name)
#
# l_ind_den <- measures %>%
#     filter(
#         m.type == "performance"
#         , m.name == "exceptions" # anything NOT acheivement
#         , ccg_code == "02Q"
#         , indicator_code == "DM002"
#         , m.stat == "denominator"
#     ) %>%
#     dcast(practice_code ~ m.stat)
#
# l_reg_num <- measures %>%
#     filter(
#         m.type == "prevalence"
#         , ccg_code == "02Q"
#         , indicator_group_code == "DM"
#         , m.stat == "numerator"
#     ) %>%
#     dcast(practice_code ~ m.name)
#
# l_ind <- l_ind_num %>%
#     merge(l_ind_den, by = "practice_code", all.x = TRUE) %>%
#     merge(l_reg_num, by = "practice_code", all.x = TRUE) %>%
#     arrange(display_order, ccg_code, practice_code)
#
# l_ind_delta <- l_ind %>%
#     mutate(
#         d_opt = treatment
#         , d_sub = suboptimal - exceptions
#         , d_exc = exceptions
#         , d_excl = denominator - qofprevalence
#         , d_model = 0
#     )
#
# fields_drop <- c(
#     "exceptions"
#     , "suboptimal"
#     , "treatment"
#     , "denominator"
#     #    , "qofprevalence"
# )
#
# fields_keep <- c(
#     "d_opt"
#     , "d_sub"
#     , "d_exc"
#     , "d_excl"
#     , "d_model"
# )
#
# l_ind_melt <- l_ind_delta %>%
#     select(-one_of(fields_drop)) %>%
#     melt(
#         measure.vars = fields_keep
#         , variable.name = "status", variable.factor = FALSE
#         , value.name = "value", value.factor = FALSE
#     ) %>%
#     mutate(value_p = value / qofprevalence) %>%
#     # dark arts to get desired order ...
#     mutate(
#         practice_code_ordered = factor(practice_code)
#         , practice_code_ordered = reorder(practice_code_ordered, desc(practice_code))
#         , practice_code_ordered = reorder(practice_code_ordered, desc(display_order))
#         # yticklabel_ordered = factor(yticklabel)
#         # , yticklabel_ordered = reorder(yticklabel_ordered, desc(yticklabel))
#         # , yticklabel_ordered = reorder(yticklabel_ordered, desc(display_order))
#     )
#
# ## Plot ####
#
# require("ggplot2")
#
# cols <- c(
#     d_opt = "forestgreen" # "green4"
#     , d_sub = "goldenrod" # "darkorange"
#     , d_exc = "firebrick" # "red"
#     , d_excl = "skyblue4" # "lightblue"
#     , d_model = "gray67"
# )
#
# #l_ind_melt <- sb__ind_dat()
#
# p <- ggplot(l_ind_melt) +
#     geom_col(
#         aes(
#             x = practice_code_ordered
#             , y = value_p
#             , fill = status
#         )
#         #, position = "dodge"
#         , position = position_stack(reverse = TRUE)
#         #, position = position_fill(reverse = TRUE)
#     ) +
#     geom_text(
#         data = subset(l_ind_melt, value > 0)
#         , aes(
#             x = practice_code_ordered
#             , y = value_p
#             , label = scales::comma(value)
#             #, label = subset(l_ind_melt, value > 0)scales::comma(value)
#         )
#         , position = position_stack(vjust = 0.5)
#         , size = 3
#     ) +
#     #scale_colour_manual(values = cols, aesthetics = "fill") +
#     scale_fill_manual(values = cols) +
#     coord_flip() +
#     theme_minimal() +
#     theme(legend.position = "top")
#
# p %>% print()
#
#
# }
