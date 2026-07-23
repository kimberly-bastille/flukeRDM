################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      compute SQ weights from averages.R
# Purpose:     Computes total projected harvest and discard WEIGHTS under status-quo
#              regulations, using average weight per harvested and per discarded
#              fish rather than a length-weight relationship applied fish by fish.
# Superseded by: the length-weight conversion now applied inside the projection (L_W_Conversion.csv)
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
#
# Dev paths: 2 hardcoded absolute paths to a developer's local machine
#   (C:\ or E:\), at lines 26 and 27.
################################################################################
################################################################################



# This file compute total projected harvest/discard weights under under status-quo regulations,  
# using data on average weight per harvested and discarded fish. 

# Pull in the weights file and model SQ output file
wts<-read_excel("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/SQ_weight_per_catch.xlsx")
SQ_output<-read_csv("E:/Lou's projects/flukeRDM/flukeRDM_iterative_data/test_output1.csv", show_col_types = FALSE)

SQ_output<-SQ_output %>% 
  dplyr::filter(metric %in% c("keep_numbers", "release_numbers", "discmort_number")) %>% 
  dplyr::filter(mode !="all modes") %>% 
  dplyr::left_join(wts, by=c("species", "mode", "state"))

SQ_output <- SQ_output %>%
  dplyr::mutate(
    keep_weightavg = case_when(
      metric == "keep_numbers"    ~ avg_wt_harv * value,
      TRUE ~ NA_real_
    ),
    release_weightavg = case_when(
      metric == "release_numbers" ~ avg_wt_rel * value,
      TRUE ~ NA_real_
    ),
    discmort_weightavg = case_when(
      metric == "discmort_number" ~ avg_wt_rel * value,
      TRUE ~ NA_real_
    )
  )

SQ_output <- SQ_output %>%
  dplyr::mutate(
    keep_weightavg     = ifelse(is.na(keep_weightavg), 0, keep_weightavg),
    release_weightavg  = ifelse(is.na(release_weightavg), 0, release_weightavg),
    discmort_weightavg = ifelse(is.na(discmort_weightavg), 0, discmort_weightavg)
  )

# catch weights by mode
SQ_output_long <- SQ_output %>%
  dplyr::select(-metric, -avg_wt_harv, -avg_wt_rel, -value) %>%
  tidyr::pivot_longer(
    cols = c(keep_weightavg, release_weightavg, discmort_weightavg),
    names_to = "metric",
    values_to = "value"
  )

data.table::setDT(SQ_output_long)

# catch weights over all modes
SQ_output_long_summed <- SQ_output_long[, .(
  value = sum(value)
), by = .(state, species, draw, metric)]

SQ_output_long_summed$mode<-"all modes"


# combine mode-specific and aggregate catch weights
catch_weights_SQ<-rbindlist(list(SQ_output_long_summed, SQ_output_long), use.names = TRUE,  fill = TRUE)
catch_weights_SQ<-catch_weights_SQ %>% 
  dplyr::mutate(value=value/2204) #convert pounds to metric tons

# CHECK -> Compute catch weights based on calibration data, compare to MRIP query site
# calib_comparison<-readRDS(file.path(iterative_input_data_cd, "calibrated_model_stats_new.rds")) %>%
#   dplyr::select(state, draw, species, mode, model_keep, model_rel, MRIP_keep, MRIP_rel, diff_keep, diff_rel, pct_diff_keep, pct_diff_rel) %>%
#   dplyr::left_join(wts, by=c("species", "mode", "state") )
# 
# calib_comparison <- calib_comparison %>%
#   dplyr::mutate(
#     keep_weightavg = avg_wt_harv * model_keep,
#     release_weightavg = avg_wt_rel * model_rel,
#     mrip_keep_weightavg = avg_wt_harv * MRIP_keep,
#     mrip_release_weightavg = avg_wt_rel * MRIP_rel)
# 
# calib_comparison_mode <- calib_comparison %>%
#   dplyr::group_by(state, species, mode) %>%
#   dplyr::summarise(mean_keep_weightavg=mean(keep_weightavg),
#                    mean_release_weightavg=mean(release_weightavg),
#                    sd_keep_weightavg=sd(keep_weightavg),
#                    sd_release_weightavg=sd(release_weightavg),
#                    mean_keep_n=mean(model_keep),
#                    mean_release_n=mean(model_rel),
#                    mean_mrip_keep_weightavg=mean(mrip_keep_weightavg),
#                    mean_mrip_release_weightavg=mean(mrip_release_weightavg)) %>%
#   dplyr::ungroup()
# 
# calib_comparison_state <- calib_comparison %>%
#   dplyr::group_by(state, species, draw) %>%
#   dplyr::summarise(keep_weightavg=sum(keep_weightavg),
#                    release_weightavg=sum(release_weightavg),
#                    keep_n=sum(model_keep),
#                    release_n=sum(model_rel),
#                    mrip_keep_weightavg=sum(mrip_keep_weightavg),
#                    mrip_release_weightavg=sum(mrip_release_weightavg),
#                    mrip_keep_n=sum(MRIP_keep),
#                    mrip_release_n=sum(MRIP_rel)) %>%
#   dplyr::ungroup()
# 
# calib_comparison_state <- calib_comparison_state %>%
#   dplyr::group_by(state, species) %>%
#   dplyr::summarise(mean_keep_weightavg=mean(keep_weightavg),
#                    mean_release_weightavg=mean(release_weightavg),
#                    sd_keep_weightavg=sd(keep_weightavg),
#                    sd_release_weightavg=sd(release_weightavg),
#                    mean_keep_n=mean(keep_n),
#                    mean_release_n=mean(release_n),
#                    mean_mrip_keep_n=sum(mrip_keep_n),
#                    mean_mrip_release_n=sum(mrip_release_n),
#                    mean_mrip_keep_weightavg=mean(mrip_keep_weightavg),
#                    mean_mrip_release_weightavg=mean(mrip_release_weightavg)) %>%
#   dplyr::ungroup()
# 
# # catch weights over all states
# calib_comparison_coast <- calib_comparison %>%
#   dplyr::group_by(species, draw) %>%
#   dplyr::summarise(keep_weightavg=sum(keep_weightavg),
#                    release_weightavg=sum(release_weightavg),
#                    keep_n=sum(model_keep),
#                    release_n=sum(model_rel),
#                    mrip_keep_weightavg=sum(mrip_keep_weightavg),
#                    mrip_release_weightavg=sum(mrip_release_weightavg),
#                    mrip_keep_n=sum(MRIP_keep),
#                    mrip_release_n=sum(MRIP_rel)) %>%
#   dplyr::ungroup()
# 
# calib_comparison_coast <- calib_comparison_coast %>%
#   dplyr::group_by( species) %>%
#   dplyr::summarise(mean_keep_weightavg=mean(keep_weightavg),
#                    mean_release_weightavg=mean(release_weightavg),
#                    sd_keep_weightavg=sd(keep_weightavg),
#                    sd_release_weightavg=sd(release_weightavg),
#                    mean_keep_n=mean(keep_n),
#                    mean_release_n=mean(release_n),
#                    mean_mrip_keep_n=sum(mrip_keep_n),
#                    mean_mrip_release_n=sum(mrip_release_n),
#                    mean_mrip_keep_weightavg=mean(mrip_keep_weightavg),
#                    mean_mrip_release_weightavg=mean(mrip_release_weightavg)) %>%
#   dplyr::ungroup()
# 
# 
# calib_comparison_coast<-calib_comparison_coast %>% 
#   dplyr::mutate(model_release_mt=mean_release_weightavg/2204, 
#                 mrip_release_mt=mean_mrip_release_weightavg/2204)