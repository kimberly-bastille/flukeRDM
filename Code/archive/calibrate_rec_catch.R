################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      calibrate_rec_catch.R
# Purpose:     Calibration-year trip simulation with no adjustment for illegal
#              harvest or voluntary release. The earliest of the three archived
#              calibrate_rec_catch* files.
# Superseded by: Code/sim/calibrate_rec_catch0_optimized.R
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
################################################################################
################################################################################



#This is the simulation model for the calibration year WITHOUT any adjustments for illegal harvest or voluntary release

s<-"MA"
i=1
mode_draw="pr"

dtrip_MA<-feather::read_feather(file.path(input_data_cd, "directed_trips_calibration_MA.feather")) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == i) %>%
  dplyr::select(mode, date, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min) %>% 
  dplyr::filter(mode==mode_draw)

catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("calib_catch_draws_",s, "_", i,".feather"))) %>% 
  dplyr::left_join(dtrip_MA, by=c("mode", "date"))%>% 
  dplyr::filter(mode==mode_draw)

angler_dems<-catch_data %>% 
  dplyr::select(date, mode, tripid, total_trips_12, age, cost)%>% 
  dplyr::filter(mode==mode_draw)

angler_dems<-dplyr::distinct(angler_dems)

######################################
##   Begin simulating trip outcomes ##
######################################

#####
sf_size_data <- read_csv(file.path(test_data_cd, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == s, draw==0 ) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

bsb_size_data <- read_csv(file.path(test_data_cd, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == s, draw==0 ) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

scup_size_data <- read_csv(file.path(test_data_cd, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == s, draw==0 ) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length)


# subset trips with zero catch, as no size draws are required
sf_zero_catch <- dplyr::filter(catch_data, sf_cat == 0)
bsb_zero_catch <- dplyr::filter(catch_data, bsb_cat == 0)
scup_zero_catch <- dplyr::filter(catch_data, scup_cat == 0)


#Check to see if there is no catch for either species and if so, pipe code around keep/release determination
sf_catch_check<-base::sum(catch_data$sf_cat)
bsb_catch_check<-base::sum(catch_data$bsb_cat)
scup_catch_check<-base::sum(catch_data$scup_cat)


# Summer flounder trip simulation

  #keep trips with positive sf catch
  sf_catch_data <- dplyr::filter(catch_data, sf_cat > 0)
  
  row_inds <- seq_len(nrow(sf_catch_data))
  
  sf_catch_data<-sf_catch_data %>%
    dplyr::slice(rep(row_inds, sf_cat))   %>%
    dplyr::mutate(fishid=dplyr::row_number())
  
  # generate lengths for each fish
  catch_size_data <- sf_catch_data %>%
    dplyr::mutate(fitted_length = sample(sf_size_data$length,
                                         nrow(.),
                                         prob = sf_size_data$fitted_prob,
                                         replace = TRUE)) %>%     #dplyr::arrange(period2, tripid, catch_draw) 
  dplyr::mutate(fitted_length=fitted_length*2.54)
  
  
  # Impose regulations, calculate keep and release per trip
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min ,1,0)) %>%
    dplyr::group_by(tripid, date, mode, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        fluke_bag > 0 ~ ifelse(csum_keep<=fluke_bag & posskeep==1,1,0),
        TRUE ~ 0))
  
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0))
  
  catch_size_data<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  #%>%
    #dplyr::rename(mode1=mode)
  
  trip_data <- catch_size_data %>%
    dplyr::group_by(date, catch_draw, tripid, mode) %>%
    dplyr::summarize(tot_keep_sf_new = sum(keep),
                     tot_rel_sf_new = sum(release),
                     .groups = "drop") %>%
    dplyr::ungroup()
  
  
  sf_zero_catch<-sf_zero_catch %>%
    dplyr::select(date, catch_draw, tripid, mode) %>%
    dplyr::mutate(tot_keep_sf_new=0,
                  tot_rel_sf_new=0)
  
  sf_trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::select(c("date", "catch_draw","tripid","mode",
                    "tot_keep_sf_new","tot_rel_sf_new"))
  
  sf_trip_data<- sf_trip_data %>% dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))
  sf_trip_data<-data.table::as.data.table(sf_trip_data)
  data.table::setkey(sf_trip_data, "domain2")
#}


# if (cod_catch_check==0 & had_catch_check!=0){
#   trip_data<-cod_catch_data
#   trip_data<- trip_data %>% 
#     dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
#     dplyr::select(-mode) %>% 
#     as.data.table()
#   
#   data.table::setkey(trip_data, "domain2")
#   
#   trip_data$tot_keep_cod_new<-0
#   trip_data$tot_rel_cod_new<-0
# }

# BSB flounder trip simulation
  
  # keep trips with positive bsb catch
  bsb_catch_data <- dplyr::filter(catch_data, bsb_cat > 0)
  
  row_inds <- seq_len(nrow(bsb_catch_data))
  
  bsb_catch_data<-bsb_catch_data %>%
    dplyr::slice(rep(row_inds, bsb_cat))   %>%
    dplyr::mutate(fishid=dplyr::row_number())
  
  # generate lengths for each fish
  catch_size_data <- bsb_catch_data %>%
    dplyr::mutate(fitted_length = sample(bsb_size_data$length,
                                         nrow(.),
                                         prob = bsb_size_data$fitted_prob,
                                         replace = TRUE)) %>%     #dplyr::arrange(period2, tripid, catch_draw) 
    dplyr::mutate(fitted_length=fitted_length*2.54)

# Impose regulations, calculate keep and release per trip
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>%
    dplyr::group_by(tripid, date, mode, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0))
  
  catch_size_data<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  #%>%
  #dplyr::rename(mode1=mode)
  
  trip_data <- catch_size_data %>%
    dplyr::group_by(date, catch_draw, tripid, mode) %>%
    dplyr::summarize(tot_keep_bsb_new = sum(keep),
                     tot_rel_bsb_new = sum(release),
                     .groups = "drop") %>%
    dplyr::ungroup()
  
  
  bsb_zero_catch<-bsb_zero_catch %>%
    dplyr::select(date, catch_draw, tripid, mode) %>%
    dplyr::mutate(tot_keep_bsb_new=0,
                  tot_rel_bsb_new=0)
  
  bsb_trip_data <- dplyr::bind_rows(trip_data, bsb_zero_catch) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::select(c("date", "catch_draw","tripid","mode",
                    "tot_keep_bsb_new","tot_rel_bsb_new"))
  
  bsb_trip_data<- bsb_trip_data %>% dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
    dplyr::select(-c("date", "catch_draw","tripid","mode"))
  bsb_trip_data<-data.table::as.data.table(bsb_trip_data)
  data.table::setkey(bsb_trip_data, "domain2")
  
  
# Scup flounder trip simulation
  
  #keep trips with positive scup catch
  scup_catch_data <- dplyr::filter(catch_data, scup_cat > 0)
  
  row_inds <- seq_len(nrow(scup_catch_data))
  
  scup_catch_data<-scup_catch_data %>%
    dplyr::slice(rep(row_inds, scup_cat))   %>%
    dplyr::mutate(fishid=dplyr::row_number())
  
  # generate lengths for each fish
  catch_size_data <- scup_catch_data %>%
    dplyr::mutate(fitted_length = sample(scup_size_data$length,
                                         nrow(.),
                                         prob = scup_size_data$fitted_prob,
                                         replace = TRUE)) %>%     #dplyr::arrange(period2, tripid, catch_draw) 
    dplyr::mutate(fitted_length=fitted_length*2.54)
  
  
  
  
  # Impose regulations, calculate keep and release per trip
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=scup_min ,1,0)) %>%
    dplyr::group_by(tripid, date, mode, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0))
  
  catch_size_data<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  #%>%
  #dplyr::rename(mode1=mode)
  
  trip_data <- catch_size_data %>%
    dplyr::group_by(date, catch_draw, tripid, mode) %>%
    dplyr::summarize(tot_keep_scup_new = sum(keep),
                     tot_rel_scup_new = sum(release),
                     .groups = "drop") %>%
    dplyr::ungroup()
  
  
  scup_zero_catch<-scup_zero_catch %>%
    dplyr::select(date, catch_draw, tripid, mode) %>%
    dplyr::mutate(tot_keep_scup_new=0,
                  tot_rel_scup_new=0)
  
  scup_trip_data <- dplyr::bind_rows(trip_data, scup_zero_catch) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::select(c("date", "catch_draw","tripid","mode",
                    "tot_keep_scup_new","tot_rel_scup_new"))
  
  scup_trip_data<- scup_trip_data %>% dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
    dplyr::select(-c("date", "catch_draw","tripid","mode"))
  scup_trip_data<-data.table::as.data.table(scup_trip_data)
  data.table::setkey(scup_trip_data, "domain2") 
  
  
  
  # merge the hadd trip data with the rest of the trip data
  trip_data<- sf_trip_data[bsb_trip_data, on = "domain2"]
  trip_data<- trip_data[scup_trip_data, on = "domain2"]
  
  trip_data<- trip_data %>% 
    dplyr::mutate(tot_scup_catch = tot_keep_scup_new + tot_rel_scup_new, 
                  tot_bsb_catch = tot_keep_bsb_new + tot_rel_bsb_new, 
                  tot_sf_catch = tot_keep_sf_new + tot_rel_sf_new)
#}

  parameters <- trip_data %>% 
    dplyr::select(date, mode, tripid) 
  
  parameters <- dplyr::distinct(parameters) 
  parameters<-  parameters %>% 
    #dplyr::arrange(date, mode, tripid) %>% 
    dplyr::mutate(beta_sqrt_sf_keep= rnorm(nrow(parameters), mean = 0.827, sd = 1.267), 
                  beta_sqrt_sf_release = rnorm(nrow(parameters), mean = 0.065 , sd = 0.325) , 
                  beta_sqrt_bsb_keep = rnorm(nrow(parameters), mean = 0.353, sd = 0.129), 
                  beta_sqrt_bsb_release = rnorm(nrow(parameters), mean = 0.074 , sd = 0), 
                  beta_sqrt_sf_bsb_keep = rnorm(nrow(parameters), mean=-0.056  , sd = 0.196 ), 
                  beta_sqrt_scup_catch = rnorm(nrow(parameters), mean = 0.018 , sd = 0), 
                  beta_opt_out = rnorm(nrow(parameters), mean =-2.056 , sd = 1.977), 
                  beta_opt_out_avidity = rnorm(nrow(parameters), mean =-0.010 , sd = 0), 
                  beta_opt_out_age = rnorm(nrow(parameters), mean =0.010 , sd = 0), 
                  beta_cost = -0.012) 

trip_data<- trip_data %>% 
  dplyr::left_join(parameters, by = c("date", "mode", "tripid")) %>% 
  dplyr::arrange(date, mode, tripid, tripid, catch_draw) %>% 
  dplyr::left_join(angler_dems, by = c("date", "mode", "tripid")) 


# Costs_new_state data sets will retain raw trip outcomes from the baseline scenario.
# We will merge these data to the prediction year outcomes to calculate changes in CS.
#baseline_outcomes[[i]] <- trip_data %>%
baseline_outcomes<- trip_data %>%
  dplyr::rename(tot_keep_bsb_base = tot_keep_bsb_new,
                tot_keep_scup_base = tot_keep_scup_new,
                tot_keep_sf_base = tot_keep_sf_new,
                tot_rel_bsb_base = tot_rel_bsb_new, 
                tot_rel_scup_base = tot_rel_scup_new,
                tot_rel_sf_base = tot_rel_sf_new)


#  utility
trip_data <-trip_data %>%
  dplyr::mutate(
    vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf_new) +
      beta_sqrt_sf_release*sqrt(tot_rel_sf_new) +
      beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_new) +
      beta_sqrt_bsb_release*sqrt(tot_rel_bsb_new) +
      beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf_new)*sqrt(tot_keep_bsb_new)) +
      beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
      beta_cost*cost)


# trip_data <- trip_data %>%
#   dplyr::mutate(period = as.numeric(as.factor(period2)))
# 
# period_names<-subset(trip_data, select=c("period", "period2"))
# period_names <- period_names[!duplicated(period_names), ]


mean_trip_data <- trip_data %>% data.table::data.table() %>% 
  .[, group_index := .GRP, by = .(date, mode, catch_draw, tripid)]

# Now expand the data to create two alternatives, representing the alternatives available in choice survey
mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
  tidyr::uncount(n_alt) %>%
  dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                opt_out = ifelse(alt == 2, 1, 0))

#Calculate the expected utility of alts 2 parameters of the utility function,
#put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)

setDT(mean_trip_data)

# Filter only alt == 2 once, and calculate vA 
mean_trip_data[alt == 2, "vA" := .(
  beta_opt_out * opt_out +
    beta_opt_out_age * (age * opt_out) +
    beta_opt_out_avidity * (total_trips_12 * opt_out) 
)]

# Pre-compute exponential terms
mean_trip_data[, `:=`(exp_vA = exp(vA))]

# Group by group_index and calculate probabilities and log-sums
mean_trip_data[, `:=`(
  probA = exp_vA / sum(exp_vA)
), by = group_index]


mean_trip_data<- subset(mean_trip_data, alt==1) %>% 
  dplyr::select(-domain2, -group_index, -exp_vA) 

# Get rid of things we don't need.
mean_trip_data <- mean_trip_data %>% 
                  dplyr::filter(alt==1) %>% 
                  dplyr::select(-matches("beta")) %>% 
                  dplyr::select(-"alt", -"opt_out", -"vA" ,-"cost", -"age", -"total_trips_12", -"catch_draw") 
                                                    
all_vars<-c()
all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("date","mode", "tripid")]
all_vars

# average outcomes across draws
mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
  .[,lapply(.SD, mean), by = c("date","mode", "tripid"), .SDcols = all_vars]

# multiply the average trip probability (probA) by each catch variable to get probability-weighted catch
list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",    "tot_keep_bsb_new",  "tot_rel_bsb_new",   "tot_keep_scup_new",
                "tot_rel_scup_new",  "tot_scup_catch",    "tot_bsb_catch",     "tot_sf_catch")

mean_trip_data <- mean_trip_data %>%
  as.data.table() %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
  .[]


dtrips<-feather::read_feather(file.path(input_data_cd, "directed_trips_calibration_MA.feather")) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == i) %>%
  dplyr::select(mode, date, dtrip) %>% 
  dplyr::filter(mode==mode_draw)

mean_trip_data<-mean_trip_data %>% 
  left_join(dtrips, by = c("mode", "date"))

mean_trip_data <-mean_trip_data %>% 
  group_by(mode, date) %>% 
  dplyr::mutate(mean_prob=mean(probA)) %>% 
  dplyr::ungroup() %>%       
  dplyr::mutate(sims=round(dtrip/mean_prob), 
                expand=sims/n_draws, 
                n_choice_occasions=1)

mean_trip_data <- mean_trip_data %>% 
  mutate(uniform=runif(n(), min=0, max=1)) %>% 
  dplyr::arrange(date, mode, uniform)

mean_trip_data1 <- mean_trip_data %>% 
  dplyr::group_by(date, mode) %>%
  dplyr::mutate(id_within_group = row_number()) %>% 
  dplyr::filter(expand<1 & id_within_group<=sims) 

mean_trip_data2 <- mean_trip_data %>% 
  dplyr::filter(expand>1)  %>% 
  dplyr::mutate(expand2=ceiling(expand)) 

row_inds <- seq_len(nrow(mean_trip_data2))

mean_trip_data2<-mean_trip_data2 %>% 
  slice(rep(row_inds,expand2))  

mean_trip_data2 <- mean_trip_data2 %>%
  dplyr::group_by(date, mode) %>%
  dplyr::mutate(id_within_group = row_number()) %>% 
  dplyr::filter(id_within_group<=sims)

results<-mean_trip_data1 %>% 
  dplyr::bind_rows(mean_trip_data2)

list_names = c("tot_bsb_catch","tot_keep_bsb_new","tot_keep_scup_new","tot_keep_sf_new","tot_rel_bsb_new",   
               "tot_rel_scup_new","tot_rel_sf_new","tot_scup_catch","tot_sf_catch",
               "probA","n_choice_occasions")

aggregate_trip_data <- results %>%
  data.table::as.data.table() %>%
  .[,lapply(.SD, sum),  by = c("date", "mode"), .SDcols = list_names]

aggregate_trip_data<-aggregate_trip_data %>% 
  dplyr::rename(estimated_trips=probA, 
                sf_catch=tot_sf_catch, 
                bsb_catch=tot_bsb_catch, 
                scup_catch=tot_scup_catch, 
                sf_keep=tot_keep_sf_new, 
                bsb_keep=tot_keep_bsb_new, 
                scup_keep=tot_keep_scup_new,
                sf_rel=tot_rel_sf_new, 
                bsb_rel=tot_rel_bsb_new, 
                scup_rel=tot_rel_scup_new)

#saveRDS(aggregate_trip_data, file = paste0(output_data_cd, "calibration_data_", s,"_", i, ".rds")) 


list_names = c("bsb_catch","bsb_keep","bsb_rel", 
               "scup_catch", "scup_keep","scup_rel", 
               "sf_catch", "sf_keep","sf_rel",
               "estimated_trips","n_choice_occasions")

summed_results <- aggregate_trip_data %>%
  data.table::as.data.table() %>%
  .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]


########
#Compare calibration output to MRIP by state-mode 

#Save simulation results by mode as objects 
# Loop over rows (modes)
for (i in 1:nrow(summed_results)) {
  mode_val <- summed_results$mode[i]
  
  # Loop over summary columns
  for (var in names(summed_results)[names(summed_results) != "mode"]) {
    value <- summed_results[[var]][i]
    obj_name <- paste0(var, "_", mode_val, "_model")
    assign(obj_name, value)
  }
}


#Save MRIP estimates  by mode as objects 
MRIP_comparison_draw <- MRIP_comparison %>% 
  dplyr::filter(draw==i & state==s)%>% 
  dplyr::filter(mode==mode_draw)

# Loop over rows (modes)
for (i in 1:nrow(MRIP_comparison_draw)) {
  mode_val <- MRIP_comparison_draw$mode[i]
  
  # Loop over summary columns
  for (var in names(MRIP_comparison_draw)[names(MRIP_comparison_draw) != "mode"]) {
    value <- MRIP_comparison_draw[[var]][i]
    obj_name <- paste0(var, "_", mode_val, "_MRIP")
    assign(obj_name, value)
  }
}

species <- c("sf", "bsb", "scup")
dispositions <- c("keep", "rel", "catch")

compare <- data.frame()

for (sp in species) {
  for (disp in dispositions) {
    for (mode in mode_draw) {
      
      # Construct variable names
      base_name <- paste(sp, disp, mode, sep = "_")
      mrip_var <- paste0(base_name, "_MRIP")
      model_var <- paste0(base_name, "_model")
      
      # Check if both variables exist
      if (exists(mrip_var) && exists(model_var)) {
        # Retrieve values
        mrip_val <- get(mrip_var)
        model_val <- get(model_var)
        
        # Calculate differences
        diff_val <- model_val - mrip_val
        pct_diff_val <- if (mrip_val != 0)  (diff_val / mrip_val) * 100 else NA
        abs_diff_val <- abs(model_val - mrip_val)
        abs_pct_diff_val <- if (mrip_val != 0)  abs((diff_val / mrip_val) * 100) else NA
        
        # Create output variable names
        assign(paste0(base_name, "_diff"), diff_val)
        assign(paste0(base_name, "_pctdiff"), pct_diff_val)
        assign(paste0(base_name, "_abs_diff"), abs_diff_val)
        assign(paste0(base_name, "_abs_pctdiff"), abs_pct_diff_val)
        
        compare <- rbind(compare, data.frame(
          species = sp,
          disposition = disp,
          mode = mode,
          MRIP = mrip_val,
          model = model_val,
          diff = diff_val,
          pct_diff = pct_diff_val, 
          abs_diff_val= abs_diff_val, 
          abs_pct_diff_val= abs_pct_diff_val
        ))
      } 
      
      else {
        warning(paste("Missing variable:", mrip_var, "or", model_var))
        
       
      }
    }
  }
}

compare<-compare %>% 
  dplyr::mutate(keep_to_rel = if_else(diff < 0, 1, 0), 
                rel_to_keep = if_else(diff > 0, 1, 0))

compare_k<-compare %>% 
  dplyr::filter(disposition=="keep") %>% 
  dplyr::select(mode, species, MRIP, model, diff, pct_diff, keep_to_rel, rel_to_keep) %>% 
  dplyr::rename(MRIP_keep=MRIP, model_keep=model)

compare_r<-compare %>% 
  dplyr::filter(disposition=="rel") %>% 
  dplyr::select(mode, species, MRIP, model) %>% 
  dplyr::rename(MRIP_rel=MRIP, model_rel=model) %>% 
  dplyr::left_join(compare_k, by=c("mode", "species"))

calib_comparison<-compare_r %>% 
  dplyr::mutate(p_rel_to_keep=abs(diff/model_rel), 
                p_keep_to_rel=abs(diff/model_keep))

saveRDS(calib_comparison, file = file.path(input_data_cd,"calib_comparison.rds"))

