################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      calibrate_rec_catch0.R
# Purpose:     Calibration PASS 0 - the strict-compliance trip simulation whose
#              gap against MRIP the next pass closes. Direct predecessor of the
#              current optimized version.
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



#This is the calibration-year trip simulation WITHOUT any adjustments for illegal harvest or voluntary release

MRIP_comparison = read_dta(file.path(iterative_input_data_cd,"simulated_catch_totals.dta")) %>% 
  dplyr::rename(estimated_trips=tot_dtrip_sim, 
                sf_catch=tot_sf_cat_sim, 
                bsb_catch=tot_bsb_cat_sim, 
                scup_catch=tot_scup_cat_sim, 
                sf_keep=tot_sf_keep_sim, 
                bsb_keep=tot_bsb_keep_sim, 
                scup_keep=tot_scup_keep_sim,
                sf_rel=tot_sf_rel_sim, 
                bsb_rel=tot_bsb_rel_sim, 
                scup_rel=tot_scup_rel_sim) 

states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
# s<-"MA"
# md<-"pr"
# i<-1

mode_draw <- c("sh", "pr", "fh")
draws <- 1:n_simulations

# Create an empty list to collect results
calib_comparison <- list()

# Counter for appending to list
k <- 1

# Loop over all combinations
for (s in states) {
  for (md in mode_draw) {
    for (i in draws) {
      
      # import necessary data
      dtripz<-feather::read_feather(file.path(iterative_input_data_cd, paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".feather"))) %>% 
        tibble::tibble() %>%
        dplyr::filter(draw == i) %>%
        dplyr::select(mode, date, dtrip, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min) %>% 
        dplyr::filter(mode==md)
      
      catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".feather"))) %>% 
        dplyr::left_join(dtripz, by=c("mode", "date"))%>% 
        dplyr::filter(mode==md) %>% 
        dplyr::select(-dtrip)
      
      
      angler_dems<-catch_data %>% 
        dplyr::select(date, mode, tripid, total_trips_12, age, cost)%>% 
        dplyr::filter(mode==md)
      
      angler_dems<-dplyr::distinct(angler_dems)
      
      sf_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
        dplyr::filter(state == s, species=="sf", draw==i) %>% 
        dplyr::filter(!is.na(fitted_prob)) %>% 
        dplyr::select(state, fitted_prob, length)
      
      bsb_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
        dplyr::filter(state == s, species=="bsb" , draw==i) %>% 
        dplyr::filter(!is.na(fitted_prob)) %>% 
        dplyr::select(state, fitted_prob, length)
      
      scup_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
        dplyr::filter(state == s, species=="scup", draw==i) %>% 
        dplyr::filter(!is.na(fitted_prob)) %>% 
        dplyr::select(state,  fitted_prob, length)
      
      
      ### Begin trip simulation ### 
      
      # subset trips with zero catch, as no size draws are required
      sf_zero_catch <- dplyr::filter(catch_data, sf_cat == 0)
      bsb_zero_catch <- dplyr::filter(catch_data, bsb_cat == 0)
      scup_zero_catch <- dplyr::filter(catch_data, scup_cat == 0)
      
      # Check if there is zero catch for any species and if so, pipe code around keep/release determination
      sf_catch_check<-base::sum(catch_data$sf_cat)
      bsb_catch_check<-base::sum(catch_data$bsb_cat)
      scup_catch_check<-base::sum(catch_data$scup_cat)
      
      
      # Summer flounder trip simulation
      if (sf_catch_check!=0){
        
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
                                             replace = TRUE)) 
      
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
                      release = ifelse(keep==0,1,0)) %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  
      
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
      }
      
       if (sf_catch_check==0){
         sf_trip_data<-catch_data %>% 
           dplyr::select("date", "catch_draw","tripid","mode") %>% 
           dplyr::mutate(tot_keep_sf_new = 0, 
                         tot_rel_sf_new= 0, 
                         domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) 
          
         sf_trip_data<-data.table::as.data.table(sf_trip_data)
         data.table::setkey(sf_trip_data, "domain2")  
       }
         

      
      # BSB trip simulation
      if (bsb_catch_check!=0){
        
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
                                             replace = TRUE)) 
      
      
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
                      release = ifelse(keep==0,1,0)) %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) 
      
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
      
      bsb_trip_data<- bsb_trip_data %>% 
        dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
        dplyr::select(-c("date", "catch_draw","tripid","mode"))
      
      bsb_trip_data<-data.table::as.data.table(bsb_trip_data)
      data.table::setkey(bsb_trip_data, "domain2")
      }
      
      if (bsb_catch_check==0){
        bsb_trip_data<-catch_data %>% 
          dplyr::select("date", "catch_draw","tripid","mode") %>% 
          dplyr::mutate(tot_keep_bsb_new = 0, 
                        tot_rel_bsb_new= 0, 
                        domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
          dplyr::select(-c("date", "catch_draw","tripid","mode"))
        
        bsb_trip_data<-data.table::as.data.table(bsb_trip_data)
        data.table::setkey(bsb_trip_data, "domain2")  
      }
      
      
      # Scup trip simulation
      if (scup_catch_check!=0){
        
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
                                             replace = TRUE)) 
      
      
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
                      release = ifelse(keep==0,1,0)) %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  
      
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
      
      scup_trip_data<- scup_trip_data %>% 
        dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
        dplyr::select(-c("date", "catch_draw","tripid","mode"))
      
      scup_trip_data<-data.table::as.data.table(scup_trip_data)
      data.table::setkey(scup_trip_data, "domain2") 
    }
      
      if (scup_catch_check==0){
        scup_trip_data<-catch_data %>% 
          dplyr::select("date", "catch_draw","tripid","mode") %>% 
          dplyr::mutate(tot_keep_scup_new = 0, 
                        tot_rel_scup_new= 0, 
                        domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
          dplyr::select(-c("date", "catch_draw","tripid","mode"))
        
        scup_trip_data<-data.table::as.data.table(scup_trip_data)
        data.table::setkey(scup_trip_data, "domain2")  
      }
      
      
      # merge the hadd trip data with the rest of the trip data
      trip_data<- sf_trip_data[bsb_trip_data, on = "domain2"]
      trip_data<- trip_data[scup_trip_data, on = "domain2"]
      
      trip_data<- trip_data %>% 
        dplyr::mutate(tot_scup_catch = tot_keep_scup_new + tot_rel_scup_new, 
                      tot_bsb_catch = tot_keep_bsb_new + tot_rel_bsb_new, 
                      tot_sf_catch = tot_keep_sf_new + tot_rel_sf_new)

      parameters <- trip_data %>% 
        dplyr::select(date, mode, tripid) 
      
      parameters <- dplyr::distinct(parameters) 
      
      # utility params - no NJ interaction
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
      
      # utility params - with NJ interaction
      # parameters<-  parameters %>% 
      #   dplyr::mutate(beta_sqrt_sf_keep= rnorm(nrow(parameters), mean = 0.643, sd = 1.267), 
      #                 beta_NJ_sf_keep= rnorm(nrow(parameters), mean = 0.280, sd = 0), 
      #                 beta_sqrt_sf_release = rnorm(nrow(parameters), mean = 0.065 , sd =0.322 ) , 
      #                 beta_sqrt_bsb_keep = rnorm(nrow(parameters), mean =0.353 , sd = 0.126), 
      #                 beta_sqrt_bsb_release = rnorm(nrow(parameters), mean = 0.074 , sd =0 ), 
      #                 beta_sqrt_sf_bsb_keep = rnorm(nrow(parameters), mean=-0.057  , sd = 0.194 ), 
      #                 beta_sqrt_scup_catch = rnorm(nrow(parameters), mean = 0.018 , sd =0 ), 
      #                 beta_opt_out = rnorm(nrow(parameters), mean = -2.091, sd = 1.983), 
      #                 beta_opt_out_avidity = rnorm(nrow(parameters), mean = -0.010, sd = 0), 
      #                 beta_opt_out_age = rnorm(nrow(parameters), mean = 0.011, sd = 0), 
      #                 beta_cost = -0.012) 
      
      trip_data<- trip_data %>% 
        dplyr::left_join(parameters, by = c("date", "mode", "tripid")) %>% 
        dplyr::arrange(date, mode, tripid, tripid, catch_draw) %>% 
        dplyr::left_join(angler_dems, by = c("date", "mode", "tripid")) 
      
      #trip_data$NJ_dummy<-case_when(s=="NJ"~1, TRUE~0)
      
      # base_outcomes_s_i data sets will retain trip outcomes from the baseline scenario.
      # We will merge these data to the prediction year outcomes to calculate changes in effort and CS.
      baseline_outcomes<- trip_data %>%
        dplyr::rename(tot_keep_bsb_base = tot_keep_bsb_new,
                      tot_keep_scup_base = tot_keep_scup_new,
                      tot_keep_sf_base = tot_keep_sf_new,
                      tot_rel_bsb_base = tot_rel_bsb_new, 
                      tot_rel_scup_base = tot_rel_scup_new,
                      tot_rel_sf_base = tot_rel_sf_new)
      
#  compute utility
      trip_data[, `:=`(
        vA_trip = beta_sqrt_sf_keep*sqrt(tot_keep_sf_new) +
          #beta_NJ_sf_keep*NJ_dummy +
          beta_sqrt_sf_release*sqrt(tot_rel_sf_new) +
          beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_new) +
          beta_sqrt_bsb_release*sqrt(tot_rel_bsb_new) +
          beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf_new)*sqrt(tot_keep_bsb_new)) +
          beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
          beta_cost*cost,
        
        vA_optout = beta_opt_out +
          beta_opt_out_age * (age) +
          beta_opt_out_avidity * (total_trips_12) 
        
      )]
      
      # trip_data <-trip_data %>%
      #   dplyr::mutate(
      #     vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf_new) +
      #       #beta_NJ_sf_keep*NJ_dummy +
      #       beta_sqrt_sf_release*sqrt(tot_rel_sf_new) +
      #       beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_new) +
      #       beta_sqrt_bsb_release*sqrt(tot_rel_bsb_new) +
      #       beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf_new)*sqrt(tot_keep_bsb_new)) +
      #       beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
      #       beta_cost*cost)
      
      
    mean_trip_data <- data.table::as.data.table(trip_data)
      
    # remove big cols
    drop_cols <- c("beta_cost","beta_opt_out","beta_opt_out_age",     
                   "beta_opt_out_avidity","beta_sqrt_bsb_keep","beta_sqrt_bsb_release", "beta_sqrt_scup_catch", 
                   "beta_sqrt_sf_bsb_keep", "beta_sqrt_sf_keep","beta_sqrt_sf_release", 
                   "age", "cost", "domain2", "total_trips_12")
    
    drop_cols <- intersect(drop_cols, names(mean_trip_data))
    if (length(drop_cols)) mean_trip_data[, (drop_cols) := NULL]
    
    # average across catch_draw
    keep_vars <- setdiff(names(mean_trip_data), c("date","mode","tripid"))
    mean_trip_data <- mean_trip_data[, lapply(.SD, mean),
                                     by = .(date, mode,tripid),
                                     .SDcols = keep_vars]
    
    # calculate probabilities and log-sums
    mean_trip_data[, probA := exp(vA_trip) / (exp(vA_trip) + exp(vA_optout))]
    
    # Get rid of things we don't need.
    mean_trip_data <- mean_trip_data %>%
      dplyr::select(-"vA_trip" ,-"vA_optout", -"catch_draw") %>%
      dplyr::arrange(date, mode, tripid)
    
      
      # multiply the average trip probability (probA) by each catch variable to get probability-weighted catch
      list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",    "tot_keep_bsb_new",  "tot_rel_bsb_new",   "tot_keep_scup_new",
                      "tot_rel_scup_new",  "tot_scup_catch",    "tot_bsb_catch",     "tot_sf_catch")
      
      mean_trip_data <- mean_trip_data %>%
        as.data.table() %>%
        .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
        .[]
      
    
      mean_trip_data<-mean_trip_data %>% 
        left_join(dtripz, by = c("mode", "date")) %>% 
        dplyr::select(-bsb_bag, -bsb_min, -fluke_bag,-fluke_min, -scup_bag, -scup_min)
      
      mean_trip_data <-mean_trip_data %>% 
        group_by(mode, date) %>% 
        dplyr::mutate(mean_prob=mean(probA)) %>% 
        dplyr::ungroup() %>%       
        dplyr::mutate(sims=round(dtrip/mean_prob), 
                      expand=sims/n_draws, 
                      n_choice_occasions=1)
      
      # Expand outcomes
      list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",  "tot_sf_catch",
                      "tot_keep_bsb_new",   "tot_rel_bsb_new",  "tot_bsb_catch",
                      "tot_keep_scup_new",  "tot_rel_scup_new", "tot_scup_catch",
                      "n_choice_occasions", "probA" )
      
      all_vars <- c(list_names)
      
      mean_trip_data <- mean_trip_data %>%
        data.table::as.data.table() %>%
        .[,as.vector(all_vars) := lapply(.SD, function(x) x * expand), .SDcols = all_vars] %>%
        .[]
      
      for (j in names(mean_trip_data)) {
        attr(mean_trip_data[[j]], "label") <- NULL
      }
      

      aggregate_trip_data <- mean_trip_data %>%
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
      for (r in 1:nrow(summed_results)) {
        mode_val <- summed_results$mode[r]
        
        # Loop over summary columns
        for (var in names(summed_results)[names(summed_results) != "mode"]) {
          value <- summed_results[[var]][r]
          obj_name <- paste0(var, "_", mode_val, "_model")
          assign(obj_name, value)
        }
      }
      
      
      #Save MRIP estimates  by mode as objects 
      MRIP_comparison_draw <- MRIP_comparison %>% 
        dplyr::filter(draw==i & state==s)%>% 
        dplyr::filter(mode==md)
      
      mode_val <- MRIP_comparison_draw$mode
      
      # Loop over summary columns
      for (var in names(MRIP_comparison_draw)[names(MRIP_comparison_draw) != "mode"]) {
        value <- MRIP_comparison_draw[[var]]
        obj_name <- paste0(var, "_", mode_val, "_MRIP")
        assign(obj_name, value)
      }
      
      
      species <- c("sf", "bsb", "scup")
      dispositions <- c("keep", "rel", "catch")
      
      compare <- data.frame()
      
      for (sp in species) {
        for (disp in dispositions) {
          
          # Construct variable names
          base_name <- paste(sp, disp, md, sep = "_")
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
              mode = md,
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
      
      
      compare<-compare %>% 
        dplyr::mutate(rel_to_keep = if_else(diff < 0, 1, 0), 
                      keep_to_rel = if_else(diff > 0, 1, 0))
      
      compare_k<-compare %>% 
        dplyr::filter(disposition=="keep") %>% 
        dplyr::select(mode, species, MRIP, model, diff, pct_diff, keep_to_rel, rel_to_keep) %>% 
        dplyr::rename(MRIP_keep=MRIP, model_keep=model)
      
      compare_r<-compare %>% 
        dplyr::filter(disposition=="rel") %>% 
        dplyr::select(mode, species, MRIP, model) %>% 
        dplyr::rename(MRIP_rel=MRIP, model_rel=model) %>% 
        dplyr::left_join(compare_k, by=c("mode", "species"))
      
      calib_comparison[[k]]<-compare_r %>% 
        dplyr::mutate(p_rel_to_keep=abs(diff/model_rel), 
                      p_keep_to_rel=abs(diff/model_keep), 
                      draw=i, state=s)
      
      
      k <- k + 1
    }
  }
}

calib_comparison_combined <- do.call(rbind, calib_comparison) 

calib_comparison_combined<-calib_comparison_combined %>% 
  dplyr::select(state, mode, species, draw, everything()) 

write_feather(calib_comparison_combined, file.path(iterative_input_data_cd, "calibration_comparison.feather"))



