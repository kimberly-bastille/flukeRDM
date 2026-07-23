################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      calibrate_rec_catch1.R
# Purpose:     Calibration PASS 1 - the trip simulation WITH harvest/release
#              reallocation applied.
# Superseded by: Code/sim/calibrate_rec_catch1_final.R
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
################################################################################
################################################################################



#This is the calibration-year trip simulation WITH any adjustments for illegal harvest or voluntary release

# Create an empty list to collect results
calib_comparison <- list()

# make some placeholders
n_sub_scup_kept<-0 # number of sublegal sized fish kept
prop_sub_scup_kept<-0 # proportion of the original num. of released fish that were illegally kept (n_sub_scup_kept/total_original_released)
n_legal_scup_rel<-0 # number of legal sized fish released 
prop_legal_scup_rel<-0 # proportion of the original num. of kept fish that were voluntarily kept (n_legal_scup_rel/total_original_kept

n_sub_sf_kept<-0 
prop_sub_sf_kept<-0 
n_legal_sf_rel<-0 
prop_legal_sf_rel<-0 

n_sub_bsb_kept<-0 
prop_sub_bsb_kept<-0
n_legal_bsb_rel<-0
prop_legal_bsb_rel<-0


# import necessary data
dtripz<-feather::read_feather(file.path(iterative_input_data_cd, paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".feather"))) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == i) %>%
  dplyr::select(mode, dtrip, date, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min) %>% 
  dplyr::filter(mode==md)

catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".feather"))) %>% 
  dplyr::left_join(dtripz, by=c("mode", "date")) %>% 
  dplyr::filter(mode==md)

angler_dems<-catch_data %>% 
  dplyr::select(date, mode, tripid, total_trips_12, age, cost) %>% 
  dplyr::filter(mode==md)

catch_data<-catch_data %>% 
  dplyr::select(-cost, -total_trips_12, -age)

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


#Create as an object the minimum size at which fish may be illegally harvested.
  #1) This floor_subl_harvest size will be 3 inches below the minimum size, by mode. 
  #1a) If the minimum size changes across the season, floor_subl_harvest=min(min_size). 
  #2) If the fishery is closed the entire season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
  #1) and #1a) below:

floor_subl_sf_harv<-min(dtripz$fluke_min)-3*2.54
floor_subl_bsb_harv<-min(dtripz$bsb_min)-3*2.54
floor_subl_scup_harv<-min(dtripz$scup_min)-3*2.54


# begin trip simulation

# subset trips with zero catch, as no size draws are required
sf_zero_catch <- dplyr::filter(catch_data, sf_cat == 0)
bsb_zero_catch <- dplyr::filter(catch_data, bsb_cat == 0)
scup_zero_catch <- dplyr::filter(catch_data, scup_cat == 0)

# Check if there is zero catch for any species and if so, pipe code around keep/release determination
sf_catch_check<-base::sum(catch_data$sf_cat)
bsb_catch_check<-base::sum(catch_data$bsb_cat)
scup_catch_check<-base::sum(catch_data$scup_cat)

calib_comparison<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) %>%
  dplyr::filter(state==s & draw==i & mode==md)


# Summer flounder trip simulation
#keep trips with positive sf catch
if (sf_catch_check!=0){
  
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

# code for trip level harvest re-allocation
catch_size_data<- catch_size_data %>%
  dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
  dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_sf_harv~1,TRUE~0))

sum_sf_rel<-sum(catch_size_data$release)
sum_sf_keep<-sum(catch_size_data$keep)

# reallocate a portion of all releases as kept if needed 
if (rel_to_keep_sf==1 & sum_sf_rel>0){
  
  catch_size_data_re_allocate<- catch_size_data %>%
    dplyr::filter(subl_harv_indicator==1) 
  
  original_rel_sf<-sum(catch_size_data_re_allocate$release)
  
  catch_size_data_re_allocate_base<- catch_size_data %>%
    dplyr::filter(subl_harv_indicator==0) 
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
    dplyr::arrange(uniform) %>%
    dplyr::ungroup()
  
  n_row_re_allocate<-nrow(catch_size_data_re_allocate)
  
  n_sub_sf_kept=round(p_rel_to_keep_sf*n_row_re_allocate)  
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
    dplyr::mutate(keep_new=case_when(fishid2<=n_sub_sf_kept~1, TRUE~ 0))
  
  n_rel_sf_kept<-sum(catch_size_data_re_allocate$keep_new)
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
    dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
    dplyr::rename(keep=keep_new, release=rel_new)
  
  catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
    dplyr::select(-subl_harv_indicator)
  
  n_kept_sf_rel<-0
  prop_sub_sf_kept<-n_rel_sf_kept/original_rel_sf
  n_rel_sf_kept
  
  rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
  
}

# Now reallocate a portion of all keeps as releases if needed 
if (keep_to_rel_sf==1 & sum_sf_keep>0){
  
  # If all kept must be release, p_keep_to_rel_sf==1
  if (all_keep_to_rel_sf==1){
    
    n_kept_sf_rel<-sum(catch_size_data$keep) 
    prop_legal_sf_rel<-n_kept_sf_rel/sum(catch_size_data$keep)
    
    catch_size_data<-catch_size_data %>% 
      dplyr::mutate(rel_new = keep+release, 
                    keep_new = 0) %>% 
      dplyr::select(-keep, -release) %>% 
      dplyr::rename(release=rel_new,  keep=keep_new) 
    
    n_rel_sf_kept<-0
    
  }
  
  #If not all kept must be release, p_keep_to_rel_sf<1
  if (all_keep_to_rel_sf!=1){
    
    catch_size_data_re_allocate<- catch_size_data %>%
      dplyr::filter(keep==1)
    
    catch_size_data_re_allocate_base<- catch_size_data %>%
      dplyr::filter(keep==0) 
    
    n_row_re_allocate<-nrow(catch_size_data_re_allocate)
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(uniform=runif(n_row_re_allocate)) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(fishid2=1:n_row_re_allocate)
    
    n_kept_sf_rel=round(p_keep_to_rel_sf*n_row_re_allocate)
    
    prop_legal_sf_rel<-n_kept_sf_rel/sum(catch_size_data$keep)
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_kept_sf_rel~1, TRUE~ 0))
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>% 
      dplyr::select(-keep, -release, -fishid2, -uniform) %>% 
      dplyr::rename(keep=keep_new, release=rel_new)
    
    sum(catch_size_data$release)
    sum(catch_size_data$keep)
    
    catch_size_data<-rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base )
    
    sum(catch_size_data$release)
    sum(catch_size_data$keep)
    
    rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
    
    n_rel_sf_kept<-0
    
  }
}


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

rm(catch_size_data, trip_data)


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

# code for trip level harvest re-allocation
catch_size_data<- catch_size_data %>%
  dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
  dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_bsb_harv~1,TRUE~0))

sum_bsb_rel<-sum(catch_size_data$release)
sum_bsb_keep<-sum(catch_size_data$keep)

# reallocate a portion of all releases as kept if needed 
if (rel_to_keep_bsb==1 & sum_bsb_rel>0){
  
  catch_size_data_re_allocate<- catch_size_data %>%
    dplyr::filter(subl_harv_indicator==1) 
  
  original_rel_bsb<-sum(catch_size_data_re_allocate$release)
  
  catch_size_data_re_allocate_base<- catch_size_data %>%
    dplyr::filter(subl_harv_indicator==0) 
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
    dplyr::arrange(uniform) %>%
    dplyr::ungroup()
  
  n_row_re_allocate<-nrow(catch_size_data_re_allocate)
  
  n_sub_bsb_kept=round(p_rel_to_keep_bsb*n_row_re_allocate)  
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
    dplyr::mutate(keep_new=case_when(fishid2<=n_sub_bsb_kept~1, TRUE~ 0))
  
  n_rel_bsb_kept<-sum(catch_size_data_re_allocate$keep_new)
  
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
    dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
    dplyr::rename(keep=keep_new, release=rel_new)
  
  
  catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
    dplyr::select(-subl_harv_indicator)
  
  n_kept_bsb_rel<-0
  prop_sub_bsb_kept<-n_rel_bsb_kept/original_rel_bsb
  n_rel_bsb_kept
  
  rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
  
}

##Now reallocate a portion of all keeps as releases if needed 
if (keep_to_rel_bsb==1 & sum_bsb_keep>0){
  
  #If all kept must be release, p_keep_to_rel_bsb==1
  if (all_keep_to_rel_bsb==1){
    
    n_kept_bsb_rel<-sum(catch_size_data$keep) 
    prop_legal_bsb_rel<-n_kept_bsb_rel/sum(catch_size_data$keep)
    
    catch_size_data<-catch_size_data %>% 
      dplyr::mutate(rel_new = keep+release, 
                    keep_new = 0) %>% 
      dplyr::select(-keep, -release) %>% 
      dplyr::rename(release=rel_new,  keep=keep_new) 
    
    n_rel_bsb_kept<-0
    
  }
  
  #If not all kept must be release, p_keep_to_rel_bsb<1
  if (all_keep_to_rel_bsb!=1){
    
    catch_size_data_re_allocate<- catch_size_data %>%
      dplyr::filter(keep==1)
    
    catch_size_data_re_allocate_base<- catch_size_data %>%
      dplyr::filter(keep==0) 
    
    n_row_re_allocate<-nrow(catch_size_data_re_allocate)
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(uniform=runif(n_row_re_allocate)) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(fishid2=1:n_row_re_allocate)
    
    n_kept_bsb_rel=round(p_keep_to_rel_bsb*n_row_re_allocate)
    prop_legal_bsb_rel<-n_kept_bsb_rel/sum(catch_size_data$keep)
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_kept_bsb_rel~1, TRUE~ 0))
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>% 
      dplyr::select(-keep, -release, -fishid2, -uniform) %>% 
      dplyr::rename(keep=keep_new, release=rel_new)
    
    sum(catch_size_data$release)
    sum(catch_size_data$keep)
    
    catch_size_data<-rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base )
    
    sum(catch_size_data$release)
    sum(catch_size_data$keep)
    
    rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
    
    n_rel_bsb_kept<-0
    
  }
}

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

rm(catch_size_data,trip_data)


# Scup  trip simulation
#keep trips with positive scup catch
if (scup_catch_check!=0){
  
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

# code for trip level harvest re-allocation
catch_size_data<- catch_size_data %>%
  dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
  dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_scup_harv~1,TRUE~0))

sum_scup_rel<-sum(catch_size_data$release)
sum_scup_keep<-sum(catch_size_data$keep)

# reallocate a portion of all releases as kept if needed 
if (rel_to_keep_scup==1 & sum_scup_rel>0){
  
  catch_size_data_re_allocate<- catch_size_data %>%
    dplyr::filter(subl_harv_indicator==1) 
  
  original_rel_scup<-sum(catch_size_data_re_allocate$release)
  
  catch_size_data_re_allocate_base<- catch_size_data %>%
    dplyr::filter(subl_harv_indicator==0) 
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
    dplyr::arrange(uniform) %>%
    dplyr::ungroup()
  
  n_row_re_allocate<-nrow(catch_size_data_re_allocate)
  n_sub_scup_kept=round(p_rel_to_keep_scup*n_row_re_allocate)  
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
    dplyr::mutate(keep_new=case_when(fishid2<=n_sub_scup_kept~1, TRUE~ 0))
  
  new_kept_scup<-sum(catch_size_data_re_allocate$keep_new)
  
  catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
    dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
    dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
    dplyr::rename(keep=keep_new, release=rel_new)
  
  
  catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
    dplyr::select(-subl_harv_indicator)
  
  n_legal_scup_rel<-0
  prop_sub_scup_kept<-new_kept_scup/original_rel_scup
  
  rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
  
  
}

##Now reallocate a portion of all keeps as releases if needed 
if (keep_to_rel_scup==1 & sum_scup_keep>0){
  
  #If all kept must be release, p_keep_to_rel_scup==1
  if (all_keep_to_rel_scup==1){
    
    n_kept_scup_rel<-sum(catch_size_data$keep) 
    prop_legal_scup_rel<-n_kept_scup_rel/sum(catch_size_data$keep)
    
    catch_size_data<-catch_size_data %>% 
      dplyr::mutate(rel_new = keep+release, 
                    keep_new = 0) %>% 
      dplyr::select(-keep, -release) %>% 
      dplyr::rename(release=rel_new,  keep=keep_new) 
    
    n_rel_scup_kept<-0
    
  }
  
  #If not all kept must be release, p_keep_to_rel_scup<1
  if (all_keep_to_rel_scup!=1){
    
    catch_size_data_re_allocate<- catch_size_data %>%
      dplyr::filter(keep==1)
    
    catch_size_data_re_allocate_base<- catch_size_data %>%
      dplyr::filter(keep==0) 
    
    n_row_re_allocate<-nrow(catch_size_data_re_allocate)
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(uniform=runif(n_row_re_allocate)) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(fishid2=1:n_row_re_allocate)
    
    n_kept_scup_rel=round(p_keep_to_rel_scup*n_row_re_allocate)
    prop_legal_scup_rel<-n_kept_scup_rel/sum(catch_size_data$keep)
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_kept_scup_rel~1, TRUE~ 0))
    
    catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
      dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>% 
      dplyr::select(-keep, -release, -fishid2, -uniform) %>% 
      dplyr::rename(keep=keep_new, release=rel_new)
    
    sum(catch_size_data$release)
    sum(catch_size_data$keep)
    
    catch_size_data<-rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base )
    
    sum(catch_size_data$release)
    sum(catch_size_data$keep)
    
    rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
    
    n_rel_scup_kept<-0
    
  }
}


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


rm(catch_size_data,trip_data)

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

trip_data$NJ_dummy<-case_when(s=="NJ"~1, TRUE~0)

# base_outcomes_s_md_i data sets will retain trip outcomes from the baseline scenario.
# We will merge these data to the prediction year outcomes to calculate changes in effort and CS.
baseline_outcomes<- trip_data %>%
  dplyr::rename(tot_keep_bsb_base = tot_keep_bsb_new,
                tot_keep_scup_base = tot_keep_scup_new,
                tot_keep_sf_base = tot_keep_sf_new,
                tot_rel_bsb_base = tot_rel_bsb_new, 
                tot_rel_scup_base = tot_rel_scup_new,
                tot_rel_sf_base = tot_rel_sf_new, 
                tot_cat_bsb_base = tot_bsb_catch, 
                tot_cat_scup_base = tot_scup_catch,
                tot_cat_sf_base = tot_sf_catch)

write_feather(baseline_outcomes, file.path(iterative_input_data_cd, paste0("base_outcomes_", s,"_", md, "_", i,".feather")))

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


#write_feather(aggregate_trip_data, file = paste0(iterative_input_data_cd, "calibration_output_", s,"_", md,"_", i, ".feather")) 


list_names = c("bsb_catch","bsb_keep","bsb_rel", 
               "scup_catch", "scup_keep","scup_rel", 
               "sf_catch", "sf_keep","sf_rel",
               "estimated_trips","n_choice_occasions")

summed_results <- aggregate_trip_data %>%
  data.table::as.data.table() %>%
  .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]


aggregate_trip_data<-aggregate_trip_data %>% 
  dplyr::select(date, mode, n_choice_occasions, estimated_trips)

write_feather(aggregate_trip_data, file.path(iterative_input_data_cd, paste0("n_choice_occasions_", s,"_", md, "_", i, ".feather")))

########
#Compare calibration output to MRIP by state-mode 

#Save simulation results by mode as objects 
# Loop over rows (modes)
for (r in 1:nrow(summed_results)) {
  mode_val <- summed_results$mode[r]
  
  # Loop over summary columns
  for (var in names(summed_results)[names(summed_results) != "mode"]) {
    value <- summed_results[[var]][r]
    obj_name <- paste0(var, "_", "model")
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
  obj_name <- paste0(var, "_", "MRIP")
  assign(obj_name, value)
}


species <- c("sf", "bsb", "scup")
dispositions <- c("keep", "rel", "catch")

compare1 <- data.frame()

# Initialize a vector to track intermediate variable names
intermediate_vars <- c()

for (sp in species) {
  for (disp in dispositions) {
    
    # Construct variable names
    base_name <- paste(sp, disp, sep = "_")
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
      
      # Create variable names and assign them
      diff_name <- paste0(base_name, "_diff")
      pct_diff_name <- paste0(base_name, "_pct_diff")
      abs_diff_name <- paste0(base_name, "_abs_diff")
      abs_pct_diff_name <- paste0(base_name, "_abs_pct_diff")
      
      assign(diff_name, diff_val)
      assign(pct_diff_name, pct_diff_val)
      assign(abs_diff_name, abs_diff_val)
      assign(abs_pct_diff_name, abs_pct_diff_val)
      
      # Store names to delete later
      intermediate_vars <- c(intermediate_vars,
                             diff_name, pct_diff_name,
                             abs_diff_name, abs_pct_diff_name)
      
      compare1 <- rbind(compare1, data.frame(
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

# Remove all intermediate variables created with assign()
rm(list = intermediate_vars)
rm(sf_keep_model, sf_keep_MRIP, sf_catch_model, sf_catch_MRIP, sf_rel_model, sf_rel_MRIP,
   bsb_keep_model, bsb_keep_MRIP, bsb_catch_model, bsb_catch_MRIP, bsb_rel_model, bsb_rel_MRIP,
   scup_keep_model, scup_keep_MRIP, scup_catch_model, scup_catch_MRIP, scup_rel_model, scup_rel_MRIP) 

compare1_k<-compare1 %>%
  dplyr::filter(disposition=="keep") %>%
  dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>% 
  dplyr::rename(MRIP_keep=MRIP, model_keep=model, diff_keep=diff, pct_diff_keep=pct_diff)

compare1_r<-compare1 %>%
  dplyr::filter(disposition=="rel") %>%
  dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>%
  dplyr::rename(MRIP_rel=MRIP, model_rel=model, diff_rel=diff, pct_diff_rel=pct_diff) %>%
  dplyr::left_join(compare1_k, by=c("mode", "species"))

compare1_c<-compare1 %>%
  dplyr::filter(disposition=="catch") %>%
  dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>% 
  dplyr::rename(MRIP_catch=MRIP, model_catch=model, diff_catch=diff, pct_diff_catch=pct_diff) %>%
  dplyr::left_join(compare1_r, by=c("mode", "species"))

calib_comparison1<-compare1_c %>%
  dplyr::mutate(draw=i, state=s) %>% 
  dplyr::mutate(rel_to_keep_new = if_else(diff_keep < 0, 1, 0), 
                keep_to_rel_new = if_else(diff_keep > 0, 1, 0)) %>% 
  dplyr::mutate(p_rel_to_keep_new=abs(diff_keep/model_rel), 
                p_keep_to_rel_new=abs(diff_keep/model_keep)) 


# Vector of object names you want to remove
objects_to_remove <- c("angler_dems", "baseline_outcomes", "baseline_output0", 
                       "bsb_catch_data", "bsb_trip_data", "bsb_zero_catch", 
                       "sf_catch_data", "sf_trip_data", "sf_zero_catch", 
                       "scup_catch_data", "scup_trip_data", "scup_zero_catch", 
                       "catch_data", "trip_data", "parameters", "mean_trip_data")

# Only remove those that exist
rm(list = objects_to_remove[objects_to_remove %in% ls()], envir = .GlobalEnv)


