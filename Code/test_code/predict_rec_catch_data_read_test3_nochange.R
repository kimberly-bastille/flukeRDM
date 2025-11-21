iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

#check

############# To Run Individual
# Variables to change 
#dr<-1
#st="NJ"
ndraws=50 #number of choice occasions to simulate per strata

library(magrittr)
############# To Run in Loop 

#for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
#  for (dr in 1:2){

# import necessary data

# For kim: We need to retain the SQ regulation variables. 
#          As of now, the SQ regulations variables are the ones without subscripts. 
#          I will copy these variables with the subscript _SQ to make this explicit.
#          The alternative regulations that will be adjusted by the users will be 
#          have subscripts _y2 (note this is slightly different from cod and haddock 2024)


directed_trips<-feather::read_feather(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_new/directed_trips_calibration_new_", st, ".feather"))) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == dr) %>%
  dplyr::select(mode, date, 
                bsb_bag, bsb_min, bsb_bag_y2, bsb_min_y2, 
                fluke_bag, fluke_min, fluke_bag_y2,fluke_min_y2,
                scup_bag, scup_min, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(fluke_min_SQ=fluke_min, fluke_bag_SQ=fluke_bag, 
                bsb_min_SQ=bsb_min, bsb_bag_SQ=bsb_bag, 
                scup_min_SQ=scup_min, scup_bag_SQ=scup_bag)



l_w_conversion <- readr::read_csv(file.path("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data", "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state==st)


sf_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="sf", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, mode)

bsb_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="bsb" , draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, mode)

scup_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="scup", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, mode)

calendar_adjustments <- readr::read_csv(
  file.path(iterative_input_data_cd, paste0("miscellanous/proj_year_calendar_adjustments_new_", st, ".csv")), show_col_types = FALSE) %>%
  dplyr::select(-state.y) %>% 
  dplyr::rename(state=state.x) %>% 
  dplyr::filter(state == st, draw==dr) %>% 
  dplyr::select(-dtrip, -dtrip_y2, -state, -draw) %>% 
  dplyr::mutate(expansion_factor=1)

# base-year trip outcomes
base_outcomes0 <- list()
n_choice_occasions0 <- list()
catch0<-list()
mode_draw <- c("sh", "pr", "fh")
for (md in mode_draw) {
  
  # pull trip outcomes from the calibration year
  base_outcomes0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("base_outcomes_new/base_outcomes_new_", st, "_", md, "_", dr, ".feather"))) %>% 
    data.table::as.data.table()
  
  base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
    dplyr::select(-domain2) %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  # pull in data on the number of choice occasions per mode-day
  n_choice_occasions0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("n_choice_occasion_new/n_choice_occasions_new_", st,"_", md, "_", dr, ".feather")))  
  n_choice_occasions0[[md]]<-n_choice_occasions0[[md]] %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  catch0[[md]] <- feather::read_feather(file.path(iterative_input_data_cd, paste0("base_outcomes_new/base_outcomes_new_", st, "_", md, "_", dr, ".feather"))) %>% 
  dplyr::left_join(directed_trips, by=c("mode", "date")) %>% 
    dplyr::mutate(draw=dr)
  
  
  
}

base_outcomes <- bind_rows(base_outcomes0)
n_choice_occasions <- bind_rows(n_choice_occasions0) %>% 
  dplyr::arrange(date_parsed, mode)
rm(base_outcomes0, n_choice_occasions0)

catch_data <- bind_rows(catch0) %>% 
  dplyr::rename(sf_cat=tot_cat_sf_base, 
                bsb_cat=tot_cat_bsb_base, 
                scup_cat=tot_cat_scup_base ) %>% 
  dplyr::select( bsb_bag ,       bsb_bag_SQ  ,   bsb_bag_y2  ,   bsb_cat  ,      bsb_min   ,     bsb_min_SQ ,    bsb_min_y2,
                  catch_draw  ,   date    ,       draw   ,        fluke_bag  ,    fluke_bag_SQ  , fluke_bag_y2  , fluke_min ,
                  fluke_min_SQ   ,fluke_min_y2 ,  mode     ,      scup_bag,       scup_bag_SQ  ,  scup_bag_y2  ,  scup_cat   ,  
                 scup_min  ,     scup_min_SQ ,   scup_min_y2 ,   sf_cat   ,      tripid    )


base_outcomes<-base_outcomes %>% 
  dplyr::arrange(date_parsed, mode, tripid, catch_draw)




# Pull in calibration comparison information about trip-level harvest/discard re-allocations 
calib_comparison<-readRDS(file.path(iterative_input_data_cd, "miscellanous/calibrated_model_stats_new.rds")) %>% 
  dplyr::filter(state==st & draw==dr ) 

calib_comparison<-calib_comparison %>% 
  dplyr::rename(n_legal_rel_bsb=n_legal_bsb_rel, 
                n_legal_rel_scup=n_legal_scup_rel, 
                n_legal_rel_sf=n_legal_sf_rel, 
                n_sub_kept_bsb=n_sub_bsb_kept,
                n_sub_kept_sf=n_sub_sf_kept,
                n_sub_kept_scup=n_sub_scup_kept,
                prop_legal_rel_bsb=prop_legal_bsb_rel,
                prop_legal_rel_sf=prop_legal_sf_rel,
                prop_legal_rel_scup=prop_legal_scup_rel,
                prop_sub_kept_bsb=prop_sub_bsb_kept,
                prop_sub_kept_sf=prop_sub_sf_kept,
                prop_sub_kept_scup=prop_sub_scup_kept,
                convergence_sf=sf_convergence,
                convergence_bsb=bsb_convergence,
                convergence_scup=scup_convergence) 

##########
# List of species suffixes
species_suffixes <- c("sf", "bsb", "scup")

# Get all variable names
all_vars <- names(calib_comparison)

# Identify columns that are species-specific (contain _sf, _bsb, or _scup)
species_specific_vars <- all_vars[
  str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
]

id_vars <- setdiff(all_vars, species_specific_vars)

calib_comparison<-calib_comparison %>% 
  dplyr::select(mode, all_of(species_specific_vars))

# Extract base variable names (without _sf, _bsb, _scup)
base_names <- unique(str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))

# Pivot the data longer on the species-specific columns
calib_comparison <- calib_comparison %>%
  pivot_longer(
    cols = all_of(species_specific_vars),
    names_to = c(".value", "species"),
    names_pattern = "(.*)_(sf|bsb|scup)"
  ) %>% 
  dplyr::distinct()


#  }
#}

