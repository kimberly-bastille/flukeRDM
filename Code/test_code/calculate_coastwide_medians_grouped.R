################################################################################
################################################################################
# Script:       calculate_coastwide_medians_grouped.R
# Purpose:      Regional-grouping variant of calculate_coastwide_medians.R.
#               Instead of treating all nine states as independently varying,
#               it reads a maintained policy template, groups states into
#               three regions (North, South and New Jersey on its own), and
#               takes the cross-product over REGIONS. States within a region
#               move together under a single regional policy ID.
# Inputs:       Management Measures Template Updated.csv,
#               output/*.csv (per-state, per-policy model output)
# Outputs:      coastwide_results_compared2.csv
# Dependencies: Packages dplyr, readr, stringr, tidyr, here. Requires the
#               per-state projection runs to already exist in output/.
# Pipeline:     Development/QA scratch, downstream of the projection stage.
#               Not called by any wrapper.
#
# Why this variant exists: management measures are negotiated regionally, so
# the independent per-state cross-product in the ungrouped version generates
# many combinations that could never be adopted. Grouping cuts the
# combinatorics from 3^9 to (north x south x nj) and restricts the results to
# packages that are actually on the table.
#
# New Jersey is its own region rather than part of North or South. That is a
# management convention, not an arbitrary split.
#
# The policy definitions live in a CSV here rather than as literals in the
# script, which is the other substantive improvement over the ungrouped
# version - a new analysis round needs a new CSV, not a code edit.
################################################################################
################################################################################

## Generate coastwide values grouped

policy_lookup <- read_csv(here::here("Data/Data_final/Management Measures Template Updated.csv"))

# file path
filepath <- here::here("output/")
options(readr.show_col_types = FALSE)
options(tibble.name_repair = "minimal")
# select data for each state

policy_nested <- policy_lookup %>%
  group_by(Region, ID_Number) %>%
  summarise(
    state_map = list(setNames(`Policy Name`, State)),
    .groups = "drop"
  )

north_ids <- policy_nested %>% filter(Region == "North")
south_ids <- policy_nested %>% filter(Region == "South")
nj_ids    <- policy_nested %>% filter(Region == "NJ")

run_grid <- expand.grid(
  north = north_ids$ID_Number,
  south = south_ids$ID_Number,
  nj    = nj_ids$ID_Number,
  stringsAsFactors = FALSE
)

filenames <- list.files(path = here::here("output/"), pattern = "\\.csv$", full.names = TRUE)
file_states <- str_extract(filenames, "(?<=output_)[A-Z]{2}")


## Get status quo
SQ_filenames <- list.files(path = here::here("output/"), pattern = "SQ.*\\.csv$", full.names = TRUE)

SQ_data_list <- map_dfr(SQ_filenames, read_csv, show_col_types = FALSE)  

SQ_data_adj <- SQ_data_list %>% 
  select(-any_of(c("...8", "filename"))) %>% 
  filter( case_when(
    state %in% c("MA", "CT", "NY", "NJ", "DE", "VA", "NC") & draw %in% c(20, 21, 78) ~ FALSE,
    state == "MD" & model != "Lou_SQ" &draw %in% c(20, 21) ~ FALSE,
    state == "MD" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
    state == "RI" & model != "Lou_SQ" & draw %in% c(76) ~ FALSE,
    state == "RI" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
    TRUE ~ TRUE))

SQ_draw_map <- SQ_data_adj %>%
  distinct(state, model, draw) %>%
  group_by(state, model) %>%
  arrange(draw, .by_group = TRUE) %>%
  mutate(new_draw = row_number()) %>%
  ungroup()

SQ <- SQ_data_adj %>%
  left_join(SQ_draw_map, by = c("state", "model", "draw")) %>%
  mutate(draw = new_draw, 
         SQ_value = value, 
         SQ_model = model) %>%
  select(-c(new_draw, value, model)) 

#state = c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
#all_results <- vector("list", nrow(policy_grid))
all_results <- vector("list", nrow(run_grid))

for(i in seq_len(nrow(run_grid))) {
  
  north_id <- run_grid$north[i]
  south_id <- run_grid$south[i]
  nj_id    <- run_grid$nj[i]
  
  # Extract state policies
  north_map <- north_ids %>%
    filter(ID_Number == north_id) %>%
    pull(state_map) %>%
    .[[1]]
  
  south_map <- south_ids %>%
    filter(ID_Number == south_id) %>%
    pull(state_map) %>%
    .[[1]]
  
  nj_map <- nj_ids %>%
    filter(ID_Number == nj_id) %>%
    pull(state_map) %>%
    .[[1]]
  
  # Merge them into one state → policy map
  policy_map_run <- c(north_map, south_map, nj_map)
  
  files_keep <- filenames[
    mapply(function(file, state) {
      
      if (is.na(state)) return(FALSE)
      
      if (!state %in% names(policy_map_run)) return(FALSE)
      
      policy <- policy_map_run[[state]]
      if (is.null(policy)) return(FALSE)
      
      stringr::str_detect(file, paste0("_", policy, "_"))
      
    }, filenames, file_states)
  ]
  
  print(i)
  print(files_keep)
  data_list <- map_dfr(files_keep, read_csv)
  
  data_adj <- data_list %>% 
    select(-any_of(c("...8", "filename"))) %>% 
    filter( case_when(
      state %in% c("MA", "CT", "NY", "NJ", "DE", "VA", "NC") & draw %in% c(20, 21, 78) ~ FALSE,
      state == "MD" & model != "Lou_SQ" &draw %in% c(20, 21) ~ FALSE,
      state == "MD" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
      state == "RI" & model != "Lou_SQ" & draw %in% c(76) ~ FALSE,
      state == "RI" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
      TRUE ~ TRUE))
  
  draw_map <- data_adj %>%
    distinct(state, model, draw) %>%
    group_by(state, model) %>%
    arrange(draw, .by_group = TRUE) %>%
    mutate(new_draw = row_number()) %>%
    ungroup()
  
  df <- data_adj %>%
    left_join(draw_map, by = c("state", "model", "draw")) %>%
    mutate(draw = new_draw) %>%
    select(-new_draw)
  
  data<- df %>% left_join(SQ, by = c("metric", "species", "mode", "state", "draw"))
  
  #Calculate coastwide median 
  
  harv <- data %>% #all_data %>%
    dplyr::filter(metric == "keep_weight" & mode == "all modes") %>%
    dplyr::group_by(metric, species, mode, draw) %>% 
    dplyr::summarise(value = sum(value), 
                     SQ_value = sum(SQ_value)) %>% 
    dplyr::mutate(pct_diff = (value - SQ_value) / (SQ_value)  * 100) %>%
    dplyr::group_by(species, metric) %>%
    dplyr::summarise(median_pct_diff = round(median(pct_diff),2), 
                     median_value = median(value)) %>%
    tidyr::pivot_wider(names_from = species, values_from = c(median_pct_diff, median_value)) %>% 
    dplyr::rename_with(
      ~ gsub("median_pct_diff_(.*)", "\\1 % change", .x),
      starts_with("median_pct_diff")
    ) %>%
    dplyr::rename_with(
      ~ gsub("median_value_(.*)", "\\1 harvest weight", .x),
      starts_with("median_value")
    )
  
  north_df <- as_tibble_row(north_map)
  south_df <- as_tibble_row(south_map)
  nj_df <- as_tibble_row(nj_map)
  results <- cbind(
    North_ID = north_id,
    South_ID = south_id,
    NJ_ID = nj_id,
    north_df, 
    south_df, 
    nj_df,
    harv
  )
  all_results <- dplyr::bind_rows(all_results, results)
  
  rm(harv, data_adj, draw_map, df, data)
}

write.csv(
  all_results,
  here::here("coastwide_results_compared2.csv"),
  row.names = FALSE)

