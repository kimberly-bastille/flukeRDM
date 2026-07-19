################################################################################
################################################################################
# Script:       calculate_coastwide_medians.R
# Purpose:      Evaluates coastwide outcomes for every COMBINATION of
#               candidate state policies. Each state has a short list of
#               policy options under consideration; this script takes the full
#               cross-product of those lists, and for each combination sums
#               the nine states' results into a coastwide total, expresses it
#               as a percent difference from status quo, and records the
#               median percent difference across draws by species and metric.
#               The output is the table used to compare candidate coastwide
#               packages against one another.
# Inputs:       output/*.csv (per-state, per-policy model output)
# Outputs:      all_coastwide_results.csv
# Dependencies: Packages dplyr, readr, stringr, tidyr, here. Requires the
#               per-state projection runs for every named policy to already
#               exist in output/.
# Pipeline:     Development/QA scratch, downstream of the projection stage.
#               Not called by any wrapper.
#
# The policy lists at the top are HARDCODED to one round of analysis - the
# names ("JB5", "MYL1", "NC_BW4", ...) are analyst-assigned scenario labels
# that must match the Run_Name embedded in the output filenames. With three
# options per state across nine states, the cross-product is 3^9 = 19,683
# combinations, which is why this script is slow.
#
# The median, not the mean, is taken across draws: the percent-difference
# distribution has a long tail wherever a status-quo denominator is small, and
# the median is insensitive to it.
#
# See also calculate_coastwide_medians_grouped.R, which does the same job but
# reads the policy definitions from a maintained CSV and groups states into
# North / South / NJ regions rather than combining them independently.
################################################################################
################################################################################

## Generate coastwide values

MA_policy = c("JB5", "MYL1", "MA_EKsplitseason6")
RI_policy = c("MYL2", "JB5", "CT7_Fluke18inch")
CT_policy = c("KB6", "KB7", "KB8")
NY_policy = c("JB5", "MYL1", "RS1")
NJ_policy = c("PC_18", "PC_11", "PC_08")
DE_policy = c("JB5", "MYL2", "NC_BW4")
MD_policy = c("JB5", "MYL2", "NC_BW4")
VA_policy = c("Analysis1", "NC_BW4", "NC_SD2")
NC_policy = c("Analysis1", "NC_BW4", "NC_SD2")

# file path
filepath <- here::here("output/")
options(readr.show_col_types = FALSE)
options(tibble.name_repair = "minimal")
# select data for each state
  policy_map <- list(
    MA = MA_policy,
    RI = RI_policy,
    CT = CT_policy,
    NY = NY_policy,
    NJ = NJ_policy,
    DE = DE_policy,
    MD = MD_policy,
    VA = VA_policy,
    NC = NC_policy)

  policy_grid <- expand.grid(
    MA = policy_map$MA,
    RI = policy_map$RI,
    CT = policy_map$CT,
    NY = policy_map$NY,
    NJ = policy_map$NJ,
    DE = policy_map$DE,
    MD = policy_map$MD,
    VA = policy_map$VA,
    NC = policy_map$NC,
    stringsAsFactors = FALSE
  )
# Name saved outputs
#coastwide_data = "percchange1"

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
  
all_results <- vector("list", nrow(policy_grid))

for (i in seq_len(nrow(policy_grid))) {
  
  print(i)
  policy_run <- policy_grid[i, ]
  policy_map_run <- as.list(policy_run)

## IGNORE EVERYTHING BELOW THIS LINE
## -------------------------------------------------------------------------------------------

# organize for missing draws

files_keep <- filenames[
  mapply(function(file, state) {
    
    policy <- policy_map_run[[state]]
    if (is.null(policy)) return(FALSE)
    
    stringr::str_detect(file, paste0("_", policy, "_"))
    
  }, filenames, file_states)
]

# files_keep <- filenames[
#   mapply(function(file, state) {
#     policy <- policy_map[state]
#     if (is.na(policy)) return(FALSE)
#     str_detect(file, paste0("_", policy, "_"))
#   }, filenames, file_states)
# ]

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
  dplyr::summarise(median_pct_diff = round(median(pct_diff),2)) %>%
  tidyr::pivot_wider(names_from = species, values_from = median_pct_diff)

results <- cbind(policy_run, harv)

all_results <- dplyr::bind_rows(all_results, results)

rm(harv, data_adj, draw_map, df, data)
}

write.csv(
  all_results,
  here::here("all_coastwide_results.csv"),
  row.names = FALSE)

#write.csv(harv, here::here(paste0(coastwide_data, ".csv")))
