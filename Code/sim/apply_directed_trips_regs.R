################################################################################
################################################################################
# Script:       apply_directed_trips_regs.R
# Purpose:      Holds the per-state regulation logic extracted from the nine
#               recDST/model_run_<ST>.R scripts. Defines one function that
#               takes a directed-trips table and a state code and returns the
#               same table with the projection-year (_y2) bag limits and
#               minimum sizes filled in from the saved scenario. This is the
#               single largest piece of duplication in the repo consolidated
#               into one place - roughly 100 lines of case_when per state,
#               nine states, all differing only in identifier names.
# Inputs:       None. Operates on the data frame passed in, and reads the
#               scenario regulation objects from the calling environment.
# Outputs:      None. Returns the modified data frame.
# Dependencies: Packages dplyr and lubridate (yday, ymd used unqualified).
#               The scenario regulation objects (SFctFH_seas1_op and the like)
#               must already exist in an enclosing environment - they are
#               created dynamically by the caller's assign() loop, which is
#               why they have no visible definition here.
# Pipeline:     ORPHANED. Nothing in the repo sources this file. Its only
#               caller is run_state_model.R, which is itself not on any active
#               code path AND does not source it - so calling that function
#               raises "could not find function apply_directed_trips_regs".
#               Wiring this up is part of finishing the per-state refactor.
#
# How to read the case_when chains (the same conventions as model_run_MA.R):
#   - They CASCADE. The first assignment to each column ends `TRUE ~ 0` (bags)
#     or `TRUE ~ 254` (sizes), setting a closed-season default; every later
#     one ends `TRUE ~ <same column>` so it can only add open days, never
#     reopen a day an earlier line closed. Order is therefore significant.
#   - `* 2.54` converts the scenario's minimum lengths from inches to
#     centimetres, the unit of the length data.
#   - 254 is the closed-season sentinel: 100 inches x 2.54, matching the
#     100-inch default in set_regulations.do. It means no fish is legal.
#   - Each species block is wrapped in an exists() test distinguishing a
#     STATEWIDE regulation entry (e.g. SFct_seas1_op) from MODE-SPECIFIC
#     entries (SFctFH_/SFctPR_/SFctSH_). Which form appears depends on what
#     the scenario author entered in the app, and because the objects are
#     created dynamically, exists() is the only way to tell.
#
# Identifier convention: <SPECIES><state><MODE>_<field>, e.g. SFctFH_seas1_op
# is summer flounder, Connecticut, for-hire, season 1 opening date. Fields are
# seas<n>_op / seas<n>_cl for dates and <n>_bag / <n>_len for the limits.
#
# States differ only in how many seasons each species defines and whether a
# statewide alternative exists for it; there is no structural difference
# between the nine branches. The final else raises an error on an unrecognized
# state, so a typo fails loudly rather than silently returning unchanged
# regulations.
################################################################################
################################################################################

#' @title Fill in projection-year regulations for one state
#' @description Applies a saved regulation scenario to a directed-trips table,
#'   writing the projection-year bag limits and minimum sizes (the _y2
#'   columns) for whichever days fall inside each species' open seasons.
#'   Dispatches on the state code to one of nine hand-written blocks; see the
#'   file header for the cascading-case_when and sentinel conventions those
#'   blocks share.
#' @param directed_trips Data frame of directed trips carrying at least `mode`
#'   and `date_adj` (day-of-year, leap-adjusted by the caller), plus the
#'   calibration-year regulation columns.
#' @param state Two-letter state code. One of CT, DE, MA, MD, NC, NJ, NY, RI,
#'   VA; anything else raises an error.
#' @return The input data frame with fluke_bag_y2, fluke_min_y2, bsb_bag_y2,
#'   bsb_min_y2, scup_bag_y2 and scup_min_y2 populated. Minimum sizes are in
#'   centimetres; a closed day carries bag 0 and minimum size 254.
#' @examples
#' \dontrun{
#' directed_trips <- apply_directed_trips_regs(directed_trips, "CT")
#' }
apply_directed_trips_regs <- function(directed_trips, state) {
  
  if (state == "CT") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFct_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFct_seas1_op)) & date_adj <= yday(ymd(SFct_seas1_cl)) ~ as.numeric(SFct_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFct_seas2_op)) & date_adj <= yday(ymd(SFct_seas2_cl)) ~ as.numeric(SFct_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFct_seas1_op)) & date_adj <= yday(ymd(SFct_seas1_cl)) ~ as.numeric(SFct_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFct_seas2_op)) & date_adj <= yday(ymd(SFct_seas2_cl)) ~ as.numeric(SFct_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas1_op)) & date_adj <= yday(ymd(SFctFH_seas1_cl)) ~ as.numeric(SFctFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas1_op)) & date_adj <= yday(ymd(SFctPR_seas1_cl)) ~ as.numeric(SFctPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas1_op)) & date_adj <= yday(ymd(SFctSH_seas1_cl)) ~ as.numeric(SFctSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas2_op)) & date_adj <= yday(ymd(SFctFH_seas2_cl)) ~ as.numeric(SFctFH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas2_op)) & date_adj <= yday(ymd(SFctPR_seas2_cl)) ~ as.numeric(SFctPR_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas2_op)) & date_adj <= yday(ymd(SFctSH_seas2_cl)) ~ as.numeric(SFctSH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas1_op)) & date_adj <= yday(ymd(SFctFH_seas1_cl)) ~ as.numeric(SFctFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas1_op)) & date_adj <= yday(ymd(SFctPR_seas1_cl)) ~ as.numeric(SFctPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas1_op)) & date_adj <= yday(ymd(SFctSH_seas1_cl)) ~ as.numeric(SFctSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas2_op)) & date_adj <= yday(ymd(SFctFH_seas2_cl)) ~ as.numeric(SFctFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas2_op)) & date_adj <= yday(ymd(SFctPR_seas2_cl)) ~ as.numeric(SFctPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas2_op)) & date_adj <= yday(ymd(SFctSH_seas2_cl)) ~ as.numeric(SFctSH_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBct_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBct_seas1_op)) & date_adj <= yday(ymd(BSBct_seas1_cl)) ~ as.numeric(BSBct_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBct_seas2_op)) & date_adj <= yday(ymd(BSBct_seas2_cl)) ~ as.numeric(BSBct_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBct_seas1_op)) & date_adj <= yday(ymd(BSBct_seas1_cl)) ~ as.numeric(BSBct_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBct_seas2_op)) & date_adj <= yday(ymd(BSBct_seas2_cl)) ~ as.numeric(BSBct_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas1_op)) & date_adj <= yday(ymd(BSBctFH_seas1_cl)) ~ as.numeric(BSBctFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas1_op)) & date_adj <= yday(ymd(BSBctPR_seas1_cl)) ~ as.numeric(BSBctPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas1_op)) & date_adj <= yday(ymd(BSBctSH_seas1_cl)) ~ as.numeric(BSBctSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas2_op)) & date_adj <= yday(ymd(BSBctFH_seas2_cl)) ~ as.numeric(BSBctFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas2_op)) & date_adj <= yday(ymd(BSBctPR_seas2_cl)) ~ as.numeric(BSBctPR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas2_op)) & date_adj <= yday(ymd(BSBctSH_seas2_cl)) ~ as.numeric(BSBctSH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas1_op)) & date_adj <= yday(ymd(BSBctFH_seas1_cl)) ~ as.numeric(BSBctFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas1_op)) & date_adj <= yday(ymd(BSBctPR_seas1_cl)) ~ as.numeric(BSBctPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas1_op)) & date_adj <= yday(ymd(BSBctSH_seas1_cl)) ~ as.numeric(BSBctSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas2_op)) & date_adj <= yday(ymd(BSBctFH_seas2_cl)) ~ as.numeric(BSBctFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas2_op)) & date_adj <= yday(ymd(BSBctPR_seas2_cl)) ~ as.numeric(BSBctPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas2_op)) & date_adj <= yday(ymd(BSBctSH_seas2_cl)) ~ as.numeric(BSBctSH_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Season 3 overrides (CT always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas3_op)) & date_adj <= yday(ymd(SFctFH_seas3_cl)) ~ as.numeric(SFctFH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas3_op)) & date_adj <= yday(ymd(SFctPR_seas3_cl)) ~ as.numeric(SFctPR_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas3_op)) & date_adj <= yday(ymd(SFctSH_seas3_cl)) ~ as.numeric(SFctSH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas3_op)) & date_adj <= yday(ymd(SFctFH_seas3_cl)) ~ as.numeric(SFctFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas3_op)) & date_adj <= yday(ymd(SFctPR_seas3_cl)) ~ as.numeric(SFctPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas3_op)) & date_adj <= yday(ymd(SFctSH_seas3_cl)) ~ as.numeric(SFctSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas3_op)) & date_adj <= yday(ymd(BSBctFH_seas3_cl)) ~ as.numeric(BSBctFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas3_op)) & date_adj <= yday(ymd(BSBctPR_seas3_cl)) ~ as.numeric(BSBctPR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas3_op)) & date_adj <= yday(ymd(BSBctSH_seas3_cl)) ~ as.numeric(BSBctSH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas3_op)) & date_adj <= yday(ymd(BSBctFH_seas3_cl)) ~ as.numeric(BSBctFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas3_op)) & date_adj <= yday(ymd(BSBctPR_seas3_cl)) ~ as.numeric(BSBctPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas3_op)) & date_adj <= yday(ymd(BSBctSH_seas3_cl)) ~ as.numeric(BSBctSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        # ---- Scup (CT) ----
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas1_op)) & date_adj <= yday(ymd(SCUPctFH_seas1_cl)) ~ as.numeric(SCUPctFH_1_bag), TRUE ~ 0),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas1_op)) & date_adj <= yday(ymd(SCUPctFH_seas1_cl)) ~ as.numeric(SCUPctFH_1_len) * 2.54, TRUE ~ 254),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas2_op)) & date_adj <= yday(ymd(SCUPctFH_seas2_cl)) ~ as.numeric(SCUPctFH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas2_op)) & date_adj <= yday(ymd(SCUPctFH_seas2_cl)) ~ as.numeric(SCUPctFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas3_op)) & date_adj <= yday(ymd(SCUPctFH_seas3_cl)) ~ as.numeric(SCUPctFH_3_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas3_op)) & date_adj <= yday(ymd(SCUPctFH_seas3_cl)) ~ as.numeric(SCUPctFH_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas4_op)) & date_adj <= yday(ymd(SCUPctFH_seas4_cl)) ~ as.numeric(SCUPctFH_4_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas4_op)) & date_adj <= yday(ymd(SCUPctFH_seas4_cl)) ~ as.numeric(SCUPctFH_4_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas1_op)) & date_adj <= yday(ymd(SCUPctPR_seas1_cl)) ~ as.numeric(SCUPctPR_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas1_op)) & date_adj <= yday(ymd(SCUPctPR_seas1_cl)) ~ as.numeric(SCUPctPR_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas2_op)) & date_adj <= yday(ymd(SCUPctPR_seas2_cl)) ~ as.numeric(SCUPctPR_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas2_op)) & date_adj <= yday(ymd(SCUPctPR_seas2_cl)) ~ as.numeric(SCUPctPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas1_op)) & date_adj <= yday(ymd(SCUPctSH_seas1_cl)) ~ as.numeric(SCUPctSH_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas1_op)) & date_adj <= yday(ymd(SCUPctSH_seas1_cl)) ~ as.numeric(SCUPctSH_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas2_op)) & date_adj <= yday(ymd(SCUPctSH_seas2_cl)) ~ as.numeric(SCUPctSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas2_op)) & date_adj <= yday(ymd(SCUPctSH_seas2_cl)) ~ as.numeric(SCUPctSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "DE") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFde_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFde_seas1_op)) & date_adj <= yday(ymd(SFde_seas1_cl)) ~ as.numeric(SFde_1_bag), TRUE ~ 0),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFde_seas1_op)) & date_adj <= yday(ymd(SFde_seas1_cl)) ~ as.numeric(SFde_1_len) * 2.54, TRUE ~ 254),
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFde_seas2_op)) & date_adj <= yday(ymd(SFde_seas2_cl)) ~ as.numeric(SFde_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFde_seas2_op)) & date_adj <= yday(ymd(SFde_seas2_cl)) ~ as.numeric(SFde_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFdeFH_seas1_op)) & date_adj <= yday(ymd(SFdeFH_seas1_cl)) ~ as.numeric(SFdeFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFdePR_seas1_op)) & date_adj <= yday(ymd(SFdePR_seas1_cl)) ~ as.numeric(SFdePR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFdeSH_seas1_op)) & date_adj <= yday(ymd(SFdeSH_seas1_cl)) ~ as.numeric(SFdeSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFdeFH_seas2_op)) & date_adj <= yday(ymd(SFdeFH_seas2_cl)) ~ as.numeric(SFdeFH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFdePR_seas2_op)) & date_adj <= yday(ymd(SFdePR_seas2_cl)) ~ as.numeric(SFdePR_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFdeSH_seas2_op)) & date_adj <= yday(ymd(SFdeSH_seas2_cl)) ~ as.numeric(SFdeSH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFdeFH_seas1_op)) & date_adj <= yday(ymd(SFdeFH_seas1_cl)) ~ as.numeric(SFdeFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFdePR_seas1_op)) & date_adj <= yday(ymd(SFdePR_seas1_cl)) ~ as.numeric(SFdePR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFdeSH_seas1_op)) & date_adj <= yday(ymd(SFdeSH_seas1_cl)) ~ as.numeric(SFdeSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFdeFH_seas2_op)) & date_adj <= yday(ymd(SFdeFH_seas2_cl)) ~ as.numeric(SFdeFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFdePR_seas2_op)) & date_adj <= yday(ymd(SFdePR_seas2_cl)) ~ as.numeric(SFdePR_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFdeSH_seas2_op)) & date_adj <= yday(ymd(SFdeSH_seas2_cl)) ~ as.numeric(SFdeSH_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBde_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBde_seas1_op)) & date_adj <= yday(ymd(BSBde_seas1_cl)) ~ as.numeric(BSBde_1_bag), TRUE ~ 0),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBde_seas1_op)) & date_adj <= yday(ymd(BSBde_seas1_cl)) ~ as.numeric(BSBde_1_len) * 2.54, TRUE ~ 254),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBde_seas2_op)) & date_adj <= yday(ymd(BSBde_seas2_cl)) ~ as.numeric(BSBde_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBde_seas2_op)) & date_adj <= yday(ymd(BSBde_seas2_cl)) ~ as.numeric(BSBde_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBdeFH_seas1_op)) & date_adj <= yday(ymd(BSBdeFH_seas1_cl)) ~ as.numeric(BSBdeFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBdePR_seas1_op)) & date_adj <= yday(ymd(BSBdePR_seas1_cl)) ~ as.numeric(BSBdePR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBdeSH_seas1_op)) & date_adj <= yday(ymd(BSBdeSH_seas1_cl)) ~ as.numeric(BSBdeSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBdeFH_seas2_op)) & date_adj <= yday(ymd(BSBdeFH_seas2_cl)) ~ as.numeric(BSBdeFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBdePR_seas2_op)) & date_adj <= yday(ymd(BSBdePR_seas2_cl)) ~ as.numeric(BSBdePR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBdeSH_seas2_op)) & date_adj <= yday(ymd(BSBdeSH_seas2_cl)) ~ as.numeric(BSBdeSH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBdeFH_seas1_op)) & date_adj <= yday(ymd(BSBdeFH_seas1_cl)) ~ as.numeric(BSBdeFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBdePR_seas1_op)) & date_adj <= yday(ymd(BSBdePR_seas1_cl)) ~ as.numeric(BSBdePR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBdeSH_seas1_op)) & date_adj <= yday(ymd(BSBdeSH_seas1_cl)) ~ as.numeric(BSBdeSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBdeFH_seas2_op)) & date_adj <= yday(ymd(BSBdeFH_seas2_cl)) ~ as.numeric(BSBdeFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBdePR_seas2_op)) & date_adj <= yday(ymd(BSBdePR_seas2_cl)) ~ as.numeric(BSBdePR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBdeSH_seas2_op)) & date_adj <= yday(ymd(BSBdeSH_seas2_cl)) ~ as.numeric(BSBdeSH_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Scup (DE) ----
    if (exists("SCUPde_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPde_seas1_op)) & date_adj <= yday(ymd(SCUPde_seas1_cl)) ~ as.numeric(SCUPde_1_bag), TRUE ~ 0),
          scup_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPde_seas1_op)) & date_adj <= yday(ymd(SCUPde_seas1_cl)) ~ as.numeric(SCUPde_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPdeFH_seas1_op)) & date_adj <= yday(ymd(SCUPdeFH_seas1_cl)) ~ as.numeric(SCUPdeFH_1_bag), TRUE ~ 0),
          scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPdePR_seas1_op)) & date_adj <= yday(ymd(SCUPdePR_seas1_cl)) ~ as.numeric(SCUPdePR_1_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPdeSH_seas1_op)) & date_adj <= yday(ymd(SCUPdeSH_seas1_cl)) ~ as.numeric(SCUPdeSH_1_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPdeFH_seas1_op)) & date_adj <= yday(ymd(SCUPdeFH_seas1_cl)) ~ as.numeric(SCUPdeFH_1_len) * 2.54, TRUE ~ 254),
          scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPdePR_seas1_op)) & date_adj <= yday(ymd(SCUPdePR_seas1_cl)) ~ as.numeric(SCUPdePR_1_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPdeSH_seas1_op)) & date_adj <= yday(ymd(SCUPdeSH_seas1_cl)) ~ as.numeric(SCUPdeSH_1_len) * 2.54, TRUE ~ scup_min_y2)
        )
    }
    
    # ---- Season 3 overrides + season 2 scup (DE always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFdeFH_seas3_op)) & date_adj <= yday(ymd(SFdeFH_seas3_cl)) ~ as.numeric(SFdeFH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFdePR_seas3_op)) & date_adj <= yday(ymd(SFdePR_seas3_cl)) ~ as.numeric(SFdePR_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFdeSH_seas3_op)) & date_adj <= yday(ymd(SFdeSH_seas3_cl)) ~ as.numeric(SFdeSH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFdeFH_seas3_op)) & date_adj <= yday(ymd(SFdeFH_seas3_cl)) ~ as.numeric(SFdeFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFdePR_seas3_op)) & date_adj <= yday(ymd(SFdePR_seas3_cl)) ~ as.numeric(SFdePR_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFdeSH_seas3_op)) & date_adj <= yday(ymd(SFdeSH_seas3_cl)) ~ as.numeric(SFdeSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBdeFH_seas3_op)) & date_adj <= yday(ymd(BSBdeFH_seas3_cl)) ~ as.numeric(BSBdeFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBdePR_seas3_op)) & date_adj <= yday(ymd(BSBdePR_seas3_cl)) ~ as.numeric(BSBdePR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBdeSH_seas3_op)) & date_adj <= yday(ymd(BSBdeSH_seas3_cl)) ~ as.numeric(BSBdeSH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBdeFH_seas3_op)) & date_adj <= yday(ymd(BSBdeFH_seas3_cl)) ~ as.numeric(BSBdeFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBdePR_seas3_op)) & date_adj <= yday(ymd(BSBdePR_seas3_cl)) ~ as.numeric(BSBdePR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBdeSH_seas3_op)) & date_adj <= yday(ymd(BSBdeSH_seas3_cl)) ~ as.numeric(BSBdeSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPdeFH_seas2_op)) & date_adj <= yday(ymd(SCUPdeFH_seas2_cl)) ~ as.numeric(SCUPdeFH_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPdePR_seas2_op)) & date_adj <= yday(ymd(SCUPdePR_seas2_cl)) ~ as.numeric(SCUPdePR_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPdeSH_seas2_op)) & date_adj <= yday(ymd(SCUPdeSH_seas2_cl)) ~ as.numeric(SCUPdeSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPdeFH_seas2_op)) & date_adj <= yday(ymd(SCUPdeFH_seas2_cl)) ~ as.numeric(SCUPdeFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPdePR_seas2_op)) & date_adj <= yday(ymd(SCUPdePR_seas2_cl)) ~ as.numeric(SCUPdePR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPdeSH_seas2_op)) & date_adj <= yday(ymd(SCUPdeSH_seas2_cl)) ~ as.numeric(SCUPdeSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "MA") {
    
    # ---- Summer Flounder (MA always applies by mode, no unified-season variant) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas1_op)) & date_adj <= yday(ymd(SFmaFH_seas1_cl)) ~ as.numeric(SFmaFH_1_bag), TRUE ~ 0),
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas2_op)) & date_adj <= yday(ymd(SFmaFH_seas2_cl)) ~ as.numeric(SFmaFH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas1_op)) & date_adj <= yday(ymd(SFmaPR_seas1_cl)) ~ as.numeric(SFmaPR_1_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas2_op)) & date_adj <= yday(ymd(SFmaPR_seas2_cl)) ~ as.numeric(SFmaPR_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas1_op)) & date_adj <= yday(ymd(SFmaSH_seas1_cl)) ~ as.numeric(SFmaSH_1_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas2_op)) & date_adj <= yday(ymd(SFmaSH_seas2_cl)) ~ as.numeric(SFmaSH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas1_op)) & date_adj <= yday(ymd(SFmaFH_seas1_cl)) ~ as.numeric(SFmaFH_1_len) * 2.54, TRUE ~ 254),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas2_op)) & date_adj <= yday(ymd(SFmaFH_seas2_cl)) ~ as.numeric(SFmaFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas1_op)) & date_adj <= yday(ymd(SFmaPR_seas1_cl)) ~ as.numeric(SFmaPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas2_op)) & date_adj <= yday(ymd(SFmaPR_seas2_cl)) ~ as.numeric(SFmaPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas1_op)) & date_adj <= yday(ymd(SFmaSH_seas1_cl)) ~ as.numeric(SFmaSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas2_op)) & date_adj <= yday(ymd(SFmaSH_seas2_cl)) ~ as.numeric(SFmaSH_2_len) * 2.54, TRUE ~ fluke_min_y2)
      )
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBma_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBma_seas1_op)) & date_adj <= yday(ymd(BSBma_seas1_cl)) ~ as.numeric(BSBma_1_bag), TRUE ~ 0),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBma_seas1_op)) & date_adj <= yday(ymd(BSBma_seas1_cl)) ~ as.numeric(BSBma_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas1_op)) & date_adj <= yday(ymd(BSBmaFH_seas1_cl)) ~ as.numeric(BSBmaFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas1_op)) & date_adj <= yday(ymd(BSBmaPR_seas1_cl)) ~ as.numeric(BSBmaPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas1_op)) & date_adj <= yday(ymd(BSBmaSH_seas1_cl)) ~ as.numeric(BSBmaSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas1_op)) & date_adj <= yday(ymd(BSBmaFH_seas1_cl)) ~ as.numeric(BSBmaFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas1_op)) & date_adj <= yday(ymd(BSBmaPR_seas1_cl)) ~ as.numeric(BSBmaPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas1_op)) & date_adj <= yday(ymd(BSBmaSH_seas1_cl)) ~ as.numeric(BSBmaSH_1_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Season 2 BSB + Scup (MA always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        bsb_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas2_op)) & date_adj <= yday(ymd(BSBmaFH_seas2_cl)) ~ as.numeric(BSBmaFH_2_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas2_op)) & date_adj <= yday(ymd(BSBmaPR_seas2_cl)) ~ as.numeric(BSBmaPR_2_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas2_op)) & date_adj <= yday(ymd(BSBmaSH_seas2_cl)) ~ as.numeric(BSBmaSH_2_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas2_op)) & date_adj <= yday(ymd(BSBmaFH_seas2_cl)) ~ as.numeric(BSBmaFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas2_op)) & date_adj <= yday(ymd(BSBmaPR_seas2_cl)) ~ as.numeric(BSBmaPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas2_op)) & date_adj <= yday(ymd(BSBmaSH_seas2_cl)) ~ as.numeric(BSBmaSH_2_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas1_op)) & date_adj <= yday(ymd(SCUPmaFH_seas1_cl)) ~ as.numeric(SCUPmaFH_1_bag), TRUE ~ 0),
        scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas2_op)) & date_adj <= yday(ymd(SCUPmaFH_seas2_cl)) ~ as.numeric(SCUPmaFH_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas3_op)) & date_adj <= yday(ymd(SCUPmaFH_seas3_cl)) ~ as.numeric(SCUPmaFH_3_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas1_op)) & date_adj <= yday(ymd(SCUPmaPR_seas1_cl)) ~ as.numeric(SCUPmaPR_1_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas2_op)) & date_adj <= yday(ymd(SCUPmaPR_seas2_cl)) ~ as.numeric(SCUPmaPR_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas1_op)) & date_adj <= yday(ymd(SCUPmaSH_seas1_cl)) ~ as.numeric(SCUPmaSH_1_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas2_op)) & date_adj <= yday(ymd(SCUPmaSH_seas2_cl)) ~ as.numeric(SCUPmaSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas1_op)) & date_adj <= yday(ymd(SCUPmaFH_seas1_cl)) ~ as.numeric(SCUPmaFH_1_len) * 2.54, TRUE ~ 254),
        scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas2_op)) & date_adj <= yday(ymd(SCUPmaFH_seas2_cl)) ~ as.numeric(SCUPmaFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas3_op)) & date_adj <= yday(ymd(SCUPmaFH_seas3_cl)) ~ as.numeric(SCUPmaFH_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas1_op)) & date_adj <= yday(ymd(SCUPmaPR_seas1_cl)) ~ as.numeric(SCUPmaPR_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas2_op)) & date_adj <= yday(ymd(SCUPmaPR_seas2_cl)) ~ as.numeric(SCUPmaPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas1_op)) & date_adj <= yday(ymd(SCUPmaSH_seas1_cl)) ~ as.numeric(SCUPmaSH_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas2_op)) & date_adj <= yday(ymd(SCUPmaSH_seas2_cl)) ~ as.numeric(SCUPmaSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "MD") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFmd_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFmd_seas1_op)) & date_adj <= yday(ymd(SFmd_seas1_cl)) ~ as.numeric(SFmd_1_bag), TRUE ~ 0),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFmd_seas1_op)) & date_adj <= yday(ymd(SFmd_seas1_cl)) ~ as.numeric(SFmd_1_len) * 2.54, TRUE ~ 254),
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFmd_seas2_op)) & date_adj <= yday(ymd(SFmd_seas2_cl)) ~ as.numeric(SFmd_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFmd_seas2_op)) & date_adj <= yday(ymd(SFmd_seas2_cl)) ~ as.numeric(SFmd_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmdFH_seas1_op)) & date_adj <= yday(ymd(SFmdFH_seas1_cl)) ~ as.numeric(SFmdFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmdPR_seas1_op)) & date_adj <= yday(ymd(SFmdPR_seas1_cl)) ~ as.numeric(SFmdPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmdSH_seas1_op)) & date_adj <= yday(ymd(SFmdSH_seas1_cl)) ~ as.numeric(SFmdSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmdFH_seas2_op)) & date_adj <= yday(ymd(SFmdFH_seas2_cl)) ~ as.numeric(SFmdFH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmdPR_seas2_op)) & date_adj <= yday(ymd(SFmdPR_seas2_cl)) ~ as.numeric(SFmdPR_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmdSH_seas2_op)) & date_adj <= yday(ymd(SFmdSH_seas2_cl)) ~ as.numeric(SFmdSH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmdFH_seas1_op)) & date_adj <= yday(ymd(SFmdFH_seas1_cl)) ~ as.numeric(SFmdFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmdPR_seas1_op)) & date_adj <= yday(ymd(SFmdPR_seas1_cl)) ~ as.numeric(SFmdPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmdSH_seas1_op)) & date_adj <= yday(ymd(SFmdSH_seas1_cl)) ~ as.numeric(SFmdSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmdFH_seas2_op)) & date_adj <= yday(ymd(SFmdFH_seas2_cl)) ~ as.numeric(SFmdFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmdPR_seas2_op)) & date_adj <= yday(ymd(SFmdPR_seas2_cl)) ~ as.numeric(SFmdPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmdSH_seas2_op)) & date_adj <= yday(ymd(SFmdSH_seas2_cl)) ~ as.numeric(SFmdSH_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBmd_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBmd_seas1_op)) & date_adj <= yday(ymd(BSBmd_seas1_cl)) ~ as.numeric(BSBmd_1_bag), TRUE ~ 0),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBmd_seas1_op)) & date_adj <= yday(ymd(BSBmd_seas1_cl)) ~ as.numeric(BSBmd_1_len) * 2.54, TRUE ~ 254),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBmd_seas2_op)) & date_adj <= yday(ymd(BSBmd_seas2_cl)) ~ as.numeric(BSBmd_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBmd_seas2_op)) & date_adj <= yday(ymd(BSBmd_seas2_cl)) ~ as.numeric(BSBmd_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmdFH_seas1_op)) & date_adj <= yday(ymd(BSBmdFH_seas1_cl)) ~ as.numeric(BSBmdFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmdPR_seas1_op)) & date_adj <= yday(ymd(BSBmdPR_seas1_cl)) ~ as.numeric(BSBmdPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmdSH_seas1_op)) & date_adj <= yday(ymd(BSBmdSH_seas1_cl)) ~ as.numeric(BSBmdSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmdFH_seas2_op)) & date_adj <= yday(ymd(BSBmdFH_seas2_cl)) ~ as.numeric(BSBmdFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmdPR_seas2_op)) & date_adj <= yday(ymd(BSBmdPR_seas2_cl)) ~ as.numeric(BSBmdPR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmdSH_seas2_op)) & date_adj <= yday(ymd(BSBmdSH_seas2_cl)) ~ as.numeric(BSBmdSH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmdFH_seas1_op)) & date_adj <= yday(ymd(BSBmdFH_seas1_cl)) ~ as.numeric(BSBmdFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmdPR_seas1_op)) & date_adj <= yday(ymd(BSBmdPR_seas1_cl)) ~ as.numeric(BSBmdPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmdSH_seas1_op)) & date_adj <= yday(ymd(BSBmdSH_seas1_cl)) ~ as.numeric(BSBmdSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmdFH_seas2_op)) & date_adj <= yday(ymd(BSBmdFH_seas2_cl)) ~ as.numeric(BSBmdFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmdPR_seas2_op)) & date_adj <= yday(ymd(BSBmdPR_seas2_cl)) ~ as.numeric(BSBmdPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmdSH_seas2_op)) & date_adj <= yday(ymd(BSBmdSH_seas2_cl)) ~ as.numeric(BSBmdSH_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Scup (MD) ----
    if (exists("SCUPmd_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPmd_seas1_op)) & date_adj <= yday(ymd(SCUPmd_seas1_cl)) ~ as.numeric(SCUPmd_1_bag), TRUE ~ 0),
          scup_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPmd_seas1_op)) & date_adj <= yday(ymd(SCUPmd_seas1_cl)) ~ as.numeric(SCUPmd_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmdFH_seas1_op)) & date_adj <= yday(ymd(SCUPmdFH_seas1_cl)) ~ as.numeric(SCUPmdFH_1_bag), TRUE ~ 0),
          scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmdPR_seas1_op)) & date_adj <= yday(ymd(SCUPmdPR_seas1_cl)) ~ as.numeric(SCUPmdPR_1_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmdSH_seas1_op)) & date_adj <= yday(ymd(SCUPmdSH_seas1_cl)) ~ as.numeric(SCUPmdSH_1_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmdFH_seas1_op)) & date_adj <= yday(ymd(SCUPmdFH_seas1_cl)) ~ as.numeric(SCUPmdFH_1_len) * 2.54, TRUE ~ 254),
          scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmdPR_seas1_op)) & date_adj <= yday(ymd(SCUPmdPR_seas1_cl)) ~ as.numeric(SCUPmdPR_1_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmdSH_seas1_op)) & date_adj <= yday(ymd(SCUPmdSH_seas1_cl)) ~ as.numeric(SCUPmdSH_1_len) * 2.54, TRUE ~ scup_min_y2)
        )
    }
    
    # ---- Season 3 overrides + season 2 scup (MD always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmdFH_seas3_op)) & date_adj <= yday(ymd(SFmdFH_seas3_cl)) ~ as.numeric(SFmdFH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmdPR_seas3_op)) & date_adj <= yday(ymd(SFmdPR_seas3_cl)) ~ as.numeric(SFmdPR_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmdSH_seas3_op)) & date_adj <= yday(ymd(SFmdSH_seas3_cl)) ~ as.numeric(SFmdSH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmdFH_seas3_op)) & date_adj <= yday(ymd(SFmdFH_seas3_cl)) ~ as.numeric(SFmdFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmdPR_seas3_op)) & date_adj <= yday(ymd(SFmdPR_seas3_cl)) ~ as.numeric(SFmdPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmdSH_seas3_op)) & date_adj <= yday(ymd(SFmdSH_seas3_cl)) ~ as.numeric(SFmdSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmdFH_seas3_op)) & date_adj <= yday(ymd(BSBmdFH_seas3_cl)) ~ as.numeric(BSBmdFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmdPR_seas3_op)) & date_adj <= yday(ymd(BSBmdPR_seas3_cl)) ~ as.numeric(BSBmdPR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmdSH_seas3_op)) & date_adj <= yday(ymd(BSBmdSH_seas3_cl)) ~ as.numeric(BSBmdSH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmdFH_seas3_op)) & date_adj <= yday(ymd(BSBmdFH_seas3_cl)) ~ as.numeric(BSBmdFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmdPR_seas3_op)) & date_adj <= yday(ymd(BSBmdPR_seas3_cl)) ~ as.numeric(BSBmdPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmdSH_seas3_op)) & date_adj <= yday(ymd(BSBmdSH_seas3_cl)) ~ as.numeric(BSBmdSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmdFH_seas2_op)) & date_adj <= yday(ymd(SCUPmdFH_seas2_cl)) ~ as.numeric(SCUPmdFH_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmdPR_seas2_op)) & date_adj <= yday(ymd(SCUPmdPR_seas2_cl)) ~ as.numeric(SCUPmdPR_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmdSH_seas2_op)) & date_adj <= yday(ymd(SCUPmdSH_seas2_cl)) ~ as.numeric(SCUPmdSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmdFH_seas2_op)) & date_adj <= yday(ymd(SCUPmdFH_seas2_cl)) ~ as.numeric(SCUPmdFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmdPR_seas2_op)) & date_adj <= yday(ymd(SCUPmdPR_seas2_cl)) ~ as.numeric(SCUPmdPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmdSH_seas2_op)) & date_adj <= yday(ymd(SCUPmdSH_seas2_cl)) ~ as.numeric(SCUPmdSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "NC") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFnc_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFnc_seas1_op)) & date_adj <= yday(ymd(SFnc_seas1_cl)) ~ as.numeric(SFnc_1_bag), TRUE ~ 0),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFnc_seas1_op)) & date_adj <= yday(ymd(SFnc_seas1_cl)) ~ as.numeric(SFnc_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFncFH_seas1_op)) & date_adj <= yday(ymd(SFncFH_seas1_cl)) ~ as.numeric(SFncFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFncPR_seas1_op)) & date_adj <= yday(ymd(SFncPR_seas1_cl)) ~ as.numeric(SFncPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFncSH_seas1_op)) & date_adj <= yday(ymd(SFncSH_seas1_cl)) ~ as.numeric(SFncSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFncFH_seas1_op)) & date_adj <= yday(ymd(SFncFH_seas1_cl)) ~ as.numeric(SFncFH_1_len) * 2.54, TRUE ~ 100),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFncPR_seas1_op)) & date_adj <= yday(ymd(SFncPR_seas1_cl)) ~ as.numeric(SFncPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFncSH_seas1_op)) & date_adj <= yday(ymd(SFncSH_seas1_cl)) ~ as.numeric(SFncSH_1_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBnc_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnc_seas1_op)) & date_adj <= yday(ymd(BSBnc_seas1_cl)) ~ as.numeric(BSBnc_1_bag), TRUE ~ 0),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnc_seas1_op)) & date_adj <= yday(ymd(BSBnc_seas1_cl)) ~ as.numeric(BSBnc_1_len) * 2.54, TRUE ~ 254),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnc_seas2_op)) & date_adj <= yday(ymd(BSBnc_seas2_cl)) ~ as.numeric(BSBnc_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnc_seas2_op)) & date_adj <= yday(ymd(BSBnc_seas2_cl)) ~ as.numeric(BSBnc_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBncFH_seas1_op)) & date_adj <= yday(ymd(BSBncFH_seas1_cl)) ~ as.numeric(BSBncFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBncPR_seas1_op)) & date_adj <= yday(ymd(BSBncPR_seas1_cl)) ~ as.numeric(BSBncPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBncSH_seas1_op)) & date_adj <= yday(ymd(BSBncSH_seas1_cl)) ~ as.numeric(BSBncSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBncFH_seas2_op)) & date_adj <= yday(ymd(BSBncFH_seas2_cl)) ~ as.numeric(BSBncFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBncPR_seas2_op)) & date_adj <= yday(ymd(BSBncPR_seas2_cl)) ~ as.numeric(BSBncPR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBncSH_seas2_op)) & date_adj <= yday(ymd(BSBncSH_seas2_cl)) ~ as.numeric(BSBncSH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBncFH_seas1_op)) & date_adj <= yday(ymd(BSBncFH_seas1_cl)) ~ as.numeric(BSBncFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBncPR_seas1_op)) & date_adj <= yday(ymd(BSBncPR_seas1_cl)) ~ as.numeric(BSBncPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBncSH_seas1_op)) & date_adj <= yday(ymd(BSBncSH_seas1_cl)) ~ as.numeric(BSBncSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBncFH_seas2_op)) & date_adj <= yday(ymd(BSBncFH_seas2_cl)) ~ as.numeric(BSBncFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBncPR_seas2_op)) & date_adj <= yday(ymd(BSBncPR_seas2_cl)) ~ as.numeric(BSBncPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBncSH_seas2_op)) & date_adj <= yday(ymd(BSBncSH_seas2_cl)) ~ as.numeric(BSBncSH_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Scup (NC) ----
    if (exists("SCUPnc_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPnc_seas1_op)) & date_adj <= yday(ymd(SCUPnc_seas1_cl)) ~ as.numeric(SCUPnc_1_bag), TRUE ~ 0),
          scup_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPnc_seas1_op)) & date_adj <= yday(ymd(SCUPnc_seas1_cl)) ~ as.numeric(SCUPnc_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPncFH_seas1_op)) & date_adj <= yday(ymd(SCUPncFH_seas1_cl)) ~ as.numeric(SCUPncFH_1_bag), TRUE ~ 0),
          scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPncPR_seas1_op)) & date_adj <= yday(ymd(SCUPncPR_seas1_cl)) ~ as.numeric(SCUPncPR_1_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPncSH_seas1_op)) & date_adj <= yday(ymd(SCUPncSH_seas1_cl)) ~ as.numeric(SCUPncSH_1_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPncFH_seas1_op)) & date_adj <= yday(ymd(SCUPncFH_seas1_cl)) ~ as.numeric(SCUPncFH_1_len) * 2.54, TRUE ~ 254),
          scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPncPR_seas1_op)) & date_adj <= yday(ymd(SCUPncPR_seas1_cl)) ~ as.numeric(SCUPncPR_1_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPncSH_seas1_op)) & date_adj <= yday(ymd(SCUPncSH_seas1_cl)) ~ as.numeric(SCUPncSH_1_len) * 2.54, TRUE ~ scup_min_y2)
        )
    }
    
    # ---- Season 2 SF + season 3 BSB + season 2 scup (NC always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFncFH_seas2_op)) & date_adj <= yday(ymd(SFncFH_seas2_cl)) ~ as.numeric(SFncFH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFncPR_seas2_op)) & date_adj <= yday(ymd(SFncPR_seas2_cl)) ~ as.numeric(SFncPR_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFncSH_seas2_op)) & date_adj <= yday(ymd(SFncSH_seas2_cl)) ~ as.numeric(SFncSH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFncFH_seas2_op)) & date_adj <= yday(ymd(SFncFH_seas2_cl)) ~ as.numeric(SFncFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFncPR_seas2_op)) & date_adj <= yday(ymd(SFncPR_seas2_cl)) ~ as.numeric(SFncPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFncSH_seas2_op)) & date_adj <= yday(ymd(SFncSH_seas2_cl)) ~ as.numeric(SFncSH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBncFH_seas3_op)) & date_adj <= yday(ymd(BSBncFH_seas3_cl)) ~ as.numeric(BSBncFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBncPR_seas3_op)) & date_adj <= yday(ymd(BSBncPR_seas3_cl)) ~ as.numeric(BSBncPR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBncSH_seas3_op)) & date_adj <= yday(ymd(BSBncSH_seas3_cl)) ~ as.numeric(BSBncSH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBncFH_seas3_op)) & date_adj <= yday(ymd(BSBncFH_seas3_cl)) ~ as.numeric(BSBncFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBncPR_seas3_op)) & date_adj <= yday(ymd(BSBncPR_seas3_cl)) ~ as.numeric(BSBncPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBncSH_seas3_op)) & date_adj <= yday(ymd(BSBncSH_seas3_cl)) ~ as.numeric(BSBncSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPncFH_seas2_op)) & date_adj <= yday(ymd(SCUPncFH_seas2_cl)) ~ as.numeric(SCUPncFH_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPncPR_seas2_op)) & date_adj <= yday(ymd(SCUPncPR_seas2_cl)) ~ as.numeric(SCUPncPR_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPncSH_seas2_op)) & date_adj <= yday(ymd(SCUPncSH_seas2_cl)) ~ as.numeric(SCUPncSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPncFH_seas2_op)) & date_adj <= yday(ymd(SCUPncFH_seas2_cl)) ~ as.numeric(SCUPncFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPncPR_seas2_op)) & date_adj <= yday(ymd(SCUPncPR_seas2_cl)) ~ as.numeric(SCUPncPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPncSH_seas2_op)) & date_adj <= yday(ymd(SCUPncSH_seas2_cl)) ~ as.numeric(SCUPncSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "NJ") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFnj_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFnj_seas1_op)) & date_adj <= yday(ymd(SFnj_seas1_cl)) ~ as.numeric(SFnj_1_bag), TRUE ~ 0),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFnj_seas1_op)) & date_adj <= yday(ymd(SFnj_seas1_cl)) ~ as.numeric(SFnj_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnjFH_seas1_op)) & date_adj <= yday(ymd(SFnjFH_seas1_cl)) ~ as.numeric(SFnjFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnjPR_seas1_op)) & date_adj <= yday(ymd(SFnjPR_seas1_cl)) ~ as.numeric(SFnjPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnjSH_seas1_op)) & date_adj <= yday(ymd(SFnjSH_seas1_cl)) ~ as.numeric(SFnjSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnjFH_seas1_op)) & date_adj <= yday(ymd(SFnjFH_seas1_cl)) ~ as.numeric(SFnjFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnjPR_seas1_op)) & date_adj <= yday(ymd(SFnjPR_seas1_cl)) ~ as.numeric(SFnjPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnjSH_seas1_op)) & date_adj <= yday(ymd(SFnjSH_seas1_cl)) ~ as.numeric(SFnjSH_1_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBnj_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas1_op)) & date_adj <= yday(ymd(BSBnj_seas1_cl)) ~ as.numeric(BSBnj_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas2_op)) & date_adj <= yday(ymd(BSBnj_seas2_cl)) ~ as.numeric(BSBnj_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas3_op)) & date_adj <= yday(ymd(BSBnj_seas3_cl)) ~ as.numeric(BSBnj_3_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas4_op)) & date_adj <= yday(ymd(BSBnj_seas4_cl)) ~ as.numeric(BSBnj_4_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas1_op)) & date_adj <= yday(ymd(BSBnj_seas1_cl)) ~ as.numeric(BSBnj_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas2_op)) & date_adj <= yday(ymd(BSBnj_seas2_cl)) ~ as.numeric(BSBnj_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas3_op)) & date_adj <= yday(ymd(BSBnj_seas3_cl)) ~ as.numeric(BSBnj_3_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBnj_seas4_op)) & date_adj <= yday(ymd(BSBnj_seas4_cl)) ~ as.numeric(BSBnj_4_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas1_op)) & date_adj <= yday(ymd(BSBnjFH_seas1_cl)) ~ as.numeric(BSBnjFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas1_op)) & date_adj <= yday(ymd(BSBnjPR_seas1_cl)) ~ as.numeric(BSBnjPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas1_op)) & date_adj <= yday(ymd(BSBnjSH_seas1_cl)) ~ as.numeric(BSBnjSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas2_op)) & date_adj <= yday(ymd(BSBnjFH_seas2_cl)) ~ as.numeric(BSBnjFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas2_op)) & date_adj <= yday(ymd(BSBnjPR_seas2_cl)) ~ as.numeric(BSBnjPR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas2_op)) & date_adj <= yday(ymd(BSBnjSH_seas2_cl)) ~ as.numeric(BSBnjSH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas3_op)) & date_adj <= yday(ymd(BSBnjFH_seas3_cl)) ~ as.numeric(BSBnjFH_3_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas3_op)) & date_adj <= yday(ymd(BSBnjPR_seas3_cl)) ~ as.numeric(BSBnjPR_3_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas3_op)) & date_adj <= yday(ymd(BSBnjSH_seas3_cl)) ~ as.numeric(BSBnjSH_3_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas4_op)) & date_adj <= yday(ymd(BSBnjFH_seas4_cl)) ~ as.numeric(BSBnjFH_4_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas4_op)) & date_adj <= yday(ymd(BSBnjPR_seas4_cl)) ~ as.numeric(BSBnjPR_4_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas4_op)) & date_adj <= yday(ymd(BSBnjSH_seas4_cl)) ~ as.numeric(BSBnjSH_4_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas1_op)) & date_adj <= yday(ymd(BSBnjFH_seas1_cl)) ~ as.numeric(BSBnjFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas1_op)) & date_adj <= yday(ymd(BSBnjPR_seas1_cl)) ~ as.numeric(BSBnjPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas1_op)) & date_adj <= yday(ymd(BSBnjSH_seas1_cl)) ~ as.numeric(BSBnjSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas2_op)) & date_adj <= yday(ymd(BSBnjFH_seas2_cl)) ~ as.numeric(BSBnjFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas2_op)) & date_adj <= yday(ymd(BSBnjPR_seas2_cl)) ~ as.numeric(BSBnjPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas2_op)) & date_adj <= yday(ymd(BSBnjSH_seas2_cl)) ~ as.numeric(BSBnjSH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas3_op)) & date_adj <= yday(ymd(BSBnjFH_seas3_cl)) ~ as.numeric(BSBnjFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas3_op)) & date_adj <= yday(ymd(BSBnjPR_seas3_cl)) ~ as.numeric(BSBnjPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas3_op)) & date_adj <= yday(ymd(BSBnjSH_seas3_cl)) ~ as.numeric(BSBnjSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas4_op)) & date_adj <= yday(ymd(BSBnjFH_seas4_cl)) ~ as.numeric(BSBnjFH_4_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas4_op)) & date_adj <= yday(ymd(BSBnjPR_seas4_cl)) ~ as.numeric(BSBnjPR_4_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas4_op)) & date_adj <= yday(ymd(BSBnjSH_seas4_cl)) ~ as.numeric(BSBnjSH_4_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Scup (NJ) ----
    if (exists("SCUPnj_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPnj_seas1_op)) & date_adj <= yday(ymd(SCUPnj_seas1_cl)) ~ as.numeric(SCUPnj_1_bag), TRUE ~ 0),
          scup_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPnj_seas1_op)) & date_adj <= yday(ymd(SCUPnj_seas1_cl)) ~ as.numeric(SCUPnj_1_len) * 2.54, TRUE ~ 254),
          scup_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPnj_seas2_op)) & date_adj <= yday(ymd(SCUPnj_seas2_cl)) ~ as.numeric(SCUPnj_2_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPnj_seas2_op)) & date_adj <= yday(ymd(SCUPnj_seas2_cl)) ~ as.numeric(SCUPnj_2_len) * 2.54, TRUE ~ scup_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnjFH_seas1_op)) & date_adj <= yday(ymd(SCUPnjFH_seas1_cl)) ~ as.numeric(SCUPnjFH_1_bag), TRUE ~ 0),
          scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnjPR_seas1_op)) & date_adj <= yday(ymd(SCUPnjPR_seas1_cl)) ~ as.numeric(SCUPnjPR_1_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnjSH_seas1_op)) & date_adj <= yday(ymd(SCUPnjSH_seas1_cl)) ~ as.numeric(SCUPnjSH_1_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnjFH_seas1_op)) & date_adj <= yday(ymd(SCUPnjFH_seas1_cl)) ~ as.numeric(SCUPnjFH_1_len) * 2.54, TRUE ~ 254),
          scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnjPR_seas1_op)) & date_adj <= yday(ymd(SCUPnjPR_seas1_cl)) ~ as.numeric(SCUPnjPR_1_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnjSH_seas1_op)) & date_adj <= yday(ymd(SCUPnjSH_seas1_cl)) ~ as.numeric(SCUPnjSH_1_len) * 2.54, TRUE ~ scup_min_y2),
          scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnjFH_seas2_op)) & date_adj <= yday(ymd(SCUPnjFH_seas2_cl)) ~ as.numeric(SCUPnjFH_2_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnjPR_seas2_op)) & date_adj <= yday(ymd(SCUPnjPR_seas2_cl)) ~ as.numeric(SCUPnjPR_2_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnjSH_seas2_op)) & date_adj <= yday(ymd(SCUPnjSH_seas2_cl)) ~ as.numeric(SCUPnjSH_2_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnjFH_seas2_op)) & date_adj <= yday(ymd(SCUPnjFH_seas2_cl)) ~ as.numeric(SCUPnjFH_2_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnjPR_seas2_op)) & date_adj <= yday(ymd(SCUPnjPR_seas2_cl)) ~ as.numeric(SCUPnjPR_2_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnjSH_seas2_op)) & date_adj <= yday(ymd(SCUPnjSH_seas2_cl)) ~ as.numeric(SCUPnjSH_2_len) * 2.54, TRUE ~ scup_min_y2)
        )
    }
    
    # ---- Season 2 SF + season 5 BSB + season 3 scup (NJ always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnjFH_seas2_op)) & date_adj <= yday(ymd(SFnjFH_seas2_cl)) ~ as.numeric(SFnjFH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnjPR_seas2_op)) & date_adj <= yday(ymd(SFnjPR_seas2_cl)) ~ as.numeric(SFnjPR_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnjSH_seas2_op)) & date_adj <= yday(ymd(SFnjSH_seas2_cl)) ~ as.numeric(SFnjSH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnjFH_seas2_op)) & date_adj <= yday(ymd(SFnjFH_seas2_cl)) ~ as.numeric(SFnjFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnjPR_seas2_op)) & date_adj <= yday(ymd(SFnjPR_seas2_cl)) ~ as.numeric(SFnjPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnjSH_seas2_op)) & date_adj <= yday(ymd(SFnjSH_seas2_cl)) ~ as.numeric(SFnjSH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas5_op)) & date_adj <= yday(ymd(BSBnjFH_seas5_cl)) ~ as.numeric(BSBnjFH_5_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas5_op)) & date_adj <= yday(ymd(BSBnjPR_seas5_cl)) ~ as.numeric(BSBnjPR_5_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas5_op)) & date_adj <= yday(ymd(BSBnjSH_seas5_cl)) ~ as.numeric(BSBnjSH_5_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnjFH_seas5_op)) & date_adj <= yday(ymd(BSBnjFH_seas5_cl)) ~ as.numeric(BSBnjFH_5_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnjPR_seas5_op)) & date_adj <= yday(ymd(BSBnjPR_seas5_cl)) ~ as.numeric(BSBnjPR_5_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnjSH_seas5_op)) & date_adj <= yday(ymd(BSBnjSH_seas5_cl)) ~ as.numeric(BSBnjSH_5_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnjFH_seas3_op)) & date_adj <= yday(ymd(SCUPnjFH_seas3_cl)) ~ as.numeric(SCUPnjFH_3_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnjPR_seas3_op)) & date_adj <= yday(ymd(SCUPnjPR_seas3_cl)) ~ as.numeric(SCUPnjPR_3_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnjSH_seas3_op)) & date_adj <= yday(ymd(SCUPnjSH_seas3_cl)) ~ as.numeric(SCUPnjSH_3_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnjFH_seas3_op)) & date_adj <= yday(ymd(SCUPnjFH_seas3_cl)) ~ as.numeric(SCUPnjFH_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnjPR_seas3_op)) & date_adj <= yday(ymd(SCUPnjPR_seas3_cl)) ~ as.numeric(SCUPnjPR_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnjSH_seas3_op)) & date_adj <= yday(ymd(SCUPnjSH_seas3_cl)) ~ as.numeric(SCUPnjSH_3_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "NY") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFny_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFny_seas1_op)) & date_adj <= yday(ymd(SFny_seas1_cl)) ~ as.numeric(SFny_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFny_seas2_op)) & date_adj <= yday(ymd(SFny_seas2_cl)) ~ as.numeric(SFny_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFny_seas1_op)) & date_adj <= yday(ymd(SFny_seas1_cl)) ~ as.numeric(SFny_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFny_seas2_op)) & date_adj <= yday(ymd(SFny_seas2_cl)) ~ as.numeric(SFny_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnyFH_seas1_op)) & date_adj <= yday(ymd(SFnyFH_seas1_cl)) ~ as.numeric(SFnyFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnyPR_seas1_op)) & date_adj <= yday(ymd(SFnyPR_seas1_cl)) ~ as.numeric(SFnyPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnySH_seas1_op)) & date_adj <= yday(ymd(SFnySH_seas1_cl)) ~ as.numeric(SFnySH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnyFH_seas2_op)) & date_adj <= yday(ymd(SFnyFH_seas2_cl)) ~ as.numeric(SFnyFH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnyPR_seas2_op)) & date_adj <= yday(ymd(SFnyPR_seas2_cl)) ~ as.numeric(SFnyPR_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnySH_seas2_op)) & date_adj <= yday(ymd(SFnySH_seas2_cl)) ~ as.numeric(SFnySH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnyFH_seas1_op)) & date_adj <= yday(ymd(SFnyFH_seas1_cl)) ~ as.numeric(SFnyFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnyPR_seas1_op)) & date_adj <= yday(ymd(SFnyPR_seas1_cl)) ~ as.numeric(SFnyPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnySH_seas1_op)) & date_adj <= yday(ymd(SFnySH_seas1_cl)) ~ as.numeric(SFnySH_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnyFH_seas2_op)) & date_adj <= yday(ymd(SFnyFH_seas2_cl)) ~ as.numeric(SFnyFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnyPR_seas2_op)) & date_adj <= yday(ymd(SFnyPR_seas2_cl)) ~ as.numeric(SFnyPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnySH_seas2_op)) & date_adj <= yday(ymd(SFnySH_seas2_cl)) ~ as.numeric(SFnySH_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBny_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBny_seas1_op)) & date_adj <= yday(ymd(BSBny_seas1_cl)) ~ as.numeric(BSBny_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBny_seas2_op)) & date_adj <= yday(ymd(BSBny_seas2_cl)) ~ as.numeric(BSBny_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBny_seas1_op)) & date_adj <= yday(ymd(BSBny_seas1_cl)) ~ as.numeric(BSBny_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBny_seas2_op)) & date_adj <= yday(ymd(BSBny_seas2_cl)) ~ as.numeric(BSBny_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnyFH_seas1_op)) & date_adj <= yday(ymd(BSBnyFH_seas1_cl)) ~ as.numeric(BSBnyFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnyPR_seas1_op)) & date_adj <= yday(ymd(BSBnyPR_seas1_cl)) ~ as.numeric(BSBnyPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnySH_seas1_op)) & date_adj <= yday(ymd(BSBnySH_seas1_cl)) ~ as.numeric(BSBnySH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnyFH_seas2_op)) & date_adj <= yday(ymd(BSBnyFH_seas2_cl)) ~ as.numeric(BSBnyFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnyPR_seas2_op)) & date_adj <= yday(ymd(BSBnyPR_seas2_cl)) ~ as.numeric(BSBnyPR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnySH_seas2_op)) & date_adj <= yday(ymd(BSBnySH_seas2_cl)) ~ as.numeric(BSBnySH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnyFH_seas1_op)) & date_adj <= yday(ymd(BSBnyFH_seas1_cl)) ~ as.numeric(BSBnyFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnyPR_seas1_op)) & date_adj <= yday(ymd(BSBnyPR_seas1_cl)) ~ as.numeric(BSBnyPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnySH_seas1_op)) & date_adj <= yday(ymd(BSBnySH_seas1_cl)) ~ as.numeric(BSBnySH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnyFH_seas2_op)) & date_adj <= yday(ymd(BSBnyFH_seas2_cl)) ~ as.numeric(BSBnyFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnyPR_seas2_op)) & date_adj <= yday(ymd(BSBnyPR_seas2_cl)) ~ as.numeric(BSBnyPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnySH_seas2_op)) & date_adj <= yday(ymd(BSBnySH_seas2_cl)) ~ as.numeric(BSBnySH_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Season 3 SF + season 3 BSB + all scup (NY always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnyFH_seas3_op)) & date_adj <= yday(ymd(SFnyFH_seas3_cl)) ~ as.numeric(SFnyFH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnyPR_seas3_op)) & date_adj <= yday(ymd(SFnyPR_seas3_cl)) ~ as.numeric(SFnyPR_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnySH_seas3_op)) & date_adj <= yday(ymd(SFnySH_seas3_cl)) ~ as.numeric(SFnySH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFnyFH_seas3_op)) & date_adj <= yday(ymd(SFnyFH_seas3_cl)) ~ as.numeric(SFnyFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFnyPR_seas3_op)) & date_adj <= yday(ymd(SFnyPR_seas3_cl)) ~ as.numeric(SFnyPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFnySH_seas3_op)) & date_adj <= yday(ymd(SFnySH_seas3_cl)) ~ as.numeric(SFnySH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnyFH_seas3_op)) & date_adj <= yday(ymd(BSBnyFH_seas3_cl)) ~ as.numeric(BSBnyFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnyPR_seas3_op)) & date_adj <= yday(ymd(BSBnyPR_seas3_cl)) ~ as.numeric(BSBnyPR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnySH_seas3_op)) & date_adj <= yday(ymd(BSBnySH_seas3_cl)) ~ as.numeric(BSBnySH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBnyFH_seas3_op)) & date_adj <= yday(ymd(BSBnyFH_seas3_cl)) ~ as.numeric(BSBnyFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBnyPR_seas3_op)) & date_adj <= yday(ymd(BSBnyPR_seas3_cl)) ~ as.numeric(BSBnyPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBnySH_seas3_op)) & date_adj <= yday(ymd(BSBnySH_seas3_cl)) ~ as.numeric(BSBnySH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas1_op)) & date_adj <= yday(ymd(SCUPnyFH_seas1_cl)) ~ as.numeric(SCUPnyFH_1_bag), TRUE ~ 0),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas1_op)) & date_adj <= yday(ymd(SCUPnyFH_seas1_cl)) ~ as.numeric(SCUPnyFH_1_len) * 2.54, TRUE ~ 254),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas2_op)) & date_adj <= yday(ymd(SCUPnyFH_seas2_cl)) ~ as.numeric(SCUPnyFH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas2_op)) & date_adj <= yday(ymd(SCUPnyFH_seas2_cl)) ~ as.numeric(SCUPnyFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas3_op)) & date_adj <= yday(ymd(SCUPnyFH_seas3_cl)) ~ as.numeric(SCUPnyFH_3_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas3_op)) & date_adj <= yday(ymd(SCUPnyFH_seas3_cl)) ~ as.numeric(SCUPnyFH_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas4_op)) & date_adj <= yday(ymd(SCUPnyFH_seas4_cl)) ~ as.numeric(SCUPnyFH_4_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPnyFH_seas4_op)) & date_adj <= yday(ymd(SCUPnyFH_seas4_cl)) ~ as.numeric(SCUPnyFH_4_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnyPR_seas1_op)) & date_adj <= yday(ymd(SCUPnyPR_seas1_cl)) ~ as.numeric(SCUPnyPR_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnyPR_seas1_op)) & date_adj <= yday(ymd(SCUPnyPR_seas1_cl)) ~ as.numeric(SCUPnyPR_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnyPR_seas2_op)) & date_adj <= yday(ymd(SCUPnyPR_seas2_cl)) ~ as.numeric(SCUPnyPR_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPnyPR_seas2_op)) & date_adj <= yday(ymd(SCUPnyPR_seas2_cl)) ~ as.numeric(SCUPnyPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnySH_seas1_op)) & date_adj <= yday(ymd(SCUPnySH_seas1_cl)) ~ as.numeric(SCUPnySH_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnySH_seas1_op)) & date_adj <= yday(ymd(SCUPnySH_seas1_cl)) ~ as.numeric(SCUPnySH_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnySH_seas2_op)) & date_adj <= yday(ymd(SCUPnySH_seas2_cl)) ~ as.numeric(SCUPnySH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPnySH_seas2_op)) & date_adj <= yday(ymd(SCUPnySH_seas2_cl)) ~ as.numeric(SCUPnySH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "RI") {
    
    # ---- Summer Flounder (SF) ----
    # Note: RI SF season 1 may be unified or mode-specific; season 2 always applied below
    if (exists("SFri_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFri_seas1_op)) & date_adj <= yday(ymd(SFri_seas1_cl)) ~ as.numeric(SFri_1_bag), TRUE ~ 0),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFri_seas1_op)) & date_adj <= yday(ymd(SFri_seas1_cl)) ~ as.numeric(SFri_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFriFH_seas1_op)) & date_adj <= yday(ymd(SFriFH_seas1_cl)) ~ as.numeric(SFriFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFriPR_seas1_op)) & date_adj <= yday(ymd(SFriPR_seas1_cl)) ~ as.numeric(SFriPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFriSH_seas1_op)) & date_adj <= yday(ymd(SFriSH_seas1_cl)) ~ as.numeric(SFriSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFriFH_seas1_op)) & date_adj <= yday(ymd(SFriFH_seas1_cl)) ~ as.numeric(SFriFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFriPR_seas1_op)) & date_adj <= yday(ymd(SFriPR_seas1_cl)) ~ as.numeric(SFriPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFriSH_seas1_op)) & date_adj <= yday(ymd(SFriSH_seas1_cl)) ~ as.numeric(SFriSH_1_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Season 2 SF + all BSB + all scup (RI always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFriFH_seas2_op)) & date_adj <= yday(ymd(SFriFH_seas2_cl)) ~ as.numeric(SFriFH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFriPR_seas2_op)) & date_adj <= yday(ymd(SFriPR_seas2_cl)) ~ as.numeric(SFriPR_2_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFriSH_seas2_op)) & date_adj <= yday(ymd(SFriSH_seas2_cl)) ~ as.numeric(SFriSH_2_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFriFH_seas2_op)) & date_adj <= yday(ymd(SFriFH_seas2_cl)) ~ as.numeric(SFriFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFriPR_seas2_op)) & date_adj <= yday(ymd(SFriPR_seas2_cl)) ~ as.numeric(SFriPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFriSH_seas2_op)) & date_adj <= yday(ymd(SFriSH_seas2_cl)) ~ as.numeric(SFriSH_2_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBriFH_seas1_op)) & date_adj <= yday(ymd(BSBriFH_seas1_cl)) ~ as.numeric(BSBriFH_1_bag), TRUE ~ 0),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBriPR_seas1_op)) & date_adj <= yday(ymd(BSBriPR_seas1_cl)) ~ as.numeric(BSBriPR_1_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBriSH_seas1_op)) & date_adj <= yday(ymd(BSBriSH_seas1_cl)) ~ as.numeric(BSBriSH_1_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBriFH_seas2_op)) & date_adj <= yday(ymd(BSBriFH_seas2_cl)) ~ as.numeric(BSBriFH_2_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBriPR_seas2_op)) & date_adj <= yday(ymd(BSBriPR_seas2_cl)) ~ as.numeric(BSBriPR_2_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBriSH_seas2_op)) & date_adj <= yday(ymd(BSBriSH_seas2_cl)) ~ as.numeric(BSBriSH_2_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBriFH_seas3_op)) & date_adj <= yday(ymd(BSBriFH_seas3_cl)) ~ as.numeric(BSBriFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBriPR_seas3_op)) & date_adj <= yday(ymd(BSBriPR_seas3_cl)) ~ as.numeric(BSBriPR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBriSH_seas3_op)) & date_adj <= yday(ymd(BSBriSH_seas3_cl)) ~ as.numeric(BSBriSH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBriFH_seas1_op)) & date_adj <= yday(ymd(BSBriFH_seas1_cl)) ~ as.numeric(BSBriFH_1_len) * 2.54, TRUE ~ 254),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBriPR_seas1_op)) & date_adj <= yday(ymd(BSBriPR_seas1_cl)) ~ as.numeric(BSBriPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBriSH_seas1_op)) & date_adj <= yday(ymd(BSBriSH_seas1_cl)) ~ as.numeric(BSBriSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBriFH_seas2_op)) & date_adj <= yday(ymd(BSBriFH_seas2_cl)) ~ as.numeric(BSBriFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBriPR_seas2_op)) & date_adj <= yday(ymd(BSBriPR_seas2_cl)) ~ as.numeric(BSBriPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBriSH_seas2_op)) & date_adj <= yday(ymd(BSBriSH_seas2_cl)) ~ as.numeric(BSBriSH_2_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBriFH_seas3_op)) & date_adj <= yday(ymd(BSBriFH_seas3_cl)) ~ as.numeric(BSBriFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBriPR_seas3_op)) & date_adj <= yday(ymd(BSBriPR_seas3_cl)) ~ as.numeric(BSBriPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBriSH_seas3_op)) & date_adj <= yday(ymd(BSBriSH_seas3_cl)) ~ as.numeric(BSBriSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas1_op)) & date_adj <= yday(ymd(SCUPriFH_seas1_cl)) ~ as.numeric(SCUPriFH_1_bag), TRUE ~ 0),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas1_op)) & date_adj <= yday(ymd(SCUPriFH_seas1_cl)) ~ as.numeric(SCUPriFH_1_len) * 2.54, TRUE ~ 254),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas2_op)) & date_adj <= yday(ymd(SCUPriFH_seas2_cl)) ~ as.numeric(SCUPriFH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas2_op)) & date_adj <= yday(ymd(SCUPriFH_seas2_cl)) ~ as.numeric(SCUPriFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas3_op)) & date_adj <= yday(ymd(SCUPriFH_seas3_cl)) ~ as.numeric(SCUPriFH_3_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas3_op)) & date_adj <= yday(ymd(SCUPriFH_seas3_cl)) ~ as.numeric(SCUPriFH_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas4_op)) & date_adj <= yday(ymd(SCUPriFH_seas4_cl)) ~ as.numeric(SCUPriFH_4_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPriFH_seas4_op)) & date_adj <= yday(ymd(SCUPriFH_seas4_cl)) ~ as.numeric(SCUPriFH_4_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPriPR_seas1_op)) & date_adj <= yday(ymd(SCUPriPR_seas1_cl)) ~ as.numeric(SCUPriPR_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPriPR_seas1_op)) & date_adj <= yday(ymd(SCUPriPR_seas1_cl)) ~ as.numeric(SCUPriPR_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPriPR_seas2_op)) & date_adj <= yday(ymd(SCUPriPR_seas2_cl)) ~ as.numeric(SCUPriPR_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPriPR_seas2_op)) & date_adj <= yday(ymd(SCUPriPR_seas2_cl)) ~ as.numeric(SCUPriPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPriSH_seas1_op)) & date_adj <= yday(ymd(SCUPriSH_seas1_cl)) ~ as.numeric(SCUPriSH_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPriSH_seas1_op)) & date_adj <= yday(ymd(SCUPriSH_seas1_cl)) ~ as.numeric(SCUPriSH_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPriSH_seas2_op)) & date_adj <= yday(ymd(SCUPriSH_seas2_cl)) ~ as.numeric(SCUPriSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPriSH_seas2_op)) & date_adj <= yday(ymd(SCUPriSH_seas2_cl)) ~ as.numeric(SCUPriSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else if (state == "VA") {
    
    # ---- Summer Flounder (SF) ----
    if (exists("SFva_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFva_seas1_op)) & date_adj <= yday(ymd(SFva_seas1_cl)) ~ as.numeric(SFva_1_bag), TRUE ~ 0),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFva_seas1_op)) & date_adj <= yday(ymd(SFva_seas1_cl)) ~ as.numeric(SFva_1_len) * 2.54, TRUE ~ 254),
          fluke_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SFva_seas2_op)) & date_adj <= yday(ymd(SFva_seas2_cl)) ~ as.numeric(SFva_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SFva_seas2_op)) & date_adj <= yday(ymd(SFva_seas2_cl)) ~ as.numeric(SFva_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFvaFH_seas1_op)) & date_adj <= yday(ymd(SFvaFH_seas1_cl)) ~ as.numeric(SFvaFH_1_bag), TRUE ~ 0),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFvaPR_seas1_op)) & date_adj <= yday(ymd(SFvaPR_seas1_cl)) ~ as.numeric(SFvaPR_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFvaSH_seas1_op)) & date_adj <= yday(ymd(SFvaSH_seas1_cl)) ~ as.numeric(SFvaSH_1_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFvaFH_seas2_op)) & date_adj <= yday(ymd(SFvaFH_seas2_cl)) ~ as.numeric(SFvaFH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFvaPR_seas2_op)) & date_adj <= yday(ymd(SFvaPR_seas2_cl)) ~ as.numeric(SFvaPR_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFvaSH_seas2_op)) & date_adj <= yday(ymd(SFvaSH_seas2_cl)) ~ as.numeric(SFvaSH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFvaFH_seas1_op)) & date_adj <= yday(ymd(SFvaFH_seas1_cl)) ~ as.numeric(SFvaFH_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFvaPR_seas1_op)) & date_adj <= yday(ymd(SFvaPR_seas1_cl)) ~ as.numeric(SFvaPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFvaSH_seas1_op)) & date_adj <= yday(ymd(SFvaSH_seas1_cl)) ~ as.numeric(SFvaSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFvaFH_seas2_op)) & date_adj <= yday(ymd(SFvaFH_seas2_cl)) ~ as.numeric(SFvaFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFvaPR_seas2_op)) & date_adj <= yday(ymd(SFvaPR_seas2_cl)) ~ as.numeric(SFvaPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFvaSH_seas2_op)) & date_adj <= yday(ymd(SFvaSH_seas2_cl)) ~ as.numeric(SFvaSH_2_len) * 2.54, TRUE ~ fluke_min_y2)
        )
    }
    
    # ---- Black Sea Bass (BSB) ----
    if (exists("BSBva_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBva_seas1_op)) & date_adj <= yday(ymd(BSBva_seas1_cl)) ~ as.numeric(BSBva_1_bag), TRUE ~ 0),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBva_seas1_op)) & date_adj <= yday(ymd(BSBva_seas1_cl)) ~ as.numeric(BSBva_1_len) * 2.54, TRUE ~ 254),
          bsb_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBva_seas2_op)) & date_adj <= yday(ymd(BSBva_seas2_cl)) ~ as.numeric(BSBva_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(date_adj >= yday(ymd(BSBva_seas2_op)) & date_adj <= yday(ymd(BSBva_seas2_cl)) ~ as.numeric(BSBva_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBvaFH_seas1_op)) & date_adj <= yday(ymd(BSBvaFH_seas1_cl)) ~ as.numeric(BSBvaFH_1_bag), TRUE ~ 0),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBvaPR_seas1_op)) & date_adj <= yday(ymd(BSBvaPR_seas1_cl)) ~ as.numeric(BSBvaPR_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBvaSH_seas1_op)) & date_adj <= yday(ymd(BSBvaSH_seas1_cl)) ~ as.numeric(BSBvaSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBvaFH_seas2_op)) & date_adj <= yday(ymd(BSBvaFH_seas2_cl)) ~ as.numeric(BSBvaFH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBvaPR_seas2_op)) & date_adj <= yday(ymd(BSBvaPR_seas2_cl)) ~ as.numeric(BSBvaPR_2_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBvaSH_seas2_op)) & date_adj <= yday(ymd(BSBvaSH_seas2_cl)) ~ as.numeric(BSBvaSH_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBvaFH_seas1_op)) & date_adj <= yday(ymd(BSBvaFH_seas1_cl)) ~ as.numeric(BSBvaFH_1_len) * 2.54, TRUE ~ 254),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBvaPR_seas1_op)) & date_adj <= yday(ymd(BSBvaPR_seas1_cl)) ~ as.numeric(BSBvaPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBvaSH_seas1_op)) & date_adj <= yday(ymd(BSBvaSH_seas1_cl)) ~ as.numeric(BSBvaSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBvaFH_seas2_op)) & date_adj <= yday(ymd(BSBvaFH_seas2_cl)) ~ as.numeric(BSBvaFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBvaPR_seas2_op)) & date_adj <= yday(ymd(BSBvaPR_seas2_cl)) ~ as.numeric(BSBvaPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBvaSH_seas2_op)) & date_adj <= yday(ymd(BSBvaSH_seas2_cl)) ~ as.numeric(BSBvaSH_2_len) * 2.54, TRUE ~ bsb_min_y2)
        )
    }
    
    # ---- Scup (VA) ----
    if (exists("SCUPva_seas1_op")) {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPva_seas1_op)) & date_adj <= yday(ymd(SCUPva_seas1_cl)) ~ as.numeric(SCUPva_1_bag), TRUE ~ 0),
          scup_min_y2 = dplyr::case_when(date_adj >= yday(ymd(SCUPva_seas1_op)) & date_adj <= yday(ymd(SCUPva_seas1_cl)) ~ as.numeric(SCUPva_1_len) * 2.54, TRUE ~ 254)
        )
    } else {
      directed_trips <- directed_trips %>%
        dplyr::mutate(
          scup_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPvaFH_seas1_op)) & date_adj <= yday(ymd(SCUPvaFH_seas1_cl)) ~ as.numeric(SCUPvaFH_1_bag), TRUE ~ 0),
          scup_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPvaPR_seas1_op)) & date_adj <= yday(ymd(SCUPvaPR_seas1_cl)) ~ as.numeric(SCUPvaPR_1_bag), TRUE ~ scup_bag_y2),
          scup_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPvaSH_seas1_op)) & date_adj <= yday(ymd(SCUPvaSH_seas1_cl)) ~ as.numeric(SCUPvaSH_1_bag), TRUE ~ scup_bag_y2),
          scup_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPvaFH_seas1_op)) & date_adj <= yday(ymd(SCUPvaFH_seas1_cl)) ~ as.numeric(SCUPvaFH_1_len) * 2.54, TRUE ~ 254),
          scup_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPvaPR_seas1_op)) & date_adj <= yday(ymd(SCUPvaPR_seas1_cl)) ~ as.numeric(SCUPvaPR_1_len) * 2.54, TRUE ~ scup_min_y2),
          scup_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPvaSH_seas1_op)) & date_adj <= yday(ymd(SCUPvaSH_seas1_cl)) ~ as.numeric(SCUPvaSH_1_len) * 2.54, TRUE ~ scup_min_y2)
        )
    }
    
    # ---- Season 3 SF + season 3 BSB + season 2 scup (VA always applies these) ----
    directed_trips <- directed_trips %>%
      dplyr::mutate(
        fluke_bag_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFvaFH_seas3_op)) & date_adj <= yday(ymd(SFvaFH_seas3_cl)) ~ as.numeric(SFvaFH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFvaPR_seas3_op)) & date_adj <= yday(ymd(SFvaPR_seas3_cl)) ~ as.numeric(SFvaPR_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFvaSH_seas3_op)) & date_adj <= yday(ymd(SFvaSH_seas3_cl)) ~ as.numeric(SFvaSH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_min_y2 = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFvaFH_seas3_op)) & date_adj <= yday(ymd(SFvaFH_seas3_cl)) ~ as.numeric(SFvaFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFvaPR_seas3_op)) & date_adj <= yday(ymd(SFvaPR_seas3_cl)) ~ as.numeric(SFvaPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2 = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFvaSH_seas3_op)) & date_adj <= yday(ymd(SFvaSH_seas3_cl)) ~ as.numeric(SFvaSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBvaFH_seas3_op)) & date_adj <= yday(ymd(BSBvaFH_seas3_cl)) ~ as.numeric(BSBvaFH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBvaPR_seas3_op)) & date_adj <= yday(ymd(BSBvaPR_seas3_cl)) ~ as.numeric(BSBvaPR_3_bag), TRUE ~ bsb_bag_y2),
        bsb_bag_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBvaSH_seas3_op)) & date_adj <= yday(ymd(BSBvaSH_seas3_cl)) ~ as.numeric(BSBvaSH_3_bag), TRUE ~ bsb_bag_y2),
        bsb_min_y2   = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBvaFH_seas3_op)) & date_adj <= yday(ymd(BSBvaFH_seas3_cl)) ~ as.numeric(BSBvaFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBvaPR_seas3_op)) & date_adj <= yday(ymd(BSBvaPR_seas3_cl)) ~ as.numeric(BSBvaPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
        bsb_min_y2   = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBvaSH_seas3_op)) & date_adj <= yday(ymd(BSBvaSH_seas3_cl)) ~ as.numeric(BSBvaSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        scup_bag_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPvaFH_seas2_op)) & date_adj <= yday(ymd(SCUPvaFH_seas2_cl)) ~ as.numeric(SCUPvaFH_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPvaPR_seas2_op)) & date_adj <= yday(ymd(SCUPvaPR_seas2_cl)) ~ as.numeric(SCUPvaPR_2_bag), TRUE ~ scup_bag_y2),
        scup_bag_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPvaSH_seas2_op)) & date_adj <= yday(ymd(SCUPvaSH_seas2_cl)) ~ as.numeric(SCUPvaSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2  = dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPvaFH_seas2_op)) & date_adj <= yday(ymd(SCUPvaFH_seas2_cl)) ~ as.numeric(SCUPvaFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPvaPR_seas2_op)) & date_adj <= yday(ymd(SCUPvaPR_seas2_cl)) ~ as.numeric(SCUPvaPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_min_y2  = dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPvaSH_seas2_op)) & date_adj <= yday(ymd(SCUPvaSH_seas2_cl)) ~ as.numeric(SCUPvaSH_2_len) * 2.54, TRUE ~ scup_min_y2)
      )
    
  } else {
    stop(paste0("State '", state, "' is not recognized. Valid options are: CT, DE, MA, MD, NC, NJ, NY, RI, VA."))
  }
  
  return(directed_trips)
}