################################################################################
################################################################################
# Script:       project_rec_catch_batch_helpers_revised.R
# Purpose:      Batch-running helpers for the refactored projection code.
#               Provides a state x draw job grid, a single-job runner, and two
#               interchangeable batch runners - one sequential, one parallel.
#               The design goal of the refactor is visible here: inputs that
#               do not vary by job are read ONCE into `common_inputs` and
#               passed down, instead of being re-read inside every iteration
#               as the production path in Code/sim does.
# Inputs:       None directly; the functions read via
#               read_projection_common_inputs() and compute_projection().
# Outputs:      None directly. run_one_projection_job() may write per-job
#               output under output_cd when asked to.
# Dependencies: project_rec_catch_final_revised_v3.R must be sourced FIRST -
#               read_projection_common_inputs() and compute_projection() are
#               defined there, not here. Packages data.table, purrr, furrr.
# Pipeline:     Development/QA scratch - the candidate replacement for the
#               Code/sim projection path. Driven by run_projection_final.R.
#               Not called by any wrapper.
# Dev paths:    2 hardcoded absolute paths to a developer's local machine
#               (C:\ or E:\), both in commented-out lines (272, 273).
################################################################################
################################################################################

# project_rec_catch_batch_helpers.R
# Batch helpers for project_rec_catch_final.R.
# Source project_rec_catch_final.R first, because read_projection_common_inputs()
# and compute_projection() are defined there.

#' @title Build the grid of projection jobs to run
#' @description Produces one row per state x draw combination, with the
#'   run-level settings attached to every row so that a batch runner can map
#'   over the rows without carrying extra state. `modes` is stored as a list
#'   column because each cell holds a character vector rather than a scalar.
#' @param states Character vector of state codes to run.
#' @param draws Integer vector of draw numbers to run.
#' @param run_tag Label identifying this run; travels through to output file
#'   naming.
#' @param ndraws Number of choice occasions to simulate per stratum. Note this
#'   is the per-stratum simulation size, not the number of draws in `draws`.
#' @param modes Fishing modes to simulate: shore, private and for-hire.
#' @return A data.table with one row per job, columns state, draw, run_tag,
#'   ndraws and modes.
#' @examples
#' \dontrun{
#' make_projection_grid(states = c("MA", "RI"), draws = 1:10)
#' }
make_projection_grid <- function(states,
                                 draws,
                                 run_tag = "final",
                                 ndraws = 50L,
                                 modes = c("sh", "pr", "fh")) {
  grid <- data.table::CJ(state = states, draw = draws, sorted = FALSE)
  
  grid[, run_tag := rep(run_tag, .N)]
  grid[, ndraws  := rep(as.integer(ndraws), .N)]
  grid[, modes   := rep(list(modes), .N)]
  
  grid[]
}

#' @title Run the projection for a single state and draw
#' @description Thin wrapper around compute_projection() that supplies the
#'   per-job arguments and the shared inputs. Kept separate from the batch
#'   runners so the same job can be invoked by hand when debugging one
#'   state x draw.
#' @param state State code for this job.
#' @param draw Draw number for this job.
#' @param iterative_input_data_cd Directory holding data generated during the
#'   simulation (calibration outputs, converted .fst files).
#' @param input_data_cd Directory holding the MRIP and biological source data.
#' @param output_cd Where per-job output is written; defaults to an
#'   archive/projection_outputs folder beneath the iterative data directory.
#' @param ndraws Choice occasions simulated per stratum.
#' @param modes Fishing modes to simulate.
#' @param run_tag Label identifying this run.
#' @param write_intermediate Whether to persist intermediate objects; off by
#'   default because they are large.
#' @param common_inputs Pre-read shared inputs. Passing these is the point of
#'   the refactor - omitting them forces a re-read for every job.
#' @param base_outcomes_date_tag Date stamp embedded in the calibration
#'   base-outcomes filenames. Hardcoded default "4_16_26" ties this script to
#'   one calibration vintage; a newer calibration needs this changed.
#' @param quiet Suppress the per-job progress message.
#' @return A data.table of projected outcomes for this state x draw.
run_one_projection_job <- function(state,
                                   draw,
                                   iterative_input_data_cd,
                                   input_data_cd,
                                   output_cd = file.path(iterative_input_data_cd, "archive/projection_outputs"),
                                   ndraws = 50L,
                                   modes = c("sh", "pr", "fh"),
                                   run_tag = "final",
                                   write_intermediate = FALSE,
                                   common_inputs = NULL,
                                   base_outcomes_date_tag = "4_16_26",
                                   quiet = FALSE) {
  if (!quiet) message("Running ", state, " draw ", draw)

  compute_projection(
    st = state,
    dr = draw,
    iterative_input_data_cd = iterative_input_data_cd,
    input_data_cd = input_data_cd,
    output_cd = output_cd,
    ndraws = ndraws,
    modes = modes,
    run_tag = run_tag,
    write_intermediate = write_intermediate,
    common_inputs = common_inputs,
    base_outcomes_date_tag = base_outcomes_date_tag
  )
}

#' @title Run a projection batch sequentially
#' @description Reads the shared inputs if they were not supplied, builds the
#'   job grid, runs every job in order via purrr::pmap, and stacks the
#'   results. This is the debugging-friendly runner: errors surface with a
#'   usable traceback and memory use stays flat, at the cost of speed. Use
#'   run_projection_batch_furrr() for production-sized batches.
#' @param states Character vector of state codes.
#' @param draws Integer vector of draw numbers.
#' @param iterative_input_data_cd Directory of simulation-generated data.
#' @param input_data_cd Directory of MRIP and biological source data.
#' @param common_inputs Pre-read shared inputs; read here when NULL.
#' @param run_tag Label identifying this run.
#' @param ndraws Choice occasions simulated per stratum.
#' @param modes Fishing modes to simulate.
#' @param output_cd Where per-job output is written.
#' @return A single data.table of every job's results, row-bound with
#'   use.names and fill so that jobs returning different columns still stack.
#' @examples
#' \dontrun{
#' run_projection_batch_purrr(states = "MA", draws = 1:2,
#'                            iterative_input_data_cd = iter_dir,
#'                            input_data_cd = in_dir)
#' }
run_projection_batch_purrr <- function(states,
                                       draws,
                                       iterative_input_data_cd,
                                       input_data_cd,
                                       common_inputs = NULL,
                                       run_tag = "final",
                                       ndraws = 50L,
                                       modes = c("sh", "pr", "fh"),
                                       output_cd = NULL) {
  
  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs(
      iterative_input_data_cd = iterative_input_data_cd,
      input_data_cd = input_data_cd,
      states = states,
      draws = draws
    )
  }
  
  grid <- make_projection_grid(
    states = states,
    draws = draws,
    run_tag = run_tag,
    ndraws = ndraws,
    modes = modes
  )
  
  out_list <- purrr::pmap(
    .l = list(
      state = grid$state,
      draw = grid$draw,
      ndraws = grid$ndraws,
      modes = grid$modes
    ),
    .f = function(state, draw, ndraws, modes) {
      run_one_projection_job(
        state = state,
        draw = draw,
        ndraws = ndraws,
        modes = modes,
        iterative_input_data_cd = iterative_input_data_cd,
        input_data_cd = input_data_cd,
        common_inputs = common_inputs,
        run_tag = run_tag,
        output_cd = output_cd
      )
    }
  )
  
  data.table::rbindlist(
    out_list,
    fill = TRUE,
    use.names = TRUE
  )
}

#' @title Run a projection batch in parallel
#' @description Identical contract to run_projection_batch_purrr() but maps
#'   the jobs across worker processes with furrr. The two are deliberately
#'   interchangeable: develop against the sequential runner, then swap in this
#'   one for a full batch without changing the call. The caller is responsible
#'   for having set a future::plan() beforehand - this function does not
#'   choose a parallel backend for you.
#' @param states Character vector of state codes.
#' @param draws Integer vector of draw numbers.
#' @param iterative_input_data_cd Directory of simulation-generated data.
#' @param input_data_cd Directory of MRIP and biological source data.
#' @param common_inputs Pre-read shared inputs; read here when NULL. Supplying
#'   these matters more in the parallel runner, since otherwise every worker
#'   repeats the same reads.
#' @param run_tag Label identifying this run.
#' @param ndraws Choice occasions simulated per stratum.
#' @param modes Fishing modes to simulate.
#' @param output_cd Where per-job output is written.
#' @return A single data.table of every job's results.
run_projection_batch_furrr <- function(states,
                                       draws,
                                       iterative_input_data_cd,
                                       input_data_cd,
                                       common_inputs = NULL,
                                       run_tag = "final",
                                       ndraws = 50L,
                                       modes = c("sh", "pr", "fh"),
                                       output_cd = NULL) {
  
  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs(
      iterative_input_data_cd = iterative_input_data_cd,
      input_data_cd = input_data_cd,
      states = states,
      draws = draws
    )
  }
  
  grid <- make_projection_grid(
    states = states,
    draws = draws,
    run_tag = run_tag,
    ndraws = ndraws,
    modes = modes
  )
  
  out_list <- furrr::pmap(
    .l = list(
      state = grid$state,
      draw = grid$draw,
      ndraws = grid$ndraws,
      modes = grid$modes
    ),
    .f = function(state, draw, ndraws, modes) {
      run_one_projection_job(
        state = state,
        draw = draw,
        ndraws = ndraws,
        modes = modes,
        iterative_input_data_cd = iterative_input_data_cd,
        input_data_cd = input_data_cd,
        common_inputs = common_inputs,
        run_tag = run_tag,
        output_cd = output_cd
      )
    }
  )
  
  data.table::rbindlist(
    out_list,
    fill = TRUE,
    use.names = TRUE
  )
}

# Example usage:
# source("project_rec_catch_final.R")
# source("project_rec_catch_batch_helpers.R")
#
# iterative_input_data_cd <- "E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
# input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data"
# states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
# draws <- 1:100
#
# common_inputs <- read_projection_common_inputs(
#   iterative_input_data_cd = iterative_input_data_cd,
#   input_data_cd = input_data_cd,
#   states = states,
#   draws = draws
# )
#
# pred <- run_projection_batch_purrr(
#   states = states,
#   draws = draws,
#   iterative_input_data_cd = iterative_input_data_cd,
#   input_data_cd = input_data_cd,
#   common_inputs = common_inputs,
#   run_tag = "candidate_reg_set_1"
# )
