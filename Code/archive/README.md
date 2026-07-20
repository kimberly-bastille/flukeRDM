# `Code/archive/` — Retired code

**Nothing in this folder runs.** No wrapper, script or app in flukeRDM calls any
file here. These are earlier versions of scripts that still exist, under
different names, in `Code/pre_sim/` and `Code/sim/`.

They are kept for reference — to see how a calculation used to be done, or to
recover an approach that was set aside — not as a fallback the pipeline can be
pointed at.

## Read these files with care

- **They are not maintained.** Hardcoded paths, data formats, variable names and
  modeling choices may be several cycles out of date.
- **Do not use them to understand current behavior.** For that, read the
  superseding file named in each header. Every file here carries a header block
  giving its purpose and its successor.
- **They received a header only** during the in-place documentation pass. Unlike
  the production scripts, they have no inline documentation, no section banners
  and no function-level docs.

## One archived file matters to a live bug

`predict_rec_catch_functions.R` is not merely historical. The nine
`recDST/model_run_<ST>.R` scripts and `Code/sim/run_state_model.R` all still
contain:

```r
source(here::here("Code/sim/predict_rec_catch_functions.R"))
source(here::here("Code/sim/predict_rec_catch.R"))
```

Neither file exists at that path. The first is *this* archived copy; the second
does not exist anywhere under that name — `Code/sim/predict_rec_catch_final.R`
is the current equivalent. This is why the `Run_Model.R` projection path fails.
See `FLAGGED_ISSUES_FLUKE.md`. Copying the archived files into `Code/sim/` would
silence the error but would run years-old logic; the fix is to update the
callers to the current filenames.

## What is here

| File | Superseded by |
|---|---|
| `calibrate_rec_catch.R` | `Code/sim/calibrate_rec_catch0_optimized.R` |
| `calibrate_rec_catch0.R` | `Code/sim/calibrate_rec_catch0_optimized.R` |
| `calibrate_rec_catch1.R` | `Code/sim/calibrate_rec_catch1_final.R` |
| `calibration routine.R` | `Code/sim/calibration_routine_final.R` |
| `copula model loop.R` | `Code/pre_sim/copula_modeling_calibration.R` |
| `copula model loop projection.R` | `Code/pre_sim/copula_modeling_projection.R` |
| `predict_rec_catch_functions.R` | `Code/sim/predict_rec_catch_final.R` |
| `predict_rec_catch_old.R` | `Code/sim/predict_rec_catch_final.R` |
| `predict_rec_catch_data_read.R` | `read_projection_common_inputs()` in `Code/sim/predict_rec_catch_final.R` |
| `get_input_data_feather.R` | no direct successor |
| `compute SQ weights from averages.R` | length-weight conversion in the projection stage |
| `weight_per_fish.do` | length-weight conversion in the projection stage |
| `catch_at_length_testing.do` | `Code/pre_sim/calibration_catch_at_length.do` |
| `projected_catch_at_length_extra_draws.do` | `Code/pre_sim/projected_catch_at_length.do` |
| `catch_per_trip_projection_part2_extra_draws.do` | `Code/pre_sim/catch_per_trip_projection_part2.do` |
| `compare catch-at-length btw calib and proj.do` | no direct successor |

## Related folders

- `Code/test_code/` — development and QA scratch work. Also not called by any
  wrapper, but unlike this folder it holds *current* experiments, including the
  candidate refactor of the projection code, and it is fully documented.
- `Code/pre_sim/`, `Code/sim/` — the production pipeline.
