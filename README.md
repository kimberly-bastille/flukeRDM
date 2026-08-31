# flukeRDM (`rdmtool`)

Recreational Fisheries Decision Support Tool for summer flounder, black sea bass and scup.

## Overview

flukeRDM implements a Stata → R → Shiny pipeline that simulates how recreational fishing
regulations — season dates, bag limits and minimum sizes — affect fishing outcomes for
summer flounder, black sea bass and scup across nine Mid-Atlantic states (MA, RI, CT, NY,
NJ, DE, MD, VA, NC). Raw MRIP survey data and stock assessment output are processed in
Stata, calibrated in R against published MRIP totals, and served through a Shiny
application that fisheries managers use to compose candidate regulation packages per state
and compare their predicted harvest, discards and angler-welfare consequences.

The scientific core is Andrew (Lou) Carr-Harris's Recreation Demand Model: a
discrete-choice model of angler trip-taking behavior, combined with copula-based
simulation of correlated three-species catch per trip and Monte Carlo propagation of
uncertainty across draws. The tool is deployed in Azure and made available to users at
[rec-tools-northeast.fisheries.noaa.gov](https:// rec-tools-northeast.fisheries.noaa.gov).

This repository is the Mid-Atlantic sibling of **groundfishRDM** (cod and haddock, Gulf of
Maine). The two share a common origin and a near-identical house style — same wrapper
filenames, same toggle convention, same `$developer` startup sequence. They have since
diverged slightly, the team has focused on groundfishRDM in advance of the 2027 management cycle.

## Repository Structure

| Path | Contents |
|------|----------|
| `Code/pre_sim/` | Stage 1. Stata data processing (17 `.do` files) plus the R scripts Stata invokes via `rscript using` — the two copula modeling scripts and Google Drive pushes. Orchestrated by `model_wrapper.do`. |
| `Code/sim/` | Stage 2. R calibration and simulation. Orchestrated by `R code wrapper.R`. Also holds `run_state_model.R`, `apply_directed_trips_regs.R` (an incomplete consolidation of the nine per-state scripts) and `required_packages.R`. |
| `Code/helpers/` | Leaf utilities: `developer_setup.R` / `developer_setup_stata.do` (path bootstrap), Google Drive auth, NAA helpers. Not orchestrators. |
| `Code/test_code/` | 21 development and QA scratch scripts (`_test1`, `_test2`, `_revised_v3`, `_nochange` naming). Not called by any wrapper. Archive candidate. |
| `Code/archive/` | 16 superseded predecessors of current `Code/sim/` scripts, including two ~2,300-line copula scripts. Carries its own `README.md`. Relevant because the broken `source()` calls in the projection path point at filenames that now exist only here. |
| `recDST/` | Nine per-state projection runners, `model_run_MA.R` through `model_run_NC.R` (~300 lines each, 70–85% identical). |
| `docs/` | The repository's richest documentation: 22 files — rendered `.Rmd`/`.html` analyses (BSB summary, status-quo comparison, coastwide output, p-star review, user survey) plus regulatory reference material (regulation tables, TC meeting decks, the FY2026 data schedule). |
| `Data/` | Pipeline output consumed downstream. Empty in a fresh checkout. |
| `output/` | Model result CSVs (`output_<ST>_<Run_Name>_<timestamp>.csv`) read by the Shiny app. Empty in a fresh checkout. |
| `saved_regs/` | Submitted regulation scenarios (`regs_<Run_Name>.csv`). Empty in a fresh checkout. |
| `keda/` | KEDA (Kubernetes Event-Driven Autoscaling) configuration — queue-creation scripts, `consume_and_run.sh` worker entrypoint, `scaledjob.yaml`. |
| `shiny-kubernetes-deployment/` | Kubernetes deployment manifests (24 YAML files) with its own `README.md`. |
| `AzurePortal/` | Exported Azure infrastructure configuration (44 files) — VNET, AKS cluster, storage, network watcher. |
| `.secrets/` | Cached `googledrive` OAuth token location. Gitignored; a placeholder is committed. |
| `app.R` | The Shiny application (root level, monolithic ~5,100-line file). |
| `Run_Model.R` | CLI entry point for a projection run: `Rscript Run_Model.R <Run_Name>`. |
| `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore` | R package scaffolding (package name `rdmtool`, version 0.1.0). Vestigial — there is no `R/` directory, so `NAMESPACE`'s `exportPattern` reaches nothing. |
| `Dockerfile` | Shiny app image (`rocker/shiny:4.3`). |
| `Dockerfile.Rmodel` | Model-runner worker image (`rocker/r-ver:4.3.2`). |
| `shiny-server.conf` | Deployment configuration. |
| `documentation.md` | User-facing glossary rendered inside the Shiny app — defines the output statistics managers see. |
| `CODE_OF_CONDUCT.md`, `License.txt` | Project boilerplate. |

## Requirements

### R Package Dependencies

Extracted by scanning every `.R` and `.Rmd` file in the repository for `library()`,
`require()`, `pkg::` usage, the `packages` vector that `Code/sim/R code wrapper.R`
attaches via `lapply(packages, library, character.only = TRUE)`, and the
`install.packages()` manifest in `Code/sim/required_packages.R`.

**Version pinning is nearly absent.** There is no `renv.lock` and no explicit version
comments anywhere in the code. `DESCRIPTION` declares only `R (>= 2.10)` and
`Imports: magrittr` — it does not reflect the pipeline's actual dependencies, and
`R (>= 2.10)` is a boilerplate default, not a real constraint (the containers run R 4.3).
Versions below are listed only where the code actually states one.

**Data manipulation and I/O**
`arrow`, `conflicted`, `data.table`, `doBy`, `dplyr`, `feather`, `fs`, `fst`, `glue`,
`haven`, `here`, `magrittr`, `openxlsx`, `plyr`, `purrr`, `readr`, `readxl`, `reshape2`,
`rlang`, `rlist`, `splitstackshape`, `stringr`, `tibble`, `tidyr`, `tidyverse`,
`WriteXLS`, `writexl`

**Statistical modeling**
`copula`, `fitdistrplus`, `Hmisc`, `logspline`, `MASS`, `psych`, `Rcpp`, `scales`,
`survey`, `univariateML`, `VineCopula`, `wCorr`, `weights`

**Parallel execution**
`furrr`, `future`, `profvis`

**Plotting and reporting**
`ggplot2`, `knitr`, `lubridate`, `markdown`, `patchwork`, `plotly`, `rgl`

**Shiny**
`DT`, `shiny`, `shinyjs`, `shinyWidgets`

**External data access**
`googledrive`, `httr`, `jsonlite`, `openssl`, `RStata`, `uuid`

Base and recommended packages that ship with R (`parallel`, `stats`, `tools`) are used but
not listed as dependencies.

Compared with groundfishRDM, flukeRDM has **no** assessment-modeling stack (`TMB`, `wham`,
`remotes`) and **no** direct database access (`DBI`, `ROracle`, `mriptacklebox`) — it
receives assessment and MRIP inputs from Google Drive rather than generating or pulling
them itself. Everything else overlaps closely.

**Caveat on `required_packages.R`.** That file's own header states its scope explicitly:
it covers `app.R`, `Run_Model.R` and the `recDST/model_run_*.R` family, but **not** the
calibration and copula scripts in `Code/sim` and `Code/pre_sim`. Installing from it alone
is not sufficient to run the full pipeline. It is not sourced by any wrapper, every call
is unconditional (so re-running reinstalls everything), and it is meant to be run by hand
once when setting up a machine. Prefer the consolidated list above.

### Software Requirements

- **R 4.3** — pinned by the Docker base images: `rocker/shiny:4.3` for the app,
  `rocker/r-ver:4.3.2` for the model-runner worker. `DESCRIPTION`'s `R (>= 2.10)` is not a
  meaningful constraint.
- **Stata 17** — per the technology-stack section of the comprehensive analysis report.
  Stata dependency scanning was out of scope for this documentation pass, so no
  user-written command inventory has been compiled. Expect to install several SSC
  packages.
- **Google Drive access** — `get_assessment_from_gdrive.do` reads from a shared drive and
  assumes the Drive client is mounted at `D:`.
- **Docker / Kubernetes / KEDA / ShinyProxy / Redis Sentinel / Azure** — production
  deployment stack. See `shiny-kubernetes-deployment/` and `AzurePortal/`.

## Running the Pipeline

### Prerequisite: set `developer`, then fix the hard-coded paths
Both the Stata and R halves require an externally-set developer identifier that
**is not defined anywhere in this repository**:

- Stata: `$developer`, asserted in `Code/helpers/developer_setup_stata.do`
- R: `developer`, asserted in `Code/helpers/developer_setup.R` via
  `stopifnot(developer %in% c("TP","LCH","ML","KB"))`

This value branches the data-root path global (`$sfdatadir` / `sf.data.dir`). It must be
set with .Rprofile or profile.do before anything runs.

Separately, `Code/sim/R code wrapper.R` hard-codes two developer-specific absolute paths
in its own top-level assignments (`input_data_cd`, `iterative_input_data_cd`, lines
50–51). Anyone other than the original developer must edit those two lines before the R
wrapper will run. This differs from groundfishRDM, where comparable hard-coded paths are
confined to standalone test scripts.

### The two entry points are not chained

flukeRDM has **two independent entry points with no code-level connection between any of them**. A person must run both in this order:

```
1.  do Code/pre_sim/model_wrapper.do        # Stage 1 — Stata
2.  Rscript Run_Model.R <Run_Name>          # Stage 3 — projection  [BROKEN, see below]
```

Every hand-off between stages is filesystem-only. Nothing verifies that Stage 1 finished
before Stage 2 starts

### Stage 1: `Code/pre_sim/model_wrapper.do`

```
 0.                                      developer_setup_stata.do            (unconditional)
 1.  pull_assessment          = 1        get_assessment_from_gdrive.do
 2.  processMRIP              = 0        MRIP_column_cases.do
 3.  assemblemriplists        = 0        MRIP_lists.do
 4.  estimate_dtrips          = 1        directed_trips_calibration.do
       4a.                               └─ set_regulations.do               (nested, unconditional)
 5.  costs_per_trip           = 1        survey_trip_costs.do
 6.  draw_angler_preferences  = 1        estimate_angler_preferences.do
 7.  catch_per_trip1          = 1        catch_per_trip_calibration_part1.do
 8.  copula_in_R              = 1        copula_modeling_calibration.R
 9.  catch_per_trip2          = 1        calibration_catch_per_trip_part2.do
10.  compare_calibration_MRIP = 1        compare_calibration_data_to_MRIP.do
11.  generate_baseline        = 1        calibration_catch_at_length.do
12.  catch_at_length_project  = 1        projected_catch_at_length.do
13.  catch_per_trip_project   = 1        [meta-toggle — all four run or skip together]
       13a.                              ├─ catch_per_trip_projection_part1.do
       13b.                              ├─ copula_modeling_projection.R
       13c.                              ├─ catch_per_trip_projection_part2.do
       13d.                              └─ compare_projection_data_to_MRIP.do
14.  run_calibration = 1                 run the calibration
```

**About the toggles.** Sixteen toggle-gated sections, defined as Stata *locals* in one
contiguous block under the `EXECUTION CONTROL` banner at `model_wrapper.do` lines 143–164,
uniformly `0`/`1`. Note two departures from groundfishRDM's one-toggle-per-script pattern:

- `catch_per_trip_project` is a **meta-toggle** gating four scripts as a unit.
- **Three toggles are defined but gate nothing.** `prep_cpt_for_dashboard` (0/OFF,
  self-labeled "NOT WRITTEN"), `Rpush_to_gdrive` (0/OFF, its script exists in
  `Code/pre_sim/` but is never called), and `angler_demogs` (1/ON, no explanatory comment
  at all — groundfishRDM has a fully wired toggle of the same name). Setting any of them
  has no effect.

`set_regulations.do` requires **manual editing every year** to enter status-quo
regulations. It is reached only through `estimate_dtrips`.

### Tipeline prototyping 

`proto` defaults to **0 (OFF)** 

### Stage 3: `Run_Model.R` — known broken as committed

```
Rscript Run_Model.R <Run_Name>
```

`Run_Model.R` reads `saved_regs/regs_<Run_Name>.csv` and sources
`recDST/model_run_<ST>.R` for each state present in it, skipping states the scenario does
not touch (so a Massachusetts-only run costs one state's runtime, not nine).

**This path does not run as committed.** Each per-state script's per-draw worker
(`get_predictions_out()`, around lines 357–358) sources two files that do not exist at the
given paths:

```r
source(here::here("Code/sim/predict_rec_catch_functions.R"))   # only exists in Code/archive/
source(here::here("Code/sim/predict_rec_catch.R"))             # exists nowhere; closest is
                                                               # Code/sim/predict_rec_catch_final.R
```
The developer team is in the process of fixing these issues.

A run fails on the first draw of the first state attempted. This is consistent with a
rename inside `Code/sim/` that did not update its callers, and the contents of
`Code/archive/` support that reading. **The developers are aware.**

`Code/sim/run_state_model.R` carries the same two broken `source()` calls plus a third
defect of its own — it calls `apply_directed_trips_regs()`, which is never sourced
anywhere in the repository. It is **not** on this code path, however: the
`model_run_<ST>.R` scripts define their own worker rather than calling
`run_state_model()`. Treat `run_state_model.R` and `apply_directed_trips_regs.R` as an
incomplete consolidation of the nine per-state scripts, not as live code.

Scripts with no confirmed caller anywhere in the repo:
`Code/sim/check calibration convergence.do`, `Code/sim/compare_savedregs_output.R` (which
additionally contains a syntax error and cannot be parsed), `Code/sim/generate_coastwide_data.R`,
`Code/helpers/googledrivesetup.R`, `Code/sim/required_packages.R`,
`Code/pre_sim/rdb_catch_per_trip_to_drive.R`.

## Data Flow Summary

```
External sources
  MRIP survey data          Stock assessment (via Google Drive)
        │                              │
        └──────────────────────────────┘
                        ▼
  STAGE 1 — Stata, Code/pre_sim/            [model_wrapper.do]
    survey-weighted domain estimation of directed trips and catch per trip, by
    state × wave × mode; trip-cost and angler-preference draws; catch-at-length
    for calibration and projection years via an age-length key
        → directed trip draws, baseline MRIP catch, catch-at-length
                        ▼
    copula_modeling_calibration.R / copula_modeling_projection.R
        — correlated three-species catch draws per trip (calibration and
          projection years; two ~730-line scripts differing in ~18 lines)
                        ▼
  STAGE 2 — R, Code/sim/                    [R code wrapper.R]
    iterative reallocation of harvest and discards until simulated totals match
    MRIP; results written as .fst for fast Shiny loading
                        ▼
  STAGE 3 — R projection             [Run_Model.R → recDST/model_run_<ST>.R]
    applies a user's per-state regulation scenario, runs the discrete-choice
    simulation across draws, aggregates with uncertainty
        → output/output_<ST>_<Run_Name>_<timestamp>.csv
                        ▼
  SHINY — app.R
    reads output/*.csv and saved_regs/*.csv; writes saved_regs/regs_<name>.csv
    and enqueues a job message. Never runs the model itself.
```

**Every hand-off is filesystem-only** — there is no code-level chain anywhere in this
pipeline, in contrast to groundfishRDM's Stata → R chain.

Cross-script state is carried by Stata **global macros** set in the wrapper and read by
every script running later in the same session — paths (`$misc_data_cd`, `$figure_cd`,
`$sfdatadir`, `$calib_catch_data_cd`), settings (`$ndraws`, `$seed 03211990`). A script
expecting a global the wrapper did not set will silently do the wrong thing rather than
error. The R side follows the same pattern with global-environment objects set at the top
of `R code wrapper.R` (`code_cd`, `input_data_cd`, `iterative_input_data_cd`,
`n_simulations`, `n_draws`).

**The input-ID contract.** The one convention that ties `app.R` to the model scripts is
input naming: Shiny input IDs follow `<SPECIES><state><MODE>_<field>`, e.g.
`SFmaFH_seas1_op`. Those exact strings are written into `saved_regs/regs_<Run_Name>.csv`
as the `input` column, and each per-state model script recreates them as variables via
`assign()`. Nothing validates that the names a scenario supplies are the names the model
expects — **renaming an input ID in `app.R` silently breaks the corresponding
`model_run_<ST>.R`** rather than raising an error.

## Running the Shiny Application

```r
# from the repository root
shiny::runApp("app.R")
```

`app.R` is a single ~5,100-line file — no `global.R`/`ui.R`/`server.R` split, no
`source()` or `system()` calls. Navigate it by the section banners in its header:
Section A is the UI, Section D (~3,200 lines, roughly two thirds of the file) is nine
near-identical per-state regulation-input blocks, and Sections E–I cover summary tables,
figures, scenario submission and result downloads. Reading one state's block in Section D
is sufficient to understand all nine.

**Must exist before launching:**

| Requirement | Produced by |
|---|---|
| `output/*.csv` | Stage 3 projection runs, which in turn need Stages 1 and 2 to have completed |
| `saved_regs/*.csv` | Previous submissions through the app; the directory must exist for new scenarios to be written |
| `AZURE_STORAGE_QUEUE_URL` | Environment variable holding a SAS-authenticated Azure Storage queue URL. Without it, submitting a run fails. |

Both directories exist in the repository but are empty. With them empty the app launches
but the results tab has nothing to display.

**The app never runs the simulation.** Submitting a scenario writes
`saved_regs/regs_<Run_Name>.csv` and posts one queue message; a separate worker
(`keda/consume_and_run.sh`, running the `Dockerfile.Rmodel` image) consumes it and runs
`Rscript Run_Model.R <Run_Name>`. Because Stage 3 is currently broken (see above), that
worker cannot presently complete a run from a clean checkout.

`documentation.md` — rendered inside the app — is the user-facing glossary of the output
statistics managers see (compensating variation, harvest/release numbers and pounds, dead
release numbers, percent difference from status quo, percent under harvest target).

## Known Issues & Technical Debt

**Blocking**
- The Stage 3 projection path does not run as committed — two missing `source()` targets
  in every per-state script. See [Running the Pipeline](#running-the-pipeline).
- `Code/sim/compare_savedregs_output.R` contains a syntax error and cannot be parsed.
- The committed default is prototype mode, and switching to production requires edits in
  three places.

**Pipeline structure**
- two entry points, zero code-level links between them. 
- Three toggles are defined but gate nothing; one (`angler_demogs`) has no explanatory
  comment at all.
- `$developer` is required 

**Portability**
- Hard-coded developer-specific absolute paths reach into production files here, not just
  test scripts: `R code wrapper.R`, `calibrate_rec_catch0_optimized.R`,
  `calibration_routine_final.R`, `predict_rec_catch_final.R` (which re-hard-codes a path,
  overriding its caller), both copula scripts, and most of `Code/test_code/`. One
  developer's folder is spelled inconsistently between files (`E:/Lou_projects` and
  `E:/Lou's projects`).
- `get_assessment_from_gdrive.do` assumes the Google Drive client mounts at `D:`.
- No dependency management — no `renv.lock`, no package versions specified.
- `plyr` is attached before `dplyr` deliberately (it masks `summarize`, `count`, `mutate`,
  `rename`); the `conflicts_prefer()` calls make the resolution explicit. Do not reorder.
- Magic numbers embedded without explanation, notably `254` — the "no minimum size"
  sentinel (100 inches × 2.54 cm).

**Interpreting all of this.** flukeRDM has had less development time than
groundfishRDM, which has already received a hardening pass that flukeRDM is slated to
receive next. Most gaps above — the broken projection path, the prototype-mode default,
the missing global fallback, the undocumented dead toggle — look like exactly what that
pass would close. 

## Documentation Index

### In this repository

| File | Contents |
|------|----------|
| `documentation.md` | User-facing glossary rendered inside the app: definitions of every output statistic managers see. |
| `docs/` (~20 files) | The richest documentation here — rendered `.Rmd`/`.html` analyses (`BSB_summary`, `SQ_comparison`, `coastwide_output`, `pstar_review`, `future_explained`, `User_Survey_Summary26`, `documentation`) plus regulatory reference material and the FY2026 data schedule. |
| `shiny-kubernetes-deployment/deployment/README.md` | Kubernetes deployment instructions. |
| `Code/archive/README.md` | What the archived predecessors are and why they were kept. |
| `CODE_OF_CONDUCT.md`, `License.txt` | Project boilerplate. |
| In-code headers | Every pipeline script carries a structured header block — Purpose, Inputs, Outputs, Dependencies, Pipeline position — plus section banners, with known defects flagged inline (`Run_Model.R` and each `model_run_<ST>.R` carry explicit `KNOWN BROKEN` notes). This is the most reliable per-file documentation in the repository and is kept current with the code. |

## Glossary

The terms below are the ones you will hit immediately. The full glossary — covering Stata
idioms (`preserve`/`restore`, `capture`, `postfile`, the accumulate idiom, variable
abbreviation), R idioms (`data.table` `:=` and NSE, `setDTthreads(1)`,
`conflicts_prefer()`, `assign()` into an environment, Shiny reactives) and the rest of the
modeling vocabulary — is in `GLOSSARY_FLUKE.md`. For the *output statistics* the app
displays, see `documentation.md` in this repository.

| Term | Meaning |
|------|---------|
| **Wrapper script** | A script whose job is running other scripts in order, with sections switched on or off by flags, rather than processing data itself. |
| **Toggle** | A `0`/`1` local macro in `model_wrapper.do` gating whether a block runs on this pass. Setting one to `0` deletes nothing — it skips a step whose output is already on disk. |
| **Meta-toggle** | A toggle gating several scripts as one unit rather than individually. `catch_per_trip_project` is the one instance here. |
| **Dead toggle** | A toggle defined in the wrapper with no matching `if` block — setting it has no effect. Three exist here. |
| **Prototype mode** | The `proto` local. When on (**the committed default**), it overwrites `$ndraws` from 100 to 3 for fast runs. |
| **Entry point** | A script a person starts directly, as opposed to one that something else calls. This repository has three, and none of them calls another. |
| **Global macro** | A Stata value defined with `global`, readable as `$name` in *any* script running later in the same session. Because they persist, a script expecting a global the wrapper never set will silently misbehave. |
| **`svyset` / `svy:`** | Declares the survey sampling design, then runs an estimator that respects it. MRIP is a complex survey — a plain `mean` gives the wrong standard error. |
| **MRIP** | Marine Recreational Information Program — the survey producing the catch and effort estimates this model calibrates to. |
| **Draw** | One complete realization of the simulation, carrying one sampled value of every uncertain input. Results are summarized as medians and intervals across draws. |
| **Stratum** | The grouping the model works in: state × wave × mode, sometimes further by kind-of-day. |
| **Wave** | A two-month MRIP sampling period; wave 1 is January–February, through wave 6. |
| **Mode** | How the trip was taken: `pr` private boat, `fh` for-hire (charter and headboat), `sh` shore. |
| **Kind-of-day** | Weekend versus weekday, used for effort estimation. |
| **Directed trip** | A trip that targeted or caught one of the three managed species. The unit effort is measured in. |
| **Calibration vs. projection year** | The calibration year is the recent year the model is tuned to reproduce; the projection year is the future year whose regulations are being evaluated. Many scripts come in matched pairs, one per period. |
| **Status quo (SQ)** | The run using current regulations, against which every alternative scenario is compared. |
| **Copula** | A way of simulating several correlated quantities (catch of the three species on one trip) while letting each keep its own marginal distribution. |
| **Reallocation / the proportion `p`** | The calibration's adjustment moving fish between kept and released until simulated totals match MRIP. |
| **Accounting vs. utility columns** | During reallocation a fish moved from kept to released changes the accounting columns and the utility columns differently — the distinction matters when reading the calibration code. |
| **Fish-level expansion** | Expanding a trip's catch count into one row per fish, so length-based regulations can be applied individually. |
| **Hurdle (two-part) model** | Modeling "did this happen at all" separately from "how much, given it happened". |
| **Catch-at-length** | The length composition of the fish anglers encounter. |
| **Age-length key** | A table converting numbers-at-age (what the assessment produces) into numbers-at-length. |
| **The 254 sentinel** | A minimum size of 254 (100 inches × 2.54 cm) meaning "no minimum size applies". |
| **Compensating variation (CV)** | The dollar amount making an angler as well off under the new policy as under the baseline. The app displays `CV_SQ − CV_alt`; see `documentation.md` for the exact definition managers see. |

## Disclaimer

This repository is a scientific product and is not official communication of the National
Oceanic and Atmospheric Administration, or the United States Department of Commerce. All
NOAA GitHub project code is provided on an 'as is' basis and the user assumes
responsibility for its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be governed by all
applicable Federal law. Any reference to specific commercial products, processes, or
services by service mark, trademark, manufacturer, or otherwise, does not constitute or
imply their endorsement, recommendation or favoring by the Department of Commerce. The
Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be
used in any manner to imply endorsement of any commercial product or activity by DOC or
the United States Government.
