################################################################################
################################################################################
# Script:       developer_setup.R
# Purpose:      Resolves the per-developer root directory for the 2028
#               management-cycle data and exposes it as the object
#               sf.data.dir. Every other R script in the repo builds its
#               data paths from sf.data.dir rather than hardcoding a root,
#               so this is the single place a new machine gets configured.
#               Rationale: processed data is far too large to commit, so the
#               developer who runs the processing steps (LCH) keeps it on an
#               external drive while everyone else keeps it under Data/ in
#               the repo, where .gitignore excludes it.
# Inputs:       None.
# Outputs:      None (creates the data directory on disk if absent; defines
#               the object sf.data.dir in the calling environment).
# Dependencies: The object `developer` must already be set to one of "TP",
#               "LCH", "ML", or "KB" before this file is sourced, and the
#               here package must be attached. Sourced independently by each
#               R script that needs a data path; it is NOT sourced by
#               "R code wrapper.R".
# Pipeline:     Setup stage. Stata twin of this file is
#               developer_setup_stata.do, which sets the global $sfdatadir
#               to the same locations. The four allowed developer values and
#               this two-branch structure are shared infrastructure with
#               GroundfishRDM, which uses $gfdatadir for the same role.
# Dev paths:    1 hardcoded absolute path to a developer's local machine
#               (E:\), at line 34 - this is the file's entire purpose (the
#               LCH branch of the per-developer split), not an oversight.
################################################################################
################################################################################

# The assertion is deliberate: an unrecognized developer name would otherwise
# fall through both branches below and leave sf.data.dir undefined, producing a
# confusing "object not found" error much later in whatever script sourced this.
stopifnot(developer %in% c("TP", "LCH", "ML", "KB"))
if (developer=="LCH"){
  sf.data.dir<-"E:/Lou_projects/flukeRDM/2028_mgt_cycle"
} else if (developer %in% c("TP","ML", "KB")){
  dir.create(here("Data","2028_mgt_cycle"), showWarnings = TRUE, recursive=TRUE)
  sf.data.dir<-here("Data","2028_mgt_cycle")
}

message("Hello ", developer, "  Use the object sf.data.dir in place of here(Data, YYYY_mgt_cycle).")

message("The value of sf.data.dir is: ", sf.data.dir)
