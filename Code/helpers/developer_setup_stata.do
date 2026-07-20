/*******************************************************************************
 Script:       developer_setup_stata.do
 Purpose:      Resolves the per-developer root directory for the 2028
               management-cycle data and exposes it as the global $sfdatadir.
               Every other .do file in the repo builds its data paths from
               $sfdatadir rather than hardcoding a root, so this is the single
               place a new machine gets configured.
               Rationale: processed data is far too large to commit, so the
               developer who runs the processing steps (LCH) keeps it on an
               external drive while everyone else keeps it under Data/ in the
               repo, where .gitignore excludes it.
 Inputs:       None.
 Outputs:      None (creates the data directory on disk if absent; defines
               the global $sfdatadir).
 Dependencies: Globals $developer (must be "LCH", "TP", "ML" or "KB") and
               $here must already be set. $developer is never assigned
               anywhere in this repo - it is an external prerequisite the
               user sets before running the wrapper.
 Pipeline:     Step 0 of model_wrapper.do, run unconditionally before every
               other step. R twin of this file is developer_setup.R, which
               sets the object sf.data.dir to the same locations. The four
               allowed developer values and this two-branch structure are
               shared infrastructure with GroundfishRDM, which uses
               $gfdatadir for the same role.
 Dev paths:    1 hardcoded absolute path to a developer's local machine
               (E:\), at line 33 - this is the file's entire purpose (the
               LCH branch of the per-developer split), not an oversight.
*******************************************************************************/

/* The assertion is deliberate: an unrecognized $developer would otherwise fall
   through both branches below and leave $sfdatadir empty, so every downstream
   path would silently resolve to a bare relative filename. */
assert inlist("$developer", "LCH", "TP", "ML", "KB")

if inlist("$developer","LCH") {
	global sfdatadir "E:\Lou_projects\flukeRDM\2028_mgt_cycle"
} 
else if inlist("$developer","TP", "ML","KB"){
	global sfdatadir "${here}\Data\2028_mgt_cycle"
}
/* make this directory if it doesn't exist.*/
capture mkdir $sfdatadir 


/* Note: the message below names "gfdatadir", the GroundfishRDM global. The
   global this script actually sets is $sfdatadir. The message text is a
   copy-paste artifact from the shared setup template; the assignment above is
   correct. Left as-is - this session documents behavior and does not change it. */
display "Hello $developer.  Use the global gfdatadir in place of \${here}\Data\YYYY mgmt cycle)."
display "The value of datadir is: $sfdatadir"


