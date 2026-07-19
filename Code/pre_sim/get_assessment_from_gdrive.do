/*******************************************************************************
 Script:       get_assessment_from_gdrive.do
 Purpose:      Copies the stock assessment numbers-at-age files from the shared
               NMFS Google Drive into the local $misc_data_cd folder, so that
               downstream projection scripts read from a local path instead of
               a network mount. The assessment output (historical
               numbers-at-age plus projections) is produced outside this repo
               and delivered via Drive.
 Inputs:       Every file in the shared Drive folder
               .../flukeRDM/input_data (as currently written the script copies
               the whole folder; see the note on the disabled block below).
               Expected members include fit_NAA_NORTH, fit_NAA_SOUTH,
               fit_proj_NAA_NORTH, fit_proj_NAA_SOUTH, and the
               J1_<year><species> series.
 Outputs:      A verbatim copy of each of those files into $misc_data_cd.
 Dependencies: Global $misc_data_cd (set in model_wrapper.do). Requires the
               Google Drive desktop client to be installed and the shared
               drive mounted at D: - the path below is a local filesystem
               path, not an API call, so no OAuth token is needed here.
 Pipeline:     Step 1 of model_wrapper.do, gated by the `pull_assessment'
               toggle (default ON). Feeds projected_catch_at_length.do, which
               is the consumer of the numbers-at-age data.
*******************************************************************************/

/* Hardcoded to the D: drive letter assigned by the Google Drive desktop client.
   A developer whose client mounts the shared drive elsewhere must edit this. */
local google_folder "D:/Shared drives/NMFS NEC READ SSB/socialsci/RecreationalDST/2028_management_cycle_data/flukeRDM/input_data"

/* Note on the two `filestubs' definitions and the commented-out loop below:
   this was the selective-copy approach - name the wanted files, resolve each
   stub to the most recent matching file on Drive, and copy only those. It is
   disabled, and both `filestubs' lines are therefore dead (the second would
   overwrite the first in any case). The active code is the copy-everything
   loop at the bottom of the file. Keep this in mind when reading: the file
   names listed here document what the pipeline EXPECTS to find, but nothing
   in the script currently verifies that those files arrived. */
/* will need to adjust this to match actual file names*/
local filestubs  "fit_NAA_NORTH fit_NAA_SOUTH fit_proj_NAA_NORTH fit_proj_NAA_SOUTH J1_2024Scup J1_2026Scup J1_2024Summer_Flounder J1_2026Summer_Flounder"
local filestubs  "fit_NAA_NORTH fit_NAA_SOUTH"
/*
foreach s of local filestubs {
	di "`s'"
    local files : dir "`google_folder'" files "`s'" // find matching file
	di "`files'"
	local last: word count `files'
	di "`last'"
	local myfile : word `last' of `files' // grab last match
    di "`myfile'"
	local myfile : subinstr local myfile `"""' "", all // remove embedded quotes
    local fullpath `"`google_folder'/`myfile'"' // build full path
    di as text "Loading: `fullpath'" 
	copy "`fullpath'" `"$misc_data_cd/`s'.csv"' , replace //copy files from google drive to misc_data_cd
}

*/

/**************************************************/
/**************************************************/
/* Section A: Copy assessment files from Drive    */
/**************************************************/
/**************************************************/

display "get_assessment_from_gdrive.do: copying assessment files from Google Drive to $misc_data_cd. This may take several minutes if the Drive client has to download files that are not cached locally."

local file_list : dir "`google_folder'" files "*"

foreach file of local file_list {
    copy "`google_folder'/`file'" "${misc_data_cd}/`file'", replace
}

display "get_assessment_from_gdrive.do: finished copying assessment files."
