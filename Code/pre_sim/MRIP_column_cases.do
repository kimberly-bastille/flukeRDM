


/*******************************************************************************
 Script:       MRIP_column_cases.do
 Purpose:      Normalizes variable names to lower case, in place, for the four
               MRIP dataset families (trip, catch, size, size_b2) across every
               year-wave in $yr_wvs. MRIP releases arrive with inconsistent
               capitalization between vintages; every downstream script refers
               to variables in lower case, so this must run once after new MRIP
               files are added or those scripts fail on unrecognized names.
 Inputs:       $misc_data_cd/{trip,catch,size,size_b2}_<yearwave>.dta for each
               entry in $yr_wvs.
 Outputs:      The same files, overwritten in place with lower-cased variable
               names.
 Dependencies: Globals $yr_wvs and $misc_data_cd (set in model_wrapper.do).
               Requires the user-written command renvarlab (from the renvars
               package) - it is not part of official Stata and must be
               installed separately.
 Pipeline:     Step 2 of model_wrapper.do, gated by the `processMRIP' toggle.
               Runs before MRIP_lists.do and everything downstream of it.
               Note: GroundfishRDM has the same script with the toggle default
               OFF and labeled "dead code"; here it defaults ON.
*******************************************************************************/

/* This script is idempotent - re-running it on already-lower-cased files is
   harmless, which is why it is safe to leave the toggle ON by default. */

/* This code only needs to be run once after new MRIP data enters the repo */
/* It handles  casing for the datasets */

display "MRIP_column_cases.do: lower-casing variable names in the MRIP trip, catch, size and size_b2 files. This rewrites every file in place and may take several minutes."

/* Each block is guarded by `capture confirm file' because not every
   year x wave combination exists - MRIP does not sample every wave in every
   year, and the repo may hold only a subset. The empty else branches are
   intentional no-ops: a missing file is skipped silently, not an error. */
qui foreach wave in $yr_wvs {				
capture confirm file "${misc_data_cd}/trip_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/trip_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/trip_`wave'.dta", replace
}

else{
	
}

capture confirm file "${misc_data_cd}/size_b2_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/size_b2_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/size_b2_`wave'.dta", replace
}

else{
	
}

capture confirm file "${misc_data_cd}/size_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/size_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/size_`wave'.dta", replace
}

else{
	
}

capture confirm file "${misc_data_cd}/catch_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/catch_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/catch_`wave'.dta", replace
}

else{
	
}

}

display "MRIP_column_cases.do: finished lower-casing MRIP variable names."


