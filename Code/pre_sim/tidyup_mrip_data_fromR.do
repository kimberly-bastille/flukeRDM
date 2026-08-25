/*******************************************************************************
 Script:       tidyup_mrip_data_fromR.do
 Purpose:      Cleans the MRIP trip/catch/size/size_b2 files freshly pulled from
               Oracle via R's tacklebox: enforces column types, converts the
               pull-date string to a Stata date, and filters each file to the
               year-wave combinations listed in $yr_wvs.
 Inputs:       The .dta files named in globals $catchlist $triplist $b2list
               $sizelist (the raw MRIP extracts written by get_mrip_oracle.R).
 Outputs:      The same files, overwritten in place, with:
                 - strat_id psu_id id_code zip forced to string
                 - year wave st forced to numeric
                 - mrip_pull_date converted to a Stata %td date
                 - rows restricted to the year-wave patterns in $yr_wvs
 Dependencies: Globals $catchlist/$triplist/$b2list/$sizelist and $yr_wvs
               (set in model_wrapper.do). Runs immediately after
               get_mrip_oracle.R.
 Pipeline:     Step 2 of model_wrapper.do, gated by `pull_MRIP' (default ON),
               directly following the get_mrip_oracle.R Oracle pull.
			   
 Note:			Exact copy of same file in groundfishRDM repository
*******************************************************************************/

display "Tidying MRIP catch/trip/size files (type enforcement + year-wave filter) ..."

foreach l in $catchlist $triplist $b2list $sizelist {

	use `l', clear
	/* enforce certain variables as strings */
	foreach var of varlist strat_id psu_id id_code zip{
		cap tostring `var', replace
	}
	/* enforce other variables as numeric */
	foreach var of varlist year wave st{
		destring `var', replace
	}

	/* filter based on the global yr_wvs */
	
	gen yr_wave=year*10+wave	
	gen yrwave_keep=0
	qui foreach l of global yr_wvs{
		replace yrwave_keep=1 if yr_wave==`l'
	}
	keep if yrwave_keep==1
	drop yrwave_keep yr_wave
	
save `l', replace
}

display "Finished tidying MRIP files."