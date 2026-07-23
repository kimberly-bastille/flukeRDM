/*******************************************************************************
 Script:       MRIP_lists.do
 Purpose:      Builds the four MRIP file-list globals - $catchlist, $triplist,
               $b2list and $sizelist - as space-separated lists of quoted
               .dta paths, one per year x wave that both exists on disk and
               contains at least one observation. Downstream scripts consume
               these globals with a single `append using $triplist'-style call
               rather than looping over years and waves themselves, so the
               non-empty check here is what keeps those appends from failing
               on an empty file.
 Inputs:       $misc_data_cd/{catch,trip,size_b2,size}_<year><wave>.dta for
               every year in $yearlist crossed with every wave in $wavelist.
               Opens each candidate file only to count its observations.
 Outputs:      None on disk. Sets the globals $catchlist, $triplist, $b2list
               and $sizelist.
 Dependencies: Globals $misc_data_cd, $yearlist and $wavelist (set in
               model_wrapper.do). Should run after MRIP_column_cases.do so
               that the files it inspects have normalized variable names.
 Pipeline:     Step 3 of model_wrapper.do, gated by the `assemblemriplists'
               toggle (default ON).

 KNOWN GAP - read before disabling the toggle:
               This script is the ONLY place these four globals are ever
               assigned. Unlike GroundfishRDM, whose model_wrapper.do pre-sets
               all four to single consolidated-file defaults before the
               optional rebuild step, the flukeRDM wrapper provides no fallback.
               Setting `assemblemriplists'=0 while leaving later steps enabled
               therefore leaves the globals empty, and the downstream scripts
               that read them (directed_trips_calibration.do,
               catch_per_trip_calibration_part1.do and others) will silently
               operate on nothing rather than erroring. The toggle currently
               defaults ON, so this is latent rather than active.
               The team is aware of this and plans to add the same fallback
               defaults GroundfishRDM uses; it is deliberately NOT fixed here.
*******************************************************************************/

/* The accumulate idiom used four times below is worth reading once carefully.
   The line
       global catchlist "$catchlist "path.dta" "
   re-expands the current value of the global and appends the new path wrapped
   in its own quote pair, producing a list of individually-quoted paths
   separated by spaces. The quoting matters because $misc_data_cd may contain
   spaces. The empty else branches are intentional no-ops - a missing or empty
   year x wave file is skipped, not an error, since MRIP does not sample every
   wave in every year. */

display "MRIP_lists.do: scanning $misc_data_cd for MRIP files across \$yearlist x \$wavelist and building the catch/trip/b2/size file-list globals. This opens each candidate file to count observations and may take a few minutes."

/**************************************************/
/**************************************************/
/* Section A: catchlist                           */
/**************************************************/
/**************************************************/

/*catchlist -- this assembles names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "$misc_data_cd/catch_`year'`wave'.dta"
	if _rc==0{
		use "$misc_data_cd/catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "$misc_data_cd/catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/**************************************************/
/**************************************************/
/* Section B: triplist                            */
/**************************************************/
/**************************************************/

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year in $yearlist{
	foreach wave in  $wavelist{
	capture confirm file "$misc_data_cd/trip_`year'`wave'.dta"
	if _rc==0{
		use "$misc_data_cd/trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "$misc_data_cd/trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/**************************************************/
/**************************************************/
/* Section C: b2list (released-fish size records) */
/**************************************************/
/**************************************************/

/*B2 Files*/
global b2list
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "$misc_data_cd/size_b2_`year'`wave'.dta"
	if _rc==0{
		use "$misc_data_cd/size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "$misc_data_cd/size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}


/**************************************************/
/**************************************************/
/* Section D: sizelist (kept-fish size records)   */
/**************************************************/
/**************************************************/

/*SIZE_LIST */
global sizelist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "$misc_data_cd/size_`year'`wave'.dta"
	if _rc==0{
	use "$misc_data_cd/size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "$misc_data_cd/size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

display "MRIP_lists.do: finished. Built \$catchlist, \$triplist, \$b2list and \$sizelist."
