


* This script create projection catch draw files for each state that contains:
	* 50 trips in each day of the calibration year in which there were directed trips, each with 30 draws of catch-per-trip
	* Demographics: age and avidity (number trips past 12 months) come from the base_outcomes files generated in the calibration simulation 

	
********************************


/* Check to make sure the copula model did not produce NAs
local statez "MA RI CT NY NJ DE MD VA NC"
foreach s of local statez{
	
forv i=1/100{

import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow
gen sf_cat=sf_keep_sim+sf_rel_sim
gen bsb_cat=bsb_keep_sim+bsb_rel_sim
gen scup_cat=scup_keep_sim+scup_rel_sim
*di "`s'"
*di `i'
qui{
	 count if sf_keep_sim==. | sf_rel_sim==. | bsb_keep_sim==. | bsb_rel_sim==. | scup_keep_sim==. | scup_rel_sim==. | sf_cat==. | bsb_cat ==. | scup_cat==. 
}

if `r(N)'>0{
	di `r(N)'
	levelsof my_dom if  sf_keep_sim==. | sf_rel_sim==. | bsb_keep_sim==. | bsb_rel_sim==. | scup_keep_sim==. | scup_rel_sim==. | sf_cat==. | bsb_cat ==. | scup_cat==. 
	
}
}
}

*/


************** Generate catch draw files ******************

* This code pulls in the catch-per-trip data estimated by the copula function in R
* note: split this into one or afew states at a time to avoid memory issues 

local statez "MA"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow 
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}


local statez "RI"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow 
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}


local statez "CT"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow 
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}




local statez "NY"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}



local statez "NJ"
foreach s of local statez {

    forvalues i = 1/1 {

		local s = "NJ"
		local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\archive\proj_catch_draws_xlsdta\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\archive\directed_trips_calibration_new_csvxlsx\directed_trips_calibration_new_`s'.xlsx", clear firstrow
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        *save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}
		use `master', clear



local statez "DE"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}


local statez "MD"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}



local statez "VA"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}




local statez "NC"
foreach s of local statez {

    forvalues i = 1/100 {

		*local s = "RI"
		*local i=1
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace sf_rel_sim=round(sf_rel_sim)
		replace bsb_rel_sim=round(bsb_rel_sim)
		replace scup_rel_sim=round(scup_rel_sim)

        gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

        mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat,  mv(0) override

        order my my_dom_id_string sim_id id sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat

        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
	
        import excel using "$iterative_input_data_cd\directed_trips_calibration_new_`s'.xlsx", clear firstrow
        keep if draw == `i'

        * waves
        gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)

        tostring month1,  replace
        tostring wave, gen(wave1)

        drop if dtrip == 0

        keep day_i date mode month month1 dtrip wave wave1 draw day

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + wave1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n

        gen my_dom_id_string = "`s'_" + wave1 + "_" + mode + "_SF"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {

            *  (i)  Trips skeleton for this domain
            use `trips', clear

            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof wave, local(wv)
            levelsof mode, local(md)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  (ii)  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
			           
			count
			local n_obs=`r(N)'
			
            if `n_obs' == 0 {     
				set obs `n'
                foreach v in sf_keep_sim sf_rel_sim sf_cat bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     


            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw   day 

            order draw my_dom_id_string mode date month day wave day_i tripid catch_draw dtrip

            sort date tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}

