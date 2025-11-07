


* This script create calibration catch draw files for each state that contains:
	* 50 trips in each day of the calibration year in which there were directed trips, each with 30 draws of catch-per-trip
	* demographics for each trip that are constant across catch draws
	
	
********************************
* Demographics: age and avidity (number trips past 12 months) 
	* Ages and avidity come from the fishing effort survey 12 MONTH files. 
	* These data are NOT publicly available and the data have not been processeed for QA/QC like the publicly available 2-month files. 
	* Data from 2018-2023 was delivered by Lucas Johanssen on 4/23/2025. A few notes/caveats from Lucas:
		* "FES QC processes focus on the 2-month reference periods, and we do very little evaluation and editing of 12-month effort responses.  
		* Responses for these fields are essentially unedited, raw data.  
		* The final weight trimming procedures focus on reducing the impacts of outlier values on wave-level estimates. 
		* The data may include records that are highly influential with respect to 12-month effort and any estimates may be highly variable.  
		* Wave data will produce independent estimates of 12-month effort."
		
*I will use the most recent year of FES survey data available (2023)

global dems
local wvs 1 2 3 4 5 6
foreach w of local wvs{
	
u  "$input_data_cd\fes_person_final_2023`w'.dta", clear 

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
keep if state!=""

tempfile dems`w'
save `dems`w'', replace
global dems "$dems "`dems`w''" " 

}
clear
dsconcat $dems

gen total_trips_12=boat_trips_12+shore_trips_12
gen total_trips_2=boat_trips+shore_trips

* Lou's QA/QC on the FES data 

drop if age==-3 // drop missing ages
keep if age>=16 // drop anglers below the minimum age required for license to align the age distribution with choice experiment sampling frame, which is based on licensees (16+)

replace total_trips_2=round(total_trips_2)
replace total_trips_12=round(total_trips_12)
drop if total_trips_2>total_trips_12 // drop if total trips 2 months>total trips 12 months

drop if total_trips_2>=62 // drop if total trips 2 months>60 
drop if total_trips_12>=365 // drop if total trips 12 months>365 

replace final=final/100 // sum of weights is almost 300 million, so I proportionally reduce the weights so my Stata doesn't blow up
replace final=round(final)

expand final 
su total_trips_12, detail  

egen p9995 = pctile(total_trips_12), p(99.95) // drop total_trips_12 above the 99.95 percentile
drop if total_trips_12>p9995

keep age total_trips_12 wave state
save "$input_data_cd\angler_dems.dta", replace 

********************************

/*
* Check to make sure the copula model did not produce NAs
local statez "MA RI"
foreach s of local statez{
	
forv i=1/100{

import excel using "$iterative_input_data_cd\catch_draws_`s'_`i'.xlsx", clear firstrow
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
*note 7/21/25 - split this three states at a time to avoid cpu memory issues 


local statez "MA RI CT"
foreach s of local statez {

    forvalues i = 1/$ndraws {

		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$input_data_cd\calib_catch_draws_`s'_`i'.xlsx", clear firstrow

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
	
        import delimited using "$iterative_input_data_cd\directed_trips_calibration_`s'.csv", clear
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
			
            if `n_obs' == 0 {                     // domain absent in catch draws
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

            tostring tripid, gen(tripid2)
            gen id_code = tripid2 + "_" + date
            egen group_id = group(id_code)
			distinct id_code
			return list
			local n_dems = `r(ndistinct)' // number of trips/anglers requiring demographics/trip costs that are constant across catch draws
            drop tripid2 id_code             

            *  (iii)  Angler demographics
            preserve
                use "$input_data_cd\angler_dems.dta", clear
                keep if wave == `wv' & state == "`s'"
				count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile dems
                save `dems', replace
			restore
				
			merge m:1 group_id using `dems', keep(3) nogen

            *  (iv)  Trip costs
            preserve
                use "$input_data_cd\trip_costs.dta", clear
                keep if state == "`s'" & mode1 == `md'
                count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile costs
                save `costs', replace
            restore
			
            merge m:1 group_id using `costs', keep(3) nogen

            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw age total_trips_12 day cost

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

        save "$iterative_input_data_cd\calib_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}



mata: mata clear
clear
local statez "NY NJ DE"

foreach s of local statez {

    forvalues i = 1/$ndraws {
     
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$input_data_cd\calib_catch_draws_`s'_`i'.xlsx", clear firstrow

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
	
        import delimited using "$iterative_input_data_cd\directed_trips_calibration_`s'.csv", clear
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
			
            if `n_obs' == 0 {                     // domain absent in catch draws
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

            tostring tripid, gen(tripid2)
            gen id_code = tripid2 + "_" + date
            egen group_id = group(id_code)
			distinct id_code
			return list
			local n_dems = `r(ndistinct)' // number of trips/anglers requiring demographics/trip costs that are constant across catch draws
            drop tripid2 id_code             

            *  (iii)  Angler demographics
            preserve
                use "$input_data_cd\angler_dems.dta", clear
                keep if wave == `wv' & state == "`s'"
				count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile dems
                save `dems', replace
			restore
				
			merge m:1 group_id using `dems', keep(3) nogen

            *  (iv)  Trip costs
            preserve
                use "$input_data_cd\trip_costs.dta", clear
                keep if state == "`s'" & mode1 == `md'
                count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile costs
                save `costs', replace
            restore
			
            merge m:1 group_id using `costs', keep(3) nogen

            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw age total_trips_12 day cost

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

        save "$iterative_input_data_cd\calib_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}



mata: mata clear
clear
local statez "MD VA NC"
foreach s of local statez {

    forvalues i = 1/$ndraws {
     
		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
        import excel using "$input_data_cd\calib_catch_draws_`s'_`i'.xlsx", clear firstrow

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
	
        import delimited using "$iterative_input_data_cd\directed_trips_calibration_`s'.csv", clear
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
			
            if `n_obs' == 0 {                     // domain absent in catch draws
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

            tostring tripid, gen(tripid2)
            gen id_code = tripid2 + "_" + date
            egen group_id = group(id_code)
			distinct id_code
			return list
			local n_dems = `r(ndistinct)' // number of trips/anglers requiring demographics/trip costs that are constant across catch draws
            drop tripid2 id_code             

            *  (iii)  Angler demographics
            preserve
                use "$input_data_cd\angler_dems.dta", clear
                keep if wave == `wv' & state == "`s'"
				count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile dems
                save `dems', replace
			restore
				
			merge m:1 group_id using `dems', keep(3) nogen

            *  (iv)  Trip costs
            preserve
                use "$input_data_cd\trip_costs.dta", clear
                keep if state == "`s'" & mode1 == `md'
                count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile costs
                save `costs', replace
            restore
			
            merge m:1 group_id using `costs', keep(3) nogen

            keep my_dom_id_string draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date day_i dtrip wave ///
                 tripid catch_draw age total_trips_12 day cost

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

        save "$iterative_input_data_cd\calib_catch_draws_`s'_`i'.dta", replace
        clear                               
    }
}

