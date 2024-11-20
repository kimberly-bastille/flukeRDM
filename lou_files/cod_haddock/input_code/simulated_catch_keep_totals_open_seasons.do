

global domainz
qui forv i=1/20{
*local i=2

import delimited using "$draw_file_cd\catch_draws`i'_full.csv", clear 

gen year2=substr(day, 6, 4)
destring year2, replace

gen date=mdy( month, day1, year2)
format date %td

gen open=1 if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 ))
replace open=0 if open==.

preserve
keep day day_i mode open
duplicates drop 
tempfile seasons
save `seasons', replace 
restore 

keep if catch_draw==1
collapse (mean)  cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch, by(day day_i mode)
*local i=1
preserve 
import delimited using  "$input_code_cd\directed_trips_calib_150draws_cm.csv", clear 
keep if draw==`i'
keep mode day  dtrip
tempfile dtrip
save `dtrip', replace
restore

merge 1:1 mode day  using `dtrip'
mvencode cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch dtrip, mv(0) override
drop _merge

*sort mode day_i 

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

merge m:1 day day_i mode using `seasons',keep(3) nogen
collapse (sum) tot* dtrip, by(mode open)
gen draw=`i'

	
tempfile domainz`i'
save `domainz`i'', replace
global domainz "$domainz "`domainz`i''" " 

}


clear
dsconcat $domainz

sort draw mode open 
order draw mode open 

export delimited using "$draw_file_cd\simulated_catch_totals_open_season.csv", replace 


