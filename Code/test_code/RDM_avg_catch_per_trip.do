/*******************************************************************************
 Script:       RDM_avg_catch_per_trip.do
 Purpose:      Computes a post-hoc adjustment scalar to correct already-
               published RDM output for a later MRIP data release. The FY2026
               groundfish RDM was run on one vintage of MRIP data; MRIP then
               released preliminary 2025 wave 5 and revised 2025 wave 4 data
               in January 2026. Rather than re-running the entire model, this
               script recomputes total catch both ways - using the data the
               RDM actually consumed, and using the newer data - at the RDM's
               own strata, and takes the ratio.

               Total catch is built the same way the model builds it: total
               directed trips at wave x month x kind-of-day x mode, multiplied
               by mean catch-per-trip at wave x mode. Doing it identically on
               both vintages is what makes the ratio interpretable as a data
               revision effect rather than a method difference.
 Inputs:       MRIP trip and catch files for the two data vintages.
 Outputs:      The adjustment scalars, for manual application to existing RDM
               output.
 Dependencies: MRIP data for both vintages must be available.
 Pipeline:     Development/QA scratch, run after a model round. Not called by
               any wrapper.

 Note the cross-project origin: the header refers to the GROUNDFISH RDM's
 FY2026 cycle, but the file lives in the flukeRDM repo. The two projects share
 developers and this approach; treat the specific year and wave references as
 describing that groundfish round, not this repo's calibration year.
*******************************************************************************/



* We originally used MRIP data from (year==2025 & inlist(wave, 1, 2, 3, 4)) | (year==2024 & inlist(wave, 5, 6)) for the groundfish RDM in FY2026's mgt. cycle. 
* MRIP released preliminary 2025w5 and updated preliminary 2025w4 data on January 21, 2026. 
* This file computes a scalar to adjust the original RDM output based on newly available/updated data MRIP data.

* I compute total directed trips at the same strata as in the RDM: wave, month, kind-of-day, mode 
* I compute mean catch-per-trip at the same strata as in the RDM: wave and mode 
* I then multiply total directed trips by the corresponding mean catch-per-trip point estimate to arrive at total catch.
* I do this based on data that was actually used in the RDM and based on the new data. 
* multiplier = (rec. catch in Wave 5 of 2025) / (rec. catch in Wave 5 of 2024) 

* set a global seed #
global seed 03211990

* years/waves of MRIP data. 
global yr_wvs 20221 20222 20223 20224 20225 20226 ///
					 20231 20232 20233 20234 20235 20236  ///
					 20241 20242 20243 20244 20245 20246  ///
					 20251 20252 20253 20254 20255 20256
					 
global yearlist 2023 2024 2025
global wavelist 1 2 3 4 5 6

global input_data_cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025" /* Lou's local data path */
global calibration_year "(year==2025 & inlist(wave, 1, 2, 3)) | (year==2024) | (year==2022 & inlist(wave, 4, 5, 6))"
global calibration_year_trips "(year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))"

// 1) Pull the MRIP data



*This code only needs to be run once after new MRIP data enters the repo
cd $input_data_cd

foreach wave in	$yr_wvs {				

capture confirm file "trip_`wave'.dta"
if _rc==0{
	use trip_`wave'.dta, clear
	renvarlab, lower
	save trip_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_b2_`wave'.dta"
if _rc==0{
	use size_b2_`wave'.dta, clear
	renvarlab, lower
	save size_b2_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_`wave'.dta"
if _rc==0{
	use size_`wave'.dta, clear
	renvarlab, lower
	save size_`wave'.dta, replace
}

else{
	
}

capture confirm file "catch_`wave'.dta"
if _rc==0{
	use catch_`wave'.dta, clear
	renvarlab, lower
	save catch_`wave'.dta, replace
}

else{
	
}

}
*

/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year in $yearlist{
	foreach wave in  $wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}


cd $input_data_cd

clear
*mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")


* Format MRIP data for estimation 
gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

* Ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

keep if $calibration_year
 
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

*gen my_dom_id_string=area_s+"_"+month+"_"+common_dom
*gen my_dom_id_string=area_s+"_"+common_dom+"_X"
gen my_dom_id_string=state+"_"+wv2+"_"+mode1+"_"+common_dom

* Define the list of species to process
local species "summerflounder blackseabass scup"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "summerflounder" local short "sf"
    if "`s'" == "blackseabass"     local short "bsb"
    if "`s'" == "scup"     			 local short "scup"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)

    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
}

rename sum_sf_tot_cat sf_cat
rename sum_sf_harvest sf_keep
rename sum_sf_releases sf_rel
rename sum_bsb_tot_cat bsb_cat
rename sum_bsb_harvest bsb_keep
rename sum_bsb_releases bsb_rel
rename sum_scup_tot_cat scup_cat
rename sum_scup_harvest scup_keep
rename sum_scup_releases scup_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "summerflounder")==0
replace no_dup=1 if strmatch(common, "blackseabass")==0
replace no_dup=1 if strmatch(common, "scup")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="SF"

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

/*
local vars sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel  scup_catch scup_keep scup_rel
foreach v of local vars{
	replace `v'=round(`v')
}
*/

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains_catch
save `domains_catch', replace 
restore


* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in   sf_cat   bsb_cat   scup_cat  {

    * Run svy mean for the variable by domain
    svy: mean `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear


split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains_catch' 
sort varname  my_dom_id

*keep varname mean se 

split my_dom_id_s, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode

*destring month, replace 
drop my*
drop _merge
drop se ll ul 
drop domain
gen kod="wd"
expand 2, gen(dup)
replace kod="we" if dup==1
drop dup 

expand 2, gen(dup)
gen month="03" if dup==0 & wave=="2"
replace month="04" if dup==1 & wave=="2"
replace month="05" if dup==0 & wave=="3"
replace month="06" if dup==1 & wave=="3"
replace month="07" if dup==0 & wave=="4"
replace month="08" if dup==1 & wave=="4"
replace month="09" if dup==0 & wave=="5"
replace month="10" if dup==1 & wave=="5"
replace month="11" if dup==0 & wave=="6"
replace month="12" if dup==1 & wave=="6"

drop dup


tempfile catch 
save `catch', replace 


*******Trips 
clear

tempfile tl1 cl1
dsconcat $triplist

/*dtrip will be used to estimate total directed trips*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
keep if $calibration_year_trips

 /* ensure only relevant states */
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)


 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 


replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "summerflounder", "blackseabass", "scup") 
mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")


* generate estimation strata

/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh)*/
gen my_dom_id_string=state+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id+"_"+w2


replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 month
rename my_dom_id_string4 kod
rename my_dom_id_string5 mode
rename my_dom_id_string6 dom_id
rename my_dom_id_string7 wave

rename b dtrip

keep if dom_id=="1" // keep directed trip estimates for species group of interest 

drop if dtrip==.
replace se=. if dtrip==se

keep if dom_id=="1"
drop dom my se ll ul 

/*
gen species = "cod"
expand 2, gen(dup)
replace species="hadd" if dup==1
drop dup
*/
gen varname="sf_cat"
expand 2, gen(dup)
replace varname="bsb_cat" if dup==1
drop dup 
expand 2 if varname=="bsb_cat", gen(dup)
replace varname="scup_cat" if dup==1
drop dup 
duplicates tag, gen(dup)
duplicates drop 
merge 1:m varname state month wave mode kod using `catch'
*keep if _merge==3
drop t pvalue df crit eform dup
keep if varname=="bsb_cat"
drop year
order state wave month kod mode varname dtrip mean
mvencode dtrip mean, mv(0) override
drop _merge

gen total_catch=dtrip*mean
rename mean avg_catch_per_trip
sort state mode wave month kod 
rename  dtrip directed_trips_2024
rename varname species 
replace species="black sea bass"
order species state mode wave month kod  directed_trips_2024 avg_catch_per_trip total_catch
rename kod kind_of_day
browse if wave=="1"
drop if wave=="1"
browse

collapse (sum) total_catch directed_trips_2024 , by(state )
gen avg_catch_per_trip=total_catch/directed_trips_2024
destring month, replace
encode st, gen(st2)
xtset st2 month 
tsline total_catch if state=="MA"
tsline avg if state=="MA"
tsline directed_trips_2024 total_catch if state=="MA", msymbol(o)
twoway (scatter total_catch month if state=="MA", connect(direct) yaxis(1))  ///
			(scatter directed_trips_2024 month if state=="MA", connect(direct) yaxis(1))   ///
			(scatter avg_catch_per_trip month if state=="MA", connect(direct) yaxis(2) ///
			legend(position(bottom) cols(3)))
			
keep if state=="MA"
