


/*******************************************************************************
 Script:       set_regulations.do
 Purpose:      Writes the bag limit and minimum size in force on every
               day x mode of the calibration year, for summer flounder, black
               sea bass and scup, for one state. Each state's regulations are
               a hand-transcribed block of season windows taken from that
               state's published measures. This is the reference table the
               whole model calibrates against: it determines which simulated
               fish are legal to keep.
 Inputs:       None - operates on the dataset already in memory.
 Outputs:      None - adds the variables fluke_bag, fluke_min, bsb_bag,
               bsb_min, scup_bag, scup_min, mode, month and day to the
               in-memory dataset.
 Dependencies: The calling dataset must already contain `state', `mode' (as
               "pr"/"fh"/"sh") and a `day' date variable. Called
               unconditionally from directed_trips_calibration.do; it is not
               gated by any wrapper toggle and is not listed in
               model_wrapper.do.
 Pipeline:     Nested inside step 4 of model_wrapper.do. Runs once per state,
               inside that script's state loop.

 THIS FILE MUST BE UPDATED EVERY YEAR. The season dates, bag limits and size
 limits below are literal values for a specific fishing year. The header note
 states the coded regulations cover FY2024 and FY2025 and so did not need
 changing for the FY2026 model - only the input MRIP data did. There is no
 check anywhere that the regulations in this file match the year of the data
 being processed, so a stale file produces a silently wrong calibration
 rather than an error.

 CRITICAL - one state at a time:
 The state blocks are selected by `levelsof state' followed by comparisons of
 the form   if "`r(levels)'" == "MA".   That is an equality test against the
 ENTIRE list of state values present in memory, not a membership test. It
 therefore only matches when the dataset holds exactly one state. If a caller
 ever passes a multi-state dataset, r(levels) becomes "CT DE MA ..." , no
 block matches, and the script silently leaves every bag at its 0 default and
 every minimum size at its 100 default - meaning no fish is legal to keep
 anywhere. The failure is silent, not an error.

 Default sentinels: bags initialize to 0 and minimum sizes to 100 (inches).
 Any day x mode not covered by a season window below therefore represents a
 closed fishery, which is the intended reading of "no regulation listed".
*******************************************************************************/

***********Set regulations for the calibration period and status-quo regulations for projection period***********
* These need to be changed every year


* Generate regulations for the calibration period
* Calibration period covers (year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))
* The regulations coded below are for FY 2024 and 2025, so do not have to change them for the FY2026 model
* Only need to change the input MRIP data when running the final model

* Generate variables for bag limits and minimum sizes
gen fluke_bag = 0
gen bsb_bag = 0
gen scup_bag = 0
gen fluke_min = 100
gen bsb_min = 100
gen scup_min = 100

* Convert fishing mode abbreviations to match the regulations table
gen fishing_mode=mode
drop mode month  day1
gen mode = ""
replace mode = "Private" if fishing_mode == "pr"
replace mode = "For-hire" if fishing_mode == "fh"
replace mode = "shore" if fishing_mode == "sh"

* Extract month and day from date variable for season comparisons
rename day date
gen month = month(date)
gen day = day((date))

* Exclude days where there was no directed trips 
* drop if dtrip==0

/* The season windows below are written as month/day inequalities rather than
   date comparisons. The recurring idiom
       inrange(month,5,9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
   means "May 24 through September 23": inside months 5-9, but excluding the
   part of May before the 24th and the part of September after the 23rd.
   Reading these correctly matters when updating the file - the two trailing
   conditions are open-ended within their end months, not standalone tests.

   Note also that r(levels) set here is relied upon by every state block for
   the rest of the file. It survives because `replace' is not an r-class
   command and so does not clear r(); inserting any r-class command between
   the blocks would break every comparison after it. */
* Create season indicator variables for each state and species
levelsof state, clean
****************
***** MASSACHUSETTS (MA)
****************
if "`r(levels)'" == "MA" {
    * SF (Fluke) - MA
    replace fluke_bag = 5 if mode == "Private" & inrange(month, 5, 9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
    replace fluke_min = 17.5 if mode == "Private" & inrange(month, 5, 9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
    
    replace fluke_bag = 5 if mode == "For-hire" & inrange(month, 5, 9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
    replace fluke_min = 17.5 if mode == "For-hire" & inrange(month, 5, 9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
    
    replace fluke_bag = 5 if mode == "shore" & inrange(month, 5, 9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
    replace fluke_min = 16.5 if mode == "shore" & inrange(month, 5, 9) & (month > 5 | day >= 24) & (month < 9 | day <= 23)
    
    * BSB (Black Sea Bass) - MA
    replace bsb_bag = 4 if mode == "Private" & ((month == 5 & day >= 18) | inrange(month, 6, 8) | (month == 9 & day <= 3))
    replace bsb_min = 16.5 if mode == "Private" & ((month == 5 & day >= 18) | inrange(month, 6, 8) | (month == 9 & day <= 3))
    
    replace bsb_bag = 4 if mode == "For-hire" & ((month == 5 & day >= 18) | inrange(month, 6, 8) | (month == 9 & day <= 3))
    replace bsb_min = 16.5 if mode == "For-hire" & ((month == 5 & day >= 18) | inrange(month, 6, 8) | (month == 9 & day <= 3))
    
    replace bsb_bag = 4 if mode == "shore" & ((month == 5 & day >= 18) | inrange(month, 6, 8) | (month == 9 & day <= 3))
    replace bsb_min = 16.5 if mode == "shore" & ((month == 5 & day >= 18) | inrange(month, 6, 8) | (month == 9 & day <= 3))
    
    * Scup - MA
    replace scup_bag = 30 if mode == "Private" & inrange(month, 5, 12)
    replace scup_min = 11 if mode == "Private" & inrange(month, 5, 12)
    
    replace scup_bag = 40 if mode == "For-hire" & ((month == 5) | (month == 6))
    replace scup_min = 11 if mode == "For-hire" & ((month == 5) | (month == 6))
    
    replace scup_bag = 30 if mode == "For-hire" & inrange(month, 7, 12)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 7, 12)
    
    replace scup_bag = 30 if mode == "shore" & inrange(month, 5, 12)
    replace scup_min = 9.5 if mode == "shore" & inrange(month, 5, 12)
}

****************
***** RHODE ISLAND (RI)
****************
if "`r(levels)'" == "RI" {
    * SF (Fluke) - RI
    replace fluke_bag = 6 if inrange(month, 4, 12)
    replace fluke_min = 19 if inrange(month, 4, 12)
    
    * BSB (Black Sea Bass) - RI
    replace bsb_bag = 2 if mode == "Private" & ((month == 5 & day >= 22) | (month == 6) | (month == 7) | (month == 8 & day <= 26))
    replace bsb_min = 16.5 if mode == "Private" & ((month == 5 & day >= 22) | (month == 6) | (month == 7) | (month == 8 & day <= 26))
    
    replace bsb_bag = 3 if mode == "Private" & ((month == 8 & day >= 27) | inrange(month, 9, 12))
    replace bsb_min = 16.5 if mode == "Private" & ((month == 8 & day >= 27) | inrange(month, 9, 12))
    
    replace bsb_bag = 2 if mode == "For-hire" & ((month == 6 & day >= 18) | (month == 7) | (month == 8 & day <= 31))
    replace bsb_min = 16 if mode == "For-hire" & ((month == 6 & day >= 18) | (month == 7) | (month == 8 & day <= 31))
    
    replace bsb_bag = 6 if mode == "For-hire" & inrange(month, 9, 12)
    replace bsb_min = 16 if mode == "For-hire" & inrange(month, 9, 12)
    
    replace bsb_bag = 2 if mode == "shore" & ((month == 5 & day >= 22) | (month == 6) | (month == 7) | (month == 8 & day <= 26))
    replace bsb_min = 16.5 if mode == "shore" & ((month == 5 & day >= 22) | (month == 6) | (month == 7) | (month == 8 & day <= 26))
    
    replace bsb_bag = 3 if mode == "shore" & ((month == 8 & day >= 27) | inrange(month, 9, 12))
    replace bsb_min = 16.5 if mode == "shore" & ((month == 8 & day >= 27) | inrange(month, 9, 12))
    
    * Scup - RI
    replace scup_bag = 30 if mode == "Private" & inrange(month, 5, 12)
    replace scup_min = 11 if mode == "Private" & inrange(month, 5, 12)
    
    replace scup_bag = 30 if mode == "For-hire" & inrange(month, 5, 8)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 5, 8)
    
    replace scup_bag = 40 if mode == "For-hire" & inrange(month, 9, 10)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 9, 10)
    
    replace scup_bag = 30 if mode == "For-hire" & (month == 11 | month == 12)
    replace scup_min = 11 if mode == "For-hire" & (month == 11 | month == 12)
    
    replace scup_bag = 30 if mode == "shore" & inrange(month, 5, 12)
    replace scup_min = 9.5 if mode == "shore" & inrange(month, 5, 12)
}

****************
***** CONNECTICUT (CT)
****************
if "`r(levels)'" == "CT" {
    * SF (Fluke) - CT
    replace fluke_bag = 3 if ((month == 5 & day >= 4) | (month == 6) | (month == 7) | (month == 8 & day <= 1))
    replace fluke_min = 19 if ((month == 5 & day >= 4) | (month == 6) | (month == 7) | (month == 8 & day <= 1))
    
    replace fluke_bag = 3 if ((month == 8 & day >= 2) | (month == 9) | (month == 10 & day <= 15))
    replace fluke_min = 19.5 if ((month == 8 & day >= 2) | (month == 9) | (month == 10 & day <= 15))
    
    * BSB (Black Sea Bass) - CT
    replace bsb_bag = 5 if mode == "Private" & ((month == 5 & day >= 18) | (month == 6 & day <= 23))
    replace bsb_min = 16 if mode == "Private" & ((month == 5 & day >= 18) | (month == 6 & day <= 23))
    
    replace bsb_bag = 5 if mode == "Private" & ((month == 7 & day >= 8) | inrange(month, 8, 10) | (month == 11 & day <= 28))
    replace bsb_min = 16 if mode == "Private" & ((month == 7 & day >= 8) | inrange(month, 8, 10) | (month == 11 & day <= 28))
    
    replace bsb_bag = 5 if mode == "For-hire" & ((month == 5 & day >= 18) | inrange(month, 6, 8))
    replace bsb_min = 16 if mode == "For-hire" & ((month == 5 & day >= 18) | inrange(month, 6, 8))
    
    replace bsb_bag = 7 if mode == "For-hire" & inrange(month, 9, 12)
    replace bsb_min = 16 if mode == "For-hire" & inrange(month, 9, 12)
    
    replace bsb_bag = 5 if mode == "shore" & ((month == 5 & day >= 18) | (month == 6 & day <= 23))
    replace bsb_min = 16 if mode == "shore" & ((month == 5 & day >= 18) | (month == 6 & day <= 23))
    
    replace bsb_bag = 5 if mode == "shore" & ((month == 7 & day >= 8) | inrange(month, 8, 10) | (month == 11 & day <= 28))
    replace bsb_min = 16 if mode == "shore" & ((month == 7 & day >= 8) | inrange(month, 8, 10) | (month == 11 & day <= 28))
    
    * Scup - CT
    replace scup_bag = 30 if mode == "Private" & inrange(month, 5, 12)
    replace scup_min = 11 if mode == "Private" & inrange(month, 5, 12)
    
    replace scup_bag = 30 if mode == "For-hire" & inrange(month, 5, 8)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 5, 8)
    
    replace scup_bag = 40 if mode == "For-hire" & inrange(month, 9, 10)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 9, 10)
    
    replace scup_bag = 30 if mode == "For-hire" & (month == 11 | month == 12)
    replace scup_min = 11 if mode == "For-hire" & (month == 11 | month == 12)
    
    replace scup_bag = 30 if mode == "shore" & inrange(month, 5, 12)
    replace scup_min = 9.5 if mode == "shore" & inrange(month, 5, 12)
}

****************
***** NEW YORK (NY)
****************
if "`r(levels)'" == "NY" {
    * SF (Fluke) - NY
    replace fluke_bag = 3 if ((month == 5 & day >= 4) | (month == 6) | (month == 7) | (month == 8 & day <= 1))
    replace fluke_min = 19 if ((month == 5 & day >= 4) | (month == 6) | (month == 7) | (month == 8 & day <= 1))
    
    replace fluke_bag = 3 if ((month == 8 & day >= 2) | (month == 9) | (month == 10 & day <= 15))
    replace fluke_min = 19.5 if ((month == 8 & day >= 2) | (month == 9) | (month == 10 & day <= 15))
    
    * BSB (Black Sea Bass) - NY
    replace bsb_bag = 3 if ((month == 6 & day >= 23) | (month == 7) | (month == 8))
    replace bsb_min = 16.5 if ((month == 6 & day >= 23) | (month == 7) | (month == 8))
    
    replace bsb_bag = 6 if inrange(month, 9, 12)
    replace bsb_min = 16.5 if inrange(month, 9, 12)
    
    * Scup - NY
    replace scup_bag = 30 if mode == "Private" & inrange(month, 5, 12)
    replace scup_min = 11 if mode == "Private" & inrange(month, 5, 12)
    
    replace scup_bag = 30 if mode == "For-hire" & inrange(month, 5, 8)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 5, 8)
    
    replace scup_bag = 40 if mode == "For-hire" & inrange(month, 9, 10)
    replace scup_min = 11 if mode == "For-hire" & inrange(month, 9, 10)
    
    replace scup_bag = 30 if mode == "For-hire" & (month == 11 | month == 12)
    replace scup_min = 11 if mode == "For-hire" & (month == 11 | month == 12)
    
    replace scup_bag = 30 if mode == "shore" & inrange(month, 5, 12)
    replace scup_min = 9.5 if mode == "shore" & inrange(month, 5, 12)
}

****************
***** NEW JERSEY (NJ)
****************
if "`r(levels)'" == "NJ" {
    * SF (Fluke) - NJ
    replace fluke_bag = 3 if ((month == 5 & day >= 4) | inrange(month, 6, 8) | (month == 9 & day <= 25))
    replace fluke_min = 18 if ((month == 5 & day >= 4) | inrange(month, 6, 8) | (month == 9 & day <= 25))
    
    * BSB (Black Sea Bass) - NJ
    replace bsb_bag = 10 if ((month == 5 & day >= 17) | (month == 6 & day <= 19))
    replace bsb_min = 12.5 if ((month == 5 & day >= 17) | (month == 6 & day <= 19))
    
    replace bsb_bag = 1 if ((month == 7) | (month == 8))
    replace bsb_min = 12.5 if ((month == 7) | (month == 8))
    
    replace bsb_bag = 10 if month == 10
    replace bsb_min = 12.5 if month == 10
    
    replace bsb_bag = 15 if (month == 11 | month == 12)
    replace bsb_min = 12.5 if (month == 11 | month == 12)
    
    * Scup - NJ
    replace scup_bag = 30 if (inrange(month, 1, 6) | inrange(month, 9, 12))
    replace scup_min = 10 if (inrange(month, 1, 6) | inrange(month, 9, 12))
}

****************
***** DELAWARE (DE)
****************
if "`r(levels)'" == "DE" {
    * SF (Fluke) - DE
    replace fluke_bag = 4 if inrange(month, 1, 5)
    replace fluke_min = 16 if inrange(month, 1, 5)
    
    replace fluke_bag = 4 if inrange(month, 6, 12)
    replace fluke_min = 17.5 if inrange(month, 6, 12)
    
    * BSB (Black Sea Bass) - DE
    replace bsb_bag = 15 if ((inrange(month, 5, 9)) | (month == 10 & day >= 10) | inrange(month, 11, 12))
    replace bsb_min = 13 if ((inrange(month, 5, 9)) | (month == 10 & day >= 10) | inrange(month, 11, 12))
    
    * Scup - DE
    replace scup_bag = 30 if inrange(month, 1, 12)
    replace scup_min = 9 if inrange(month, 1, 12)
}

****************
***** MARYLAND (MD)
****************
if "`r(levels)'" == "MD" {
    * SF (Fluke) - MD
    replace fluke_bag = 4 if inrange(month, 1, 5)
    replace fluke_min = 16 if inrange(month, 1, 5)
    
    replace fluke_bag = 4 if inrange(month, 6, 12)
    replace fluke_min = 17.5 if inrange(month, 6, 12)
    
    * BSB (Black Sea Bass) - MD
    replace bsb_bag = 15 if ((inrange(month, 5, 9)) | (month == 10 & day >= 10) | inrange(month, 11, 12))
    replace bsb_min = 13 if ((inrange(month, 5, 9)) | (month == 10 & day >= 10) | inrange(month, 11, 12))
    
    * Scup - MD
    replace scup_bag = 30 if inrange(month, 1, 12)
    replace scup_min = 9 if inrange(month, 1, 12)
}

****************
***** VIRGINIA (VA)
****************
if "`r(levels)'" == "VA" {
    * SF (Fluke) - VA
    replace fluke_bag = 4 if inrange(month, 1, 5)
    replace fluke_min = 16 if inrange(month, 1, 5)
    
    replace fluke_bag = 4 if inrange(month, 6, 12)
    replace fluke_min = 17.5 if inrange(month, 6, 12)
    
    * BSB (Black Sea Bass) - VA
    replace bsb_bag = 15 if ((inrange(month, 5, 7) & (month < 7 | day <= 15)) | ((month == 7 & day >= 27) | inrange(month, 8, 12)))
    replace bsb_min = 13 if ((inrange(month, 5, 7) & (month < 7 | day <= 15)) | ((month == 7 & day >= 27) | inrange(month, 8, 12)))
    
    * Scup - VA
    replace scup_bag = 30 if inrange(month, 1, 12)
    replace scup_min = 9 if inrange(month, 1, 12)
}

****************
***** NORTH CAROLINA (NC)
****************
if "`r(levels)'" == "NC" {
    * SF (Fluke) - NC
    replace fluke_bag = 1 if ((month == 8 & day >= 16) | (month == 9 & day <= 3))
    replace fluke_min = 15 if ((month == 8 & day >= 16) | (month == 9 & day <= 3))
    
    * BSB (Black Sea Bass) - NC
    replace bsb_bag = 15 if ((inrange(month, 5, 9)) | (month == 10 & day >= 10) | inrange(month, 11, 12))
    replace bsb_min = 13 if ((inrange(month, 5, 9)) | (month == 10 & day >= 10) | inrange(month, 11, 12))
    
    * Scup - NC
    replace scup_bag = 30 if inrange(month, 1, 12)
    replace scup_min = 9 if inrange(month, 1, 12)
}

* Drop temporary variables
drop mode 
rename fishing_mode mode
*************************

tempfile regulations
save `regulations', replace 

*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full

gen day=day(day_y2)
gen month=month(day_y2)
gen year_y2=year(day_y2)
drop if day_y2==$leap_yr_days
gen dow_y2 = dow(day_y2)  

gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		
replace kod_y2="we" if $fed_holidays_y2

gen month2_y2= string(month,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup


merge 1:m  mode day month using `regulations'
drop if date==$leap_yr_days
drop _merge 
order year mode month kod dow day  draw fluke_bag fluke_min bsb_bag bsb_min scup_bag scup_min  day_y2 dow_y2 kod_y2 month_y2
sort  state mode date draw


*************************
*Create status-quo regualtions for projection period here: td(01jan2026)  -  td(31dec2026)
gen fluke_bag_y2=fluke_bag 
gen fluke_min_y2=fluke_min

gen bsb_bag_y2=bsb_bag
gen bsb_min_y2=bsb_min

gen scup_bag_y2=scup_bag
gen scup_min_y2=scup_min

*************************


*translate to inches
replace fluke_min = fluke_min*2.54
replace bsb_min = bsb_min*2.54
replace scup_min = scup_min*2.54

replace fluke_min_y2 = fluke_min_y2*2.54
replace bsb_min_y2 = bsb_min_y2*2.54
replace scup_min_y2 = scup_min_y2*2.54


