
/*******************************************************************************
 Script:       survey_trip_costs.do
 Purpose:      Builds a simulated trip-cost distribution for each
               state x mode, to supply the cost term in the angler utility
               function. Costs are modeled as a two-part ("hurdle") process
               estimated from the 2022 expenditure survey: a Bernoulli draw
               decides whether the trip incurs any expenditure at all, and a
               lognormal draw sets the amount when it does. Both parts are
               estimated with survey weights, and the lognormal is calibrated
               so its simulated mean reproduces the survey-estimated mean of
               positive costs. 10,000 trips are simulated per domain.
 Inputs:       gulf_atl_2022.dta, prim1.dta, prim2.dta
 Outputs:      trip_costs.dta
 Dependencies: Globals $misc_data_cd, $seed and $inflation_expansion.
               Requires the user-written command renvarlab, and xsvmat for
               extracting the survey estimation table.
 Pipeline:     Step 5 of model_wrapper.do, gated by the `costs_per_trip'
               toggle (default ON). Its output is read by the R calibration
               and projection stages, which assign each simulated trip a cost.

 Why a hurdle model rather than a single distribution: a large share of
 recreational trips report zero expenditure (shore anglers especially), so
 the cost distribution has a spike at zero that no single continuous
 distribution fits well. Splitting "did they spend anything" from "how much"
 lets each part be estimated where it applies.
*******************************************************************************/

* New code 4/30/26

** This code creates trip cost distributions based on the Sabrina's 2012 trip expenditure survey data

/* The header comment says 2012 but the data file read below is the 2022
   survey. The 2012 reference is stale. */

/* Seeded from the shared $seed so the 10,000 simulated cost draws are
   reproducible across runs. */
set seed $seed

display "survey_trip_costs.do: estimating survey-weighted hurdle cost models by state x mode and simulating 10,000 trip costs per domain. The per-domain svy estimation loops may take several minutes."

*Enter a directory with the expenditure survey data 
u "$misc_data_cd\gulf_atl_2022.dta", clear
renvarlab *, lower


/**************************************************/
/**************************************************/
/* Section A: Load and clean expenditure survey   */
/**************************************************/
/**************************************************/

/* The mode-specific blanking below removes expenditure categories that cannot
   logically apply to that mode - a for-hire passenger does not buy boat fuel
   or rent a boat, a private-boat angler does not pay a guide or tip crew, and
   a shore angler does none of these. Nonzero values in those cells are
   survey reporting errors, so they are set to missing rather than kept.
   They are then folded to zero by the mvencode below, which is what makes
   them drop out of the expenditure total. */
* As per Sabrina, run the following code before using the 2022 data. This code sets certain expenditure variables to missing depending on the trip mode.
* For-Hire trips: set boat fuel and boat rental to missing
replace bfuelexp = . if mode == "For-Hire"
replace brentexp = . if mode == "For-Hire"

* Private Boat trips: set guide costs and crew tips to missing
replace guideexp = . if mode == "Private Boat"
replace crewexp  = . if mode == "Private Boat"

* Shore trips: set all of those to missing
replace bfuelexp = . if mode == "Shore"
replace crewexp  = . if mode == "Shore"
replace guideexp = . if mode == "Shore"
replace brentexp = . if mode == "Shore"


*keep only the states we need (MA-NC) 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
/* These last two are dead: the keep above retains only MA-NC, so no
   observation with st==23 or st==33 survives to be labeled. */
replace state="ME" if st==23
replace state="NH" if st==33

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override


/* The "other" free-text category collects anything the respondent wrote in.
   The four inlist blocks below zero out write-ins that are not trip costs -
   licenses, boat repair and registration are annual or capital expenses, and
   casino/spa/water-park spending is not fishing-related at all. Including
   them would inflate the per-trip cost the utility function sees. This is a
   hand-curated list against one survey's write-ins; a new survey vintage
   would need it extended. */
*replace some non-trip expenses included in "other" category as zero
replace othexp=0 if inlist(oth_cat, "2 LICENSES", "BOAT REPAIR", "Boat Towing", "CART", "FISHING LICENSE")
replace othexp=0 if inlist(oth_cat,"LICENSE", "LICENSES", "MONEY SPENT AT CASINO", "NEW ROD", "SEATOW", "SPA", "HAT")
replace othexp=0 if inlist(oth_cat,"ALL WATERS LICENSE", "ANGLER GOT A SPEEDING TICKET", "BOAT CLEANING", "CASINOS", "ENTERTAINMENT")
replace othexp=0 if inlist(oth_cat,"FIREWOOD", "POOL", "REGISTRATION", "SUNGLASSES", "TAKING BOAT TO CAR WASH", "WATER PARK", "WOOD")


* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

/* Declares the expenditure survey's complex design so every later `svy:'
   estimate carries correct weights and standard errors. singleunit(certainty)
   tells Stata to treat a stratum containing a single PSU as contributing no
   sampling variance rather than erroring out - without it the per-domain
   subpopulation estimates below would fail wherever a domain happens to have
   a lone PSU. */
svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

merge m:1 prim1 using "$misc_data_cd\prim1.dta", keep(1 3) nogen 
merge m:1 prim2 using "$misc_data_cd\prim2.dta", keep(1 3) nogen 


*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

*Adjust for inflation
replace total_exp = total_exp*$inflation_expansion

/* common_dom splits the sample into trips that targeted one of the three
   managed species ("1") and everything else ("2"). Only domain "1" is kept
   for the final output - the model needs the cost of a DIRECTED trip, and
   anglers targeting these species have a different cost profile than the
   general recreational population. Domain "2" is carried through the
   estimation anyway so the survey design degrees of freedom are computed on
   the full sample. */
*New approach computes trip cost distribution based on directed trips for sf, bsb, or scup
gen common_dom="1" if inlist(prim1_common, "SUMMER FLOUNDER", "BLACK SEA BASS", "SCUP") | inlist(prim2_common, "SUMMER FLOUNDER", "BLACK SEA BASS", "SCUP")
replace common_dom="2" if common_dom==""

*keep if common_dom=="1"

gen domain=state+"_"+mode1+"_"+common_dom
encode domain, gen(domain2)


preserve
keep domain domain2
duplicates drop 
tempfile domains
save `domains', replace 
restore


preserve
svy: mean total_exp, over(domain2)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 domain2
destring domain2, replace
merge 1:1 domain2 using `domains'

drop rname domain2 _merge 
order domain

split domain, parse(_)
rename domain1 state
rename domain2 mode
rename domain3 common_dom

renam b cost 
keep state mode common_dom cost se  ll ul
order state mode common_dom cost se  ll ul
tempfile observed 
save `observed', replace 
restore


/**************************************************/
/**************************************************/
/* Section B: Estimate the hurdle model by domain */
/**************************************************/
/**************************************************/

/* Three postfile loops follow, one per quantity the simulation needs, each
   estimating over the same domain list:
     meanpos  - survey mean of cost among POSITIVE-cost trips
     p_pos    - survey mean of pos_cost, i.e. P(spend > 0), the hurdle
     ln_parms - survey means of ln(cost) and ln(cost)^2 among positive costs,
                from which the log-scale variance is recovered as
                sig2 = E[ln(cost)^2] - E[ln(cost)]^2
   postfile is used rather than collapse because each estimate needs its own
   svy subpop call; results are accumulated to a file one domain at a time.
   The 1e-10 floor on s2 guards domains with too few positive observations to
   support a variance estimate, where the second-moment subtraction can come
   out zero or slightly negative from rounding. */
*Two-part ("hurdle") simulation with a calibrated lognormal for positive costs, by state×mode domain.
drop domain
egen str5 domain = concat(state mode1 common_dom), punct("_")
encode domain, gen(dom2)

*keep if domain=="CT_pr"
svy: mean total_exp, over(dom2)
gen cost=total_exp


preserve
keep if common_dom=="1"
keep cost state mode1  
bysort state mode1: egen max_cost=max(cost)
keep state mode1 max
rename mode1 mode
duplicates drop 
tempfile max_cost
save `max_cost', replace 
restore

* Observed cap (e.g., 99th percentile) for positive costs
/*
preserve
keep if cost>0 & !missing(cost, dom2)

tempfile caps
postfile C int dom2 double cap99 using `caps', replace

levelsof dom2, local(domlist)
foreach d of local domlist {
     _pctile cost [pw=sample_wt] if dom2==`d', p(99)
    scalar cap =  r(r1)
    post C (`d') (cap)
}
postclose C
use `caps', clear
save `caps', replace
restore
*/
*----------------------------
* Cost indicators
*----------------------------
gen byte pos_cost = cost > 0 if !missing(cost)

gen double lncost  = ln(cost)  if cost > 0
gen double lncost2 = lncost^2  if cost > 0

svy: mean pos_cost, over(dom2)



*Estimate the mean positive cost by domain (survey-weighted)
*used to calibrate the lognormal so the simulated positive-cost mean matches the survey positive-cost mean.
preserve
keep if cost>0 & !missing(dom2)

tempfile meanpos
postfile M int dom2 double mean_pos using `meanpos', replace

levelsof dom2, local(domlist)
foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean cost
    matrix b = e(b)
    post M (`d') (b[1,1])
}
postclose M
use `meanpos', clear
save `meanpos', replace
restore



*estimate survey conditional mean of positive costs by domain
*provides Bernoulli probability used later in simulation: spend = (runiform() < p_hat)
preserve
tempfile p_pos
postfile P int dom2 str7 domain double p_hat se_p long N using `p_pos', replace

levelsof dom2, local(domlist)

foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean pos_cost
    matrix b = e(b)
    matrix V = e(V)

    scalar p  = b[1,1]
    scalar se = sqrt(V[1,1])

    quietly count if dom2==`d'
    local domname : label (dom2) `d'

    post P (`d') ("`domname'") (p) (se) (r(N))
}
postclose P
restore


*Estimate lognormal dispersion for positive costs by domain (survey-weighted)
*gives the shape/variance of the positive-cost distribution on the log scale.
preserve
keep if cost > 0 & !missing(dom2, lncost, lncost2)

tempfile ln_parms
postfile L int dom2 str7 domain double mu_hat m2_hat sig2_hat double v11 v22 v12 long N using `ln_parms', replace

levelsof dom2, local(domlist)

foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean lncost lncost2
    matrix b = e(b)
    matrix V = e(V)

    scalar mu  = b[1,1]
    scalar m2  = b[1,2]
    scalar s2  = m2 - mu^2
    if (s2 < 1e-10) scalar s2 = 1e-10

    quietly count if dom2==`d'
    local domname : label (dom2) `d'

    post L (`d') ("`domname'") ///
        (mu) (m2) (s2) ///
        (V[1,1]) (V[2,2]) (V[1,2]) ///
        (r(N))
}
postclose L
restore

use `p_pos', clear
merge 1:1 dom2 using `ln_parms', nogen
*merge 1:1 dom2 using `caps', nogen
merge 1:1 dom2 using `meanpos', nogen

/**************************************************/
/**************************************************/
/* Section C: Simulate 10,000 trip costs per domain*/
/**************************************************/
/**************************************************/

/* The calibration below is the key formula in this script. For a lognormal
   with log-scale parameters (mu, sig2), the mean on the ORIGINAL scale is
       E[X] = exp(mu + sig2/2)
   Setting
       mu_adj = ln(mean_pos) - sig2_hat/2
   makes exp(mu_adj + sig2_hat/2) = mean_pos exactly. In words: keep the
   spread the survey data implied, but shift the location so the simulated
   dollar-scale mean reproduces the survey's estimated mean positive cost.
   Using the raw survey mean of ln(cost) as mu instead would systematically
   UNDERSTATE mean cost, because the mean of a lognormal exceeds the
   exponential of its log-mean whenever there is any dispersion. */
*calibrate the lognormal mean to match mean_pos
*simulated positive-cost mean should line up with the survey-estimated positive-cost mean (up to Monte Carlo error), while keeping the estimated log-variance sig2_hat
gen double mu_adj = ln(mean_pos) - 0.5*sig2_hat

*keep if domain=="DE_fh"
local n_draws = 10000

expand `n_draws'
bysort dom2: gen long draw = _n


*Simulate trip costs 
*Part A - zero costs:
gen byte spend = runiform() < p_hat

* Part B  - Positive costs
gen double cost_sim = 0
replace cost_sim = exp(rnormal(mu_adj, sqrt(sig2_hat))) if spend==1

* Check mass at zero
by dom2: egen share_zero = mean(cost_sim==0)
list dom2 domain p_hat share_zero in 1/10
*replace cost_sim = cap99 if cost_sim > cap99 & spend==1

/* SUSPECTED BUG - flagged for developer confirmation, deliberately NOT fixed.
   The keep below names `cost', not `cost_sim'. `cost' was created earlier
   (gen cost=total_exp) and holds the OBSERVED survey expenditure; `cost_sim'
   is the simulated draw this whole script exists to produce. Because Stata
   resolves an exact variable-name match before considering abbreviation,
   `cost' should select the observed variable and drop cost_sim - which would
   mean the two lines below referencing cost_sim operate on a dropped
   variable, and that trip_costs.dta ships observed costs replicated 10,000
   times rather than simulated ones.
   This has not been confirmed by running the code, and the script is
   evidently in production use, so treat it as a question rather than a
   finding: does this file error at the cost_sim reference below, and if not,
   which variable ends up in trip_costs.dta? */
split domain, parse(_)
rename domain1 state
rename domain2 mode
rename domain3 common_dom
rename draw tripid
keep mode cost tripid state common_dom
compress

format cost %9.2f
order state mode common_dom tripid cost
keep if common_dom=="1"

/* Right-tail cap. A lognormal has unbounded support, so a small number of
   the 10,000 draws land far above anything the survey observed; those would
   otherwise create simulated anglers facing implausible trip costs. Each
   draw is capped at the maximum observed cost for its state x mode. An
   earlier version capped at the 99th percentile instead - that code is
   retained commented out above. */
*when cost_sim>max(observed cost), set cost_sim=cost_sim>max(observed cost), by state-mode
merge m:1 state mode using `max_cost'
replace cost=max if cost_sim>max
drop max common _merge 
/*
*compare simulated versus observed
collapse (mean) cost_sim=cost (sd) sd_cost=cost, by(state mode common_dom)
merge 1:1 state mode common_dom using `observed'
gen se_sim=sqrt(sd)

order state mode common_dom cost_sim cost se_sim se
gen pct_dif=((cost_sim-cost)/cost)*100

su pct_dif
*/

save "$misc_data_cd\trip_costs.dta", replace

display "survey_trip_costs.do: finished. Wrote trip_costs.dta."


