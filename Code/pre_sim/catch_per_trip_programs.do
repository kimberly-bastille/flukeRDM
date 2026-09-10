/*******************************************************************************
 Script:       catch_per_trip_programs.do
 Purpose:      Stata programs shared by the catch-per-trip scripts
               (catch_per_trip_calibration_part1.do,
               catch_per_trip_projection_part1.do,
               calibration_catch_per_trip_part2.do and
               catch_per_trip_projection_part2.do). Each program
               is one block that the pre-refactor scripts repeated verbatim,
               or with one or two parameters changed. Nothing here runs on
               its own: the file only DEFINES programs. Each caller does this
               file at its top, behind capture program drop guards.

               Programs, in the order defined:
                 make_domain_expr         build the +"_"+ join expression
                 sf_label_states          MRIP st code -> two-letter state
                 sf_prep_mrip_trip_catch  MRIP trip+catch -> svyset trip-level data
                 sf_post_svy_by_domain    svy: mean|total by domain -> postfile
                 decode_svy_domains       r(table) column names -> my_dom_id_string
                 split_domain_string      split my_dom_id_string into named parts
                 sf_export_mrip_totals    Part B tidy and save (MRIP totals .dta)
                 sf_impute_pse_round      one round of SE imputation for
                                          single-PSU strata
                 sf_build_cpt_strata      the whole "Part A": per-stratum mean
                                          catch-per-trip with SE, to xlsx
               Part2 (catch draw files):
                 sample_with_replacement  pool in memory -> exactly n rows,
                                          drawn with replacement
                 sf_load_directed_trips   one state's directed-trips calendar
                                          -> caller's tempfile
                 sf_expand_trips_and_ids  one draw's calendar -> 50 trips x 30
                                          catch draws, with mode_id / wave_id
                 sf_sample_catch_by_mode_wave
                                          attach a copula catch outcome to
                                          every trip row, by mode x wave

 Naming:       Programs adapted from groundfishRDM but NOT identical to the
               groundfish program of the same purpose carry an sf_ prefix, so
               that a later cross-repo refactor cannot apply one repo's
               program to the other by accident. Programs whose body is
               identical to the groundfish one keep the groundfish name
               (make_domain_expr, decode_svy_domains, split_domain_string).
               Programs with no groundfish counterpart are prefixed too.

 History:      These programs were extracted from four catch-per-trip scripts
               that had each repeated the same blocks. "The original" in the
               notes below means those pre-refactor scripts; they are in git
               history, before the retirement commit. The extraction was
               validated by exact-match comparison of every output file, old
               against new. Two things follow, and both matter when editing:
               (1) Behaviour marked PRESERVED looks like a mistake and is kept
               on purpose - an always-true condition, a variable assigned and
               never read, an unreachable guard. Read the note at each site
               before changing it.
               (2) The part2 programs keep every sort, merge, duplicates drop
               and egen group of the original, including ones whose result is
               never used, because each consumes the sort RNG and the tie
               order of later sorts depends on it. Deleting an apparently dead
               sort here changes the sampled output.
 Inputs:       None directly. The part1 programs read the globals the
               callers already depend on: $triplist, $catchlist, and the
               year-window global whose NAME the caller passes
               (calibration_year or projection_catch_per_trip_years).
               sf_load_directed_trips reads $misc_data_cd.
 Outputs:      None directly. sf_build_cpt_strata writes the xlsx path it is
               given; sf_export_mrip_totals writes the .dta path it is given;
               sf_load_directed_trips writes the tempfile it is given.
 Dependencies: User-written commands dsconcat, renvarlab and distinct.
               set varabbrev is not relied on: every variable is written out
               in full.
 Style:        #delimit ; throughout. Only /* */ comments are used, because
               a line starting with * or // is not a comment once the
               delimiter is ; .
*******************************************************************************/

#delimit ;

/******************************************************************************
 make_domain_expr
 Builds the Stata string expression that joins a list of variables with "_",
 e.g. vars(state wv2 mode1 common_dom) returns
   r(expr) = state+"_"+wv2+"_"+mode1+"_"+common_dom
 which is exactly the expression text the original file used, so the values
 produced are identical.
 Identical to the groundfishRDM program of the same name.
 Parameters:
   vars : whitespace-separated list of string variables, in the order joined
******************************************************************************/
capture program drop make_domain_expr ;
program define make_domain_expr, rclass ;
    syntax , VARS(string) ;
    local expr "" ;
    local k = 0 ;
    foreach v of local vars {;
        local k = `k' + 1 ;
        if `k' == 1 {;
            local expr `"`v'"' ;
        };
        else {;
            local expr `"`expr'+"_"+`v'"' ;
        };
    };
    return local expr `"`expr'"' ;
end ;

/******************************************************************************
 sf_label_states
 Creates the string variable state from the numeric MRIP/FES state code st,
 for the nine states the model covers (MA MD RI CT NY NJ DE VA NC), in the
 order the original scripts wrote the assignments. Rows with any other st
 get state == "" ; the caller decides whether to drop them.
 This nine-line block was repeated in every part1 MRIP prep block (now
 sf_prep_mrip_trip_catch) and in the FES demographic pool of calibration
 part2. No groundfishRDM program (groundfish kept its copy inline).
 Parameters: none. Acts on the data in memory; requires variable st.
******************************************************************************/
capture program drop sf_label_states ;
program define sf_label_states ;
    gen state="MA" if st==25 ;
    replace state="MD" if st==24 ;
    replace state="RI" if st==44 ;
    replace state="CT" if st==9 ;
    replace state="NY" if st==36 ;
    replace state="NJ" if st==34 ;
    replace state="DE" if st==10 ;
    replace state="VA" if st==51 ;
    replace state="NC" if st==37 ;
end ;

/******************************************************************************
 sf_prep_mrip_trip_catch
 Reads the MRIP trip and catch extracts named by $triplist and $catchlist,
 merges catch onto trips, keeps the nine mid-Atlantic states and the
 requested year window, classifies mode and species domain (SF = trip caught
 or targeted summer flounder, black sea bass or scup; ZZ = everything else,
 including North Carolina trips outside the northern counties), builds
 per-trip keep/release/catch totals for the three species, collapses to one
 row per trip, svysets, and saves the my_dom_id <-> my_dom_id_string map to
 a caller-owned tempfile (and optionally the trip-level data itself).
 This is the block the original scripts repeated five times: Part A of both
 calibration and projection part1, and each of the three Part B sub-blocks
 of calibration part1.

 Derives from groundfishRDM prep_mrip_trip_catch. Differences: three
 species instead of two; nine states instead of three; the North Carolina
 county filter; no site-list import and no stock-area variable; no shore-mode
 drop; the year window is a parameter (yearglobal) because fluke has a
 calibration window and a projection window; basefile is optional because
 Part B never reads it.

 Parameters:
   domvars     : variables joined with "_" to form my_dom_id_string, in
                 order. This string is the secondary sort key when
                 collapsing to one row per trip, so it must be passed
                 exactly as the original block built it.
                 Part A:  state wv2 mode1 common_dom
                 B.1:     state mode1 common_dom
                 B.2:     state common_dom
                 B.3:     state mode1 wv2 common_dom   (see the note below)
   yearglobal  : NAME of the global holding the year/wave filter expression
                 (calibration_year or projection_catch_per_trip_years).
                 Applied as   keep if ${`yearglobal'}
   domainsfile : path of a tempfile created by the caller. Receives the
                 my_dom_id <-> my_dom_id_string map.
   basefile    : optional. Path of a tempfile created by the caller. If
                 given, receives the trip-level svyset data. Part A passes
                 it; Part B does not, and then issues no save at all, exactly
                 as the original Part B blocks did not.

 Note on the B.3 wave string (output-identical to the original): this
 program always creates wv2 after the North Carolina filter, for every
 domain including B.3. The original B.3 block alone created its wave string
 as w2 immediately after gen st2, and never created wv2. The value is
 string(wave) either way; B.3 uses it only inside my_dom_id_string, whose
 value is therefore unchanged, and B.3 saves no dataset containing the
 variable (its .dta output gets its wave column from splitting the domain
 string). Only the in-memory variable name and position differ.
******************************************************************************/
capture program drop sf_prep_mrip_trip_catch ;
program define sf_prep_mrip_trip_catch ;
    syntax , DOMVARS(string) YEARGLOBAL(string) DOMAINSFILE(string)
             [ BASEFILE(string) ] ;

    /* Pull in MRIP data */
    clear ;
    mata: mata clear ;

    tempfile tl1 cl1 ;
    dsconcat $triplist ;

    sort year strat_id psu_id id_code ;
    drop if strmatch(id_code, "*xx*")==1 ;
    duplicates drop ;
    save `tl1' ;
    clear ;

    dsconcat $catchlist ;
    sort year strat_id psu_id id_code ;
    replace common=subinstr(lower(common)," ","",.) ;
    save `cl1' ;

    /* PRESERVED: this replace acts on the in-memory copy after cl1 was
       saved, so it has no effect on cl1; the same replace is issued again
       after the merge below. Kept as in the original. */
    replace var_id=strat_id if strmatch(var_id,"") ;

    use `tl1' ;
    /* Keep all trips including catch==0 */
    merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate ;
    replace var_id=strat_id if strmatch(var_id,"") ;

    /* Format MRIP data for estimation */

    /* Ensure only relevant states, then the requested year window */
    keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37) ;

    keep if ${`yearglobal'} ;

    gen st2 = string(st,"%02.0f") ;

    /* the nine gen/replace lines that label the states, shared with
       calibration part2's FES pool */
    sf_label_states ;

    gen mode1="sh" if inlist(mode_fx, "1", "2", "3") ;
    replace mode1="pr" if inlist(mode_fx, "7") ;
    replace mode1="fh" if inlist(mode_fx, "4", "5") ;

    /* classify trips into the domain we care about (caught or targeted
       fluke, sea bass or scup) and everything else, marked "ZZ".
       PRESERVED: prim2_common is assigned from prim1_common, as in the
       original. prim2_common is not used downstream. */
    replace prim1_common=subinstr(lower(prim1_common)," ","",.) ;
    replace prim2_common=subinstr(lower(prim1_common)," ","",.) ;

    /* We need to retain 1 observation for each strat_id, psu_id, and id_code
       A.  Trip targeted or caught fluke, sea bass or scup -> domain "SF"
       B.  Trip did not target or catch any of the three  -> domain "ZZ" */
    gen common_dom="ZZ" ;
    replace common_dom="SF" if inlist(common, "summerflounder") ;
    replace common_dom="SF" if inlist(common, "blackseabass") ;
    replace common_dom="SF" if inlist(common, "scup") ;

    replace common_dom="SF"  if inlist(prim1_common, "summerflounder") ;
    replace common_dom="SF"  if inlist(prim1_common, "blackseabass") ;
    replace common_dom="SF"  if inlist(prim1_common, "scup") ;

    /* keep only NC north based on county delineation from Tracey */
    replace common_dom="ZZ"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187) ;

    /* wv2 is created here in every instance - see the header note */
    tostring wave, gen(wv2) ;
    tostring year, gen(yr2) ;

    make_domain_expr, vars(`domvars') ;
    gen my_dom_id_string=`r(expr)' ;

    /* For each trip compute total catch/harvest/discards of the three
       species. The loop generates the variables in the same order the
       original wrote them out: all sf, then all bsb, then all scup. */
    foreach sp in sf bsb scup {;
        if "`sp'" == "sf"   local name "summerflounder" ;
        if "`sp'" == "bsb"  local name "blackseabass" ;
        if "`sp'" == "scup" local name "scup" ;

        gen `sp'_tot_cat=tot_cat if common=="`name'" ;
        egen sum_`sp'_tot_cat=sum(`sp'_tot_cat), by(strat_id psu_id id_code) ;

        gen `sp'_harvest=landing if common=="`name'" ;
        egen sum_`sp'_harvest=sum(`sp'_harvest), by(strat_id psu_id id_code) ;

        gen `sp'_releases=release if common=="`name'" ;
        egen sum_`sp'_releases=sum(`sp'_releases), by(strat_id psu_id id_code) ;
    };

    drop sf_tot_cat sf_harvest sf_releases bsb_tot_cat bsb_harvest bsb_releases  scup_tot_cat scup_harvest scup_releases ;
    foreach sp in sf bsb scup {;
        rename sum_`sp'_tot_cat `sp'_cat ;
        rename sum_`sp'_harvest `sp'_keep ;
        rename sum_`sp'_releases `sp'_rel ;
    };

    /* Set a variable "no_dup"=0 if the record is a catch record of one of
       the three species and no_dup=1 otherwise.
       PRESERVED: the three replaces are OR'd, so every row fails at least
       one of the equalities and no_dup is 1 on every row. Kept as in the
       original; the sort below is then decided by my_dom_id_string. */
    gen no_dup=0 ;
    replace no_dup=1 if  strmatch(common, "summerflounder")==0 ;
    replace no_dup=1 if strmatch(common, "blackseabass")==0 ;
    replace no_dup=1 if strmatch(common, "scup")==0 ;

    /* We sort on year, strat_id, psu_id, id_code, "no_dup", and
       "my_dom_id_string". For records with duplicate year, strat_id, psu_id,
       and id_codes, the first entry will be the species catch record if it
       exists (domain "SF"). If there is no such catch but the trip targeted
       one of the species, the secondary sort on "my_dom_id_string" ensures
       the trip is properly classified. After sorting, we generate a count
       variable (count_obs1 from 1....n) and keep only the "first"
       observation within each "year, strat_id, psu_id, and id_codes" group,
       because total catch per trip of the species of interest was already
       computed above and saved in a single trip-row. */
    bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n ;

    keep if count_obs1==1 ;

    order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common ;

    svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty) ;

    drop if wp_int==0 ;
    encode my_dom_id_string, gen(my_dom_id) ;

    preserve ;
    keep my_dom_id my_dom_id_string ;
    duplicates drop ;
    save "`domainsfile'", replace ;
    restore ;

    if "`basefile'" != "" {;
        save "`basefile'", replace ;
    };
end ;

/******************************************************************************
 sf_post_svy_by_domain
 Runs svy: <stat> for each variable, over(<over>), and collects estimate,
 SE and 95% CI for every domain into a postfile. On exit the collected
 results are the dataset in memory (varname, domain, <stat>, se, <civars>).
 Requires the data to be svyset with the over() variable present.

 Derives from groundfishRDM post_svy_by_domain. Differences: over() is a
 parameter because calibration Part B runs over my_dom_id2; civars() is a
 parameter because Part B names its CI columns ll ul (and those names reach
 the saved .dta as llsf_keep_mrip etc.) while Part A names them ll95 ul95;
 domlabel() lets the SE-imputation round post the domain string literally.

 Parameters:
   stat     : "mean" or "total". Also names the estimate column.
   vars     : variables to estimate, in order.
   over     : domain variable for over(). Default my_dom_id.
   civars   : names of the two CI columns. Default "ll95 ul95".
   domlabel : if given, this literal is posted as the domain instead of the
              r(table) column name. Used only by sf_impute_pse_round, which
              estimates one outcome for one domain at a time.
******************************************************************************/
capture program drop sf_post_svy_by_domain ;
program define sf_post_svy_by_domain ;
    syntax , STAT(string) VARS(string) [ OVER(string) CIVARS(string) DOMLABEL(string) ] ;

    if "`over'" == "" {;
        local over "my_dom_id" ;
    };
    if "`civars'" == "" {;
        local civars "ll95 ul95" ;
    };

    /* Create a postfile to collect results */
    tempfile results ;
    postfile handle str15 varname str15 domain float `stat' se `civars' using `results', replace ;

    /* Loop over variables */
    foreach var of local vars {;

        /* Run svy for the variable by domain */
        svy: `stat' `var', over(`over') ;

        /* Grab result matrix and domain labels */
        matrix M = r(table) ;
        local colnames : colnames M ;

        /* Loop over columns (domains) */
        foreach col of local colnames {;
            local m  = M[1, "`col'"] ;
            local se = M[2, "`col'"] ;
            local lb = M[5, "`col'"] ;
            local ub = M[6, "`col'"] ;

            if "`domlabel'" == "" {;
                post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub') ;
            };
            else {;
                post handle ("`var'") ("`domlabel'") (`m') (`se') (`lb') (`ub') ;
            };
        };
    };

    postclose handle ;

    /* Load results back into memory */
    use `results', clear ;
end ;

/******************************************************************************
 decode_svy_domains
 The r(table) column names posted by sf_post_svy_by_domain look like
 c.<var>@<k>bn.<over variable>. This extracts <k>, the encoded domain id,
 and merges the domain string back on by my_dom_id. Leaves _merge in the
 data, as the original did; the caller's next keep drops it. The parse
 reads only the integer between "@" and "bn", so it does not matter whether
 the over() variable was my_dom_id or my_dom_id2.
 Identical to the groundfishRDM program of the same name.
 Parameters:
   domainsfile : the my_dom_id <-> my_dom_id_string map written by
                 sf_prep_mrip_trip_catch
******************************************************************************/
capture program drop decode_svy_domains ;
program define decode_svy_domains ;
    syntax , DOMAINSFILE(string) ;

    split domain, parse("@") ;
    drop domain1 ;
    split domain2, parse(.) ;
    split domain21, parse(b) ;

    drop domain2 domain21 domain22 domain212 ;
    destring domain211, replace ;
    rename domain211 my_dom_id ;
    merge m:1 my_dom_id using "`domainsfile'" ;
    sort varname  my_dom_id ;
end ;

/******************************************************************************
 split_domain_string
 Splits my_dom_id_string on "_" and renames the pieces in order.
 Identical to the groundfishRDM program of the same name.
 Parameters:
   names : new names for the pieces, in order. The count must equal the
           number of "_"-separated parts, exactly as the original's renames
           assumed.
******************************************************************************/
capture program drop split_domain_string ;
program define split_domain_string ;
    syntax , NAMES(string) ;

    split my_dom_id_string, parse(_) ;
    local k = 0 ;
    foreach n of local names {;
        local k = `k' + 1 ;
        rename my_dom_id_string`k' `n' ;
    };
end ;

/******************************************************************************
 sf_export_mrip_totals
 Part B tidy-and-save: reshapes the decoded svy: total results wide by
 variable, postfixes every estimate column with _mrip, splits the domain
 string into named parts, keeps the SF domain, drops the domain flag,
 orders, and saves. Expects the caller to have already kept
   varname total se ll ul my_dom_id_string
 as the original did just before its reshape.

 Derives from groundfishRDM export_mrip_totals. Differences: fluke's
 column names (total se ll ul) and domain filter (common_dom=="SF"), and
 fluke's order of operations, which runs ds/renvarlab BEFORE splitting the
 domain string (groundfish split first). No filter or order step is
 reordered relative to the original.

 Parameters:
   domvars : names of the domain-string parts, in order, e.g.
             state mode common_dom. Must include common_dom.
   saveas  : full path of the .dta to write
******************************************************************************/
capture program drop sf_export_mrip_totals ;
program define sf_export_mrip_totals ;
    syntax , DOMVARS(string) SAVEAS(string) ;

    reshape wide total se ll ul, i(my_dom_id_string) j(varname) string ;

    ds my_dom_id_string, not ;
    renvarlab `r(varlist)', postfix(_mrip) ;

    split_domain_string, names(`domvars') ;
    keep if common_dom=="SF" ;
    drop common_dom ;

    local cd "common_dom" ;
    local ordervars : list domvars - cd ;
    order my_dom_id_string `ordervars' ;

    save "`saveas'", replace ;
end ;

/******************************************************************************
 sf_impute_pse_round
 One round of the standard-error imputation for strata that had a single
 PSU (and therefore a mean but no SE). For every stratum still missing an
 SE, it pools the stratum's own wave with its shoulder wave(s) from the
 trip-level data, re-estimates the outcome's mean and SE on that pooled
 sample, and records the proportional SE (pse_impute = se/mean). Per-stratum
 results are saved to tempfiles whose names accumulate in a global, and
 dsconcat'd into memory at the end.
 The original scripts had this loop twice (round 1 with one shoulder wave,
 round 2 with two) in each of calibration and projection part1.
 No groundfishRDM counterpart (groundfish kept its single copy inline).

 Parameters:
   missingfile : tempfile of missing-SE strata, one row each, holding
                 strata_id, state, mode, wave, common_dom, varname,
                 my_dom_id_string and the shoulder-wave variable(s)
   basefile    : tempfile from sf_prep_mrip_trip_catch (svyset trip data)
   shoulders   : one or two shoulder-wave variables in missingfile, e.g.
                 shoulder_wave    or    shoulder_wave1 shoulder_wave2.
                 Each is levelsof'd for the stratum and appended, in this
                 order, to the inlist() of waves pooled with the stratum's
                 own wave, so the filter expression is textually the same
                 as the original's.
   accumglobal : NAME of the global that accumulates the per-stratum
                 tempfile names (impute for round 1, impute2 for round 2).
                 A global, as in the original, so it remains defined in the
                 session afterwards exactly as before.
   stratz      : the list of strata_id values to loop over, as produced by
                 the caller's   levelsof strata_id, local(stratz).
                 Optional only so that an empty list (no stratum needs
                 imputation) reaches the loop and the dsconcat, where the
                 original would have reached them too.
 On exit the concatenated results (varname, my_dom_id_string, pse_impute)
 are the dataset in memory.
******************************************************************************/
capture program drop sf_impute_pse_round ;
program define sf_impute_pse_round ;
    syntax , MISSINGFILE(string) BASEFILE(string) SHOULDERS(string)
             ACCUMGLOBAL(string) [ STRATZ(string) ] ;

    global `accumglobal' ;
    foreach s of local stratz {;
        u "`missingfile'", clear ;
        keep if strata_id==`s' ;

        levelsof state, local(st) clean ;
        levelsof common_dom, local(common_dom2) clean ;
        levelsof mode, local(md) clean ;
        levelsof wave, local(wave1) clean ;
        /* build   "`wave1'", "`shoulder'"[, "`shoulder2'"]   for inlist() */
        local wlist `""`wave1'""' ;
        foreach sv of local shoulders {;
            levelsof `sv', local(wsh) clean ;
            local wlist `"`wlist', "`wsh'""' ;
        };
        levelsof varname, local(outcome) clean ;
        levelsof my_dom_id_string, local(my_dom_id_string) clean ;

        u "`basefile'", clear ;
        keep if state=="`st'" & mode1=="`md'" & inlist(wv2, `wlist') & common_dom=="`common_dom2'" ;
        drop my_dom_id_string my_dom_id ;
        gen my_dom_id_string="`my_dom_id_string'" ;
        encode my_dom_id_string, gen(my_dom_id) ;

        /* svy: mean of the one outcome on the pooled sample, posting the
           stratum's own domain string as the domain label */
        sf_post_svy_by_domain, stat(mean) vars(`outcome') domlabel(`my_dom_id_string') ;

        gen pse_impute=se/mean ;
        rename domain my_dom_id_string ;
        keep varname pse_impute my_dom_id_string ;

        tempfile impute`s' ;
        save `impute`s'', replace ;
        global `accumglobal' "${`accumglobal'} "`impute`s''" " ;
    };
    dsconcat ${`accumglobal'} ;
end ;

/******************************************************************************
 sf_build_cpt_strata
 "Part A" of catch-per-trip part1, shared by the calibration and projection
 scripts: estimates mean harvest-, discard- and catch-per-trip with standard
 errors by state x wave x mode x species-domain for the requested year
 window, imputes standard errors for single-PSU strata in two rounds
 (own wave + one shoulder wave, then own wave + two shoulder waves), falls
 back to se = mean, flags per species whether a stratum has keep only /
 release only / both / neither, and flags strata where keep and release
 never co-occur on a trip or are perfectly rank-correlated (so the copula
 step models them as independent). Exports the per-trip data with the
 stratum-level estimates attached to the xlsx the R copula step reads.
 No groundfishRDM counterpart as a single program (groundfish has one
 stage, so its Part A is inline).

 Parameters:
   yearglobal : NAME of the year/wave filter global (see
                sf_prep_mrip_trip_catch)
   saveas     : full path of the xlsx written by export excel
   indzero    : if given, the three *_keep_and_rel_ind flags are mvencode'd
                to 0 before export. PRESERVED asymmetry: the original
                projection script did this and the original calibration
                script did not, so the calibration xlsx carries blank cells
                in those three columns where the projection xlsx carries 0.
                Projection passes indzero; calibration does not.
 Stops with return code 1 (as the original did with exit 1) if any stratum
 with a non-zero mean still has no standard error after both rounds.
******************************************************************************/
capture program drop sf_build_cpt_strata ;
program define sf_build_cpt_strata ;
    syntax , YEARGLOBAL(string) SAVEAS(string) [ INDZERO ] ;

    /* Pull in MRIP data, keep the year window, collapse to trips, svyset.
       basefile keeps the trip-level data for the imputation rounds and
       for the final merge; domains maps my_dom_id to its string. */
    tempfile domains basefile ;
    sf_prep_mrip_trip_catch, domvars(state wv2 mode1 common_dom)
        yearglobal(`yearglobal') domainsfile(`domains') basefile(`basefile') ;

    /* Here I will estimate mean catch/harvest/discards per trip for each
       stratum in order to identify strata with missing SE. For strata with
       missing SE's, I'll follow a similar approach to MRIP's hot and cold
       deck imputation for observations with missing lengths and weights.

       From the MRIP data handbook:
       "For intercepted angler trips with landings where both length and
       weight measurements are missing, paired length and weight
       observations are imputed from complete cases using hot and cold deck
       imputation. (...) Up to five rounds of imputation are conducted in an
       attempt to fill in missing values. These rounds begin with imputation
       cells that correspond to the most detailed MRIP estimation cells, but
       are aggregated to higher levels in subsequent rounds to bring in more
       length-weight data.
         - Round 1: Current year, two-month sampling wave, sub-region, state, mode, area fished, species.
         - Round 2: Current year, half-year, sub-region, state, mode, species.
         - Round 3: Current + most recent prior year, two-month sampling wave, sub-region, state, mode, area fished, species.
         - Round 4: Current + most recent prior year, sub-region, state, mode, species.
         - Round 5: Current + most recent prior year, sub-region, species."

       The estimation strata here are: year window + wave + state + mode,
       for harvest/discards/catch per trip. For strata with missing SE, a
       PSE is imputed from other strata and applied to the missing-SE strata:
         - Round 1: own wave + ONE shoulder wave (two-wave period), state, mode
         - Round 2: own wave + TWO shoulder waves (half-year period), state, mode */

    /* svy: mean of the nine outcomes by domain, into a postfile, then map
       the r(table) column names back to the domain strings */
    sf_post_svy_by_domain, stat(mean)
        vars(sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat) ;
    decode_svy_domains, domainsfile(`domains') ;
    keep varname mean se my_dom_id_string ;

    tempfile base_results ;
    save `base_results', replace ;

    /* the strata that need an imputed SE */
    drop if mean==0 ;
    gen pse=se/mean ;
    keep if se==. ;

    split_domain_string, names(state wave mode common_dom) ;

    /* Round 1 shoulder wave: the other wave of the same two-wave period */
    gen shoulder_wave="2" if wave=="1" ;
    replace shoulder_wave="1" if wave=="2" ;
    replace shoulder_wave="4" if wave=="3" ;
    replace shoulder_wave="3" if wave=="4" ;
    replace shoulder_wave="6" if wave=="5" ;
    replace shoulder_wave="5" if wave=="6" ;

    gen strata_id=_n ;
    levelsof strata_id, local(stratz) ;

    tempfile missing_se ;
    save `missing_se', replace ;

    /* Round 1 */
    sf_impute_pse_round, missingfile(`missing_se') basefile(`basefile')
        shoulders(shoulder_wave) accumglobal(impute) stratz(`stratz') ;

    merge 1:1  varname my_dom_id_string using `missing_se' ;

    preserve ;
    keep if pse_impute!=. ;
    tempfile round1 ;
    save `round1', replace ;
    restore ;

    drop if pse_impute!=. ;

    drop shoulder_wave* ;

    /* Round 2 shoulder waves: the other two waves of the same half-year */
    gen shoulder_wave1="2" if wave=="1" ;
    gen shoulder_wave2="3" if wave=="1" ;

    replace shoulder_wave1="1" if wave=="2" ;
    replace shoulder_wave2="3" if wave=="2" ;

    replace shoulder_wave1="1" if wave=="3" ;
    replace shoulder_wave2="2" if wave=="3" ;

    replace shoulder_wave1="5" if wave=="4" ;
    replace shoulder_wave2="6" if wave=="4" ;

    replace shoulder_wave1="4" if wave=="5" ;
    replace shoulder_wave2="6" if wave=="5" ;

    replace shoulder_wave1="4" if wave=="6" ;
    replace shoulder_wave2="5" if wave=="6" ;

    levelsof strata_id, local(stratz) ;

    drop _merge ;

    tempfile missing_se ;
    save `missing_se', replace ;

    /* Round 2 */
    sf_impute_pse_round, missingfile(`missing_se') basefile(`basefile')
        shoulders(shoulder_wave1 shoulder_wave2) accumglobal(impute2) stratz(`stratz') ;

    merge 1:1  varname my_dom_id_string using `missing_se' ;
    append using `round1' ;
    keep varname my_dom_id_string pse_impute ;
    merge 1:1 varname my_dom_id_string using `base_results' ;

    replace se=mean*pse_impute if se==. & _merge==3 ;

    keep if strmatch(my_dom_id_string, "*SF*")==1 ;

    /* If after the two rounds there is still a missing standard error for
       strata with positive mean catch, set se = mean (high uncertainty) */
    replace se=mean if mean>0 & !missing(mean) & se==. ;

    /* Stop code if non-zero mean harvest/discards/catch-per-trip are
       missing standard errors */
    summarize if mean != 0 & missing(se) ;

    /* If any observations meet the condition, stop */
    if r(N) > 0 {;
        display "Stopping: mean is not zero and se is missing for some observations." ;
        exit 1 ;
    };

    gen missing_se=1 if _merge==3 ;
    drop _merge ;
    sort my_dom_id_string varname ;
    drop pse ;
    reshape wide mean se missing_se, i(my_dom_id_string) j(varname) string ;

    /* make indicator variables for whether each domain contains keep,
       discards, or keep and discards of each species */
    mvencode meanbsb_keep meanbsb_rel meanscup_keep meanscup_rel meansf_keep meansf_rel, mv(0) override ;

    foreach sp in sf bsb scup {;
        gen `sp'_only_keep=1 if mean`sp'_keep>0 & mean`sp'_rel==0 ;
        gen `sp'_only_rel=1 if mean`sp'_rel>0 & mean`sp'_keep==0 ;
        gen `sp'_keep_and_rel=1 if mean`sp'_rel>0 & mean`sp'_keep>0 ;
        gen `sp'_no_catch=1 if mean`sp'_rel==0 & mean`sp'_keep==0 ;
    };

    mvencode sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch, mv(0) override ;

    merge 1:m my_dom_id_string using `basefile' ;
    keep if strmatch(my_dom_id_string, "*SF*")==1 ;

    /* condition for when keep and release are both positive for a stratum,
       but they never occur on the same trip: model these distributions as
       independent */
    foreach sp in sf bsb scup {;
        gen tab=1 if `sp'_keep>0 & `sp'_keep!=. & `sp'_rel>0 & `sp'_rel!=. ;
        egen sumtab=sum(tab), by(my_dom_id_string) ;
        gen `sp'_keep_and_rel_ind=1 if `sp'_keep_and_rel==1 & sumtab==0 ;
        replace `sp'_keep_and_rel=0 if `sp'_keep_and_rel_ind==1 ;
        drop tab sumtab ;
    };

    /* condition for when keep and release are both positive for a stratum,
       but occurred together on only one trip so that the correlation==1:
       model these distributions as independent.
       PRESERVED: a missing rank correlation is treated like a perfect one,
       as in the original. */
    foreach sp in sf bsb scup {;
        gen perfect_corr=. ;
        levelsof my_dom_id_string if `sp'_keep_and_rel==1, local(doms) ;
        foreach d of local doms {;
            di "`d'" ;
            egen rank_keep = rank(`sp'_keep) if my_dom_id_string=="`d'" ;
            egen rank_rel  = rank(`sp'_rel) if my_dom_id_string=="`d'" ;
            count if  my_dom_id_string=="`d'" ;
            if `r(N)'>1 {;
                corr rank_keep rank_rel if my_dom_id_string=="`d'"  [aw=wp_int] ;
                if `r(rho)'==1 | `r(rho)'==. {;
                    replace perfect_corr=1 if my_dom_id_string=="`d'" ;
                };
            };
            drop rank* ;
        };

        replace `sp'_keep_and_rel=0 if `sp'_keep_and_rel==1 & perfect_corr==1 ;
        replace `sp'_keep_and_rel_ind=1 if perfect_corr==1 ;
        drop perfect_corr ;
    };

    keep wp_int my_dom_id_string meanbsb_cat-id_code year yr2 st  state common_dom sf_cat-scup_rel my_dom_id sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch bsb_keep_and_rel_ind sf_keep_and_rel_ind scup_keep_and_rel_ind ;

    mvencode se*, mv(0) override ;
    mvencode missing*, mv(0) override ;
    if "`indzero'" != "" {;
        mvencode sf_keep_and_rel_ind bsb_keep_and_rel_ind scup_keep_and_rel_ind, mv(0) override ;
    };

    export excel "`saveas'", firstrow(variables) replace ;
end ;

/******************************************************************************/
/******************************************************************************/
/* Part2 programs: the catch draw files                                       */
/******************************************************************************/
/******************************************************************************/

/******************************************************************************
 sample_with_replacement
 Turns the dataset in memory (a pool) into exactly n randomly chosen rows,
 sampling with replacement: the pool is duplicated ceil(n / rows) times so it
 has at least n rows, then exactly n rows are drawn at random. This is the
 idiom the original part2 scripts used five times (costs, preference
 parameters, demographics and catch in calibration; catch in projection).
 "sample" is the only RNG-consuming call and it is issued exactly as before,
 so the random stream is consumed identically.
 Identical to the groundfishRDM program of the same name.
 Parameters:
   n : number of rows the dataset must have on exit
******************************************************************************/
capture program drop sample_with_replacement ;
program define sample_with_replacement ;
    syntax , N(integer) ;

    quietly count ;
    local mult = ceil(`n'/r(N)) ;
    expand `mult' ;
    sample `n', count ;
end ;

/******************************************************************************
 sf_load_directed_trips
 Reads one state's directed-trips calendar
 ($misc_data_cd\directed_trips_calibration_<state>.csv, written by
 directed_trips_calibration.do), recomputes month, month1 and the MRIP
 wave from the date string (waves are two-month periods: 1 = Jan-Feb ...
 6 = Nov-Dec), drops days with no directed trips and the regulation and
 dtrip columns, and saves the result to the caller's tempfile. The
 calendar keeps one row per draw x mode x day; the caller keeps one draw
 at a time from it.
 This block opened the per-state loop of both part2 scripts.
 Derives from the inline calendar block of groundfishRDM part2.
 Differences: one CSV per state; no day / day_y2 date juggling; the
 month1 string is recomputed.
 Parameters:
   state  : two-letter state code, selects the CSV
   saveas : path of a tempfile declared by the caller; receives the
            filtered calendar (the original's `base')
******************************************************************************/
capture program drop sf_load_directed_trips ;
program define sf_load_directed_trips ;
    syntax , STATE(string) SAVEAS(string) ;

    import delimited using "$misc_data_cd\directed_trips_calibration_`state'.csv", clear ;

    gen double date_num = date(date, "DMY") ;
    drop month month1 ;
    gen byte   month    = month(date_num) ;
    gen str2   month1   = string(month, "%02.0f") ;
    gen byte   wave     = cond(inlist(month,1,2),1,
                          cond(inlist(month,3,4),2,
                          cond(inlist(month,5,6),3,
                          cond(inlist(month,7,8),4,
                          cond(inlist(month,9,10),5,6))))) ;
    drop date_num ;

    drop if dtrip==0 ;

    drop  dtrip *_bag *_min *_y2 ;

    save "`saveas'", replace ;
end ;

/******************************************************************************
 sf_expand_trips_and_ids
 Acts on one draw's calendar rows in memory (one row per mode x day).
 Expands each mode x day into 50 simulated trips, and each trip into 30
 catch draws (both fixed design constants: 50 stands for variation across
 anglers fishing the same day, 30 for uncertainty in what any one of them
 catches). Then, exactly as the original did:
   - counts the distinct trips per mode (pr fh sh) and per wave (1..6),
   - builds mode_id (1..n within mode) from the unique date x mode x tripid
     rows and merges it on,
   - builds wave_id (1..n within wave) from the unique date x wave x tripid
     rows, sorted date wave tripid, and merges it on.
 mode_id and wave_id are the keys the calibration script later uses to
 attach resampled costs (by mode) and demographics (by wave). The counts
 are returned so the calibration caller can size those resamples:
   r(n_pr) r(n_fh) r(n_sh) r(n_wave1) ... r(n_wave6)
 The projection script computed all of this too and used none of it; it is
 still executed there because egen group, duplicates drop, sort, bysort and
 merge all consume the sort RNG, and removing them would change the tie
 order of every later sort.
 PRESERVED: `egen long dom' is never used afterwards; `by mode: gen
 mode_id' relies on duplicates drop having left the data sorted (no
 explicit sort, unlike the wave_id block). Both as in the original.
 Derives from the inline expansion block of groundfishRDM part2.
 Differences: no month_id block; three modes (pr fh sh) instead of two.
 Parameters: none.
******************************************************************************/
capture program drop sf_expand_trips_and_ids ;
program define sf_expand_trips_and_ids, rclass ;

    /* Expand to 50 trips x 30 catch draws within each (mode,date) */
    egen long dom = group(mode date) ;
    expand 50 ;
    bysort mode date: gen int tripid = _n ;
    expand 30 ;
    bysort mode date tripid: gen byte catch_draw = _n ;

    egen group=group(date tripid mode) ;

    foreach md in pr fh sh {;
        qui distinct group if mode=="`md'" ;
        local n_`md' = `r(ndistinct)' ;
    };

    preserve ;
    keep date mode tripid ;
    duplicates drop ;
    by mode: gen mode_id=_n ;
    tempfile mode_id ;
    save `mode_id', replace ;
    restore ;

    merge m:1 date mode tripid using `mode_id', keep(3) nogen ;

    forvalues w=1/6 {;
        qui distinct group if wave==`w' ;
        local n_wave`w' = `r(ndistinct)' ;
    };

    preserve ;
    keep date wave tripid ;
    duplicates drop ;
    sort date wave tripid ;
    bysort wave: gen wave_id=_n ;
    tempfile wave_id ;
    save `wave_id', replace ;
    restore ;

    merge m:1 date wave tripid using `wave_id', keep(3) nogen ;

    foreach k in pr fh sh wave1 wave2 wave3 wave4 wave5 wave6 {;
        return scalar n_`k' = `n_`k'' ;
    };
end ;

/******************************************************************************
 sf_sample_catch_by_mode_wave
 Attaches a catch outcome to every simulated trip row in memory (the
 expanded data from sf_expand_trips_and_ids, plus whatever the caller has
 merged on since). Loads the copula output for this state x draw (one row
 per simulated outcome, with my_dom_id_string = state_wave_mode_domain),
 splits the domain string into wave and mode, and, for each mode x wave
 group of trip rows, samples one pool row per trip row with replacement
 (sample_with_replacement), stacks the sampled chunks, and merges them 1:1
 onto the trip rows by (g, gid). On exit the data are exactly where the
 original's `compress' after the merge left them; the caller does the
 file-specific tail (sort, species totals, keep, order, save).
 PRESERVED, as in the original: n_g is computed and never used; the
 "Not enough catch rows" guard after sample_with_replacement cannot fire
 (the sample leaves exactly n_needed rows); wave is destring'd four times,
 of which only the first converts anything; the three di lines per group
 are kept so the console log reads as before.
 Derives from the inline catch-sampling loop of groundfishRDM part2.
 Differences: the stratum is mode x wave (wave arrives as a string from
 split, hence the destrings) instead of mode x month; the pool is per
 state; the optional _sim renaming; three species.
 Parameters:
   rawfile    : full path of the copula output for this state x draw
                (calib_catch_draws_raw_<ST>_<i>.dta or
                proj_catch_draws_raw_<ST>_<i>.dta)
   draw       : the draw number, used only in the unreachable guard's message
   state      : optional, used only in that message. The original
                calibration script printed its `st' local there; the
                original projection script printed an undefined macro
                (blank). Passing the state code from the projection caller
                changes only the text of a message that cannot print.
   simpostfix : if given, every pool column other than my_dom_id_string
                state wave mode is renamed with the postfix _sim
                (ds ... , not / renvarlab), as the calibration script did.
                The projection script did not rename.
******************************************************************************/
capture program drop sf_sample_catch_by_mode_wave ;
program define sf_sample_catch_by_mode_wave ;
    syntax , RAWFILE(string) DRAW(integer) [ STATE(string) SIMPOSTFIX ] ;

    /* the pool of copula outcomes for this state x draw, keyed by mode x wave */
    preserve ;
        use "`rawfile'", clear ;
        split my_dom_id_string, parse(_) ;
        rename my_dom_id_string2 wave ;
        rename my_dom_id_string3 mode ;
        drop my_dom_id_string4 ;
        keep my_dom_id_string state wave mode  sf_* bsb_* scup_* ;
        if "`simpostfix'" != "" {;
            ds my_dom_id_string state wave mode, not ;
            renvarlab `r(varlist)', postfix(_sim) ;
        };
        tempfile excelpool ;
        save `excelpool', replace ;
    restore ;

    /* sample catch outcomes by (mode,wave) */
    egen long g = group(mode wave) ;
    bysort g: gen long gid = _n ;
    bysort g: gen long n_g = _N ;
    levelsof g, local(gs) ;

    tempfile trips_expanded ;
    save `trips_expanded', replace ;

    /* Build catch outcomes dataset with keys (g, gid) */
    clear ;
    tempfile catchall ;
    save `catchall', emptyok replace ;
    local seeded 0 ;

    foreach gg of local gs {;
        use `trips_expanded', clear ;
        keep if g==`gg' ;
        keep mode wave ;
        local md  = mode[1] ;
        local wv  = wave[1] ;
        local n_needed = _N ;
        di "`md'" ;
        di "`wv'" ;
        di `n_needed' ;
        use `excelpool', clear ;
        keep if wave=="`wv'" & mode=="`md'" ;

        /* The pool for this mode x wave usually holds far fewer rows than
           the number of simulated trips needing an outcome, so it is
           replicated and sampled down to exactly n_needed rows: sampling
           WITH replacement, each pool outcome reusable across trips. */
        sample_with_replacement, n(`n_needed') ;

        /* PRESERVED: cannot fire, see header */
        quietly count ;
        if (r(N) < `n_needed') {;
            di as error "Not enough catch rows for st=`state' draw=`draw' mode=`md' wave=`wv' need=`n_needed' have=" r(N) ;
            continue ;
        };

        gen long g   = `gg' ;
        gen long gid = _n ;
        destring wave, replace ;

        tempfile chunk ;
        save `chunk', replace ;

        if (`seeded'==0) {;
            use `chunk', clear ;
            destring wave, replace ;
            save `catchall', replace ;
            local seeded 1 ;
        };
        else {;
            use `catchall', clear ;
            append using `chunk' ;
            destring wave, replace ;
            save `catchall', replace ;
        };
    };

    /* Merge sampled catch onto trips by (g,gid) */
    use `trips_expanded', clear ;
    destring wave, replace ;
    merge 1:1 g gid using `catchall', keep(3) nogen ;

    drop g gid n_g ;
    compress ;
end ;

#delimit cr
