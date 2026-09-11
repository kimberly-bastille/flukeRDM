/*******************************************************************************
 Script:       refval_tools.do
 Purpose:      Comparison tooling for the refactor validation harness in
               model_wrapper.do (catch-per-trip refactor: Pair A =
               catch_per_trip_calibration_part1 / catch_per_trip_projection
               _part1; Pair B = calibration_catch_per_trip_part2 /
               catch_per_trip_projection_part2). Defines the programs the
               harness blocks call:
                 refval_stamp        timestamp for report file names
                 refval_capture      fingerprint (and optionally copy aside) the
                                     outputs of one run (side = new or old)
                 refval_compare      compare the new and old fingerprints and
                                     copies, write PASS/FAIL report
               plus three small helpers (refval_basename, refval_load,
               refval_fingerprint) and refval_draw_files, which builds the
               per-state x per-draw file lists the part2 blocks pass in.
               Nothing here writes to a production path. Production files are
               only READ, and COPIED into $refval_cd.
 Origin:       Adapted from groundfishRDM/Code/refactor_validation/
               refval_tools.do (branch refactor_calib). The program bodies are
               unchanged; only this header and the report labels in
               refval_compare differ (fluke runs the refactored script FIRST
               and the original LAST, see below).
 Inputs:       $refval_cd, set by the harness config block in model_wrapper.do.
 Outputs:      All under $refval_cd:
                 <stem>_new.<ext>, <stem>_old.<ext>      copies of pipeline outputs
                 refval_fp_<part>_<side>_<ts>.dta        one row per output file
                 refval_results_<part>_<ts>.dta          per-file verdicts
                 refval_report_<part>_<ts>.log           the human-readable report
 Dependencies: Base Stata 14 or later (strrpos, strL variables). Commands used:
               cf, datasignature, checksum, copy, import excel,
               import delimited. (postfile is deliberately NOT used: it cannot
               write strL, and the schema column needs strL.)
 Comparison:   Exact match only. No tolerance.
               .dta / .xlsx: PASS needs identical N, k, schema, fingerprint
               and a clean cf on the copies. The xlsx is compared as
               imported (import excel, firstrow), which is what the R copula
               step reads; raw xlsx bytes carry metadata and are not compared.
               .csv: PASS is decided by a raw-byte match (file length and
               checksum); the imported-data checks and cf still run and are
               reported, as diagnostics for a FAIL. (No Pair A or Pair B
               output is a csv; kept for completeness.)
 Run order:    "new" is the _refactored script and runs FIRST; "old" is the
               original production script and runs LAST, so that after the
               harness the production paths hold exactly what a harness-free
               run would have produced and the rest of the pipeline (the R
               copula step in particular) consumes the original's output.
 Retirement:   After the user confirms an exact match on a full production
               run, the harness blocks in model_wrapper.do, this folder and
               the original scripts are removed together.
*******************************************************************************/

#delimit ;

/******************************************************************************
 refval_stamp
 Returns r(ts), a file-name-safe timestamp such as 20260909_143012, built
 from the session clock. One stamp is taken per harness block so every file
 that block writes shares it.
 Parameters: none
******************************************************************************/
capture program drop refval_stamp ;
program define refval_stamp, rclass ;
    local d = string(date(c(current_date), "DMY"), "%tdCCYYNNDD") ;
    local t = subinstr(c(current_time), ":", "", .) ;
    return local ts "`d'_`t'" ;
end ;

/******************************************************************************
 refval_basename
 Splits a full path into r(name) (file name with extension), r(stem)
 (without extension) and r(ext) (extension without the dot). Accepts either
 slash direction, since the wrapper's globals mix them.
 Parameters:
   file : full path
******************************************************************************/
capture program drop refval_basename ;
program define refval_basename, rclass ;
    syntax , FILE(string) ;
    local f = subinstr(`"`file'"', char(92), "/", .) ;
    local p = strrpos(`"`f'"', "/") ;
    local name = substr(`"`f'"', `p' + 1, strlen(`"`f'"')) ;
    local d = strrpos("`name'", ".") ;
    if `d' > 0 {;
        local stem = substr("`name'", 1, `d' - 1) ;
        local ext  = substr("`name'", `d' + 1, strlen("`name'")) ;
    };
    else {;
        local stem "`name'" ;
        local ext "" ;
    };
    return local name "`name'" ;
    return local stem "`stem'" ;
    return local ext  "`ext'" ;
end ;

/******************************************************************************
 refval_load
 Loads one pipeline output into memory the way the pipeline itself consumes
 it. A .dta is loaded with -use-. An .xlsx is loaded with
 -import excel, first-, which is exactly how part1 re-imports its own xlsx
 before saving the .dta, and is the content the R copula step reads. The
 raw xlsx bytes are deliberately not compared: xlsx files carry metadata
 that is not data. A .csv is loaded with -import delimited-, with no
 options, which is how catch_at_length_projection.do reads the calibration
 CSVs; for .csv files the raw bytes ARE compared, by refval_compare.
 Parameters:
   file : full path of a .dta, .xlsx or .csv file
******************************************************************************/
capture program drop refval_load ;
program define refval_load ;
    syntax , FILE(string) ;
    if lower(substr(`"`file'"', -5, 5)) == ".xlsx" {;
        import excel using `"`file'"', clear first ;
    };
    else if lower(substr(`"`file'"', -4, 4)) == ".csv" {;
        import delimited using `"`file'"', clear ;
    };
    else {;
        use `"`file'"', clear ;
    };
end ;

/******************************************************************************
 refval_fingerprint
 Loads one file and returns
   r(N)      number of observations
   r(k)      number of variables
   r(schema) the sort key, then every variable in dataset order with its
             storage type, display format, value-label name and variable
             label. Two files with different variable order, types or
             labels get different schemas.
   r(sig)    r(datasignature) computed AFTER adding a row-index variable
             (_refval_rownum = _n). datasignature on its own ignores row
             order; tying each row to its position makes the checksum change
             if the rows are permuted. This is the exact-content check for
             files that are not copied aside for cf.
 The in-memory data are clobbered.
 Parameters:
   file : full path of a .dta or .xlsx file
******************************************************************************/
capture program drop refval_fingerprint ;
program define refval_fingerprint, rclass ;
    syntax , FILE(string) ;
    refval_load , file(`"`file'"') ;
    local N = _N ;
    local k = c(k) ;
    local sortedby : sortedby ;
    local schema `"sortedby[`sortedby']"' ;
    unab vars : _all ;
    foreach v of local vars {;
        local t   : type `v' ;
        local fmt : format `v' ;
        local vl  : value label `v' ;
        local lab : variable label `v' ;
        local schema `"`schema' `v'[`t' `fmt' vl=`vl' `"`lab'"']"' ;
    };
    quietly gen double _refval_rownum = _n ;
    quietly datasignature ;
    local sig "`r(datasignature)'" ;
    return scalar N = `N' ;
    return scalar k = `k' ;
    return local  sig "`sig'" ;
    return local  schema `"`schema'"' ;
end ;

/******************************************************************************
 refval_draw_files
 Builds the double-quoted, whitespace-separated list of per-state x per-draw
 file paths in the form refval_capture's files() and copyfiles() expect:
   "<dir>/<prefix>_<state>_<draw>.dta"
 for every state in states() and every draw 1..ndraws(), states outer,
 draws inner (the order part2 writes them). Returns r(files).
 The part2 harness blocks use it three times per stage: for the raw copula
 draws (existence check before running anything), for the part2 outputs
 (the fingerprint list) and for the copy-aside subset (ndraws() =
 refval_copy_draws). A forward slash joins dir and prefix on purpose: a
 backslash before a backtick would stop macro expansion.
 Parameters:
   dir    : directory, e.g. $calib_catch_data_cd
   prefix : file stem up to the state, e.g. calib_catch_draws or
            calib_catch_draws_raw
   states : whitespace-separated state codes, in the order part2 loops them
   ndraws : draws 1..ndraws are listed; 0 gives an empty list
******************************************************************************/
capture program drop refval_draw_files ;
program define refval_draw_files, rclass ;
    syntax , DIR(string) PREFIX(string) STATES(string) NDRAWS(integer) ;
    local files "" ;
    foreach s of local states {;
        forvalues i = 1/`ndraws' {;
            local files `"`files' "`dir'/`prefix'_`s'_`i'.dta""' ;
        };
    };
    return local files `"`files'"' ;
end ;

/******************************************************************************
 refval_capture
 Records a fingerprint row for every file in files() into
 $refval_cd/refval_fp_<part>_<side>_<ts>.dta, and copies each file that is
 also listed in copyfiles() to $refval_cd/<stem>_<side>.<ext>. Call it
 immediately after a run, before the next run overwrites the production
 paths. A listed file that does not exist gets a row with sig = "MISSING",
 which refval_compare reports as FAIL.
 Parameters:
   part      : label used in output file names, e.g. part1 or part2
   side      : new (the production script's run) or old (the retired _old.do run)
   ts        : timestamp from refval_stamp, shared by both sides
   files     : whitespace-separated list of DOUBLE-QUOTED full paths to
               fingerprint
   copyfiles : optional, same form. Subset of files to copy aside for cf.
               Anything not listed here is checked by fingerprint only.
******************************************************************************/
capture program drop refval_capture ;
program define refval_capture ;
    syntax , PART(string) SIDE(string) TS(string) FILES(string asis)
             [COPYFILES(string asis)] ;

    local fpfile "$refval_cd/refval_fp_`part'_`side'_`ts'.dta" ;

    /* One row per file is accumulated in memory and saved at the end.
       schema must be strL (a wide file's schema can exceed the 2045-character
       str# limit) and postfile cannot write strL, so rows are added with
       set obs / replace. The fingerprinting of each file loads that file into
       memory; preserve/restore keeps the accumulator across that. */
    clear ;
    quietly {;
        set obs 0 ;
        gen str100 name = "" ;
        gen str8 side = "" ;
        gen double N = . ;
        gen double k = . ;
        gen str100 sig = "" ;
        gen strL schema = "" ;
        gen double filelen = . ;
        gen double checksum = . ;
        gen byte copied = . ;
    };

    foreach f of local files {;
        refval_basename , file(`"`f'"') ;
        local name "`r(name)'" ;
        local stem "`r(stem)'" ;
        local ext  "`r(ext)'" ;

        capture confirm file `"`f'"' ;
        if _rc {;
            di as error "refval_capture: `side' output not found: `f'" ;
            local N = . ;
            local k = . ;
            local sig "MISSING" ;
            local schema "" ;
            local flen = . ;
            local csum = . ;
            local copied = 0 ;
        };
        else {;
            quietly checksum `"`f'"' ;
            local flen = r(filelen) ;
            local csum = r(checksum) ;

            preserve ;
            refval_fingerprint , file(`"`f'"') ;
            local N = r(N) ;
            local k = r(k) ;
            local sig "`r(sig)'" ;
            local schema `"`r(schema)'"' ;
            restore ;

            /* copy aside only if this file is in copyfiles() */
            local copied = 0 ;
            foreach c of local copyfiles {;
                if `"`c'"' == `"`f'"' {;
                    local copied = 1 ;
                };
            };
            if `copied' {;
                copy `"`f'"' "$refval_cd/`stem'_`side'.`ext'", replace ;
            };
        };

        quietly {;
            set obs `=_N + 1' ;
            replace name     = "`name'"       in `=_N' ;
            replace side     = "`side'"       in `=_N' ;
            replace N        = `N'            in `=_N' ;
            replace k        = `k'            in `=_N' ;
            replace sig      = "`sig'"        in `=_N' ;
            replace schema   = `"`schema'"'   in `=_N' ;
            replace filelen  = `flen'         in `=_N' ;
            replace checksum = `csum'         in `=_N' ;
            replace copied   = `copied'       in `=_N' ;
        };
        di as text "refval_capture `side': `name'  N=`N' k=`k' sig=`sig' copied=`copied'" ;
    };
    quietly save "`fpfile'", replace ;
    clear ;
end ;

/******************************************************************************
 refval_compare
 Reads the new and old fingerprint files for one part and timestamp, merges
 them by file name, and marks each file PASS or FAIL:
   .dta / .xlsx : PASS only if ALL of these hold: present on both sides,
                  same N, same k, same schema, same fingerprint, and, where
                  both sides were copied aside, -cf _all- finds zero
                  differences (old copy in memory, new copy as using; the
                  schema check already guarantees the two variable lists
                  are identical, so one direction is complete).
   .csv         : PASS only if the raw bytes match (same file length and
                  same checksum). The imported-data checks and cf are still
                  run and reported, so a FAIL says which variable differs.
 Writes $refval_cd/refval_results_<part>_<ts>.dta (one row per file) and
 $refval_cd/refval_report_<part>_<ts>.log (this program's console output,
 including cf's per-variable mismatch counts). Returns r(pass) = 1 if every
 file passed, r(nfail), and r(report).
 Parameters:
   part     : as passed to refval_capture
   ts       : as passed to refval_capture
   verbose  : 0 (default) cf prints per-variable mismatch counts;
              1 cf also lists every differing observation (can be huge)
   newlabel : optional, name of the production (refactored) script, for the
              report header. Defaults to calibration_catch_per_trip_<part>.do
   oldlabel : optional, name of the retired original script, for the report
              header. Defaults to calibration_catch_per_trip_<part>_old.do
******************************************************************************/
capture program drop refval_compare ;
program define refval_compare, rclass ;
    syntax , PART(string) TS(string) [VERBOSE(integer 0) NEWLABEL(string) OLDLABEL(string)] ;

    if "`newlabel'" == "" {;
        local newlabel "`part'_refactored.do" ;
    };
    if "`oldlabel'" == "" {;
        local oldlabel "`part'.do" ;
    };

    local report "$refval_cd/refval_report_`part'_`ts'.log" ;
    capture log close refval ;
    log using "`report'", name(refval) text replace ;

    di _n as text "===== REFACTOR VALIDATION REPORT: `part' =====" ;
    di as text "timestamp     : `ts'" ;
    di as text "ndraws        : $ndraws" ;
    di as text "Stata         : `c(stata_version)' `c(flavor)' `c(machine_type)'  rng=`c(rng_current)'" ;
    di as text "refval_cd     : $refval_cd" ;
    di as text "new = `newlabel' (refactored script, ran FIRST)" ;
    di as text "old = `oldlabel' (original production script, ran LAST, so production paths hold its output)" ;

    /* new-side fingerprints, suffixed _new */
    use "$refval_cd/refval_fp_`part'_new_`ts'.dta", clear ;
    drop side ;
    foreach v in N k sig schema filelen checksum copied {;
        rename `v' `v'_new ;
    };
    tempfile newfp ;
    quietly save `newfp' ;

    /* old-side fingerprints, suffixed _old, merged 1:1 on file name */
    use "$refval_cd/refval_fp_`part'_old_`ts'.dta", clear ;
    drop side ;
    foreach v in N k sig schema filelen checksum copied {;
        rename `v' `v'_old ;
    };
    quietly merge 1:1 name using `newfp' ;
    quietly gen byte present_ok = (_merge == 3) ;
    drop _merge ;

    quietly gen byte N_ok      = present_ok & (N_old == N_new) ;
    quietly gen byte k_ok      = present_ok & (k_old == k_new) ;
    quietly gen byte schema_ok = present_ok & (schema_old == schema_new) ;
    quietly gen byte sig_ok    = present_ok & (sig_old == sig_new)
                                 & sig_old != "MISSING" & sig_new != "MISSING" ;
    quietly gen byte cf_run    = 0 ;
    quietly gen double cf_rc   = . ;
    quietly gen double cf_ndiff = . ;

    /* cf on the copies, old in memory vs new using */
    if `verbose' {;
        local cfopt "verbose" ;
    };
    else {;
        local cfopt "" ;
    };
    local nfiles = _N ;
    forvalues j = 1/`nfiles' {;
        local both = copied_old[`j'] == 1 & copied_new[`j'] == 1 ;
        if `both' {;
            local name = name[`j'] ;
            refval_basename , file("`name'") ;
            local stem "`r(stem)'" ;
            local ext  "`r(ext)'" ;
            preserve ;
            refval_load , file("$refval_cd/`stem'_new.`ext'") ;
            tempfile newdat ;
            quietly save `newdat' ;
            refval_load , file("$refval_cd/`stem'_old.`ext'") ;
            di _n as text "--- cf `name': old (`stem'_old.`ext') vs new (`stem'_new.`ext') ---" ;
            capture noisily cf _all using `newdat', `cfopt' ;
            local rc = _rc ;
            local nd = . ;
            capture local nd = r(Nsum) ;
            if "`nd'" == "" {;
                local nd = . ;
            };
            if `rc' == 0 {;
                di as text "cf: no differences" ;
            };
            else {;
                di as error "cf: differences found (rc=`rc', Nsum=`nd')" ;
            };
            restore ;
            quietly replace cf_run   = 1     in `j' ;
            quietly replace cf_rc    = `rc'  in `j' ;
            quietly replace cf_ndiff = `nd'  in `j' ;
        };
    };

    /* raw-byte match: length and checksum as recorded by refval_capture */
    quietly gen byte bytes_ok = present_ok & (filelen_old == filelen_new)
                                & (checksum_old == checksum_new) ;
    quietly gen byte is_csv = lower(substr(name, -4, 4)) == ".csv" ;

    quietly gen str4 status = "PASS" ;
    quietly replace status = "FAIL" if !present_ok | !N_ok | !k_ok | !schema_ok | !sig_ok ;
    quietly replace status = "FAIL" if cf_run == 1 & cf_rc != 0 ;
    /* .csv: the raw-byte match decides on its own, either way */
    quietly replace status = cond(bytes_ok, "PASS", "FAIL") if is_csv ;

    order name status is_csv bytes_ok present_ok N_old N_new N_ok k_old k_new k_ok
          schema_ok sig_ok cf_run cf_rc cf_ndiff sig_old sig_new filelen_old
          filelen_new checksum_old checksum_new copied_old copied_new
          schema_old schema_new ;
    sort name ;

    di _n as text "----- per-file results -----" ;
    di as text "(status for .csv files is decided by bytes_ok alone, the other columns are diagnostics)" ;
    list name status is_csv bytes_ok N_old N_new k_old k_new schema_ok sig_ok cf_run cf_rc cf_ndiff,
         noobs sep(0) string(40) abbreviate(10) ;

    /* spell out any schema mismatch so the report says which variable changed */
    quietly count if present_ok & !schema_ok ;
    if r(N) > 0 {;
        di _n as text "----- schema mismatches -----" ;
        forvalues j = 1/`nfiles' {;
            if present_ok[`j'] & !schema_ok[`j'] {;
                di as text "file: " name[`j'] ;
                di as text "  old: " schema_old[`j'] ;
                di as text "  new: " schema_new[`j'] ;
            };
        };
    };
    quietly count if !present_ok ;
    if r(N) > 0 {;
        di _n as text "----- files missing on one side -----" ;
        list name sig_old sig_new if !present_ok, noobs sep(0) ;
    };

    quietly count if status == "FAIL" ;
    local nfail = r(N) ;
    local pass = (`nfail' == 0) ;
    quietly count if cf_run == 1 ;
    local ncf = r(N) ;

    di _n as text "files compared : `nfiles'   with cf: `ncf'   failed: `nfail'" ;
    if `pass' {;
        di as result "===== `part' OVERALL: PASS (exact match on every file) =====" ;
    };
    else {;
        di as error  "===== `part' OVERALL: FAIL (`nfail' file(s) differ, see rows above) =====" ;
    };
    di as text "results dataset: $refval_cd/refval_results_`part'_`ts'.dta" ;
    di as text "this report    : `report'" ;

    quietly save "$refval_cd/refval_results_`part'_`ts'.dta", replace ;
    log close refval ;

    return scalar pass  = `pass' ;
    return scalar nfail = `nfail' ;
    return local  report "`report'" ;
end ;

#delimit cr
