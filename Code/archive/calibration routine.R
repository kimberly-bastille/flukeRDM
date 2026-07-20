################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      calibration routine.R
# Purpose:     The search that drives PASS 1: reads the model-versus-MRIP harvest
#              differences from PASS 0 and re-runs the simulation, adjusting
#              per-trip outcomes until simulated harvest is within 5 percent or
#              500 fish of MRIP. Same convergence criterion the current code uses.
# Superseded by: Code/sim/calibration_routine_final.R
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
################################################################################
################################################################################


#This file pulls in the data from step 1, i.e., the differences between model simulated harvest 
#and MRIP estimates of harvest, and re-runs the calibration model but this time adjusts per-trip
#outcomes until simulated harvest in numbers of fish is within 5% or 500 fish of the MRIP estimate. 

#Set number of original draws. We use 125 for the final run. Choose a lot fewer for test runs
n_simulations<-125

n_draws<-50 #Number of simulated trips per day


MRIP_comparison = read_dta(file.path(iterative_input_data_cd,"simulated_catch_totals.dta")) %>% 
  dplyr::rename(estimated_trips=tot_dtrip_sim, 
                sf_catch=tot_sf_cat_sim, 
                bsb_catch=tot_bsb_cat_sim, 
                scup_catch=tot_scup_cat_sim, 
                sf_keep=tot_sf_keep_sim, 
                bsb_keep=tot_bsb_keep_sim, 
                scup_keep=tot_scup_keep_sim,
                sf_rel=tot_sf_rel_sim, 
                bsb_rel=tot_bsb_rel_sim, 
                scup_rel=tot_scup_rel_sim) 

baseline_output0<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) 



states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
mode_draw <- c("sh", "pr", "fh")
draws <- 1:n_simulations

# i<-1
# s<-"MA"
# md<-"pr"
#  
# Create an empty list to collect results
calibrated <- list()
 
# Counter for appending to list
k <- 1

# Loop over all combinations
for (s in states){
  for (md in mode_draw){
    for (i in draws) {
      
      calib_comparison<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) %>% 
        dplyr::filter(state==s & draw==i & mode==md)

      for (p in 1:nrow(calib_comparison)) {
        sp <- calib_comparison$species[p]
        
        assign(paste0("rel_to_keep_", sp), calib_comparison$rel_to_keep[p])
        assign(paste0("keep_to_rel_", sp), calib_comparison$keep_to_rel[p])
        assign(paste0("harv_diff_", sp), calib_comparison$diff[p])
        assign(paste0("harv_pct_diff_", sp), calib_comparison$pct_diff[p])

        if (calib_comparison$rel_to_keep[p] == 1) {
          assign(paste0("p_rel_to_keep_", sp), calib_comparison$p_rel_to_keep[p])
          assign(paste0("p_keep_to_rel_", sp), 0)
          
        }
        
        if (calib_comparison$keep_to_rel[p] == 1) {
          assign(paste0("p_keep_to_rel_", sp), calib_comparison$p_keep_to_rel[p])
          assign(paste0("p_rel_to_keep_", sp), 0)
          
        }
      }
      
      all_keep_to_rel_sf<-case_when(p_keep_to_rel_sf==1~1, TRUE~0)
      all_keep_to_rel_bsb<-case_when(p_keep_to_rel_bsb==1~1, TRUE~0)
      all_keep_to_rel_scup<-case_when(p_keep_to_rel_scup==1~1, TRUE~0)
      
      source(file.path(code_cd, "calibrate_rec_catch1.R"))
      
        for (p in 1:nrow(calib_comparison1)) {
          sp <- calib_comparison1$species[p]
          
          assign(paste0("MRIP_keep_", sp), calib_comparison1$MRIP_keep[p])
          assign(paste0("model_keep_", sp), calib_comparison1$model_keep[p])
          assign(paste0("harv_diff_", sp), calib_comparison1$diff_keep[p])
          assign(paste0("harv_pct_diff_", sp), calib_comparison1$pct_diff_keep[p])
          # assign(paste0("rel_to_keep_", sp), calib_comparison1$rel_to_keep_new[p])
          # assign(paste0("keep_to_rel_", sp), calib_comparison1$keep_to_rel_new[p])
        }
        
        message("run ", i, " state ", s, " mode ", md)
        message("model_sf_harv: ", model_keep_sf)
        message("mrip_sf_harv: ", MRIP_keep_sf)
        message("diff_sf_harv: ", harv_diff_sf)
        message("pct_diff_sf_harv: ", harv_pct_diff_sf)
        message("rel_to_keep_sf: ", rel_to_keep_sf)
        message("p_rel_to_keep_sf: ", p_rel_to_keep_sf)
        message("p_keep_to_rel_sf: ", p_keep_to_rel_sf)
        
        message("model_bsb_harv: ", model_keep_bsb)
        message("mrip_bsb_harv: ", MRIP_keep_bsb)
        message("diff_bsb_harv: ", harv_diff_bsb)
        message("pct_diff_bsb_harv: ", harv_pct_diff_bsb)
        message("rel_to_keep_bsb: ", rel_to_keep_bsb)
        message("p_rel_to_keep_bsb: ", p_rel_to_keep_bsb)
        message("p_keep_to_rel_bsb: ", p_keep_to_rel_bsb)
        
        message("model_scup_harv: ", model_keep_scup)
        message("mrip_scup_harv: ", MRIP_keep_scup)
        message("diff_scup_harv: ", harv_diff_scup)
        message("pct_diff_scup_harv: ", harv_pct_diff_scup)
        message("rel_to_keep_scup: ", rel_to_keep_scup)
        message("p_rel_to_keep_scup: ", p_rel_to_keep_scup)
        message("p_keep_to_rel_scup: ", p_keep_to_rel_scup)
        
        sf_achieved<-case_when((abs(harv_diff_sf)<500 | (abs(harv_pct_diff_sf)<5 & !is.na(harv_pct_diff_sf))) ~1, TRUE~0)
        bsb_achieved<-case_when((abs(harv_diff_bsb)<500 | (abs(harv_pct_diff_bsb)<5 & !is.na(harv_pct_diff_bsb))) ~1, TRUE~0)
        scup_achieved<-case_when((abs(harv_diff_scup)<500 | (abs(harv_pct_diff_scup)<5 & !is.na(harv_pct_diff_scup))) ~1, TRUE~0)
      
        # Here I add a non-convergence indicator and artificially deem the run as achieved
        sf_convergence<-1
        bsb_convergence<-1
        scup_convergence<-1
        
        
        while(sf_achieved+bsb_achieved+scup_achieved<3){
       
          #For draws where release_to_keep==1:
          #If baseline sf harvest is less than MRIP, but in a new run sf harvest is greater than MRIP, 
          #reduce the baseline p_rel_to_keep value 
          
          if(sf_achieved!=1){
            if(rel_to_keep_sf==1){
              if(harv_diff_sf>0){
                p_rel_to_keep_sf<-p_rel_to_keep_sf - p_rel_to_keep_sf*.15
              }
              #If baseline sf harvest is less than MRIP, and in the new run sf harvest is still less than MRIP, 
              #increase the baseline p_rel_to_keep value 
              if(harv_diff_sf<0) {
                p_rel_to_keep_sf<-p_rel_to_keep_sf + p_rel_to_keep_sf*.16
              }
            }
            #For draws where keep_to_release==1
            #If in the baseline run, harvest is less than MRIP, but in a new run harvest is greater than MRIP, 
            #reduce the baseline p_keep_to_rel value 
            if(keep_to_rel_sf==1 & all_keep_to_rel_sf!=1) {
              if(harv_diff_sf>0){
                p_keep_to_rel_sf<-p_keep_to_rel_sf + p_keep_to_rel_sf*.16
              }
              #If in the baseline run, harvest is less than MRIP, and in the new run harvest is still less than MRIP, 
              #increase the baseline p_keep_to_rel value 
              if(harv_diff_sf<0){
                p_keep_to_rel_sf<-p_keep_to_rel_sf - p_keep_to_rel_sf*.15
              }
            }
            
          }
          
          
          #BSB
          #For draws where release_to_keep==1:
          #If baseline sf harvest is less than MRIP, but in a new run sf harvest is greater than MRIP, 
          #reduce the baseline p_rel_to_keep value 
          if(bsb_achieved!=1){
            if(rel_to_keep_bsb==1){
              if(harv_diff_bsb>0){
                p_rel_to_keep_bsb<-p_rel_to_keep_bsb - p_rel_to_keep_bsb*.15
              }
              #If baseline bsb harvest is less than MRIP, and in the new run bsb harvest is still less than MRIP, 
              #increase the baseline p_rel_to_keep value 
              if(harv_diff_bsb<0) {
                p_rel_to_keep_bsb<-p_rel_to_keep_bsb + p_rel_to_keep_bsb*.16
              }
            }
            #For draws where keep_to_release==1
            #If in the baseline run, harvest is less than MRIP, but in a new run harvest is greater than MRIP, 
            #reduce the baseline p_keep_to_rel value 
            if(keep_to_rel_bsb==1 & all_keep_to_rel_bsb!=1) {
              if(harv_diff_bsb>0){
                p_keep_to_rel_bsb<-p_keep_to_rel_bsb + p_keep_to_rel_bsb*.16
              }
              #If in the baseline run, harvest is less than MRIP, and in the new run harvest is still less than MRIP, 
              #increase the baseline p_keep_to_rel value 
              if(harv_diff_bsb<0){
                p_keep_to_rel_bsb<-p_keep_to_rel_bsb - p_keep_to_rel_bsb*.15
              }
            }
            
          }
          
          #Scup
          #For draws where release_to_keep==1:
          #If baseline sf harvest is less than MRIP, but in a new run sf harvest is greater than MRIP, 
          #reduce the baseline p_rel_to_keep value 
          if(scup_achieved!=1){
            if(rel_to_keep_scup==1){
              if(harv_diff_scup>0){
                p_rel_to_keep_scup<-p_rel_to_keep_scup - p_rel_to_keep_scup*.15
              }
              #If baseline scup harvest is less than MRIP, and in the new run scup harvest is still less than MRIP, 
              #increase the baseline p_rel_to_keep value 
              if(harv_diff_scup<0) {
                p_rel_to_keep_scup<-p_rel_to_keep_scup + p_rel_to_keep_scup*.16
              }
            }
            #For draws where keep_to_release==1
            #If in the baseline run, harvest is less than MRIP, but in a new run harvest is greater than MRIP, 
            #reduce the baseline p_keep_to_rel value 
            if(keep_to_rel_scup==1 & all_keep_to_rel_scup!=1) {
              if(harv_diff_scup>0){
                p_keep_to_rel_scup<-p_keep_to_rel_scup + p_keep_to_rel_scup*.16
              }
              #If in the baseline run, harvest is less than MRIP, and in the new run harvest is still less than MRIP, 
              #increase the baseline p_keep_to_rel value 
              if(harv_diff_scup<0){
                p_keep_to_rel_scup<-p_keep_to_rel_scup - p_keep_to_rel_scup*.15
              }
            }
            
          }
          
          if(all_keep_to_rel_sf==1 & sf_achieved!=1) {
              p_keep_to_rel_sf<-1
          }
            
          if(all_keep_to_rel_bsb==1 & bsb_achieved!=1) {
              p_keep_to_rel_bsb<-1
          }
            
          if(all_keep_to_rel_scup==1 & scup_achieved!=1) {
            p_keep_to_rel_scup<-1
          }
          
          source(file.path(code_cd, "calibrate_rec_catch1.R"))
          
          for (p in 1:nrow(calib_comparison1)) {
            sp <- calib_comparison1$species[p]
            
            assign(paste0("MRIP_keep_", sp), calib_comparison1$MRIP_keep[p])
            assign(paste0("model_keep_", sp), calib_comparison1$model_keep[p])
            assign(paste0("harv_diff_", sp), calib_comparison1$diff_keep[p])
            assign(paste0("harv_pct_diff_", sp), calib_comparison1$pct_diff_keep[p])
            # assign(paste0("rel_to_keep_", sp), calib_comparison1$rel_to_keep_new[p])
            # assign(paste0("keep_to_rel_", sp), calib_comparison1$keep_to_rel_new[p])
          
          }
          
          message("run ", i, " state ", s, " mode ", md)
          message("model_sf_harv: ", model_keep_sf)
          message("mrip_sf_harv: ", MRIP_keep_sf)
          message("diff_sf_harv: ", harv_diff_sf)
          message("pct_diff_sf_harv: ", harv_pct_diff_sf)
          message("rel_to_keep_sf: ", rel_to_keep_sf)
          message("p_rel_to_keep_sf: ", p_rel_to_keep_sf)
          message("p_keep_to_rel_sf: ", p_keep_to_rel_sf)
          
          message("model_bsb_harv: ", model_keep_bsb)
          message("mrip_bsb_harv: ", MRIP_keep_bsb)
          message("diff_bsb_harv: ", harv_diff_bsb)
          message("pct_diff_bsb_harv: ", harv_pct_diff_bsb)
          message("rel_to_keep_bsb: ", rel_to_keep_bsb)
          message("p_rel_to_keep_bsb: ", p_rel_to_keep_bsb)
          message("p_keep_to_rel_bsb: ", p_keep_to_rel_bsb)

          message("model_scup_harv: ", model_keep_scup)
          message("mrip_scup_harv: ", MRIP_keep_scup)
          message("diff_scup_harv: ", harv_diff_scup)
          message("pct_diff_scup_harv: ", harv_pct_diff_scup)
          message("rel_to_keep_scup: ", rel_to_keep_scup)
          message("p_rel_to_keep_scup: ", p_rel_to_keep_scup)
          message("p_keep_to_rel_scup: ", p_keep_to_rel_scup)

          sf_achieved<-case_when((abs(harv_diff_sf)<500 | (abs(harv_pct_diff_sf)<5 & !is.na(harv_pct_diff_sf))) ~1, TRUE~0)
          bsb_achieved<-case_when((abs(harv_diff_bsb)<500 | (abs(harv_pct_diff_bsb)<5 & !is.na(harv_pct_diff_bsb))) ~1, TRUE~0)
          scup_achieved<-case_when((abs(harv_diff_scup)<500 | (abs(harv_pct_diff_scup)<5 & !is.na(harv_pct_diff_scup))) ~1, TRUE~0)
          
          # In some cases, there are not enough fish within 3-inches of the minimum size to re-allocate 
          # from kept to release. In this case, p_rel_to_keep_SP will continue to grow unbounded. 
          

          
          if(rel_to_keep_sf==1 & p_rel_to_keep_sf>1){
            sf_convergence<-0
            sf_achieved<-1
            
          }
         
          
          if(rel_to_keep_bsb==1 & p_rel_to_keep_bsb>1){
            bsb_convergence<-0
            bsb_achieved<-1
            
          }
          
          
          if(rel_to_keep_scup==1 & p_rel_to_keep_scup>1){
            scup_convergence<-0
            scup_achieved<-1
            
          }
        }
        
        k <- k + 1
        
        calibrated[[k]] <- calib_comparison1 %>% 
          dplyr::mutate(keep_to_rel_sf=keep_to_rel_sf, 
                        rel_to_keep_sf=  rel_to_keep_sf,
                        p_rel_to_keep_sf=p_rel_to_keep_sf,
                        p_keep_to_rel_sf= p_keep_to_rel_sf,
                        sf_convergence=sf_convergence,
                        
                        keep_to_rel_bsb=keep_to_rel_bsb, 
                        rel_to_keep_bsb=  rel_to_keep_bsb,
                        p_rel_to_keep_bsb= p_rel_to_keep_bsb, 
                        p_keep_to_rel_bsb= p_keep_to_rel_bsb,
                        bsb_convergence=bsb_convergence,
                        
                        keep_to_rel_scup=keep_to_rel_scup, 
                        rel_to_keep_scup =rel_to_keep_scup,
                        p_rel_to_keep_scup=p_rel_to_keep_scup,
                        p_keep_to_rel_scup=p_keep_to_rel_scup, 
                        scup_convergence=scup_convergence,
                        
                        n_sub_scup_kept=n_sub_scup_kept, 
                        prop_sub_scup_kept=prop_sub_scup_kept, 
                        n_legal_scup_rel=n_legal_scup_rel, 
                        prop_legal_scup_rel=prop_legal_scup_rel, 
                        
                        n_sub_sf_kept=n_sub_sf_kept, 
                        n_legal_sf_rel=n_legal_sf_rel, 
                        prop_sub_sf_kept=prop_sub_sf_kept, 
                        prop_legal_sf_rel=prop_legal_sf_rel, 
                        
                        n_sub_bsb_kept=n_sub_bsb_kept, 
                        n_legal_bsb_rel=n_legal_bsb_rel, 
                        prop_sub_bsb_kept=prop_sub_bsb_kept, 
                        prop_legal_bsb_rel=prop_legal_bsb_rel)
        
        
        }
  }
}


calibrated_combined <- do.call(rbind, calibrated) %>% 
  dplyr::select(-rel_to_keep_new, -keep_to_rel_new, -p_keep_to_rel_new, -p_rel_to_keep_new)

saveRDS(calibrated_combined, file = file.path(iterative_input_data_cd, "calibrated_model_stats.rds"))





