################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      copula model loop projection.R
# Purpose:     Copula simulation of correlated per-trip harvest and releases for
#              the PROJECTION period. Near-duplicate of 'copula model loop.R' - the
#              same duplication that persists between the two current copula
#              scripts.
# Superseded by: Code/pre_sim/copula_modeling_projection.R
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
################################################################################
################################################################################

# ---- packages ----
required_pkgs <- c(
  "survey", "copula", "MASS", "fitdistrplus", "readxl",
  "weights", "wCorr", "patchwork", "Hmisc", "tidyr",
  "dplyr", "ggplot2", "writexl", "plyr", "conflicted"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install these packages before running the script: ",
       paste(missing_pkgs, collapse = ", "))
}

library(patchwork)
library(survey)
library(copula)
library(MASS)
library(fitdistrplus)
library(readxl)
library(Hmisc)
library(weights)
library(wCorr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(plyr)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)

# ---- controls ----
n_sim   <- 5000
n_draws <- 10
n_reps  <- 200

statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
statez <- c("MA")

input_file <- "E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws/projected_mrip_catch_processed.xlsx"

full_df <- readxl::read_xlsx(input_file)

# ---- helper functions ----

get_rep_stat <- function(svy_obj) {
  out <- survey::svymean(svy_obj, return.replicates = TRUE, na.rm = TRUE)
  list(
    mean = as.numeric(coef(out)),
    reps = as.numeric(attr(out, "replicates"))
  )
}

get_rep_var <- function(df, varname, rep_design) {
  rep_wgts <- survey::weights(rep_design, type = "analysis")
  out <- sapply(seq_len(ncol(rep_wgts)), function(i) {
    rep_data <- rep_wgts[, i]
    svy_var <- survey::svydesign(
      ids = ~psu_id,
      strata = ~strat_id,
      weights = ~rep_data,
      nest = TRUE,
      data = df
    )
    as.numeric(coef(survey::svyvar(stats::as.formula(paste0("~", varname)), svy_var, na.rm = TRUE)))
  })
  as.numeric(out)
}

safe_theta <- function(mu, var, theta_cap = 1000) {
  mu  <- pmax(as.numeric(mu), 1e-8)
  var <- pmax(as.numeric(var), 1e-8)
  theta <- mu^2 / pmax(var - mu, 1e-6)
  theta <- pmax(theta, 1e-6)
  theta <- pmin(theta, theta_cap)
  theta[!is.finite(theta)] <- theta_cap
  theta
}

weighted_sample_for_copula <- function(df, value_cols, n_sim, weight_col = "wp_int", jitter_ties = FALSE) {
  w <- df[[weight_col]]
  w[is.na(w) | w < 0] <- 0
  
  if (sum(w) <= 0) {
    stop("All sampling weights are zero or missing in this domain.")
  }
  
  idx <- sample.int(
    n = nrow(df),
    size = min(n_sim, nrow(df)),
    replace = TRUE,
    prob = w
  )
  
  out <- df[idx, , drop = FALSE]
  
  if (jitter_ties) {
    for (v in value_cols) {
      out[[v]] <- out[[v]] + stats::runif(nrow(out), -1e-8, 1e-8)
    }
  }
  
  out
}

cap_with_resample <- function(x, max_x) {
  x <- round(x)
  x[x < 0] <- 0
  
  if (all(x <= max_x)) return(x)
  
  x_cap <- pmin(x, max_x)
  overflow <- which(x > max_x)
  excess <- sum(x[overflow] - max_x)
  
  while (excess > 0) {
    candidates <- which(x_cap < max_x)
    if (length(candidates) == 0) break
    
    room <- max_x - x_cap[candidates]
    probs <- room / sum(room)
    chosen <- sample(candidates, size = 1, prob = probs)
    x_cap[chosen] <- x_cap[chosen] + 1
    excess <- excess - 1
  }
  
  x_cap
}

n_psu <- function(df) {
  dplyr::n_distinct(df$psu_id)
}



for(s in statez) {
  
  # Load data
  df <- full_df %>% dplyr::filter(state == s)
  
  
  
  # Strata can fall into one of four categories:
  
  # 1) both mean harvest- and discards-per-trip>0 
  # 2) mean discards-per-trip>0, mean harvest-per-trip==0
  # 3) mean harvest-per-trip>0, mean discards-per-trip==0
  # 4) mean discards-per-trip==0, mean discards-per-trip==0
  
  # I used copula model to simulate 1), whereas 2) and 3) are distributed NB
  
  
  
  ############ SUMMER FLOUNDER ############
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
  df_full1 <- df %>% filter(sf_keep_and_rel==1 )
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
  df_full2 <- df %>% filter(sf_only_rel==1 )
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
  df_full3 <- df %>% filter(sf_only_keep==1)
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
  df_full4 <- df %>% filter(sf_no_catch==1)
  
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0 BUT positive values of harvest/discards never occur simultaneously
  df_full5 <- df %>% filter(sf_keep_and_rel_ind==1)
  
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
  if (nrow(df_full1) > 0) {
    
    
    all_results1 <- list()
    
    for (dom in unique(df_full1$my_dom_id_string)) {
      
      df <- df_full1 %>% filter(my_dom_id_string == dom)
      
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_keep <- survey::svymean(~sf_keep, svy_design, na.rm = TRUE)
      mean_rel  <- survey::svymean(~sf_rel,  svy_design, na.rm = TRUE)
      
      mu_keep  <- as.numeric(coef(mean_keep))
      mu_rel   <- as.numeric(coef(mean_rel))
      var_keep <- as.numeric(attr(mean_keep, "var"))
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      
      # Fallback SEs
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sesf_keep, na.rm = TRUE)^2
      }
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sesf_rel, na.rm = TRUE)^2
      }
      
      max_keep <- round(max(df$sf_keep, na.rm = TRUE)) + 2
      max_rel  <- round(max(df$sf_rel,  na.rm = TRUE)) + 6
      
      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_keep_obj <- get_rep_stat(~sf_keep, rep_design)
      rep_rel_obj  <- get_rep_stat(~sf_rel,  rep_design)
      
      rep_means_keep <- rep_keep_obj$reps
      rep_means_rel  <- rep_rel_obj$reps
      
      # Replicate variances
      rep_vars_keep <- get_rep_var(df, "sf_keep", rep_design)
      rep_vars_rel  <- get_rep_var(df, "sf_rel",  rep_design)
      
      # Negative binomial size parameters
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      
      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
      }
      
      # Fit the copula to a sample (max 5000 obs.) of the original data b/c it can take a while if N is large
      
      df$w_int_rounded <- round(df$wp_int)
      df_expanded <- uncount(df, weights = w_int_rounded) 
      df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), n_sim)) 
      
      
      # Create pseudo-observations (rank-based empirical CDFs)
      df_expanded <- df_expanded %>%
        mutate(
          rank_keep = rank(sf_keep, ties.method = "average"),
          rank_rel  = rank(sf_rel,  ties.method = "average"),
          u_keep = rank_keep / (n() + 1),
          u_rel  = rank_rel / (n() + 1)
        )
      

      # Fit copula using pseudo-observations
      
      u_mat <- cbind(df_expanded$u_keep, df_expanded$u_rel)
      
      
      
      tau_hat <- cor(u_mat[,1], u_mat[,2], method = "kendall")
      
      # Assess dependence:
      # tau>=0.3: use Gumbel copula
      # tau<=-.3: normal copula, which allows for negative dependence. 
      # -.3>=tau<=.3: frank copula, for moderate, neutral dependence
      
      
      copula_fit <- try({
        if (tau_hat >= 0.3) {
          copula::fitCopula(copula::gumbelCopula(dim = 2), u_mat, method = "mpl", start = 1)
        } else if (tau_hat <= -0.3) {
          copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
        } else {
          copula::fitCopula(copula::frankCopula(dim = 2), u_mat, method = "mpl", start = 1)
        }
      }, silent = TRUE)
      
      if (inherits(copula_fit, "try-error")) {
        tau_hat <- 0
        cop_name <- "Gaussian_fallback_independenceish"
        copula_fit <- copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
      }
      
      
      
      # Simulate from the fitted copula
      sim_datasets <- list()
      i <- 1
      while (i <= n_draws) {
        
        sim_u <- rCopula(n_sim, copula_fit@copula)
        
        # Sample mu_keep and mu_rel with uncertainty
        sampled_mu_keep <-  rnorm(1, mu_keep, sqrt(var_keep))
        sampled_mu_rel  <- rnorm(1, mu_rel,  sqrt(var_rel))
        
        if (n_psu(df) <= 1) {
          sampled_theta_keep <- theta_hat_keep_single
          sampled_theta_rel  <- theta_hat_rel_single
        } else {
          sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
          sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
        }
        
        
        # Convert uniform to NB using quantiles
        sim_keep <- qnbinom(sim_u[,1], size = sampled_theta_keep, mu = sampled_mu_keep)
        
        # Convert uniform to NB using quantiles
        sim_rel  <- qnbinom(sim_u[,2], size = sampled_theta_rel, mu = sampled_mu_rel)
        
        if (!any(is.na(sim_keep)) && !any(is.na(sim_rel)) && !any(is.infinite(sim_keep)) && !any(is.infinite(sim_rel))){
 
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)

          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, 
                                          sf_keep_sim = sim_keep, 
                                          sf_rel_sim = sim_rel,  
                                          my_dom_id_string = my_dom_id_string) 
          
          i <- i + 1  # Only increment if no NaNs
        }
      }
      
      
      # Combine all simulated datasets, tagging each with its simulation ID
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      all_results1[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results1", "n_sim", "n_draws", "s")
      rm(list = setdiff(ls(), keep))
      
      
    }
    
    final_result1 <- bind_rows(all_results1)
    final_result1 <- final_result1 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup() 
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  
  ################
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
  
  if (nrow(df_full2) > 0) {
    
    all_results2 <- list()
    
    for (dom in unique(df_full2$my_dom_id_string)) {
      
      df <- df_full2 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_rel  <- survey::svymean(~sf_rel,  svy_design, na.rm = TRUE)
      
      mu_rel   <- as.numeric(coef(mean_rel))
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      
      # Fallback SEs
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sesf_rel, na.rm = TRUE)^2
      }
      
      max_rel  <- round(max(df$sf_rel,  na.rm = TRUE)) + 6
      
      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_rel_obj  <- get_rep_stat(~sf_rel,  rep_design)
      
      rep_means_rel  <- rep_rel_obj$reps
      
      # Replicate variances
      rep_vars_rel  <- get_rep_var(df, "sf_rel",  rep_design)
      
      # Negative binomial size parameters
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      
      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
      }
      
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu <- max(1e-8, rnorm(1, mu_rel, sqrt(var_rel)))
        
        if (n_psu(df) <= 1) {
          sampled_theta <- theta_hat_rel_single
        } else {
          sampled_theta  <- sample(rep_theta_rel,  1, replace = TRUE)
        }
        

        sim_rel <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
        
        
        if (!any(is.na(sim_rel)) && !any(is.infinite(sim_rel))) {

          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, sf_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
          i <- i + 1  # Only increment if no NaNs
        }
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            dplyr::mutate(sim_id = i)
        })
      )
      
      all_results2[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results2", "final_result1", "n_sim", "n_draws", "s")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result2 <- bind_rows(all_results2)
    final_result2 <- final_result2 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number(), sf_keep_sim=0) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
  if (nrow(df_full3) > 0) {
    
    all_results3 <- list()
    
    for (dom in unique(df_full3$my_dom_id_string)) {
      
      df <- df_full3 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_keep <- survey::svymean(~sf_keep, svy_design, na.rm = TRUE)

      mu_keep  <- as.numeric(coef(mean_keep))
      var_keep <- as.numeric(attr(mean_keep, "var"))

      # Fallback SEs
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sesf_keep, na.rm = TRUE)^2
      }

      
      max_keep <- round(max(df$sf_keep, na.rm = TRUE)) + 2

      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_keep_obj <- get_rep_stat(~sf_keep, rep_design)

      rep_means_keep <- rep_keep_obj$reps

      # Replicate variances
      rep_vars_keep <- get_rep_var(df, "sf_keep", rep_design)

      # Negative binomial size parameters
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)

      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu <- max(1e-8, rnorm(1, mu_keep, sqrt(var_keep)))
        
        if (n_psu(df) <= 1) {
          sampled_theta <- theta_hat_keep_single         # Single-PSU theta
        } else {
          sampled_theta <- sample(rep_theta_keep, 1, replace = TRUE)         # Sample theta
          
        }
        

        sim_keep <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
        
        if (!any(is.na(sim_keep)) && !any(is.infinite(sim_keep))) {
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, sf_keep_sim = sim_keep, my_dom_id_string = my_dom_id_string)
          i <- i + 1  # Only increment if no NaNs
        }
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      
      all_results3[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results3", "final_result1", "final_result2","n_sim", "n_draws", "s")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result3 <- bind_rows(all_results3)
    final_result3 <- final_result3 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number(), sf_rel_sim=0) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3",  "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
  if (nrow(df_full4) > 0) {
    
    all_results4 <- list()
    
    for (dom in unique(df_full4$my_dom_id_string)) {
      
      sim_datasets <- list()
      i <- 1
      while (i <= n_draws) {
        
        sim_keep <- rep(0, n_sim) 
        sim_rel <- rep(0, n_sim)
        my_dom_id_string <- rep(dom, n_sim)
        
        sim_datasets[[i]] <- data.frame(sim_id = i, sf_keep_sim = sim_keep, sf_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
        i <- i + 1 
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      
      all_results4[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results4", "final_result1", "final_result2","final_result3", "n_sim", "n_draws", "s")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result4 <- bind_rows(all_results4)
    final_result4 <- final_result4 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3", "final_result4","df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0 BUT positive values of harvest/discards never occur simultaneously
  if (nrow(df_full5) > 0) {
    
    all_results5 <- list()
    
    for (dom in unique(df_full5$my_dom_id_string)) {
      
      df <- df_full5 %>% filter(my_dom_id_string == dom)
      
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      mean_rel  <- survey::svymean(~sf_rel,  svy_design, na.rm = TRUE)
      mean_keep <- survey::svymean(~sf_keep, svy_design, na.rm = TRUE)
      
      mu_rel  <- as.numeric(coef(mean_rel))
      mu_keep <- as.numeric(coef(mean_keep))
      
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      var_keep <- as.numeric(attr(mean_keep, "var"))
      
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sesf_rel, na.rm = TRUE)^2
      }
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sesf_keep, na.rm = TRUE)^2
      }
      
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      rep_rel_obj  <- get_rep_stat(~sf_rel,  rep_design)
      rep_keep_obj <- get_rep_stat(~sf_keep, rep_design)
      
      rep_means_rel  <- rep_rel_obj$reps
      rep_means_keep <- rep_keep_obj$reps
      
      rep_vars_rel  <- get_rep_var(df, "sf_rel",  rep_design)
      rep_vars_keep <- get_rep_var(df, "sf_keep", rep_design)
      
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)
      
      if (n_psu(df) <= 1) {
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu_rel  <- max(1e-8, rnorm(1, mu_rel,  sqrt(var_rel)))
        sampled_mu_keep <- max(1e-8, rnorm(1, mu_keep, sqrt(var_keep)))
        
        if (n_psu(df) <= 1) {
          sampled_theta_rel  <- theta_hat_rel_single
          sampled_theta_keep <- theta_hat_keep_single
        } else {
          sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
          sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
        }
        
        sim_rel  <- qnbinom(runif(n_sim), size = sampled_theta_rel,  mu = sampled_mu_rel)
        sim_keep <- qnbinom(runif(n_sim), size = sampled_theta_keep, mu = sampled_mu_keep)
        
         if (!any(is.na(sim_keep)) && !any(is.na(sim_rel)) && !any(is.infinite(sim_keep)) && !any(is.infinite(sim_rel))){
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(
            sim_id = i, 
            sf_keep_sim = sim_keep, 
            sf_rel_sim = sim_rel,  
            my_dom_id_string = my_dom_id_string) 
          
          i <- i + 1  # Only increment if no NaNs
        }
        
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            dplyr::mutate(sim_id = i)
        })
      )
      
      all_results5[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results5","final_result1", "final_result2","final_result3", "final_result4", "n_sim", "n_draws", "s")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result5 <- bind_rows(all_results5) %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup()
    
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3", "final_result4","final_result5", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  # COMBINE DRAWS ACROSS DOMAINS AND SIMULATIONS
  
  # Initialize an empty list
  results_list <- list()
  
  # Check for each final_results object and add to the list if it exists
  if (exists("final_result1")) results_list <- append(results_list, list(final_result1))
  if (exists("final_result2")) results_list <- append(results_list, list(final_result2))
  if (exists("final_result3")) results_list <- append(results_list, list(final_result3))
  if (exists("final_result4")) results_list <- append(results_list, list(final_result4))
  if (exists("final_result5")) results_list <- append(results_list, list(final_result5))
  
  # Combine all existing results into one data frame
  combined_results_SF <- do.call(rbind, results_list)
  
  
  # List the objects you want to keep
  keep <- c("n_sim", "n_draws", "combined_results_SF", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  ############ BLACK SEA BASS ############  
  df <- full_df %>% dplyr::filter(state == s)
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
  df_full1 <- df %>% filter(bsb_keep_and_rel==1 )
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
  df_full2 <- df %>% filter(bsb_only_rel==1 )
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
  df_full3 <- df %>% filter(bsb_only_keep==1)
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
  df_full4 <- df %>% filter(bsb_no_catch==1)
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0 BUT positive values of harvest/discards never occur simultaneously
  df_full5 <- df %>% filter(bsb_keep_and_rel_ind==1)
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
  if (nrow(df_full1) > 0) {
    min_n=round(min(df_full1$wp_int))
    
    all_results1 <- list()
    
    for (dom in unique(df_full1$my_dom_id_string)) {
      df <- df_full1 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_keep <- survey::svymean(~bsb_keep, svy_design, na.rm = TRUE)
      mean_rel  <- survey::svymean(~bsb_rel,  svy_design, na.rm = TRUE)
      
      mu_keep  <- as.numeric(coef(mean_keep))
      mu_rel   <- as.numeric(coef(mean_rel))
      var_keep <- as.numeric(attr(mean_keep, "var"))
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      
      # Fallback SEs
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sebsb_keep, na.rm = TRUE)^2
      }
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sebsb_rel, na.rm = TRUE)^2
      }
      
      max_keep <- round(max(df$bsb_keep, na.rm = TRUE)) + 2
      max_rel  <- round(max(df$bsb_rel,  na.rm = TRUE)) + 6
      
      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_keep_obj <- get_rep_stat(~bsb_keep, rep_design)
      rep_rel_obj  <- get_rep_stat(~bsb_rel,  rep_design)
      
      rep_means_keep <- rep_keep_obj$reps
      rep_means_rel  <- rep_rel_obj$reps
      
      # Replicate variances
      rep_vars_keep <- get_rep_var(df, "bsb_keep", rep_design)
      rep_vars_rel  <- get_rep_var(df, "bsb_rel",  rep_design)
      
      # Negative binomial size parameters
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      
      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
      }
      
      # Fit the copula to a sample (max 5000 obs.) of the original data b/c it can take a while if N is large
      
      df$w_int_rounded <- round(df$wp_int)
      df_expanded <- uncount(df, weights = w_int_rounded) 
      df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), n_sim)) 
      
      # Create pseudo-observations (rank-based empirical CDFs)
      df_expanded <- df_expanded %>%
        mutate(
          rank_keep = rank(bsb_keep, ties.method = "average"),
          rank_rel  = rank(bsb_rel,  ties.method = "average"),
          u_keep = rank_keep / (n() + 1),
          u_rel  = rank_rel / (n() + 1)
        )
      
      # Fit copula using pseudo-observations
      
      u_mat <- cbind(df_expanded$u_keep, df_expanded$u_rel)
      
      # Assess dependence:
      # tau>=0.3: use Gumbel copula
      # tau<=-.3: normal copula, which allows for negative dependence. 
      # -.3>=tau<=.3: frank copula, for moderate, neutral dependence
      
      tau_hat <- cor(u_mat[,1], u_mat[,2], method = "kendall")
      
      copula_fit <- try({
        if (tau_hat >= 0.3) {
          copula::fitCopula(copula::gumbelCopula(dim = 2), u_mat, method = "mpl", start = 1)
        } else if (tau_hat <= -0.3) {
          copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
        } else {
          copula::fitCopula(copula::frankCopula(dim = 2), u_mat, method = "mpl", start = 1)
        }
      }, silent = TRUE)
      
      if (inherits(copula_fit, "try-error")) {
        tau_hat <- 0
        cop_name <- "Gaussian_fallback_independenceish"
        copula_fit <- copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
      }
      
      
      
      # Simulate from the fitted copula
      sim_datasets <- list()
      i <- 1
      while (i <= n_draws) {
        
        sim_u <- rCopula(n_sim, copula_fit@copula)
        
        # Sample mu_keep and mu_rel with uncertainty
        sampled_mu_keep <-  rnorm(1, mu_keep, sqrt(var_keep))
        sampled_mu_rel  <- rnorm(1, mu_rel,  sqrt(var_rel))
        
      if (n_psu(df) <= 1) {
          sampled_theta_keep <- theta_hat_keep_single
          sampled_theta_rel  <- theta_hat_rel_single
        } else {
          sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
          sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
        }
        
        # Convert uniform to NB using quantiles
        sim_keep <- qnbinom(sim_u[,1], size = sampled_theta_keep, mu = sampled_mu_keep)
        
        
        # Convert uniform to NB using quantiles
        sim_rel  <- qnbinom(sim_u[,2], size = sampled_theta_rel, mu = sampled_mu_rel)
        
         if (!any(is.na(sim_keep)) && !any(is.na(sim_rel)) && !any(is.infinite(sim_keep)) && !any(is.infinite(sim_rel))){
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, bsb_keep_sim = sim_keep, bsb_rel_sim = sim_rel,  my_dom_id_string = my_dom_id_string) 
          
          i <- i + 1  # Only increment if no NaNs
        }
      }
      
      
      # Combine all simulated datasets, tagging each with its simulation ID
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      all_results1[[dom]] <- combined_sim
      keep <- c("min_n", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results1", "n_sim", "n_draws", "s", "combined_results_SF")
      rm(list = setdiff(ls(), keep))
      
      
    }
    
    final_result1 <- bind_rows(all_results1)
    final_result1 <- final_result1 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup() 
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  ################
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
  
  if (nrow(df_full2) > 0) {
    
    all_results2 <- list()
    
    for (dom in unique(df_full2$my_dom_id_string)) {
      
      df <- df_full2 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_rel  <- survey::svymean(~bsb_rel,  svy_design, na.rm = TRUE)
      
      mu_rel   <- as.numeric(coef(mean_rel))
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      
      # Fallback SEs

      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sebsb_rel, na.rm = TRUE)^2
      }
      
      max_rel  <- round(max(df$bsb_rel,  na.rm = TRUE)) + 6
      
      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_rel_obj  <- get_rep_stat(~bsb_rel,  rep_design)
      
      rep_means_rel  <- rep_rel_obj$reps
      
      # Replicate variances
      rep_vars_rel  <- get_rep_var(df, "bsb_rel",  rep_design)
      
      # Negative binomial size parameters
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      
      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu <- max(1e-8, rnorm(1, mu_rel, sqrt(var_rel)))

        if (n_psu(df) <= 1) {
          sampled_theta <- theta_hat_rel_single
        } else {
          sampled_theta <- sample(rep_theta_rel, 1, replace = TRUE)         # Sample thet
        }
        
        sim_rel <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
        
        
        if (!any(is.na(sim_rel)) && !any(is.infinite(sim_rel))) {
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, bsb_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
          i <- i + 1  # Only increment if no NaNs
        }
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            dplyr::mutate(sim_id = i)
        })
      )
      
      all_results2[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results2", "final_result1", "n_sim", "n_draws", "s", "combined_results_SF")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result2 <- bind_rows(all_results2)
    final_result2 <- final_result2 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number(), bsb_keep_sim=0) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
  if (nrow(df_full3) > 0) {
    
    all_results3 <- list()
    
    for (dom in unique(df_full3$my_dom_id_string)) {
      
      df <- df_full3 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_keep <- survey::svymean(~bsb_keep, svy_design, na.rm = TRUE)

      mu_keep  <- as.numeric(coef(mean_keep))
      var_keep <- as.numeric(attr(mean_keep, "var"))

      # Fallback SEs
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sebsb_keep, na.rm = TRUE)^2
      }

      
      max_keep <- round(max(df$bsb_keep, na.rm = TRUE)) + 2

      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_keep_obj <- get_rep_stat(~bsb_keep, rep_design)

      rep_means_keep <- rep_keep_obj$reps

      # Replicate variances
      rep_vars_keep <- get_rep_var(df, "bsb_keep", rep_design)

      # Negative binomial size parameters
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)

      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu <- max(1e-8, rnorm(1, mu_keep, sqrt(var_keep)))

        if (n_psu(df) <= 1) {
          sampled_theta <- theta_hat_keep_single         # Single-PSU theta
        } else {
          sampled_theta <- sample(rep_theta_keep, 1, replace = TRUE)         # Sample theta
        }
        
        sim_keep <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
        
        if (!any(is.na(sim_keep)) && !any(is.infinite(sim_keep))) {
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          #sim_keep <- pmin(sim_keep, max_keep*2)
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, bsb_keep_sim = sim_keep, my_dom_id_string = my_dom_id_string)
          i <- i + 1  # Only increment if no NaNs
        }
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      
      all_results3[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results3", "final_result1", "final_result2","n_sim", "n_draws", "s", "combined_results_SF")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result3 <- bind_rows(all_results3)
    final_result3 <- final_result3 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number(), bsb_rel_sim=0) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3",  "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
  if (nrow(df_full4) > 0) {
    
    all_results4 <- list()
    
    for (dom in unique(df_full4$my_dom_id_string)) {
      
      sim_datasets <- list()
      i <- 1
      while (i <= n_draws) {
        
        sim_keep <- rep(0, n_sim) 
        sim_rel <- rep(0, n_sim)
        my_dom_id_string <- rep(dom, n_sim)
        
        sim_datasets[[i]] <- data.frame(sim_id = i, bsb_keep_sim = sim_keep, bsb_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
        i <- i + 1 
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      
      all_results4[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results4", "final_result1", "final_result2","final_result3", "n_sim", "n_draws", "s", "combined_results_SF")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result4 <- bind_rows(all_results4)
    final_result4 <- final_result4 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3", "final_result4","df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0 BUT positive values of harvest/discards never occur simultaneously
  if (nrow(df_full5) > 0) {
    
    all_results5 <- list()
    
    for (dom in unique(df_full5$my_dom_id_string)) {
      
      df <- df_full5 %>% filter(my_dom_id_string == dom)
      
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      mean_rel  <- survey::svymean(~bsb_rel,  svy_design, na.rm = TRUE)
      mean_keep <- survey::svymean(~bsb_keep, svy_design, na.rm = TRUE)
      
      mu_rel  <- as.numeric(coef(mean_rel))
      mu_keep <- as.numeric(coef(mean_keep))
      
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      var_keep <- as.numeric(attr(mean_keep, "var"))
      
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sebsb_rel, na.rm = TRUE)^2
      }
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sebsb_keep, na.rm = TRUE)^2
      }
      
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      rep_rel_obj  <- get_rep_stat(~bsb_rel,  rep_design)
      rep_keep_obj <- get_rep_stat(~bsb_keep, rep_design)
      
      rep_means_rel  <- rep_rel_obj$reps
      rep_means_keep <- rep_keep_obj$reps
      
      rep_vars_rel  <- get_rep_var(df, "bsb_rel",  rep_design)
      rep_vars_keep <- get_rep_var(df, "bsb_keep", rep_design)
      
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)
      
      if (n_psu(df) <= 1) {
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu_rel  <- max(1e-8, rnorm(1, mu_rel,  sqrt(var_rel)))
        sampled_mu_keep <- max(1e-8, rnorm(1, mu_keep, sqrt(var_keep)))
        
        if (n_psu(df) <= 1) {
          sampled_theta_rel  <- theta_hat_rel_single
          sampled_theta_keep <- theta_hat_keep_single
        } else {
          sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
          sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
        }
        
        sim_rel  <- qnbinom(runif(n_sim), size = sampled_theta_rel,  mu = sampled_mu_rel)
        sim_keep <- qnbinom(runif(n_sim), size = sampled_theta_keep, mu = sampled_mu_keep)
        
         if (!any(is.na(sim_keep)) && !any(is.na(sim_rel)) && !any(is.infinite(sim_keep)) && !any(is.infinite(sim_rel))){
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, bsb_keep_sim = sim_keep, bsb_rel_sim = sim_rel,  my_dom_id_string = my_dom_id_string) 
          
          i <- i + 1  # Only increment if no NaNs
        }
        
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            dplyr::mutate(sim_id = i)
        })
      )
      
      all_results5[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results5","final_result1", "final_result2","final_result3", "final_result4", "n_sim", "n_draws", "s", "combined_results_SF")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result5 <- bind_rows(all_results5) %>% 
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup()
    
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3", "final_result4","final_result5", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s",  "combined_results_SF")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  # COMBINE DRAWS ACROSS DOMAINS AND SIMULATIONS
  
  # Initialize an empty list
  results_list <- list()
  
  # Check for each final_results object and add to the list if it exists
  if (exists("final_result1")) results_list <- append(results_list, list(final_result1))
  if (exists("final_result2")) results_list <- append(results_list, list(final_result2))
  if (exists("final_result3")) results_list <- append(results_list, list(final_result3))
  if (exists("final_result4")) results_list <- append(results_list, list(final_result4))
  if (exists("final_result5")) results_list <- append(results_list, list(final_result5))
  
  # Combine all existing results into one data frame
  combined_results_BSB <- do.call(rbind, results_list)
  
  
  # List the objects you want to keep
  keep <- c("n_sim", "n_draws", "combined_results_BSB", "combined_results_SF", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  
  ############ SCUP ############  
  df <- full_df %>% dplyr::filter(state == s)
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
  df_full1 <- df %>% filter(scup_keep_and_rel==1 )
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
  df_full2 <- df %>% filter(scup_only_rel==1 )
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
  df_full3 <- df %>% filter(scup_only_keep==1)
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
  df_full4 <- df %>% filter(scup_no_catch==1)
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0 BUT positive values of harvest/discards never occur simultaneously
  df_full5 <- df %>% filter(scup_keep_and_rel_ind==1)
  
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
  if (nrow(df_full1) > 0) {
    
    
    all_results1 <- list()
    
    for (dom in unique(df_full1$my_dom_id_string)) {
      
      df <- df_full1 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_keep <- survey::svymean(~scup_keep, svy_design, na.rm = TRUE)
      mean_rel  <- survey::svymean(~scup_rel,  svy_design, na.rm = TRUE)
      
      mu_keep  <- as.numeric(coef(mean_keep))
      mu_rel   <- as.numeric(coef(mean_rel))
      var_keep <- as.numeric(attr(mean_keep, "var"))
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      
      # Fallback SEs
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sescup_keep, na.rm = TRUE)^2
      }
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sescup_rel, na.rm = TRUE)^2
      }
      
      max_keep <- round(max(df$scup_keep, na.rm = TRUE)) + 2
      max_rel  <- round(max(df$scup_rel,  na.rm = TRUE)) + 6
      
      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_keep_obj <- get_rep_stat(~scup_keep, rep_design)
      rep_rel_obj  <- get_rep_stat(~scup_rel,  rep_design)
      
      rep_means_keep <- rep_keep_obj$reps
      rep_means_rel  <- rep_rel_obj$reps
      
      # Replicate variances
      rep_vars_keep <- get_rep_var(df, "scup_keep", rep_design)
      rep_vars_rel  <- get_rep_var(df, "scup_rel",  rep_design)
      
      # Negative binomial size parameters
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      
      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
      }
      
      # Fit the copula to a sample (max 5000 obs.) of the original data b/c it can take a while if N is large
      
      df$w_int_rounded <- round(df$wp_int)
      df_expanded <- uncount(df, weights = w_int_rounded) 
      df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), n_sim)) 
      
      
      # Create pseudo-observations (rank-based empirical CDFs)
      df_expanded <- df_expanded %>%
        mutate(
          rank_keep = rank(scup_keep, ties.method = "average"),
          rank_rel  = rank(scup_rel,  ties.method = "average"),
          u_keep = rank_keep / (n() + 1),
          u_rel  = rank_rel / (n() + 1)
        )
      
      # Fit copula using pseudo-observations
      
      u_mat <- cbind(df_expanded$u_keep, df_expanded$u_rel)
      
      # Assess dependence:
      # tau>=0.3: use Gumbel copula
      # tau<=-.3: normal copula, which allows for negative dependence. 
      # -.3>=tau<=.3: frank copula, for moderate, neutral dependence
      
      tau_hat <- cor(u_mat[,1], u_mat[,2], method = "kendall")
      
      copula_fit <- try({
        if (tau_hat >= 0.3) {
          copula::fitCopula(copula::gumbelCopula(dim = 2), u_mat, method = "mpl", start = 1)
        } else if (tau_hat <= -0.3) {
          copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
        } else {
          copula::fitCopula(copula::frankCopula(dim = 2), u_mat, method = "mpl", start = 1)
        }
      }, silent = TRUE)
      
      if (inherits(copula_fit, "try-error")) {
        tau_hat <- 0
        cop_name <- "Gaussian_fallback_independenceish"
        copula_fit <- copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
      }
      
      
      
      # Simulate from the fitted copula
      sim_datasets <- list()
      i <- 1
      while (i <= n_draws) {
        
        sim_u <- rCopula(n_sim, copula_fit@copula)
        
        # Sample mu_keep and mu_rel with uncertainty
        sampled_mu_keep <-  rnorm(1, mu_keep, sqrt(var_keep))
        sampled_mu_rel  <- rnorm(1, mu_rel,  sqrt(var_rel))
        
       if (n_psu(df) <= 1) {
          sampled_theta_keep <- theta_hat_keep_single
          sampled_theta_rel  <- theta_hat_rel_single
        } else {
          sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
          sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
        }
        
        # Convert uniform to NB using quantiles
        sim_keep <- qnbinom(sim_u[,1], size = sampled_theta_keep, mu = sampled_mu_keep)
        
        
        # Convert uniform to NB using quantiles
        sim_rel  <- qnbinom(sim_u[,2], size = sampled_theta_rel, mu = sampled_mu_rel)
        
         if (!any(is.na(sim_keep)) && !any(is.na(sim_rel)) && !any(is.infinite(sim_keep)) && !any(is.infinite(sim_rel))){
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, scup_keep_sim = sim_keep, scup_rel_sim = sim_rel,  my_dom_id_string = my_dom_id_string) 
          
          i <- i + 1  # Only increment if no NaNs
        }
      }
      
      
      # Combine all simulated datasets, tagging each with its simulation ID
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      all_results1[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results1", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
      rm(list = setdiff(ls(), keep))
      
      
    }
    
    final_result1 <- bind_rows(all_results1)
    final_result1 <- final_result1 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup() 
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF","combined_results_BSB")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  ################
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
  
  if (nrow(df_full2) > 0) {
    
    all_results2 <- list()
    
    for (dom in unique(df_full2$my_dom_id_string)) {
      
      df <- df_full2 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_rel  <- survey::svymean(~scup_rel,  svy_design, na.rm = TRUE)
      
      mu_rel   <- as.numeric(coef(mean_rel))
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      
      # Fallback SEs
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sescup_rel, na.rm = TRUE)^2
      }
      
      max_rel  <- round(max(df$scup_rel,  na.rm = TRUE)) + 6
      
      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_rel_obj  <- get_rep_stat(~scup_rel,  rep_design)
      
      rep_means_rel  <- rep_rel_obj$reps
      
      # Replicate variances
      rep_vars_rel  <- get_rep_var(df, "scup_rel",  rep_design)
      
      # Negative binomial size parameters
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      
      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu <- max(1e-8, rnorm(1, mu_rel, sqrt(var_rel)))
        
         if (n_psu(df) <= 1) {
          sampled_theta <- theta_hat_rel_single         # Single-PSU theta
        } else {
          sampled_theta <- sample(rep_theta_rel, 1, replace = TRUE)         # Sample theta
        }
        sim_rel <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
        
        
        if (!any(is.na(sim_rel)) && !any(is.infinite(sim_rel))) {
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, scup_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
          i <- i + 1  # Only increment if no NaNs
        }
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            dplyr::mutate(sim_id = i)
        })
      )
      
      all_results2[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results2", "final_result1", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result2 <- bind_rows(all_results2)
    final_result2 <- final_result2 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number(), scup_keep_sim=0) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2", "df_full1", "df_full2", "df_full3", "df_full4","df_full5",  "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
  if (nrow(df_full3) > 0) {
    
    all_results3 <- list()
    
    for (dom in unique(df_full3$my_dom_id_string)) {
      
      df <- df_full3 %>% filter(my_dom_id_string == dom)
      
      # Define survey design
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      # Point estimates
      mean_keep <- survey::svymean(~scup_keep, svy_design, na.rm = TRUE)

      mu_keep  <- as.numeric(coef(mean_keep))
      var_keep <- as.numeric(attr(mean_keep, "var"))

      # Fallback SEs
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sescup_keep, na.rm = TRUE)^2
      }

      max_keep <- round(max(df$scup_keep, na.rm = TRUE)) + 2

      # Bootstrap replicate design
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      # Replicate means
      rep_keep_obj <- get_rep_stat(~scup_keep, rep_design)

      rep_means_keep <- rep_keep_obj$reps

      # Replicate variances
      rep_vars_keep <- get_rep_var(df, "scup_keep", rep_design)

      # Negative binomial size parameters
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)

      # Better single-PSU / low-PSU fallback
      if (n_psu(df) <= 1) {
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu <- max(1e-8, rnorm(1, mu_keep, sqrt(var_keep)))
        
      if (n_psu(df) <= 1) {
          sampled_theta <- theta_hat_keep_single         # Single-PSU theta
          
        } else {
          sampled_theta <- sample(rep_theta_keep, 1, replace = TRUE)         # Sample theta
          
        }        
        sim_keep <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
        
        if (!any(is.na(sim_keep)) && !any(is.infinite(sim_keep))) {
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, scup_keep_sim = sim_keep, my_dom_id_string = my_dom_id_string)
          i <- i + 1  # Only increment if no NaNs
        }
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      
      all_results3[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results3", "final_result1", "final_result2","n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result3 <- bind_rows(all_results3)
    final_result3 <- final_result3 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number(), scup_rel_sim=0) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3",  "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  ### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
  if (nrow(df_full4) > 0) {
    
    all_results4 <- list()
    
    for (dom in unique(df_full4$my_dom_id_string)) {
      
      sim_datasets <- list()
      i <- 1
      while (i <= n_draws) {
        
        sim_keep <- rep(0, n_sim) 
        sim_rel <- rep(0, n_sim)
        my_dom_id_string <- rep(dom, n_sim)
        
        sim_datasets[[i]] <- data.frame(sim_id = i, scup_keep_sim = sim_keep, scup_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
        i <- i + 1 
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            mutate(sim_id = i)
        })
      )
      
      all_results4[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results4", "final_result1", "final_result2","final_result3", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result4 <- bind_rows(all_results4)
    final_result4 <- final_result4 %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup()
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3", "final_result4","df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
  
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  ### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0 BUT positive values of harvest/discards never occur simultaneously
  if (nrow(df_full5) > 0) {
    
    all_results5 <- list()
    
    for (dom in unique(df_full5$my_dom_id_string)) {
      
      df <- df_full5 %>% filter(my_dom_id_string == dom)
      
      svy_design <- survey::svydesign(
        ids = ~psu_id,
        strata = ~strat_id,
        weights = ~wp_int,
        nest = TRUE,
        data = df
      )
      options(survey.lonely.psu = "certainty")
      
      mean_rel  <- survey::svymean(~scup_rel,  svy_design, na.rm = TRUE)
      mean_keep <- survey::svymean(~scup_keep, svy_design, na.rm = TRUE)
      
      mu_rel  <- as.numeric(coef(mean_rel))
      mu_keep <- as.numeric(coef(mean_keep))
      
      var_rel  <- as.numeric(attr(mean_rel,  "var"))
      var_keep <- as.numeric(attr(mean_keep, "var"))
      
      if (!is.finite(var_rel) || is.na(var_rel) || var_rel <= 0) {
        var_rel <- mean(df$sescup_rel, na.rm = TRUE)^2
      }
      if (!is.finite(var_keep) || is.na(var_keep) || var_keep <= 0) {
        var_keep <- mean(df$sescup_keep, na.rm = TRUE)^2
      }
      
      rep_design <- survey::as.svrepdesign(svy_design, type = "bootstrap", replicates = n_reps)
      
      rep_rel_obj  <- get_rep_stat(~scup_rel,  rep_design)
      rep_keep_obj <- get_rep_stat(~scup_keep, rep_design)
      
      rep_means_rel  <- rep_rel_obj$reps
      rep_means_keep <- rep_keep_obj$reps
      
      rep_vars_rel  <- get_rep_var(df, "scup_rel",  rep_design)
      rep_vars_keep <- get_rep_var(df, "scup_keep", rep_design)
      
      rep_theta_rel  <- safe_theta(rep_means_rel,  rep_vars_rel)
      rep_theta_keep <- safe_theta(rep_means_keep, rep_vars_keep)
      
      if (n_psu(df) <= 1) {
        theta_hat_rel_single  <- safe_theta(mu_rel,  var_rel)
        theta_hat_keep_single <- safe_theta(mu_keep, var_keep)
      }
      
      sim_datasets <- vector("list", n_draws)
      i <- 1
      while (i <= n_draws) {
        
        sampled_mu_rel  <- max(1e-8, rnorm(1, mu_rel,  sqrt(var_rel)))
        sampled_mu_keep <- max(1e-8, rnorm(1, mu_keep, sqrt(var_keep)))
        
        if (n_psu(df) <= 1) {
          sampled_theta_rel  <- theta_hat_rel_single
          sampled_theta_keep <- theta_hat_keep_single
        } else {
          sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
          sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
        }
        
        sim_rel  <- qnbinom(runif(n_sim), size = sampled_theta_rel,  mu = sampled_mu_rel)
        sim_keep <- qnbinom(runif(n_sim), size = sampled_theta_keep, mu = sampled_mu_keep)
        
         if (!any(is.na(sim_keep)) && !any(is.na(sim_rel)) && !any(is.infinite(sim_keep)) && !any(is.infinite(sim_rel))){
          
          ###### REDISTRIBUTE KEEP ########
          excess_keep <- sim_keep[sim_keep > max_keep] - max_keep
          total_excess_keep <- sum(excess_keep)
          
          # Set max values to max_keep
          sim_keep[sim_keep > max_keep] <- max_keep
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_keep_idx <- which(sim_keep > 0 & sim_keep < max_keep)
          if (length(positive_keep_idx) > 0 && total_excess_keep > 0) {
            weights_keep <- sim_keep[positive_keep_idx] / sum(sim_keep[positive_keep_idx])
            sim_keep[positive_keep_idx] <- sim_keep[positive_keep_idx] +
              rmultinom(1, total_excess_keep, prob = weights_keep)
          }
          
          ####### REDISTRIBUTE RELEASE ########
          excess_rel <- sim_rel[sim_rel > max_rel] - max_rel
          total_excess_rel <- sum(excess_rel)
          
          # Set max values to max_rel
          sim_rel[sim_rel > max_rel] <- max_rel
          
          # Identify candidates for redistribution (positive and not at max already)
          positive_rel_idx <- which(sim_rel > 0 & sim_rel < max_rel)
          if (length(positive_rel_idx) > 0 && total_excess_rel > 0) {
            weights_rel <- sim_rel[positive_rel_idx] / sum(sim_rel[positive_rel_idx])
            sim_rel[positive_rel_idx] <- sim_rel[positive_rel_idx] +
              rmultinom(1, total_excess_rel, prob = weights_rel)
          }
          
          
          #sim_keep <- pmin(sim_keep, max_keep*2)
          #sim_rel <- pmin(sim_rel, round(max_rel*2.5))
          
          my_dom_id_string<-dom
          
          sim_datasets[[i]] <- data.frame(sim_id = i, scup_keep_sim = sim_keep, scup_rel_sim = sim_rel,  my_dom_id_string = my_dom_id_string) 
          
          i <- i + 1  # Only increment if no NaNs
        }
        
        
      }
      
      # Combine all simulations
      combined_sim <- bind_rows(
        lapply(seq_along(sim_datasets),  function(i) {
          sim_datasets[[i]] %>%
            dplyr::mutate(sim_id = i)
        })
      )
      
      all_results5[[dom]] <- combined_sim
      keep <- c("df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "all_results5","final_result1", "final_result2","final_result3", "final_result4", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
      rm(list = setdiff(ls(), keep))
    }
    
    final_result5 <- bind_rows(all_results5) %>%
      group_by(my_dom_id_string, sim_id) %>%
      mutate(id = row_number()) %>%
      ungroup()    
  }
  
  # List the objects you want to keep
  keep <- c("final_result1", "final_result2","final_result3", "final_result4","final_result5", "df_full1", "df_full2", "df_full3", "df_full4", "df_full5", "n_sim", "n_draws", "s", "combined_results_SF", "combined_results_BSB")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  
  
  # COMBINE DRAWS ACROSS DOMAINS AND SIMULATIONS
  
  # Initialize an empty list
  results_list <- list()
  
  # Check for each final_results object and add to the list if it exists
  if (exists("final_result1")) results_list <- append(results_list, list(final_result1))
  if (exists("final_result2")) results_list <- append(results_list, list(final_result2))
  if (exists("final_result3")) results_list <- append(results_list, list(final_result3))
  if (exists("final_result4")) results_list <- append(results_list, list(final_result4))
  if (exists("final_result5")) results_list <- append(results_list, list(final_result5))
  
  # Combine all existing results into one data frame
  combined_results_SCUP <- do.call(rbind, results_list)
  
  
  # List the objects you want to keep
  keep <- c("n_sim", "n_draws", "combined_results_BSB", "combined_results_SCUP", "combined_results_SF", "s")
  
  # Remove everything else
  rm(list = setdiff(ls(), keep))
  
  
  # Merge catch outcomes for the three species
  
  catch_draws<- combined_results_SF %>% 
    left_join(combined_results_BSB, by=c("sim_id", "id", "my_dom_id_string")) %>% 
    left_join(combined_results_SCUP, by=c("sim_id", "id", "my_dom_id_string")) 
  
  split_datasets <- split(catch_draws, catch_draws$sim_id)
  
  # Loop over the list and write each one to an Excel file
  
  for (name in names(split_datasets)) {
    
    # Clean name for safe filenames
    safe_name <- gsub("[^A-Za-z0-9_]", "_", name)
    
    # Write to Excel
    write_xlsx(split_datasets[[name]], paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws/proj_catch_draws_", s, "_", safe_name, ".xlsx"))
  }
  
  rm(catch_draws, combined_results_BSB, combined_results_SF, combined_results_SCUP, split_datasets)
}


