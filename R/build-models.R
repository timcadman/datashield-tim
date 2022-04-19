################################################################################
# 3. Function to build model formulae
################################################################################
dt.buildModels <- function(
  avail_exp = NULL, avail_cov = NULL, outcome = "ppd", conns = NULL){
  
  coh <- names(avail_exp)[names(avail_exp) != "variable"]
  vars <- avail_exp$variable
  covs <- avail_cov$variable
  
  exp_coh <- avail_exp %>%
    pmap(function(...){
      
      tmp <- c(...)
      tmp[which(tmp == TRUE)] %>%
        names()
    }) %>%
    set_names(vars) %>%
    map(as_tibble) %>%
    bind_rows(.id = "exposure") %>%
    dplyr::rename(cohort = value)
  
  cov_coh <- coh %>%
    map(function(x){
      avail_cov %>%
        dplyr::select(variable, x) %>%
        dplyr::filter(!!sym(x) == TRUE) %>%
        pull(variable)
    }) %>%
    set_names(coh)
  
  cov_coh_tib <- tibble(
    cohort = coh,
    covariates = cov_coh
  )
  
  if(length(covs) > 1){
    
    out <- left_join(exp_coh, cov_coh_tib, by = "cohort") %>%
      mutate(outcome = outcome)
    
    formulas <- out %>%
      pmap(function(exposure, covariates, outcome, ...){
        
        paste0(
          paste0(outcome, "~", exposure, "+"), 
          paste(unlist(covariates), collapse = "+")
        )
    })
    
    out <- out %>% mutate(formula = formulas)
    
  } else if(length(covs) == 1){
    
    out <- left_join(exp_coh, cov_coh_tib, by = "cohort") %>%
      mutate(
        outcome = outcome,
        formula = paste0(
          outcome, "~", exposure, "+", covariates)
      ) 
  }
  
  out <- out %>%
    dplyr::select(exposure, outcome, covariates, formula, cohort)
  
  return(out)
  
}