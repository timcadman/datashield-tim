#' Function to extract model coefficients into tibble which can be used
#' to make forest plot
#' 
#' @param obj object returned by ds.glmSLMA
#' @param mod list model in standard format. This is just use to get the cohorts
#' which were included in the model. Hopefully in future these will be 
#' extractable from the slma object and this function can be simplified
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% mutate left_join bind_rows
#' 
#' @return tibble in long format with coefficients by study and combined
#' 
#' @export
forestDataSLMA <- function(obj, mod){
  
  cohort <- se <- NULL
  
  ## By study
  betas <- as_tibble(t(obj$betamatrix.valid)) %>%
    mutate(cohort = mod$cohort) %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "beta",
      cols = -cohort)
  
  ses <- as_tibble(t(obj$sematrix.valid)) %>%
    mutate(cohort = mod$cohort) %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "se",
      cols = -cohort)
  
  ## Combined
  beta_comb <- as_tibble(t(obj$SLMA.pooled.ests.matrix[, "pooled.ML"])) %>%
    mutate(cohort = "combined") %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "beta",
      cols = -cohort)
  
  se_comb <- as_tibble(t(obj$SLMA.pooled.ests.matrix[, "se.ML"])) %>%
    mutate(cohort = "combined") %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "se",
      cols = -cohort)
  
  out_sep <- left_join(betas, ses, by = c("cohort", "variable"))
  out_comb <- left_join(beta_comb, se_comb, by = c("cohort", "variable"))
  
  out <- bind_rows(out_sep, out_comb) %>%
    mutate(
      ci_5 = beta - 1.96*se,
      ci_95 = beta + 1.96*se)
  
  return(out)
  
}