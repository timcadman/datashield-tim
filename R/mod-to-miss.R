#' Takes a list of formula components and extracts those needed for missingness diagnostics
#' 
#' @param x list containing 4 elements: exposure, outcome, covariates, cohorts
#'
#' @return a string of the created formula
#' 
#' @export

dt.modToMiss <- function(x){
  
  out <- list(
    vars = list(c(x$exposure, x$outcome, x$covariates)), 
    cohorts = x$cohorts, 
    name = paste0(x$exposure, "_", x$outcome, "_m"))
  
}