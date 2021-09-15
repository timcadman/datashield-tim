#' Converts a list of formula components into a formula which can be used in 
#' a glm.
#' 
#' @param x list containing 4 elements: exposure, outcome, covariates, cohorts
#' @param type type of model formula will be used for: either "ipd" or "slma"
#' @param dummy_suff the suffix used by dummy variables
#' @param data the dataframe to be used in the models
#' @importFrom rlang arg_match
#'
#' @return a string of the created formula
#' 
#' @export
ds.makeGlmForm <- function(x, type = c("ipd", "slma"), dummy_suff = "_dummy", data = "analysis_df"){
  
  mod <- . <- NULL
  
  type <- arg_match(type)
  
  if(type == "ipd"){
    
    if(length(x$covariates) == 0){
      
      mod <- list(
        model = paste0(
          x$outcome, "~", x$exposure, "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
        cohorts = x$"cohorts"
      )
    } else if(length(x$covariates) >0){
      
      mod <- list(
        model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+"), 
                       "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
        cohorts = x$"cohorts"
      )
      
    }
  }
  
  else if(type == "slma"){
    
    if(length(x$covariates) == 0){
      mod <- list(
        model = paste0(x$outcome, "~", x$exposure), 
        cohorts = x$cohorts
      )
      
    } else if(length(x$covariates) > 0){
      mod <- list(
        model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+")), 
        cohorts = x$cohorts
      )
      
    }
    
  }
  
  return(mod)
}





