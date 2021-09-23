#' Make changes to the parameters specified in initial formula list
#' 
#' @param model list containing four elements (exposure, outcome, covariates, cohort) 
#' @param var variable(s) to add or remove
#' @param type whether to add or remove variables
#' @param category list element to modify. Must be one of the four elements specified above.
#' @importFrom rlang arg_match :=
#' @importFrom purrr map list_modify
#' 
#'
#' @return runs regression model
#' 
#' @export
dt.changeForm <- function(model, var, type = c("add", "remove"), category = c("exposure", "outcome", "covariates", "cohorts")){
  
type <- arg_match(type)
category <- arg_match(category)

  if(type == "remove"){
  
  model %>% map(function(x){list_modify(x, !!category := x[[category]][!x[[category]] %in% var])})
  } else if(type == "add"){

  model %>% map(function(x){list_modify(x, !!category := c(x[[category]], var))})
  }
  
}