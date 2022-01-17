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
dt.changeForm <- function(model = NULL, elements = NULL, vars = NULL, 
                          type = NULL, category = NULL){
  
  if (is.null(model)) {
    stop("Please specify a model")
  }
  
  if (is.null(elements)) {
    stop("Please specify name of list element(s) to modify")
  }
  
  if (is.null(var)) {
    stop("Please specify variable to be added or removed")
  }
  
## ---- Check values provide to elements argument are names of list element ----
dont_exist <- elements[elements %in% names(model) == FALSE]
  
if(length(dont_exist) > 0){
  
  stop(paste0("The following strings provided to the 'elements' argument are not
       names of components of the provided list:  ",  dont_exist))
}  
  
type <- arg_match(type, c("add", "remove"))
category <- arg_match(category, c("exposure", "outcome", "covariates", "cohorts"))

## ---- Get original order of list so it can be returned in the same order -----
name_order <- names(model)

## ---- Separate elements to be changed or not ---------------------------------
to_change <- model[elements]
no_change <- model[model != elements]


## ---- Do the business --------------------------------------------------------
if(type == "remove"){
  
  to_change %<>% map(function(x){list_modify(x, !!category := x[[category]][!x[[category]] %in% vars])})
} else if(type == "add"){
  
  to_change %<>% map(function(x){list_modify(x, !!category := c(x[[category]], vars))})
}


## ---- Return list with changed elements --------------------------------------
out <- c(to_change, no_change)
out <- out[name_order]

return(out)
  
}