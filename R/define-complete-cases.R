#' Defines whether or not each subject is a complete case based on provided variables
#' 
#' @param df serverside dataframe to use
#' @param vars vector of vars to define completeness
#' @param newobj name of new object
#' @param conns datashield connections
#' @importFrom DSI datashield.connections_find
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.asNumeric ds.make ds.replaceNA ds.Boole
#' @importFrom dsHelper dh.tidyEnv
#' @importFrom dplyr %>%
#'
#' @return creates binary serverside variable indicating missingness
#' 
#' @export
dt.defineCompleteCase <- function(
  df = NULL, vars = NULL, newobj = NULL, conns = NULL){

  outcome <- NULL
  
  if (is.null(df)) {
    stop("Please specify a data frame")
  }
  
  if (is.null(vars)) {
    stop("Please specify variables for which to define complete cases")
  }
  
  if (is.null(outcome)) {
    stop("Please specify name for new object")
  }
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  vars %>%
    map(
      ~ds.asNumeric(
        x.name = paste0(df, "$", .), 
        newobj = paste0(., "_fact"))
      )
  
  form <- paste(paste0(vars, "_fact"), collapse = "+")
  
  ds.make(
    toAssign = form,
    newobj = paste0(newobj, "_tmp"), 
    datasources = conns
  )
  
  ds.replaceNA(
    x = paste0(newobj, "_tmp"),
    forNA = rep("-99999", length(conns)),
    newobj = paste0(newobj, "_na"), 
    datasources = conns)
  
  ds.Boole(
    V1 = paste0(newobj, "_na"),
    V2 = "-99999",
    Boolean.operator = ">",
    newobj = newobj,
    datasources = conns
  )
  
  dh.tidyEnv(
    obj = c(paste0(newobj, "_tmp"), paste0(newobj, "_na"))
  )
  
}