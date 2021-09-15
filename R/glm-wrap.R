#' Wrapper for the glm function to save repeating arguments when using many times in the same script
#' 
#' @param mod output from function ds.makeGlmForm
#' @param type type of analysis (either "ipd" or "slma") 
#' @param dummy_suff suffix used for dummy variables in case of ipd
#' @param df serverside dataframe
#' @param conns datashield connections object
#' @importFrom dsBaseClient ds.glm ds.glmSLMA
#'
#' @return runs regression model
#' 
#' @export
dt.glmWrap <- function(mod, type = c("ipd", "slma"), dummy_suff = "_dummy", df = "analysis_df", conns = NULL){
  
  type <- arg_match(type)

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if(type == "ipd"){
    
    out <- ds.glm(
      formula = mod$model,
      data = df, 
      family = "gaussian", 
      datasources = conns[mod$cohorts])
    
  }
  
  
  else if(type == "slma"){
    
    out <- ds.glmSLMA(
      formula = mod$model,
      dataName = df, 
      family = "gaussian",
      datasources = conns[mod$cohorts])
  }
  
  return(out)
}
