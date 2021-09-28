#' Wrapper for the glm function to save repeating arguments when using many times in the same script
#' 
#' @param mod output from function ds.makeGlmForm
#' @param type type of analysis (either "ipd" or "slma") 
#' @param df serverside dataframe
#' @param conns datashield connections object
#' @importFrom dsBaseClient ds.glm ds.glmSLMA
#'
#' @return runs regression model
#' 
#' @export
dt.glmWrap <- function(mod, type = c("ipd", "slma"), df = "analysis_df",
                       family = c("gaussian", "binomial"), conns = NULL){
  
  type <- arg_match(type)
  family <- arg_match(family)

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if(type == "ipd"){
    
    out <- ds.glm(
      formula = mod$model,
      data = df, 
      family = family, 
      datasources = conns[mod$cohorts])
    
  }
  
  else if(type == "slma"){
    
    out <- ds.glmSLMA(
      formula = mod$model,
      dataName = df, 
      family = family,
      datasources = conns[mod$cohorts])
  }
  
  return(out)
}
