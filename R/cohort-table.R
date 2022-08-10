#' Make a summary table with cohort information
#' 
#' @param ref path to .csv file containing information on cohorts. At a 
#' minimum must include a column 'cohort' with cohorts in the same format as 
#' the connections object.
#' @param dataset Character giving the name of the serverside dataset from which
#' to calculate cohort ns
#' @template conns 
#' @importFrom dsBaseClient ds.dim
#' @importFrom readr read_csv
#' @importFrom dplyr %>% bind_rows mutate filter left_join
#' @importFrom purrr map set_names
#' @importFrom tibble as_tibble
#' 
#' @return Tibble containing same columns as input .csv plus column 'cohort_ns'.
#' 
#' @export
dt.cohortTable <- function(ref, dataset, conns){

value <- cohort <- NULL

cohort_details <- read_csv(ref) 

cohort_dims <- ds.dim(dataset)

cohort_ns <- cohort_dims %>%
  map(~.[[1]] %>% as_tibble) %>%
  set_names(c(names(conns), "combined")) %>%
  bind_rows(.id = "cohort")

avail_coh <- names(conns)

out <- left_join(cohort_details, cohort_ns, by = "cohort") %>%
  mutate(cohort_ns = value) %>%
  dplyr::filter(cohort %in% avail_coh)

return(out)
}