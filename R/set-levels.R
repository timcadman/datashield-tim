dt.setLevels <- function(out, ref){
  
  left_join(full_values.ref, out, 
            by = c("variable", "category", "cat_label")) %>% 
    pull(category) %>% 
    unique
  
} 